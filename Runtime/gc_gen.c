/* Not thread-safe */
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "gc_large.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "platform.h"
#include "client.h"
#include "show.h"

/* ------------------  Generational array allocation routines ------------------- */

static mem_t alloc_big(int byteLen, int hasPointers)
{
  mem_t region = NULL;
  Thread_t *curThread = getThread();
  unsigned long *saveregs = curThread->saveregs;
  int wordLen = byteLen / (sizeof (val_t));

  /* Should be at least 0.5K to be considered big */
  assert(byteLen >= 512);                
  assert(wordLen * (sizeof (val_t)) == byteLen);

  /* Try to allocate */
  if (hasPointers) {
    if (tenuredFrom->cursor + wordLen <= tenuredFrom->top) {
      region = tenuredFrom->cursor;
      tenuredFrom->cursor += wordLen;
    }
  }
  else
    region = gc_large_alloc(byteLen);

  /* If allocation failed, perform GC and try again */
  if (region == NULL) {
    GCFromC(curThread, 4, 1);
    if (hasPointers) {
      if (tenuredFrom->cursor + wordLen <= tenuredFrom->top) {
	region = tenuredFrom->cursor;
	tenuredFrom->cursor += wordLen;
      }
    }
    else
      region = gc_large_alloc(byteLen);
  }

  assert(region != NULL);
  assert(tenuredFrom->cursor < tenuredFrom->top);

  /* Update Statistics */
  gcstat_normal(byteLen, 0, 0);
  return region;
}


ptr_t alloc_bigintarray_Gen(int elemLen, int initVal, int ptag)
{
  /* elemements are byte-sized */
  int wordLen = 1 + (elemLen + 3) / 4;
  mem_t space = alloc_big(4 * wordLen,0);
  ptr_t res = space + 1;
  init_iarray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigptrarray_Gen(int elemLen, ptr_t initVal, int ptag)
{
  int wordLen = 1 + elemLen;
  mem_t space = alloc_big(4 * wordLen,1);
  ptr_t res = space + 1;
  init_parray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigfloatarray_Gen(int elemLen, double initVal, int ptag)
{
  ptr_t res = NULL;
  mem_t region = NULL;
  Thread_t *curThread = getThread();
  int wordLen = 2 + (elemLen << 1);  /* Includes one word for alignment */

  region = alloc_big(4 * wordLen,0);
  if ((((val_t)region) & 7) != 0) {
    region[0] = SKIP_TAG | (1 << SKIPLEN_OFFSET);
    res = region + 1;
  }
  else {
    region[wordLen-1] = SKIP_TAG | (1 << SKIPLEN_OFFSET);
    res = region;
  }
  init_farray(res, elemLen, initVal);
  return res;
}

/* --------------------- Generational collector --------------------- */

int GCTry_Gen(SysThread_t *sysThread, Thread_t *th)
{
  assert(sysThread->userThread == NULL);
  assert(th->sysThread == NULL);
  if (sysThread->allocLimit == StartHeapLimit) {
    sysThread->allocStart = nursery->bottom;
    sysThread->allocCursor = nursery->bottom;
    sysThread->allocLimit = nursery->top;
    nursery->cursor = nursery->top;
  }
  if (th->requestInfo > 0) {
    unsigned int bytesAvailable = (val_t) sysThread->allocLimit - 
				  (val_t) sysThread->allocCursor;
    return (th->requestInfo <= bytesAvailable);
  }
  else if (th->requestInfo < 0) {
    unsigned int bytesAvailable = (val_t) sysThread->writelistEnd - 
                                  (val_t) sysThread->writelistCursor;
    return ((-th->requestInfo) <= bytesAvailable);
  }
  else 
    assert(0);
  return 0;
}

void GCStop_Gen(SysThread_t *sysThread)
{
  Thread_t *oneThread = &(Threads[0]);     /* In a sequential collector, 
					      there is only one user thread */
  int req_size = 0;
  Queue_t *root_lists = sysThread->root_lists;
  Queue_t *largeRoots = sysThread->largeRoots;
  unsigned int allocated = (sizeof (val_t)) * (nursery->top - nursery->cursor);
  unsigned int copied = 0;

  if (oneThread->requestInfo > 0)
    req_size = oneThread->requestInfo;
    
  /* A Major GC is forced if the tenured space is potentially too small.  Start timer. */
  GCType = Minor;
  if (oneThread->request == MajorGCRequestFromC)
    GCType = Major;
  else if ((tenuredFrom->top - tenuredFrom->cursor) < 
	   (nursery->top - nursery->bottom))
    GCType = Major;                    /* Forced Major */
  start_timer(&sysThread->gctime);
  GCStatus = GCOn;
  if (GCType != Minor)
    start_timer(&(sysThread->majorgctime));

  /* Sanity checks */
  assert((0 <= req_size) && (req_size < pagesize));
  assert(nursery->cursor     <= nursery->top);
  assert(tenuredFrom->cursor <= tenuredFrom->top);
  assert(tenuredTo->cursor   <= tenuredTo->top);
  /* At this point, the nursery and tenured from-space may have cross-pointers */
  if (paranoid) 
    paranoid_check_all(nursery, tenuredFrom, NULL, NULL);

  /* Compute stack/register/global roots to sysThread->root_lists. */
  QueueClear(sysThread->root_lists);
  local_root_scan(sysThread,oneThread);
  if (GCType == Minor)
    minor_global_scan(sysThread);
  else
    major_global_scan(sysThread);

  /* Perform just a minor GC */
  if (GCType == Minor) {

    /* --- forward the roots and the writelist; then Cheney-scan until no gray objects */
    mem_t tenuredFromAlloc = tenuredFrom->cursor;
    tenuredFromAlloc = forward1_root_lists(root_lists, tenuredFromAlloc,
					   &nursery->range, &tenuredFrom->range);
    tenuredFromAlloc = forward1_writelist(sysThread, tenuredFromAlloc, 
					  &nursery->range, &tenuredFrom->range);
    tenuredFromAlloc = scan1_until(tenuredFrom->cursor,tenuredFromAlloc,
				   &nursery->range, &tenuredFrom->range);
    copied = (sizeof (val_t)) * (tenuredFromAlloc - tenuredFrom->cursor),
    tenuredFrom->cursor = tenuredFromAlloc;
  }
  
  else if (GCType == Major) {

    int tenuredToSize = Heap_GetSize(tenuredTo);
    int maxLive = (sizeof (val_t)) * (tenuredFrom->cursor - tenuredFrom->bottom) +
                  (sizeof (val_t)) * (nursery->top - nursery->bottom);
    mem_t tenuredToAlloc = tenuredTo->bottom;
    Heap_t *fromHeaps[3] = {NULL, NULL, NULL};
    fromHeaps[0] = nursery;
    fromHeaps[1] = tenuredFrom;

    /* Write list can be ignored */
    discard_writelist(sysThread);

    /* Resize tospace so live data fits, if possible */
    if (maxLive >= tenuredToSize) {
      printf("WARNING: GC failure possible since maxPossibleLive = %d > toSpaceSize = %d\n",
	     maxLive, tenuredToSize);
      Heap_Unprotect(tenuredTo, tenuredToSize);
    }    
    else
      Heap_Unprotect(tenuredTo, maxLive);

    /* do the normal roots; writelist can be skipped on a major GC;
       then the usual Cheney scan followed by sweeping the large-object region */
    tenuredToAlloc = forward2_root_lists(root_lists, tenuredToAlloc, 
					 &nursery->range, &tenuredFrom->range, &tenuredTo->range,
					 &large->range, largeRoots);
    tenuredToAlloc = scan2_region(tenuredTo->bottom,tenuredToAlloc,tenuredTo->range.high,
				  &nursery->range,&tenuredFrom->range,&tenuredTo->range,
				  &large->range, largeRoots);
    gc_large_addRoots(largeRoots);
    gc_large_flush();
    copied = (sizeof (val_t)) * (tenuredToAlloc - tenuredTo->bottom),
    tenuredTo->cursor = tenuredToAlloc;

    /* Resize the tenured area now, discard old space, flip space  */
    HeapAdjust(1, req_size, fromHeaps, tenuredTo);
    typed_swap(Heap_t *, tenuredFrom, tenuredTo);

  }
  else
    assert(0);

  /* Update sole thread's allocation pointers and root lists */
  QueueClear(largeRoots);
  QueueClear(root_lists);
  sysThread->allocStart = nursery->bottom;
  sysThread->allocCursor = nursery->bottom;
  sysThread->allocLimit = nursery->top;
  assert(sysThread->writelistCursor == sysThread->writelistStart);

  /* Sanity checks afterwards */
  Heap_Check(nursery);
  Heap_Check(tenuredFrom);
  Heap_Check(tenuredTo);
  if (paranoid) {
    paranoid_check_all(nursery, tenuredFrom, tenuredFrom, NULL);
    bzero((char *) nursery->bottom, (sizeof (val_t)) * (nursery->top - nursery->bottom));
  }

  /* stop timer and update counts */
  gcstat_normal(allocated, copied, 0);
  if (GCType != Minor) {
    NumMajorGC++;
    stop_timer(&sysThread->majorgctime);
  }
  GCStatus = GCOff;
  NumGC++;
  stop_timer(&sysThread->gctime); 
}


void gc_init_Gen() 
{
  int cache_size = GetBcacheSize();

  init_int(&YoungHeapByte, (int)(0.85 * cache_size));
  init_int(&MaxHeap, 80 * 1024);
  init_int(&MinHeap, 1024);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  init_double(&MinRatio, 0.2);
  init_double(&MaxRatio, 0.8);
  init_int(&MinRatioSize, 512);
  init_int(&MaxRatioSize, 50 * 1024);
  assert(MinHeap >= 1.2*(YoungHeapByte / 1024));

  nursery = Heap_Alloc(YoungHeapByte, YoungHeapByte);
  tenuredFrom = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  tenuredTo = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  gc_large_init(0);
}


void gc_finish_Gen()
{
  Thread_t *th = getThread();
  unsigned int allocsize = (unsigned int)(th->saveregs[ALLOCPTR]) - 
                           (unsigned int)(nursery->cursor);
  gcstat_normal(allocsize,0,0);
}
