/* Not thread-safe */

#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "stack.h"
#include "thread.h"
#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "global.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "show.h"


static Heap_t *fromheap = NULL, *toheap = NULL;


/* ------------------  Semispace array allocation routines ------------------- */

static mem_t alloc_big(int byteLen, int hasPointers)
{
  ptr_t res = 0;
  Thread_t *curThread = getThread();
  unsigned long *saveregs = curThread->saveregs;
  mem_t alloc = (mem_t) saveregs[ALLOCPTR];
  mem_t limit = (mem_t) saveregs[ALLOCLIMIT];
  int wordLen = byteLen / (sizeof (val_t));

  /* Make sure there is enough space */
  if (alloc + wordLen > limit) {
    GCFromC(curThread, byteLen, 0);
    alloc = (mem_t) saveregs[ALLOCPTR];
    limit = (mem_t) saveregs[ALLOCLIMIT];
    assert(alloc + wordLen <= limit);
  }

  assert(wordLen * (sizeof (val_t)) == byteLen);
  assert(alloc + wordLen < limit);

  /* Perform allocation */
  res = alloc;
  alloc += wordLen;
  saveregs[ALLOCPTR] = (unsigned long) alloc;

  /* Update statistics */
  gcstat_normal(byteLen, 0, 0);

  return res;
}

ptr_t alloc_bigintarray_Semi(int elemLen, int initVal, int ptag)
{
  /* elemements are byte-sized */
  int wordLen = 1 + (elemLen + 3) / 4;
  mem_t space = alloc_big(4 * wordLen,0);
  ptr_t res = space + 1;
  init_iarray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigptrarray_Semi(int elemLen, ptr_t initVal, int ptag)
{
  int wordLen = 1 + elemLen;
  mem_t space = alloc_big(4 * wordLen,1);
  ptr_t res = space + 1;
  init_parray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigfloatarray_Semi(int elemLen, double initVal, int ptag)
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

/* --------------------- Semispace collector --------------------- */
int GCTry_Semi(SysThread_t *sysThread, Thread_t *th)
{
  assert(sysThread->userThread == NULL);
  assert(th->sysThread == NULL);
  if (th->requestInfo == 0)  /* write list request */
    return 0;
  assert(th->requestInfo > 0);
  if (sysThread->allocLimit == StartHeapLimit) {
    unsigned int bytesAvailable;
    sysThread->allocStart = fromheap->bottom;
    sysThread->allocCursor = fromheap->bottom;
    sysThread->allocLimit = fromheap->top;
    bytesAvailable = (((unsigned int)sysThread->allocLimit) - 
		      ((unsigned int)sysThread->allocStart));
    return (th->requestInfo <= bytesAvailable);
  }
  return 0;
}

void GCStop_Semi(SysThread_t *sysThread)
{
  int i;  
  Thread_t *oneThread;
  int bytesRequested;
  mem_t allocCursor = sysThread->allocCursor;
  mem_t allocLimit = sysThread->allocLimit;
  mem_t to_ptr = toheap->bottom;
  range_t from_range, to_range;
  Queue_t *root_lists = sysThread->root_lists;

  /* Start timing this collection */
  start_timer(&sysThread->gctime);
  GCStatus = GCOn;
  oneThread = &(Threads[0]);             /* In a sequential collector, 
					    there is only one user thread */
  bytesRequested = oneThread->requestInfo;

  /* Check that processor is unmapped, write list is not overflowed, allocation region intact */
  assert(sysThread->userThread == NULL);
  assert(allocCursor <= allocLimit);
  assert(bytesRequested >= 0);

  fromheap->alloc_start = allocCursor;

  if (paranoid) 
    paranoid_check_all(fromheap, NULL, NULL, NULL);

  /* Compute the roots from the stack and register set */
  QueueClear(sysThread->root_lists);
  local_root_scan(sysThread,oneThread,fromheap);
  major_global_scan(sysThread);
    
  /* Get tospace and ranges ready for the collection */
  Heap_Resize(toheap, fromheap->top - fromheap->bottom);
  Heap_Unprotect(toheap);
  SetRange(&from_range, fromheap->bottom, fromheap->top);
  SetRange(&to_range, toheap->bottom, toheap->top);


  /* forward the roots */
  to_ptr = forward1_root_lists(root_lists, to_ptr, 
			       &from_range, &to_range);

  /* perform a Cheney scan */
  to_ptr = scan1_until(to_range.low,to_ptr,&from_range,&to_range);
  toheap->alloc_start = to_ptr;

  if (debug && SHOW_HEAPS) {
	memdump("From Heap After collection:", fromheap->bottom,40,0);
        memdump("To Heap After collection:", toheap->bottom,40,0);
	show_heap_raw("FINAL FROM",(allocCursor - fromheap->bottom),
		      fromheap->bottom,fromheap->top,
		      toheap->bottom,toheap->top);
  }

  if (paranoid) 
    paranoid_check_all(fromheap, NULL, toheap, NULL);

  gcstat_normal((sizeof (val_t)) * (allocCursor - fromheap->alloc_start),
		(sizeof (val_t)) * (to_ptr - toheap->bottom),
		(sysThread->writelistCursor - sysThread->writelistStart));

  /* Resize the tospace, discard old space, flip space */
  {
    Heap_t *froms[2] = {NULL, NULL};
    froms[0] = fromheap;
    HeapAdjust(0, bytesRequested, froms, toheap);
    Heap_Protect(fromheap);
    typed_swap(Heap_t *, fromheap, toheap);
  }

  /* Update systhread's allocation variables */
  fromheap->alloc_start = to_ptr;
  sysThread->allocStart = fromheap->alloc_start;
  sysThread->allocCursor = fromheap->alloc_start;
  sysThread->allocLimit = fromheap->top;
  sysThread->writelistCursor = sysThread->writelistStart;

  NumGC++;
  GCStatus = GCOff;

  /* Stop timer for collection */
  stop_timer(&sysThread->gctime);
}


void gc_init_Semi()
{
  init_int(&MaxHeap, 80 * 1024);
  init_int(&MinHeap, 256);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  init_double(&MinRatio, 0.1);
  init_double(&MaxRatio, 0.7);
  init_int(&MinRatioSize, 512);         
  init_int(&MaxRatioSize, 50 * 1024);
  fromheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
}


void gc_finish_Semi()
{
  Thread_t *th = getThread();
  int allocsize = th->saveregs[ALLOCPTR] - (unsigned int)(fromheap->alloc_start);
  gcstat_normal(allocsize,0,0);
}
