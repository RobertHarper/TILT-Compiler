#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "gc_para.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "platform.h"
#include "client.h"
#include "show.h"
#include "gc_large.h"


static Heap_t *nursery = NULL;
static Heap_t *tenuredFrom = NULL, *tenuredTo = NULL;

/* ------------------  Generational array allocation routines ------------------- */

static mem_t alloc_big(int byteLen, int hasPointers)
{
  mem_t region = 0;
  Thread_t *curThread = getThread();
  unsigned long *saveregs = curThread->saveregs;
  int wordLen = byteLen / (sizeof (val_t));
  int request = RoundUp(byteLen, pagesize);

  /* Should be at least 0.5K to be considered big */
  assert(byteLen >= 512);                
  assert(wordLen * (sizeof (val_t)) == byteLen);

  /* Get the large region */
  if (hasPointers) {
    if (saveregs[ALLOCPTR] + byteLen <= saveregs[ALLOCLIMIT]) {
      region = (mem_t) saveregs[ALLOCPTR];
      saveregs[ALLOCPTR] = (unsigned long) (region + wordLen);
    }
  }
  else
    region = gc_large_alloc(byteLen);

  /* If allocation failed, perform GC and try again */
  if (region == NULL) {
    GCFromC(curThread, 4, 1);
    if (hasPointers) {
      if (saveregs[ALLOCPTR] + byteLen <= saveregs[ALLOCLIMIT]) {
	region = (mem_t) saveregs[ALLOCPTR];
	saveregs[ALLOCPTR] = (unsigned long) (region + wordLen);
      }
    }
    else
      region = gc_large_alloc(byteLen);
  }
  
  assert(region != NULL);
  assert(saveregs[ALLOCPTR] <= saveregs[ALLOCLIMIT]);

  /* Update Statistics */
  gcstat_normal(byteLen, 0, 0);
  return region;
}


ptr_t alloc_bigintarray_GenPara(int elemLen, int initVal, int ptag)
{
  /* elemements are byte-sized */
  int wordLen = 1 + (elemLen + 3) / 4;
  mem_t space = alloc_big(4 * wordLen,0);
  ptr_t res = space + 1;
  init_iarray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigptrarray_GenPara(int elemLen, ptr_t initVal, int ptag)
{
  int wordLen = 1 + elemLen;
  mem_t space = alloc_big(4 * wordLen,1);
  ptr_t res = space + 1;
  init_parray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigfloatarray_GenPara(int elemLen, double initVal, int ptag)
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
    region[wordLen-1] = SKIP_TAG  | (1 << SKIPLEN_OFFSET);
    res = region;
  }
  init_farray(res, elemLen, initVal);
  return res;
}

/* --------------------- Generational collector --------------------- */



int GCTry_GenPara(SysThread_t *sysThread, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo,pagesize);
  mem_t tmp_alloc, tmp_limit;

  if (th->requestInfo == 0)
    return 0;
  GetHeapArea(nursery,roundSize,&tmp_alloc,&tmp_limit);
  if (tmp_alloc) {
    if (diag) 
      printf("Proc %d: Grabbed %d page(s) at %d\n",sysThread->stid,roundSize/pagesize,tmp_alloc);
    sysThread->allocStart = tmp_alloc;
    sysThread->allocCursor = tmp_alloc;
    sysThread->allocLimit = tmp_limit;
    return 1;
  }
  return 0;
}

/* These are barrier synchronizations which reset the previous one when completed.
   So, after barrier_n is passed, all processors reset barrier_(n-1).  For these
   cyclical scheme to work, there must be at least three barriers. 
*/
static long synch1 = 0;   /* waiting for mutators to stop and first processor to finish prelim work */
static long synch2 = 0;   /* waiting for processors to process threads so that
			     thread roots are moved to shared area and GCtype */
static long synch3 = 0;   /* waiting for first processor, using GCtype, to 
			     set to_alloc_start, compute global roots, and transfer to shared area */
static long synch4 = 0;   /* waiting for main collection to complete */
static long synch5 = 0;   /* waiting for large area flush, heap resize, space flip before resuming mutators */

void GCStop_GenPara(SysThread_t *sysThread)
{
  int isFirst = 0;                    /* Am I the first processor? */
  mem_t to_alloc = 0, to_limit = 0;   /* Processor-specific allocation pointers */
  mem_t to_alloc_start = NULL;        /* Used only by "first" thread */
  unsigned int allocated = 0;
  unsigned int copied = 0;
  unsigned int writes = (sizeof (val_t)) * (sysThread->writelistCursor - 
					    sysThread->writelistStart);
  range_t nurseryRange, tenuredFromRange, tenuredToRange, largeRange;
  static long req_size;            /* These are shared across processors. */
  Thread_t *curThread = NULL;

  /* start timer */
  start_timer(&sysThread->gctime);
  GCStatus = GCOn;

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     prelimiary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  isFirst = (asynchReachBarrier(&synch1)) == 0;
  if (isFirst) {
    /* A Major GC is forced if the tenured space is potentially too small */
    if ((tenuredFrom->top - tenuredFrom->alloc_start) < 
	2 * (nursery->top - nursery->bottom))
      GCType = Major;    /* ForcedMajor */
    else 
      GCType = Minor;
    allocated = (sizeof (val_t)) * (nursery->top - nursery->alloc_start);  
    req_size = 0;
    ResetJob();                        /* Reset counter so all user threads are scanned */
    asynchReachBarrier(&synch1);       /* First processor is counted twice. */
  }
  /* Wait for all threads to reach this point; note that the first thread is counted twice */
  if (diag)
    printf("Proc %d: waiting for %d systhreads to stop mutator %s\n",
	   sysThread->stid, (NumSysThread + 1) - synch1, isFirst ? "First" : "");
  while (!(asynchCheckBarrier(&synch1, NumSysThread + 1, &synch5)))
    ;

  /* Get local ranges ready for use; check local stack empty; reset root lists */
  SetRange(&nurseryRange, nursery->bottom, nursery->top);
  SetRange(&tenuredFromRange, tenuredFrom->bottom, tenuredFrom->top);
  SetRange(&tenuredToRange, tenuredTo->bottom, tenuredTo->top);
  SetRange(&largeRange, large->bottom, large->top);
  assert(sysThread->LocalCursor = 0);
  QueueClear(sysThread->root_lists);

  /* All processors compute thread-specific roots in parallel
     and determine whether a major GC has been requested. */
  while ((curThread = NextJob()) != NULL) {
    assert(curThread->requestInfo >= 0);
    FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(sysThread,curThread,nursery);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC) 
      GCType = Major;      
    if (diag)
      printf("Proc %d:    computed roots of userThread %d\n", sysThread->stid,curThread->tid);      
  }
  asynchReachBarrier(&synch2);

  /* The "first" GC processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. */
  if (isFirst) {
    while (!(asynchCheckBarrier(&synch2, NumSysThread, &synch1)))
      ;
    to_alloc_start = (GCType == Minor) ? tenuredFrom->alloc_start : tenuredTo->alloc_start;
    if (GCType == Minor)
      minor_global_scan(sysThread);
    else {
      int tenuredToSize = (sizeof (val_t)) * (tenuredTo->top - tenuredTo->bottom);
      int maxLive = (sizeof (val_t)) * (tenuredFrom->alloc_start - tenuredFrom->bottom) +
	            (sizeof (val_t)) * (nursery->top - nursery->bottom);
      if (maxLive >= tenuredToSize) {
	printf("WARNING at GC %d: failure possible since maxPossibleLive = %d > toSpaceSize = %d\n",
	       NumGC, maxLive, tenuredToSize);
	Heap_Resize(tenuredTo, tenuredToSize);
      }    
      else
	Heap_Resize(tenuredTo, maxLive);
      Heap_Unprotect(tenuredTo);
      major_global_scan(sysThread);
    }
    asynchReachBarrier(&synch2);            /* First processor is counted twice */
  }
  while (!(asynchCheckBarrier(&synch2, NumSysThread + 1, &synch1)))
    ;

  /* Now forward all the roots which initializes the local work stacks */
  while (!QueueIsEmpty(sysThread->root_lists)) {
    /* Cannot dequeue from roots since this may be a global */
    Queue_t *roots = (Queue_t *) Dequeue(sysThread->root_lists);
    int i, len = QueueLength(roots);
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      if (GCType == Minor) 
	forward1_coarseParallel_stack(root,&to_alloc,&to_limit,tenuredFrom,&nurseryRange,sysThread);
      else
	forward2_coarseParallel_stack(root,&to_alloc,&to_limit,tenuredTo,&nurseryRange,
			      &tenuredFromRange,&largeRange,sysThread);
    }
    if (GCType == Minor) 
      forward1_writelist_coarseParallel_stack(&to_alloc, &to_limit,
				      tenuredFrom, &nurseryRange, &tenuredFromRange, sysThread);
  }

  /* Move everything from local stack to global stack to balance work */
  SynchStart();
  SynchMid();
  moveToGlobalStack(sysThread->LocalStack, &(sysThread->LocalCursor));
  SynchEnd();


  /* Get work from global stack; operate on local stack; put work back on global stack 
     If global stack is empty between SynchEnd and SynchStart, then we are done. 
     We have to check that synch2 is high enough so we know all threads have reached
     the point where all work is shared.  */
  asynchReachBarrier(&synch3);
  if (diag)
    printf("Proc %d: Entering global state\n",sysThread->stid);
  while (!(asynchCheckBarrier(&synch3, NumSysThread, &synch2))) {
    if (!(isEmptyGlobalStack())) {
      int i, numToFetch = 10;
      SynchStart();
      fetchFromGlobalStack(sysThread->LocalStack, &(sysThread->LocalCursor), numToFetch);
      /* Work on up to 10 items */
      for (i=0; i < 10 && sysThread->LocalCursor > 0; i++) {
	loc_t grayCell = (loc_t)(sysThread->LocalStack[--sysThread->LocalCursor]);
	if (GCType == Minor)
	  scan1_object_coarseParallel_stack(grayCell,&to_alloc,&to_limit,tenuredFrom,
				    &nurseryRange,&tenuredToRange,sysThread);
	else 
	  scan2_object_coarseParallel_stack(grayCell,&to_alloc,&to_limit,tenuredTo,
				    &nurseryRange,&tenuredFromRange,&tenuredToRange,sysThread);
      }
      SynchMid();
      moveToGlobalStack(sysThread->LocalStack, &(sysThread->LocalCursor));
      SynchEnd();
    }
  }
  if (GCType != Minor)
    gc_large_addRoots(sysThread->largeRoots);

  if (paranoid)   /* Zero out rest of region */
    if (to_alloc < to_limit)
      *to_alloc = SKIP_TAG | ((to_limit - to_alloc) << SKIPLEN_OFFSET);

  /* Wait for all active threads to reach this point so all forwarding is complete */
  if (diag)
    printf("Proc %d: waiting for %d systhreads to finish collecting\n",sysThread->stid, 
	   NumSysThread - synch4);
  synchBarrier(&synch4, NumSysThread, &synch3);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    Heap_t *froms[3] = {NULL, NULL, NULL};
    Heap_t *to = (GCType == Minor) ? tenuredFrom : tenuredTo;
    long alloc = (sizeof (val_t)) * (nursery->top - nursery->bottom);
    long copied = (GCType == Minor) ?
                  (sizeof (val_t)) * (to_alloc_start - to->alloc_start) :
                  (sizeof (val_t)) * (to->alloc_start - to->bottom);
    froms[0] = nursery;
    if (GCType != Minor)
      froms[1] = tenuredFrom;

    if (paranoid) {
      if (GCType == Minor)
	paranoid_check_all(nursery, tenuredFrom, tenuredFrom, NULL);
      else
	paranoid_check_all(nursery, tenuredFrom, tenuredTo, NULL);
      bzero((char *)nursery->bottom, (sizeof (val_t)) * (nursery->top - nursery->bottom));
      if (GCType != Minor)
	bzero((char *)tenuredFrom->bottom, (sizeof (val_t)) * (tenuredFrom->top - tenuredFrom->bottom));
    }
    
    /* Resize heaps and do stats */
    gcstat_normal(alloc,copied,0);
    nursery->alloc_start = nursery->bottom;
    if (GCType != Minor) {
      gc_large_flush();
      HeapAdjust(0,req_size,froms,tenuredTo);
      typed_swap(Heap_t *, tenuredFrom, tenuredTo);
      NumMajorGC++;
    }
    NumGC++;
  }


  /* Each thread has a separate write list */
  gcstat_normal(0,0, sysThread->writelistCursor - sysThread->writelistStart);

  /* All system threads need to reset their limit pointer */
  sysThread->allocStart = StartHeapLimit;
  sysThread->allocCursor = StartHeapLimit;
  sysThread->allocLimit = StartHeapLimit;
  sysThread->writelistCursor = sysThread->writelistStart;

  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting for %d threads to sync on space flip\n",sysThread->stid, 
	   NumSysThread - synch4);
  synchBarrier(&synch5, NumSysThread, &synch4);

  /* stop timer */
  GCStatus = GCOff;
  stop_timer(&sysThread->gctime); 
}

void gc_poll_GenPara(SysThread_t *sth)
{
  if (synch1)
    GCStop_GenPara(sth);
}

void gc_init_GenPara() 
{
  /* secondary cache size */
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
  gc_large_init(1);
}


void gc_finish_GenPara()
{
  Thread_t *th = getThread();
  unsigned int allocsize = (unsigned int)(th->saveregs[ALLOCPTR]) - 
                           (unsigned int)(nursery->alloc_start);
  gcstat_normal(allocsize,0,0);
}
