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



/* ------------------  Generational array allocation routines ------------------- */

static mem_t alloc_big(int byteLen, int hasPointers)
{
  mem_t region = 0;
  Thread_t *curThread = getThread();
  Proc_t *proc = curThread->proc;
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
    region = gc_large_alloc(proc,byteLen);

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
      region = gc_large_alloc(proc,byteLen);
  }
  
  assert(region != NULL);
  assert(saveregs[ALLOCPTR] <= saveregs[ALLOCLIMIT]);

  /* Update Statistics */
  gcstat_normal(proc,byteLen, 0, 0);
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
void GCRelease_GenPara(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  gcstat_normal(proc, alloc, 0, 0);
}

int GCTry_GenPara(Proc_t *proc, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo,pagesize);

  if (th->requestInfo > 0) {
    GetHeapArea(nursery,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
    if (proc->allocStart) {
      if (diag) 
	printf("Proc %d: Grabbed %d page(s) at %d\n",proc->stid,roundSize/pagesize,proc->allocStart);
      return 1;
    }
  }
  else if (th->requestInfo < 0) {
    unsigned int bytesAvailable = (((unsigned int)proc->writelistEnd) - 
				   ((unsigned int)proc->writelistCursor));
    return ((-th->requestInfo) <= bytesAvailable);
  }
  else 
    assert(0);
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
			     set to_cursor_first, compute global roots, and transfer to shared area */
static long synch4 = 0;   /* waiting for main collection to complete */
static long synch5 = 0;   /* waiting for large area flush, heap resize, space flip before resuming mutators */

void GCStop_GenPara(Proc_t *proc)
{
  int isFirst = 0;                    /* Am I the first processor? */
  mem_t to_cursor_first = NULL;       /* Used only by "first" thread */
  int copied = 0, write = (proc->writelistCursor - proc->writelistStart) / 2;
  static long req_size;            /* These are shared across processors. */
  Thread_t *curThread = NULL;

  GCStatus = GCOn;

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     prelimiary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  isFirst = (asynchReachBarrier(&synch1)) == 0;
  if (isFirst) {
    if (paranoid) 
      paranoid_check_all(nursery, tenuredFrom, NULL, NULL);
    /* A Major GC is forced if the tenured space is potentially too small */
    if (Heap_GetAvail(tenuredFrom) < 2 * Heap_GetSize(nursery)) 
      GCType = Major;
    else 
      GCType = Minor;
    req_size = 0;
    ResetJob();                        /* Reset counter so all user threads are scanned */
    asynchReachBarrier(&synch1);       /* First processor is counted twice. */
  }
  /* Wait for all threads to reach this point; note that the first thread is counted twice */
  if (diag)
    printf("Proc %d: waiting for %d procs to stop mutator %s\n",
	   proc->stid, (NumProc + 1) - synch1, isFirst ? "First" : "");
  while (!(asynchCheckBarrier(&synch1, NumProc + 1, &synch5)))
    ;

  /* Get local ranges ready for use; check local stack empty; reset root lists */
  assert(proc->localStack.cursor == 0);
  QueueClear(proc->root_lists);

  /* All processors compute thread-specific roots in parallel
     and determine whether a major GC has been requested. */
  while ((curThread = NextJob()) != NULL) {
    /* If negative, requestnfo signifies full writelist */
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(proc,curThread);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC) 
      GCType = Major;      
    if (diag)
      printf("Proc %d:    computed roots of userThread %d\n", proc->stid,curThread->tid);      
  }
  asynchReachBarrier(&synch2);

  /* The "first" GC processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. */
  if (isFirst) {
    while (!(asynchCheckBarrier(&synch2, NumProc, &synch1)))
      ;
    to_cursor_first = (GCType == Minor) ? tenuredFrom->cursor : tenuredTo->cursor;
    if (GCType == Minor)
      minor_global_scan(proc);
    else {
      int tenuredToSize = Heap_GetMaximumSize(tenuredTo);
      int maxLive = (sizeof (val_t)) * (tenuredFrom->cursor - tenuredFrom->bottom) +
	            (sizeof (val_t)) * (nursery->top - nursery->bottom);
      if (maxLive >= tenuredToSize) {
	printf("WARNING at GC %d: failure possible since maxPossibleLive = %d > toSpaceSize = %d\n",
	       NumGC, maxLive, tenuredToSize);
	Heap_Resize(tenuredTo, tenuredToSize, 1);
      }    
      else
	Heap_Resize(tenuredTo, maxLive, 1);
      major_global_scan(proc);
    }
    asynchReachBarrier(&synch2);            /* First processor is counted twice */
  }
  while (!(asynchCheckBarrier(&synch2, NumProc + 1, &synch1)))
    ;

  if (GCType == Minor)
    SetCopyRange(&proc->minorRange, proc, tenuredFrom, expandWithPad, dischargeWithPad);
  else 
    SetCopyRange(&proc->majorRange, proc, tenuredTo, expandWithPad, dischargeWithPad);
  
  /* Now forward all the roots which initializes the local work stacks */
  while (!QueueIsEmpty(proc->root_lists)) {
    /* Cannot dequeue from roots since this may be a global */
    Queue_t *roots = (Queue_t *) Dequeue(proc->root_lists);
    int i, len = QueueLength(roots);
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      if (GCType == Minor) 
	forward1_coarseParallel_stack(root,&proc->minorRange,&nursery->range,proc);
      else
	forward2_coarseParallel_stack(root,&proc->majorRange,&nursery->range,&tenuredFrom->range,&large->range,proc);
    }
    proc->numRoot += len;
  }
  if (GCType == Minor) 
    forward1_writelist_coarseParallel_stack(&proc->minorRange, &nursery->range, &tenuredFrom->range, proc);
  else
    discard_writelist(proc);

  /* Move everything from local stack to global stack to balance work */
  SynchStart(workStack);
  SynchMid(workStack);
  moveToGlobalStack(workStack,&proc->localStack);
  SynchEnd(workStack,NULL);


  /* Get work from global stack; operate on local stack; put work back on global stack 
     If global stack is empty between SynchEnd and SynchStart, then we are done. 
     We have to check that synch2 is high enough so we know all threads have reached
     the point where all work is shared.  */
  asynchReachBarrier(&synch3);
  if (diag)
    printf("Proc %d: Entering global state\n",proc->stid);
  while (1) {
    int i, numToFetch = 10, numToWork = 20;
    if (asynchCheckBarrier(&synch3, NumProc, &synch2) &&
	isEmptyGlobalStack(workStack))
      break;
    /* Stack may be empty at this point */
    SynchStart(workStack);
    fetchFromGlobalStack(workStack,&proc->localStack, numToFetch);
    for (i=0; i < numToWork && proc->localStack.cursor > 0; i++) {
      int bytesCopied;
      loc_t grayCell = (loc_t)(proc->localStack.stack[--proc->localStack.cursor]);
      if (GCType == Minor)
	(void) scan1_object_coarseParallel_stack(grayCell,&proc->minorRange,&nursery->range,&tenuredFrom->range,proc,&bytesCopied);
      else 
	(void) scan2_object_coarseParallel_stack(grayCell,&proc->majorRange,&nursery->range,&tenuredFrom->range,&large->range,proc,&bytesCopied);
    }
    SynchMid(workStack);
    moveToGlobalStack(workStack,&proc->localStack);
    SynchEnd(workStack,NULL);
  }
  assert(isEmptyGlobalStack(workStack));
  if (GCType != Minor)
    gc_large_addRoots(proc->largeRoots);

  if (GCType == Minor)
    ClearCopyRange(&proc->minorRange);
  else
    ClearCopyRange(&proc->majorRange);

  /* Wait for all active threads to reach this point so all forwarding is complete */
  if (diag)
    printf("Proc %d: waiting for %d procs to finish collecting\n",proc->stid, 
	   NumProc - synch4);
  synchBarrier(&synch4, NumProc, &synch3);
  gcstat_normal(proc, 0, copied, write);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    Heap_t *froms[3] = {NULL, NULL, NULL};
    Heap_t *to = (GCType == Minor) ? tenuredFrom : tenuredTo;
    long alloc = (sizeof (val_t)) * (nursery->top - nursery->bottom);
    long copied = (GCType == Minor) ?
                  (sizeof (val_t)) * (to_cursor_first - to->cursor) :
                  (sizeof (val_t)) * (to->cursor - to->bottom);
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
    
    if (GCType == Minor)
      minor_global_promote();

    /* Resize heaps and do stats */
    gcstat_normal(proc,alloc,copied,0);
    nursery->cursor = nursery->bottom;
    if (GCType != Minor) {
      gc_large_flush();
      HeapAdjust(0,req_size,froms,tenuredTo);
      typed_swap(Heap_t *, tenuredFrom, tenuredTo);
      NumMajorGC++;
    }
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  assert(proc->writelistCursor == proc->writelistStart);

  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting for %d threads to sync on space flip\n",proc->stid, 
	   NumProc - synch4);
  synchBarrier(&synch5, NumProc, &synch4);

  GCStatus = GCOff;
}

void gc_poll_GenPara(Proc_t *sth)
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
  workStack = SharedStack_Alloc(8192);
}

