#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>
#include <strings.h>

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


mem_t AllocBigArray_GenPara(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj;
  int tagByteLen = spec->byteLen + 4;
  Align_t align = (spec->type == DoubleField) ? OddWordAlign : NoWordAlign; /* Since there is one tag word */

  /* Allocate the space */
  if (spec->type == PointerField) {
    region = AllocFromHeap(fromSpace, thread, tagByteLen, align);
    if (region == NULL) {
      GCFromC(thread, tagByteLen + 4, 1);
      region = AllocFromHeap(fromSpace, thread, tagByteLen, align);
    }
  }
  else {
    region = gc_large_alloc(proc, tagByteLen, align);
    if (region == NULL) {
      GCFromC(thread, 4, 1);       /* not bytelen since object is not from tenured space */
      region = gc_large_alloc(proc, tagByteLen, align);
    }
  }
  assert(region != NULL);

  /* Allocate object; update stats; initialize */
  obj = region + 1;
  proc->majorUsage.bytesAllocated += tagByteLen;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : init_parray(obj, spec->elemLen, spec->pointerVal); 
                        pushStack(&proc->rootVals, spec->pointerVal);        
                        pushStack(proc->primaryReplicaObjFlips, obj);        
                        break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Generational collector --------------------- */
void GCRelease_GenPara(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->minorUsage.bytesAllocated += alloc;
}

int GCTry_GenPara(Proc_t *proc, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo, minOffRequest);

  if (th->requestInfo > 0) {
    GetHeapArea(nursery,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
    if (proc->allocStart) {
      if (diag) 
	printf("Proc %d: Grabbed %d page(s) at %d\n",proc->procid,roundSize/pagesize,proc->allocStart);
      return 1;
    }
  }
  else if (th->requestInfo < 0) {
    unsigned int bytesAvailable = sizeof(val_t) * (proc->writelistEnd - proc->writelistCursor);
    return ((-th->requestInfo) <= bytesAvailable);
  }
  else 
    assert(0);
  return 0;
}

/* These are barrier synchronizations which reset the previous one when completed.
   So, after barrier_n is passed, all processors reset barrier_(n-1).  For this
   cyclical scheme to work, there must be at least three barriers. 
*/
static long numWaitProc = 0;   /* waiting for mutators to stop and first processor to finish prelim work */
static long numRootProc = 0;   /* waiting for computation of roots and GCtype */
static long numWorkProc = 0;   /* waiting for roots to be forwarded and shared work to complete */
static long numDoneProc = 0;   /* waiting for large area flush, heap resize, space flip before resuming mutators */

void GCStop_GenPara(Proc_t *proc)
{
  int isFirst = 0;                    /* Am I the first processor? */
  static long req_size;            /* These are shared across processors. */
  Thread_t *curThread = NULL;

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     prelimiary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  isFirst = (asynchReachBarrier(&numWaitProc)) == 0;
  if (isFirst) {
    paranoid_check_all(nursery, fromSpace, NULL, NULL, largeSpace);
    /* A Major GC is forced if the tenured space is potentially too small */
    if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery)) 
      GCType = Major;
    else 
      GCType = Minor;
    req_size = 0;
    resetSharedStack(workStack, NumProc);
    ResetJob();                        /* Reset counter so all user threads are scanned */
    asynchReachBarrier(&numWaitProc);  /* First processor is counted twice. */
  }
  /* Wait for all threads to reach this point; note that the first thread is counted twice */
  if (diag)
    printf("Proc %d: waiting for %d procs to stop mutator %s\n",
	   proc->procid, (NumProc + 1) - numWaitProc, isFirst ? "First" : "");
  while (!(asynchCheckBarrier(&numWaitProc, NumProc + 1, &numDoneProc)))
    ;

  /* Get local ranges ready for use; check local stack empty; reset root lists */
  assert(isEmptyStack(&proc->minorStack));
  assert(isEmptyStack(&proc->majorStack));
  resetStack(proc->roots);

  /* All processors compute thread-specific roots in parallel
     and determine whether a major GC has been requested. */
  procChangeState(proc, GCStack);
  while ((curThread = NextJob()) != NULL) {
    /* If negative, requestnfo signifies full writelist */
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(proc,curThread);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC)  /* Upgrade to major GC */
      GCType = Major;      
    if (diag)
      printf("Proc %d:    computed roots of userThread %d\n", proc->procid,curThread->tid);      
  }
  asynchReachBarrier(&numRootProc);

  proc->gcSegment1 = (GCType == Minor) ? MinorWork : MajorWork;
  proc->gcSegment2 = FlipBoth;

  /* The "first" GC processor is in charge of the globals but must wait 
     until all threads are processed before knowing if GC is major. */
  procChangeState(proc, GCGlobal);
  if (isFirst) {
    while (!(asynchCheckBarrier(&numRootProc, NumProc, &numWaitProc)))
      ;
    if (GCType == Minor)
      minor_global_scan(proc);
    else {
      int toSpaceSize = Heap_GetMaximumSize(toSpace);
      int maxLive = (sizeof (val_t)) * (fromSpace->cursor - fromSpace->bottom) +
	            (sizeof (val_t)) * (nursery->top - nursery->bottom);
      if (maxLive >= toSpaceSize) {
	printf("WARNING at GC %d: failure possible since maxPossibleLive = %d > toSpaceSize = %d\n",
	       NumGC, maxLive, toSpaceSize);
	Heap_Resize(toSpace, toSpaceSize, 1);
      }    
      else
	Heap_Resize(toSpace, maxLive, 1);
      major_global_scan(proc);
      gc_large_startCollect();
    }
    asynchReachBarrier(&numRootProc);            /* First processor is counted twice */
  }
  while (!(asynchCheckBarrier(&numRootProc, NumProc + 1, &numWaitProc)))    /* GCType will be correct past the barrier */
    ;

  procChangeState(proc, GC);
  if (GCType == Major)
    proc->gcSegment1 = MajorWork;

  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  if (GCType == Minor) 
    add_writelist_to_rootlist(proc, &nursery->range, &fromSpace->range);
  else
    discard_writelist(proc);

  if (GCType == Minor) {
    if (IsNullCopyRange(&proc->minorRange))   /* First minor GC (after a major GC) */
      SetCopyRange(&proc->minorRange, proc, fromSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
  }
  else 
    SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
  
  /* Now forward all the roots which initializes the local work stacks */
  proc->numRoot += lengthStack(proc->roots);
  while (!isEmptyStack(proc->roots)) {
      ploc_t root = (ploc_t) popStack(proc->roots);
      if (GCType == Minor) 
	locCopy1_copyCopySync_primaryStack(proc,root,&proc->minorStack,&proc->minorRange,&nursery->range);
      else
	locCopy2L_copyCopySync_primaryStack(proc,root,&proc->majorStack,&proc->majorRange,&nursery->range,&fromSpace->range,&largeSpace->range);
  }
  while (!isEmptyStack(&proc->rootVals)) {
      ptr_t rootVal = (ptr_t) popStack(&proc->rootVals);
      if (GCType == Minor) 
	copy1_copyCopySync_primaryStack(proc,rootVal,&proc->minorStack,&proc->minorRange,&nursery->range);
      else
	copy2L_copyCopySync_primaryStack(proc,rootVal,&proc->majorStack,&proc->majorRange,&nursery->range,&fromSpace->range,&largeSpace->range);
  }
  if (GCType == Minor)
    while (!isEmptyStack(proc->primaryReplicaObjFlips)) {
      /* Not transferScanObj_* since this object is a primaryReplica.
	 Since this is a stop-copy collector, we can use _locCopy_ immediately */
      ptr_t obj = popStack(proc->primaryReplicaObjFlips);
      scanObj_locCopy1_copyCopySync_primaryStack(proc, obj, &proc->minorStack,&proc->minorRange,&nursery->range);
    }
  else
    resetStack(proc->primaryReplicaObjFlips);

  /* Move everything from local stack to global stack to balance work; note the omitted popSharedStack */
  pushSharedStack(workStack, 
		  &proc->rootVals,
		  (GCType == Minor) ? &proc->minorStack : &proc->majorStack,
		  (GCType == Minor) ? &proc->minorSegmentStack : &proc->majorSegmentStack);

  /* Get work from global stack; operate on local stack; put work back on global stack */
  while (1) {
    int i, globalEmpty;
    popSharedStack(workStack, 
		   &proc->rootVals, 0,
		   (GCType == Minor) ? &proc->minorStack : &proc->majorStack, objFetchSize,
		   (GCType == Minor) ? &proc->minorSegmentStack : &proc->majorSegmentStack, 0);
    if (GCType == Minor) {
      for (i=0; i < localWorkSize; i++) {
	ptr_t gray = popStack(&proc->minorStack);
	if (gray == NULL)
	  break;
	(void) transferScanObj_locCopy1_copyCopySync_primaryStack(proc,gray,&proc->minorStack,&proc->minorRange,&nursery->range);
      }
    }
    else {
      for (i=0; i < localWorkSize; i++) {
	ptr_t gray = popStack(&proc->majorStack);
	if (gray == NULL)
	  break;
	(void) transferScanObj_locCopy2L_copyCopySync_primaryStack(proc,gray,&proc->majorStack,&proc->majorRange,
								  &nursery->range,&fromSpace->range,&largeSpace->range);
      }
    }
    globalEmpty = pushSharedStack(workStack, 
				  &proc->rootVals, 
				  (GCType == Minor) ? &proc->minorStack : &proc->majorStack,
				  (GCType == Minor) ? &proc->minorSegmentStack : &proc->majorSegmentStack);
    if (globalEmpty)
      break;
  }
  assert(isEmptyStack(&proc->minorStack));
  assert(isEmptyStack(&proc->majorStack));


  if (GCType == Major) {                          /* Carry over minor ranges across minor GCs */
    ClearCopyRange(&proc->minorRange);
    ClearCopyRange(&proc->majorRange);
  }

  /* Wait for all active threads to reach this point so all forwarding is complete */
  if (diag)
    printf("Proc %d: waiting for %d procs to finish collecting\n",proc->procid, 
	   NumProc - numWorkProc);
  synchBarrier(&numWorkProc, NumProc, &numRootProc);


  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    double liveRatio = 0.0;
    if (GCType == Minor)
      paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
    else
      paranoid_check_all(nursery, fromSpace, toSpace, NULL, largeSpace);

    /* Globals, statistics, resize/reset/flip */
    if (GCType == Minor) {
      minor_global_promote();
      liveRatio = (double) (fromSpace->cursor - fromSpace->prevCursor) / (double) (nursery->cursor - nursery->bottom); 
      fromSpace->prevCursor = fromSpace->cursor;
      add_statistic(&proc->minorSurvivalStatistic, liveRatio);
    }
    else {
      gc_large_endCollect();
      liveRatio = HeapAdjust2(req_size,nursery,fromSpace,toSpace);
      add_statistic(&proc->majorSurvivalStatistic, liveRatio);
      Heap_Resize(fromSpace, 0, 1);
      typed_swap(Heap_t *, fromSpace, toSpace);
      NumMajorGC++;
    }
    Heap_Reset(nursery);
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  assert(proc->writelistCursor == proc->writelistStart);

  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting for %d threads to sync on space flip\n",proc->procid, 
	   NumProc - numDoneProc);
  synchBarrier(&numDoneProc, NumProc, &numWorkProc);
}

void GCPoll_GenPara(Proc_t *proc)
{
  if (numWaitProc)
    GCStop_GenPara(proc);
}

void GCInit_GenPara() 
{
  /* secondary cache size */
  int cache_size = GetBcacheSize();
  init_int(&YoungHeapByte, (int)(0.85 * cache_size));
  
  init_int(&MaxHeap, 128 * 1024);
  init_int(&MinHeap, 1024);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  init_double(&MinRatio, 0.2);
  init_double(&MaxRatio, 0.8);
  init_int(&MinRatioSize, 512);
  init_int(&MaxRatioSize, 50 * 1024);
  assert(MinHeap >= 1.2*(YoungHeapByte / 1024));
  nursery = Heap_Alloc(YoungHeapByte, YoungHeapByte);
  fromSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  gc_large_init();
  workStack = SharedStack_Alloc(0, 16384, 1024);
}

