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
#include "show.h"
#include "platform.h"
#include "forward.h"

/* Shared Stack and Local Stacks
   -----------------------------
   It is tricky to know when a minor collection is over.  Generally,
   the collection is over when the local stacks and the global stack
   are simultaneously empty.  To implement this, the shared global
   stack structure keeps an integer count that indicates how many
   local stacks are (possibly) non-empty.  Basically, collection work
   follows the form:

      popSharedObjStack       (* increments activeStack *)
      ... do some work on local stack ...
      if (pushSharedObjStack)   (* decrements activeStack;
         collector off;             return 1 if shared stack is empty and activeStack is 0 *)

   Initialization of the global shared stack from the local stacks is
   done with specialReturnSharedObjStack:

      ... find roots and generate work on local stack ...
      initializeSharedObjStack    (* does not decrement activeStack *)

   Thus, after initialization, the activeStack count is accurate.
   However, a processor, when processing the write list and
   replicating the most recently allocated memory region, can generate
   work on its local stack (rather than from the global stack).  We
   can try to retain the accuracy of activeStack as follows:

   (1) ... local stack empty here ...
      popSharedObjStack zero items  
       ... replicate page / process write list generating work on local stack ...
      pushSharedObjStack              

   The problem with (1) is subtle: the collection may not terminate.
   When all the live data has been copied, but prior to the the
   collector being signalled off, one processor might have been just
   returning from mutation work.  It replicates the recently allocated
   region, generating work on the local stack, and then the global
   stack.  When the collector tries to turn off, the global stack is
   not empty and so all processsors continue to run more mutators.
   Though unlikely, this can go on indefinitely.  In some sense the
   problem is that we are not differentiating the data that is live at
   the beginning of the collection from data that was allocated during
   collection.  Once the first type of data has been processed, the
   collection is (or must soon be) over since the amount allocated per
   round is bounded.

   Since we do not want this second type of work to influence activeStack
   and hence termination, we could try:

   (2) ... local stack empty here ...
       ... replicate page / process write list generating work on local stack ...
       initializeSharedObjStack    

   The problem with (2) is that once termination is signalled by another processor,
   we do not want to return work to the shared stack.  We can thus test for whether 
   the collector is off first.

   (3) ... local stack empty here ...
       ... replicate page / process write list generating work on local stack ...
       if collector is off
         then initializeSharedObjStack    
	 else complete all local work (bounded in size)

   The check is, however, not properly synchronized with the collection being turned off.
   It might be that the check was made before the collector is turned off.  This could
   lead to the collector being turned off, followed by more work appearing on the global 
   stack.  The only safe place to check is inside the shared stack routines
   which can assure the proper synchronization. So the final version for
   transferring work is to have

   (4) ... local stack empty here ...
       ... replicate page / process write list generating work on local stack ...
       if conditionalReturnToSharedObjStack  (* if global stack empty and activeStack is 0,
          then ;                               then return false
	  else complete all local work         else transfer objects; return true
					     does not decrement activeStack *)

   The follownig is relevant to both parallel and concurrent collectors.
   In order for a garbage collection to start or finish, all the processors must
   execute the code that initializes the global stack before checking
   that the global stack is empty,   This forces an extra synchronization
   in the collector code, preventing any collector from doing work
   until all the collectors have initialized the global stack.  That is,

   Phase 1:
     if (isFirstProcessor)
       ... get from/to spaces ready ...
   Phase 2:
     ... compute roots ...
     initializeSharedObjStack
   Phase 3:                       <--- must insure all work is on global stack
     ... do work ...

   A more elegant and efficient solution is to replace initializeSharedObjStack 
   with resetSharedObjStack which simply updates activeStack with the given number.  
   The initialization code then becomes:
   
   Phase 1:
     if (isFirstProcessor)
       ... get from/to spaces ready ...
       startSharedObjStack(numProc)   <--- makes shared stack aware that local stacks are not empty
   Phase 2:
                             <-- FetchFromSharedObjStack must be omitted for correct numActiveStack 
     ... compute roots ...
     pushSharedObjStack
     ... do work ...

   How much space to reserve
   -------------------------
   To make sure the garbage collection does not fall behind, we observe that
   we need to reserve 1/(1+k) of the heap space if the collection rate is k (> 1).
   For example, if we copy 2 bytes for every byte allocated (k = 2),
   then the collector will reserve 1/3 of the heap space so collection will 
   commence when the haep is 2/3 used.  This simple analysis is incomplete
   as it ignores the work associated with handling the stack and globals,
   which have potentially non-constants costs.  For example, it is necessary
   to compute all the global roots on collector startup and to flip all
   global roots upon completion.

   Computation of the root entails examining uninitialized globals for initialized 
   pointer locations.  Although any one global can be initialized only once,
   all the initialization can occur within one GC.  Even if the globals are 
   uninitialized, one must examine them to verify that this is the case.
   Since globals are not read-only, we must capture all the roots without
   any mutations occurring or else some live data may become unreachable by the
   collector.


*/


/* ------------------  Parallel array allocation routines ------------------- */

mem_t AllocBigArray_SemiConc(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj;
  int i, segments = (spec->byteLen <= arraySegmentSize) ? 0 : DivideUp(spec->byteLen, arraySegmentSize);
  int tagByteLen = spec->byteLen + 4 + 4 * segments;
  Align_t align = (spec->type == DoubleField) ? 
                        ((segments & 1) ? EvenWordAlign : OddWordAlign) : 
                        NoWordAlign; 

  /* Allocate the space */
  region = AllocFromThread(thread, tagByteLen, align);
  if (region == NULL) 
    region = AllocFromHeap(fromSpace, thread, tagByteLen, align);
  if (region == NULL) {
    GCFromC(thread, tagByteLen + 4, 0); 
    region = AllocFromThread(thread, tagByteLen, align);
  }
  assert(region != NULL);

  /* Allocate object; initialize segment tags; update stats; initialize */
  obj = region + 1 + segments;
  for (i=0; i<segments; i++)
    obj[-(2+i)] = SEGPROCEED_TAG;
  proc->majorUsage.bytesAllocated += tagByteLen;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : init_parray(obj, spec->elemLen, spec->pointerVal); break;
    case MirrorPointerField : init_double_ptr_array(obj, spec->elemLen, spec->pointerVal); break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Concurrent collector --------------------- */
static long req_size = 0;  /* Total number of bytes requested by all threads */

/* These are barrier synchronizations which reset the previous one when completed.
   So, after barrier_n is passed, all processors reset barrier_(n-1).  For this
   cyclical scheme to work, there must be at least three barriers. 
*/
static long numWaitOnProc = 0;  /* waiting for mutators to stop; first proc does prelim work */
static long numWasteOnProc = 0; /* required by scheme */
static long numDoneOnProc = 0;  /* waiting for work to be on shared stack */

static long numWaitOffProc = 0; /* waiting for mutators to stop; first proc does prelim work */
static long numFlipOffProc = 0; /* waiting for all roots to be replaced with replica */
static long numDoneOffProc = 0; /* waitinf for heap resize/space flip to complete */


static void CollectorOn(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst, rootCount = 0;

  if (diag)
    printf("Proc %d: CollectorOn\n", proc->procid); 

  switch (GCStatus) {
  case GCOff:                   /* Signalling to other processors that collector is turning on */
    GCStatus = GCPendingOn;
    StopAllThreads();
    break;
  case GCPendingOn: break;      /* Responding to signal that collector is turning on */
  case GCOn: assert(0);         /* Collector already on */
  case GCPendingOff: assert(0); /* Someone is turning the collector off while we are turning it on! */
  default: assert(0);
  }

  /* Check local stack empty; reset root lists */
  assert(isEmptyStack(&proc->majorObjStack));
  assert(isEmptyStack(proc->rootLocs));
  SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);

  isFirst = (synchBarrier(&numWaitOnProc, NumProc, &numDoneOnProc)) == 0;
  if (isFirst) {
    int neededSize = Heap_GetSize(fromSpace);
    int pages = Heap_ResetFreshPages(fromSpace);

    proc->segUsage.pagesTouched += pages / 100;   /* These refer to pages of the pageMap itself */
    neededSize += neededSize / majorCollectionRate;
    Heap_Resize(fromSpace,neededSize,0);
    Heap_Resize(toSpace,neededSize,1);
    paranoid_check_all(fromSpace, NULL, NULL, NULL, NULL);
    req_size = 0;
    resetSharedStack(workStack,NumProc);
    procChangeState(proc, GCGlobal);
    major_global_scan(proc);
    procChangeState(proc, GC);
    ResetJob();
    asynchReachBarrier(&numWaitOnProc);
  }
  while (!asynchCheckBarrier(&numWaitOnProc,  NumProc + 1, &numDoneOnProc))
    ;
  procChangeState(proc, GCStack);
  while ((curThread = NextJob()) != NULL) {
    Thread_t *thread = initial_root_scan(proc,curThread);
    if (thread != NULL)
      pushStack(&proc->threads, (ptr_t) thread);
    if (curThread->requestInfo >= 0) /* Allocation request */
      FetchAndAdd(&req_size, curThread->requestInfo);
  }
  procChangeState(proc, GC);

  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);

  /* Note the omission of popSharedStack since we used resetSharedStack */
  pushSharedStack(workStack,&proc->threads,proc->globalLocs,proc->rootLocs,&proc->majorObjStack,&proc->majorSegmentStack);
  assert(isEmptyStack(&proc->majorObjStack));
  GCStatus = GCOn;
  synchBarrier(&numWasteOnProc, NumProc, &numWaitOnProc);
  synchBarrier(&numDoneOnProc, NumProc, &numWasteOnProc);
  flushStore();
}


INLINE(flipRootLoc)
void flipRootLoc(ploc_t root)
{
  ptr_t primary = *root;
  if (inHeap(primary,fromSpace)) {
    ptr_t replica = (ptr_t) primary[-1];
    *root = replica;
  }
  else if (paranoid) {
    assert(IsGlobalData(primary) || 
	   IsTagData(primary) ||
	   inHeap(primary,toSpace));                      /* root was already flipped - duplicate root */
  }
}

static void CollectorOff(Proc_t *proc)
{
  extern int stkSize;
  extern double s1, s2, s3, s4;
  double t1, t2, t3, t4;
  Thread_t *curThread = NULL;
  int isFirst;
  int rootCount = 0;
  ploc_t rootLoc, globalLoc;

  t1 = segmentTime(proc);
  if (diag)
    printf("Proc %d: entered CollectorOff\n", proc->procid);
  assert(isEmptyStack(&proc->majorObjStack));
  flushStore();

  switch (GCStatus) {
  case GCOff: assert(0);       /* Collector already off! */
  case GCPendingOn: assert(0); /* Someone is turning the collector off while we are turning it on! */
  case GCOn:                   /* First to signal turning collector off */
    GCStatus = GCPendingOff;
    flushStore();
    StopAllThreads();
    break;
  case GCPendingOff: break;   /* Responding to someone's signal to turn collector off */
  default: assert(0);
  }

  ClearCopyRange(&proc->majorRange);
  isFirst = (asynchReachBarrier(&numWaitOffProc)) == 0;
  if (isFirst) {
    ResetJob();
    asynchReachBarrier(&numWaitOffProc);
  }
  while (!asynchCheckBarrier(&numWaitOffProc, NumProc+1, &numDoneOffProc))
    ;

  assert(isEmptyStack(proc->rootLocs));

  /* Replace all roots (global, local registers, local stack) with replica */
  procChangeState(proc, GCGlobal);
  if (isFirst) 
    minor_global_scan(proc);                /* Minor scan since the tenured roots are already flipped */
  while (globalLoc = (ploc_t) popStack(proc->globalLocs)) {
    ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
    flipRootLoc(replicaLoc);
  }

  procChangeState(proc, GCStack);
  while ((curThread = NextJob()) != NULL) 
    complete_root_scan(proc, curThread);
  procChangeState(proc, GC);

  t2 = segmentTime(proc);
  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);
  /* Flip stack slots */
  while (rootLoc = (ploc_t) popStack(proc->rootLocs)) 
    flipRootLoc(rootLoc);
  t3 = segmentTime(proc);

  synchBarrier(&numFlipOffProc, NumProc, &numWaitOffProc);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    double liveRatio = 0.0;
    /* Check the fromspace and tospace heap - zero out all of fromspace */
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
    
    /* Resize heaps and do stats */
    liveRatio = HeapAdjust1(req_size,fromSpace,toSpace);
    add_statistic(&proc->majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace, 0, 1);
    typed_swap(int, primaryGlobalOffset, replicaGlobalOffset);
    typed_swap(int, primaryStackletOffset, replicaStackletOffset);
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
    GCStatus = GCOff;
    flushStore();
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  assert(proc->writelistCursor == proc->writelistStart);

  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting to sync on completion of CollectorOff\n",proc->procid);
  flushStore();
  synchBarrier(&numDoneOffProc, NumProc, &numFlipOffProc);
  flushStore();

  t4 = segmentTime(proc);
  /*
  printf("\ncomplete_root_scan: %3.2f + %3.2f + %3.2f = %3.2f ms    size = %d\n", 
	 s2-s1, s3-s2, s4-s3, s4-s1, stkSize);
  printf("CollectorOff %5d: %3.2f + %3.2f + %3.2f + %3.2f = %3.2f ms\n", 
	 proc->segmentNumber, t1, t2-t1, t3-t2, t4-t3, t4);
	 */
}

void GCRelease_SemiConc(Proc_t *proc)
{
  mem_t allocCurrent = proc->allocStart;
  mem_t allocStop = proc->allocCursor;
  ploc_t writelistCurrent = proc->writelistStart;
  ploc_t writelistStop = proc->writelistCursor;
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  int gcOn = (GCStatus == GCOn) || (GCStatus == GCPendingOff);

  proc->allocStart = proc->allocCursor;  /* allocation area is NOT reused */  
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  proc->writelistCursor = proc->writelistStart;  /* write list reused once processed */
  proc->minorUsage.bytesAllocated += alloc;

  if (!gcOn) 
    return;

  if (diag)
    printf("Proc %d: Scanning/Replicating %d to %d\n",proc->procid,allocCurrent,allocStop);
  while (allocCurrent + 1 <= allocStop) { /* There may be no data for empty array */
    int objSize;
    ptr_t obj = allocCurrent;  /* Eventually becomes start of object */
    tag_t tag = *obj;
    if (IS_SKIP_TAG(tag)) {
      allocCurrent += GET_SKIPWORD(tag);
      continue;
    }
    while (tag == SEGPROCEED_TAG || tag == SEGSTALL_TAG)
      tag = *(++obj); /* Skip past segment tags */
    obj++;            /* Skip past object tag */
    objSize = splitAlloc_primaryStack(proc,obj,&proc->majorObjStack,&proc->majorRange,fromSpace);
    if (objSize == 0) {
      mem_t dummy = NULL;
      objSize = objectLength(obj, &dummy);
      assert(dummy == allocCurrent);
    }
    allocCurrent += objSize / sizeof(val_t);
  }

  if (diag)
    printf("Proc %d: Processing writes from %d to %d\n",proc->procid,writelistCurrent,writelistStop);
  while (writelistCurrent < writelistStop) {
    ptr_t primary = *writelistCurrent++, replica;
    int byteDisp = (int) *writelistCurrent++;
    ptr_t possPrevPtrVal = (ptr_t) *writelistCurrent++;  /* Pointer value only if pointer array was modified */
    int wordDisp = byteDisp/ sizeof(val_t);
    int doublewordDisp = byteDisp / (sizeof(double));
    int byteLen;  /* Length of data portion of array */
    tag_t tag;

    if (!inHeap(primary, fromSpace))
      continue;
    splitAlloc1_copyCopySync_primaryStack(proc,primary,&proc->majorObjStack,&proc->majorRange,fromSpace);
    replica = (ptr_t) primary[-1];
    tag = replica[-1];
    byteLen = GET_ANY_ARRAY_LEN(tag);
    if (byteLen <= arraySegmentSize)
      while (primary[-1] == STALL_TAG)
	;
    else {
      int segment = DivideUp(byteDisp, arraySegmentSize);
      while (primary[-2-segment] == SEGSTALL_TAG)
	;
    }
    assert(primaryArrayOffset == 0);
    switch (GET_TYPE(tag)) {
    case PTR_ARRAY_TYPE: 
    case MIRROR_PTR_ARRAY_TYPE: {
      ptr_t primaryField = (ptr_t) primary[wordDisp];
      ptr_t replicaField = primaryField;
      /* Snapshot-at-the-beginning (Yuasa) write barrier requires copying prevPtrVal 
	 even if it might die to prevent the mutator from hiding live data */
      splitAlloc1_copyCopySync_primaryStack(proc,possPrevPtrVal, &proc->majorObjStack, &proc->majorRange, fromSpace);
      locSplitAlloc1_copyCopySync_primaryStack(proc,&replicaField, &proc->majorObjStack, &proc->majorRange, fromSpace);
      replica[wordDisp] = (val_t) replicaField;  /* update replica with replicated object */
      break;
    }
    case WORD_ARRAY_TYPE: {
      int primaryField = (int) primary[wordDisp];
      replica[wordDisp] = primaryField;       /* update replica with primary's non-pointer value */
      break;
    }
    case QUAD_ARRAY_TYPE: {
      double primaryField = ((double *) primary)[doublewordDisp];
      ((double *)replica)[doublewordDisp] = primaryField;  /* update replica with primary's non-pointer value */
      break;
    }
    default: assert(0);
    }
  }
  condPushSharedStack(workStack,&proc->threads,proc->globalLocs,proc->rootLocs,
		      &proc->majorObjStack,&proc->majorSegmentStack);
}


static int intcompare(int *i, int *j)
{
  if (*i > *j)
    return 1;
  if (*i < *j)
    return -1;
  return 0;
}

/* GCStatus is either GCOn or GCPendingOff.
   Upon entry and exit, the local work stack is empty.
   The routine will work up until updateWorkDone() > workToDo is reached.  It may copy slightly more.
   If the local flag is set, then the shared stack is not accessed.
*/
static void do_work(Proc_t *proc, int workToDo, int local)
{
  int counter, start, end;
  ptr_t gray;

  if (!local) {
    assert(isEmptyStack(&proc->majorObjStack));
    assert(isEmptyStack(&proc->majorSegmentStack));
  }
  
  if (local) {
    assert(isEmptyStack(proc->rootLocs));
    assert(isEmptyStack(proc->globalLocs));
    while (!isEmptyStack(&proc->majorSegmentStack)) {
      gray = popStack3(&proc->majorSegmentStack,(ptr_t *)&start,(ptr_t *)&end);
      (void) transferScanSegment_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(proc,gray,start,end,
											&proc->majorObjStack,&proc->majorSegmentStack,
											&proc->majorRange, fromSpace);
    }
    while ((gray = popStack(&proc->majorObjStack)) != NULL) {
      (void) transferScanObj_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(proc,gray,&proc->majorObjStack,&proc->majorSegmentStack,
										    &proc->majorRange,fromSpace);
    }
  }
  else {
    while (updateWorkDone(proc) < workToDo) {
      int i, globalEmpty;
      Thread_t *thread;
      ploc_t rootLoc, globalLoc;

      popSharedStack(workStack, &proc->threads, threadFetchSize,
		     proc->globalLocs, globalLocFetchSize,
		     proc->rootLocs, rootLocFetchSize, 
		     &proc->majorObjStack, objFetchSize, &proc->majorSegmentStack, segFetchSize);
      while (updateWorkDone(proc) < workToDo &&
	     ((thread = (Thread_t *) popStack(&proc->threads)) != NULL)) {
	if (!work_root_scan(proc, thread, workToDo))
	  pushStack(&proc->threads, (ptr_t) thread);
      }
      while (!recentWorkDone(proc, localWorkSize) &&
	     (rootLoc = (ploc_t) popStack(proc->rootLocs)) != NULL) 
	locSplitAlloc1_copyCopySync_primaryStack(proc,rootLoc,&proc->majorObjStack,&proc->majorRange,fromSpace); 
      while (!recentWorkDone(proc, localWorkSize) &&
	     (globalLoc = (ploc_t) popStack(proc->globalLocs)) != NULL) {
	ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	locSplitAlloc1_copyCopySync_primaryStack(proc,replicaLoc,&proc->majorObjStack,&proc->majorRange,fromSpace); 
      }
      while (updateWorkDone(proc) < workToDo) {
	int start, end;
	ptr_t gray = popStack3(&proc->majorSegmentStack,(ptr_t *)&start,(ptr_t *)&end);
	if (gray == NULL)
	  break;
	(void) transferScanSegment_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(proc,gray,start,end,
											  &proc->majorObjStack,&proc->majorSegmentStack,
											  &proc->majorRange, fromSpace);
      }
      while (!recentWorkDone(proc, localWorkSize) && 
	     ((gray = popStack(&proc->majorObjStack)) != NULL)) {
	(void) transferScanObj_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(proc,gray,&proc->majorObjStack,&proc->majorSegmentStack,
										      &proc->majorRange, fromSpace);
      }
      globalEmpty = pushSharedStack(workStack,&proc->threads,proc->globalLocs,proc->rootLocs,
				    &proc->majorObjStack, &proc->majorSegmentStack);
      if (globalEmpty) {
	if (diag)
	  printf("Proc %d: Turning Collector Off\n", proc->procid); 
	GCStatus = GCPendingOff;
	break;
      }
    }
  }
  assert(isEmptyStack(&proc->majorObjStack));
}


static int GCTry_SemiConcHelp(Proc_t *proc, int roundSize)
{
  GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  if (proc->allocStart)
    return 1;
  return 0;
}

int GCTry_SemiConc(Proc_t *proc, Thread_t *th)
{
  int satisfied = 0, requestInfo = th->requestInfo;
  int roundOffSize, roundOnSize, workToDo;

  assert(proc->writelistCursor + 3 <= proc->writelistEnd);
  flushStore();

  if (requestInfo < 0) {
    unsigned int bytesAvailable = sizeof(val_t) * (proc->writelistEnd - proc->writelistCursor);
    assert((-requestInfo) <= bytesAvailable);
    return 1;
  }
  assert(requestInfo > 0);
  roundOffSize = RoundUp(requestInfo, minOffRequest);
  roundOnSize = RoundUp(requestInfo, minOnRequest);
  workToDo = (int) (1.0 + majorCollectionRate) * roundOnSize;  /* + 1 for the work in replicating primary */
  /* XXXXXXXXXXXXXX might fall behind XXXXXXXXX */
  if (roundOnSize > 2 * minOnRequest)
    workToDo = (int) (1.0 + majorCollectionRate) * (2 * minOnRequest);

  while (!satisfied) {
    switch (GCStatus) {
    case GCOff:                                         /* Possible GC states: before; after */
    case GCPendingOn:
      if (GCStatus == GCOff &&
	  GCTry_SemiConcHelp(proc,roundOffSize)) {      /* Off, PendingOn; same */
	do_global_work(proc, workToDo);
	satisfied = 1;
	break;
      }
      procChangeState(proc, GC);
      proc->gcSegment1 = MinorWork;
      proc->gcSegment2 = FlipOn;
      CollectorOn(proc);                                /* Off, PendingOn; On, PendingOff */
      assert(GCStatus == GCOn || GCStatus == GCPendingOff);
      do_work(proc, workToDo, 0);                       /* On, PendingOff; same */
      if ((th == NULL) ||
	  ((th != NULL) && GCStatus == GCOn &&
	   GCTry_SemiConcHelp(proc,roundOnSize))) {     /* On, PendingOff; same */
	satisfied = 1;
	break;
      }
      if (GCStatus == GCPendingOff)                     /* On, PendingOff */
	break;
      printf("Proc %d: Concurrent collector fell too far behind  - check parameters\n", proc->procid);
      assert(0);
      break;
     /* We don't let a collector flip off until the next segment */
     case GCOn:       
       procChangeState(proc, GC);
       proc->gcSegment1 = MinorWork;                  /* On, PendingOff; same */
       do_work(proc, workToDo, 0);                    
       assert(GCTry_SemiConcHelp(proc,roundOnSize));  
       satisfied = 1;
       if (GCStatus == GCPendingOff)
	 assert(isEmptyStack(&proc->majorObjStack));
       assert(GCStatus == GCOn || GCStatus == GCPendingOff);
       break;
     case GCPendingOff:
       procChangeState(proc, GC);
       proc->gcSegment1 = MinorWork;
       proc->gcSegment2 = FlipOff;
       do_work(proc,                                     /* PendingOff; same */ 
	       sizeof(val_t) * Heap_GetSize(fromSpace),  /* Actually bounded by allocation of all procs since GCOff triggered. */
	       1);                                       
       CollectorOff(proc);                               /* PendingOff; Off, PendingOn */
       assert(GCStatus == GCOff || GCStatus == GCPendingOn);
       break;
    default: 
      assert(0);
    }
  }
  return 1;
}

void GCPoll_SemiConc(Proc_t *proc)
{
  switch (GCStatus) {
  case GCOff:
    return;
  case GCPendingOn:
    CollectorOn(proc);
    return;
  case GCOn:
    do_work(proc, (int) majorCollectionRate * minOnRequest, 0);  /* collect as though one page had been allocated */
    return;
  case GCPendingOff:
    do_work(proc,                                     /* PendingOff; same */ 
	    sizeof(val_t) * Heap_GetSize(fromSpace),  /* Actually bounded by allocation of all procs since GCOff triggered. */
	    1);                                       
    CollectorOff(proc);
    return;
  }
}

void GCInit_SemiConc()
{
  printf("temporary stuff in collector\n");
  init_int(&MaxHeap, 128 * 1024);
  init_int(&MinHeap, 256);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  init_double(&MinRatio, 0.1);
  init_double(&MaxRatio, 0.7);
  init_int(&MinRatioSize, 512);         
  init_int(&MaxRatioSize, 50 * 1024);
  fromSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  workStack = SharedStack_Alloc(100, 16 * 1024, 1024, 32 * 1024, 4 * 1024);
  arraySegmentSize = 4 * 1024;
  mirrorGlobal = 1;
}

