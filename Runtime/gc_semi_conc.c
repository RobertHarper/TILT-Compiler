#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>
#include <values.h>

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


/* -------------- Helper Function - should be moved??? XXXXXXXXXXXX --------------- */
INLINE(flipRootLoc)
void flipRootLoc(ploc_t root)
{
  ptr_t primary = *root;
  if (inHeap(primary,fromSpace)) {
    ptr_t replica = (ptr_t) primary[-1];
    *root = replica;
    assert(inHeap(replica,toSpace)); 
  }
  else if (paranoid) {
    assert(IsGlobalData(primary) || 
	   IsTagData(primary) ||
	   inHeap(primary,toSpace));                      /* root was already flipped - duplicate root */
  }
}

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
  proc->segUsage.bytesAllocated += tagByteLen;

  /* Allocate object; initialize segment tags; update stats; initialize */
  obj = region + 1 + segments;
  for (i=0; i<segments; i++)
    obj[-(2+i)] = SEGPROCEED_TAG;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : init_parray(obj, spec->elemLen, spec->pointerVal); break;
    case MirrorPointerField : init_double_ptr_array(obj, spec->elemLen, spec->pointerVal); break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Concurrent collector --------------------- */
static long totalRequest = 0;     /* Total number of bytes requested by all threads */
static long totalReplicated = 0;  /* Total number of bytes replicated by all processors */
static long totalUnused = 0;

static void CollectorOn(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst, rootCount = 0;

  if (collectDiag >= 2)
    printf("Proc %d: CollectorOn\n", proc->procid); 
  
  procChangeState(proc, GC);
  proc->segmentType |= (MajorWork | FlipOn);

  switch (GCStatus) {
  case GCOff:                   /* Signalling to other processors that collector is turning on */
    GCStatus = doAgressive ? GCPendingAgressive : GCPendingOn;
    StopAllThreads();
    break;
  case GCPendingOn:             /* Responding to signal that collector is turning on */
  case GCPendingAgressive: break;
  default: assert(0);
  }

  /* Check local stack empty; reset root lists */
  assert(isEmptyStack(&proc->majorObjStack));
  assert(isEmptyStack(proc->rootLocs));
  SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);

  isFirst = (weakBarrier(barriers, &proc->barrierPhase) == 0);
  if (isFirst) {
    int neededSize = Heap_GetSize(fromSpace);
    int pages = Heap_ResetFreshPages(proc,fromSpace);

    neededSize += neededSize / majorCollectionRate;
    Heap_Resize(fromSpace,neededSize,0);
    Heap_Resize(toSpace,neededSize,1);
    paranoid_check_all(fromSpace, NULL, NULL, NULL, NULL);
    totalRequest = totalReplicated = totalUnused = 0;
    resetSharedStack(workStack,NumProc);
    procChangeState(proc, GCGlobal);
    major_global_scan(proc);
    procChangeState(proc, GC);
    ResetJob();
  }
  strongBarrier(barriers, &proc->barrierPhase);

  procChangeState(proc, GCStack);
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  while ((curThread = NextJob()) != NULL) {
    int started = initial_root_scan(proc,curThread);
    if (started)
      pushStack(&proc->threads, (ptr_t) curThread);
    if (curThread->requestInfo >= 0) /* Allocation request */
      FetchAndAdd(&totalRequest, curThread->requestInfo);
  }
  procChangeState(proc, GC);

  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);
  /* Note the omission of popSharedStack since we used resetSharedStack */
  pushSharedStack(0,workStack,&proc->threads,proc->globalLocs,proc->rootLocs,&proc->majorObjStack,&proc->majorSegmentStack);
  assert(isEmptyStack(&proc->majorObjStack));
  GCStatus = doAgressive ? GCAgressive : GCOn;
  strongBarrier(barriers, &proc->barrierPhase);
}


static void CollectorTransition(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst, rootCount = 0;

  if (collectDiag >= 2)
    printf("Proc %d: CollectorTransition\n", proc->procid); 
  
  procChangeState(proc, GC);
  proc->segmentType |= (MajorWork | FlipTransition);

  switch (GCStatus) {
  case GCAgressive:                 /* Signalling to other processors that collector is turning on */
    GCStatus = GCPendingOn;
    StopAllThreads();
    break;
  case GCPendingOn: break;         /* Responding to signal that collector is turning on */
  default: assert(0);
  }

  isFirst = (weakBarrier(barriers, &proc->barrierPhase) == 0);
  if (isFirst) {
    resetSharedStack(workStack,NumProc);
    procChangeState(proc, GCGlobal);
    major_global_scan(proc);
    procChangeState(proc, GC);
    ResetJob();
  }
  strongBarrier(barriers, &proc->barrierPhase);

  /* Check local stack empty; reset root lists */
  procChangeState(proc, GCStack);
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  while ((curThread = NextJob()) != NULL) {
    discard_root_scan(proc,curThread);
    if (curThread->used == 0)
      continue;
    if (initial_root_scan(proc,curThread))
      pushStack(&proc->threads, (ptr_t) curThread);
    if (curThread->requestInfo >= 0) /* Allocation request */
      FetchAndAdd(&totalRequest, curThread->requestInfo);
  }

  procChangeState(proc, GC);
  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);
  /* Note the omission of popSharedStack since we used resetSharedStack */
  pushSharedStack(0,workStack,&proc->threads,proc->globalLocs,proc->rootLocs,&proc->majorObjStack,&proc->majorSegmentStack);
  assert(isEmptyStack(&proc->majorObjStack));
  GCStatus = GCOn;
  strongBarrier(barriers, &proc->barrierPhase);
}


static void CollectorOff(Proc_t *proc)
{
  extern int stkSize;
  Thread_t *curThread = NULL;
  int isFirst;
  int rootCount = 0;
  ploc_t rootLoc, globalLoc;

  proc->segmentType |= MajorWork | FlipOff;

  if (collectDiag >= 2)
    printf("Proc %d: entered CollectorOff\n", proc->procid);
  assert(isEmptyStack(&proc->majorObjStack));
  flushStore();

  switch (GCStatus) {
  case GCOn:                   /* First to signal turning collector off */
    GCStatus = GCPendingOff;
    flushStore();
    StopAllThreads();
    break;
  case GCPendingOff: break;   /* Responding to someone's signal to turn collector off */
  default: assert(0);
  }

  ClearCopyRange(&proc->majorRange);
  isFirst = (weakBarrier(barriers, &proc->barrierPhase) == 0);
  if (isFirst) 
    ResetJob();
  strongBarrier(barriers, &proc->barrierPhase);

  /* Replace all roots (global, local registers, local stack) with replica */
  assert(isEmptySharedStack(workStack));
  assert(isEmptyStack(proc->rootLocs));
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

  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);
  /* Flip stack slots */
  while (rootLoc = (ploc_t) popStack(proc->rootLocs)) 
    flipRootLoc(rootLoc);
  FetchAndAdd(&totalReplicated, proc->segUsage.bytesReplicated + proc->cycleUsage.bytesReplicated);
  strongBarrier(barriers, &proc->barrierPhase);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    double liveRatio = 0.0;
    /* Check the fromspace and tospace heap - zero out all of fromspace */
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
    
    /* Resize heaps and do stats */
    liveRatio = HeapAdjust1(totalRequest,totalUnused,totalReplicated, 1.0/ (1.0 + majorCollectionRate),
			    fromSpace,toSpace);
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
  proc->allocStart = proc->allocCursor = proc->allocLimit = StartHeapLimit;
  assert(proc->writelistCursor == proc->writelistStart);

  strongBarrier(barriers, &proc->barrierPhase);
}

void GCRelease_SemiConc(Proc_t *proc)
{
  mem_t allocCurrent = proc->allocStart;
  mem_t allocStop = proc->allocCursor;
  ploc_t writelistCurrent = proc->writelistStart;
  ploc_t writelistStop = proc->writelistCursor;
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  int gcAgressive = doAgressive ? (GCStatus == GCAgressive) || (GCStatus == GCPendingOn) : 0;
  int gcOn = gcAgressive || (GCStatus == GCOn) || (GCStatus == GCPendingOff);

  proc->allocStart = proc->allocCursor;  /* allocation area is NOT reused */  
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  proc->writelistCursor = proc->writelistStart;  /* write list reused once processed */
  proc->segUsage.bytesAllocated += alloc;

  if (gcOn && !gcAgressive) {
    if (collectDiag >= 2)
      printf("Proc %d: Scanning/Replicating %d to %d\n",proc->procid,allocCurrent,allocStop);

    procChangeState(proc, GCReplicate);
    proc->segUsage.bytesReplicated += sizeof(val_t) * (allocStop - allocCurrent);
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
      objSize = alloc_copyCopySync_primaryStack(proc,obj,&proc->majorRange,&proc->majorObjStack);
      if (objSize == 0) {
	mem_t dummy = NULL;
	objSize = objectLength(obj, &dummy);
	assert(dummy == allocCurrent);
      }
      allocCurrent += objSize / sizeof(val_t);
    }
  }

  if (collectDiag >= 2)
    printf("Proc %d: Processing writes from %d to %d\n",proc->procid,writelistCurrent,writelistStop);
  procChangeState(proc, GCWrite);
  while (writelistCurrent < writelistStop) {
    ptr_t primary = *writelistCurrent++, replica;
    int byteDisp = (int) *writelistCurrent++;
    ptr_t possPrevPtrVal = (ptr_t) *writelistCurrent++;  /* Pointer value only if pointer array was modified */
    int wordDisp = byteDisp/ sizeof(val_t);
    int doublewordDisp = byteDisp / (sizeof(double));
    int byteLen;  /* Length of data portion of array */
    tag_t tag;

    if (IsGlobalData(primary)) {
      add_global_root(proc,primary);
      continue;
    }
    if (!gcOn)
      continue;
    if (!inHeap(primary, fromSpace))
      continue;
    alloc1_copyCopySync_primaryStack(proc,primary,&proc->majorObjStack,&proc->majorRange,fromSpace); 
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
      alloc1_copyCopySync_primaryStack(proc,possPrevPtrVal, &proc->majorObjStack, &proc->majorRange, fromSpace);
      locAlloc1_copyCopySync_primaryStack(proc,&replicaField, &proc->majorObjStack, &proc->majorRange, fromSpace);
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
  if (gcOn)
    pushSharedStack(1,workStack,&proc->threads,proc->globalLocs,proc->rootLocs,
		    &proc->majorObjStack,&proc->majorSegmentStack);
}


/* GCStatus is either GCOn or GCPendingOff.
   Upon entry and exit, the local work stack is empty.
   The routine will work up until updateWorkDone() > workToDo is reached.  It may copy slightly more.
   If the local flag is set, then the shared stack is not accessed.
*/
static void do_work(Proc_t *proc, int workToDo)
{
  int counter, start, end;
  ptr_t gray;

  procChangeState(proc, GC);
  proc->segmentType |= MajorWork; 

  assert(isEmptyStack(&proc->majorObjStack));
  assert(isEmptyStack(&proc->majorSegmentStack));
  
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
      locAlloc1_copyCopySync_primaryStack(proc,rootLoc,&proc->majorObjStack,&proc->majorRange,fromSpace);
    while (!recentWorkDone(proc, localWorkSize) &&
	   (globalLoc = (ploc_t) popStack(proc->globalLocs)) != NULL) {
      ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
      locAlloc1_copyCopySync_primaryStack(proc,replicaLoc,&proc->majorObjStack,&proc->majorRange,fromSpace);  
      proc->segUsage.globalsProcessed++;
    }
    while (updateWorkDone(proc) < workToDo) {
      int start, end;
      ptr_t gray = popStack3(&proc->majorSegmentStack,(ptr_t *)&start,(ptr_t *)&end);
      if (gray == NULL)
	break;
	transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primaryStack(proc,gray,start,end,
										   &proc->majorObjStack,&proc->majorSegmentStack,
										   &proc->majorRange, fromSpace); 
    }
    while (!recentWorkDone(proc, localWorkSize) && 
	   ((gray = popStack(&proc->majorObjStack)) != NULL)) {
      transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primaryStack(proc,gray,&proc->majorObjStack,
									     &proc->majorSegmentStack,
									     &proc->majorRange, fromSpace); 
    }
    globalEmpty = pushSharedStack(0,workStack,&proc->threads,proc->globalLocs,proc->rootLocs,
				  &proc->majorObjStack, &proc->majorSegmentStack);
    if (globalEmpty) {
      if (collectDiag >= 2)
	printf("Proc %d: Turning Collector Off\n", proc->procid); 
      if (GCStatus == GCAgressive)
	GCStatus = GCPendingOn;
      else if (GCStatus == GCOn)
	GCStatus = GCPendingOff;
      break;
    }
  }
  assert(isEmptyStack(&proc->majorObjStack));
}

/* Satisfy space requrements by allocating space.  
   If enough allocation space exists, no additional space is obtained.
   Write list is not extended. 
   */
static int GC_SemiConcHelp(Thread_t *th, Proc_t *proc, int roundSize)
{
  int success;
  if (th == NULL)
    return 1;
  if (GCSatisfiable(proc,th))
    return 1;
  GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  if (collectDiag >= 2)
    printf("Proc %d: GCSemiConcHelp %s in getting %d bytes\n",
	   proc->procid, (proc->allocStart != NULL) ? "succeeded" : "failed", roundSize);
  return GCSatisfiable(proc,th);
}

void GC_SemiConc(Proc_t *proc, Thread_t *th)
{
  int requestInfo = th->requestInfo;
  int roundOffSize, roundOnSize, workToDo;
  assert(proc->writelistCursor + 3 <= proc->writelistEnd);
  flushStore();

  assert(requestInfo != 0);
  if (requestInfo < 0) {
    roundOffSize = RoundUp(4, minOffRequest);
    roundOnSize = RoundUp(4, minOnRequest);
  }
  else {
    roundOffSize = RoundUp(requestInfo, minOffRequest);
    roundOnSize = RoundUp(requestInfo, minOnRequest);
  }
  workToDo = (int) (1.0 + majorCollectionRate) * roundOnSize;  /* + 1 for the work in replicating primary */
  /* XXXXXXXXXXXXXX might fall behind XXXXXXXXX */
  if (roundOnSize > 2 * minOnRequest)
    workToDo = (int) (1.0 + majorCollectionRate) * (2 * minOnRequest);

  retry:
    switch (GCStatus) {
    case GCOff:                                         
      if (GC_SemiConcHelp(th,proc,roundOffSize))       
	goto satisfied;
      if (doAgressive)
	goto pendingAgressive;
      goto pendingOn;
    case GCPendingAgressive:
    pendingAgressive:
      CollectorOn(proc);                               
      if (GC_SemiConcHelp(th,proc,roundOnSize))
	goto satisfied;
      if (GCStatus == GCPendingOn)
	goto retry;
      goto fail;
    case GCPendingOn:
    pendingOn:
      if (doAgressive)
	CollectorTransition(proc);
      else
	CollectorOn(proc);
      if (GC_SemiConcHelp(th,proc,roundOnSize))
	goto satisfied;
      if (GCStatus == GCPendingOff)
	goto retry;
      goto fail;
    case GCAgressive:
    case GCOn:       
       do_work(proc, workToDo);
       if (GC_SemiConcHelp(th,proc,roundOnSize))
	 goto satisfied;
       goto fail;	 
     case GCPendingOff:
       do_work(proc, MAXINT);
       CollectorOff(proc);
       goto retry;
    default: 
      assert(0);
    }

 satisfied:
    return;
 fail:
    printf("Proc %d: Concurrent collector fell too far behind  - check parameters\n", proc->procid);
    assert(0);

}

void GCPoll_SemiConc(Proc_t *proc)
{
  switch (GCStatus) {
  case GCOff:
    return;
  case GCOn:
  case GCAgressive:
    do_work(proc, (int) majorCollectionRate * minOnRequest);  /* collect as though one page had been allocated */
    return;
  case GCPendingOff:
    do_work(proc, MAXINT);
    CollectorOff(proc);
    return;
  case GCPendingAgressive:
    CollectorOn(proc);
    return;
  case GCPendingOn:
    if (doAgressive)
      CollectorTransition(proc);
    else
      CollectorOn(proc);
    return;
  }
}

void GCInit_SemiConc()
{
  int expandedSize, reducedSize;
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
  expandedSize = Heap_GetSize(fromSpace);
  reducedSize = expandedToReduced(expandedSize, majorCollectionRate);
  Heap_Resize(fromSpace, reducedSize, 1);
  Heap_Resize(toSpace, reducedSize, 1);
  workStack = SharedStack_Alloc(100, 16 * 1024, 12 * 1024, 64 * 1024, 16 * 1024);
  barriers = createBarriers(NumProc, 10);
  arraySegmentSize = 2 * 1024;
  mirrorGlobal = 1;
}

