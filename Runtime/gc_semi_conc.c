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
#pragma INLINEP(flipRootLoc)
static INLINE
void flipRootLoc(ploc_t root)
{
  ptr_t primary = *root;
  if (inHeap(primary,fromSpace)) {
    ptr_t replica = (ptr_t) primary[-1];
    *root = replica;
    if (!inHeap(replica,toSpace)) {
      printf("root = %d   primary = %d   replica = %d\n", root, primary, replica);
    }
    assert(inHeap(replica,toSpace)); 
  }
  else if (paranoid) {
    assert(IsGlobalData(primary) || 
	   IsTagData(primary) ||
	   inHeap(primary,toSpace));                      /* root was already flipped - duplicate root */
  }
}

static int shouldDoubleAllocate(void)
{
  int gcAgressive = doAgressive ? (GCStatus == GCAgressive) || (GCStatus == GCPendingOn) : 0;
  int gcOn = gcAgressive || (GCStatus == GCOn) || (GCStatus == GCPendingOff);
  return (gcOn && !gcAgressive);
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
  /* If collection is on in commit mode, we must double-allocate */
  if (shouldDoubleAllocate()) {
    if (ordering == StackOrder) {
      if (grayAsReplica)
	alloc1_copyCopySync_replicaSet(proc, obj, fromSpace);
      else
	alloc1_copyCopySync_primarySet(proc, obj, fromSpace);
    }
    else if (ordering == HybridOrder) {
      alloc1_copyCopySync(proc, obj, fromSpace);
    }
    else
      DIE("bad ordering");
  }
  return obj;
}

/* --------------------- Concurrent collector --------------------- */
static long totalRequest = 0;     /* Total number of bytes requested by all threads */
static long totalReplicated = 0;  /* Total number of bytes replicated by all processors */
static long totalUnused = 0;
long workAhead = 0;        /* Total amount of work that is ahead of that required by amount allocated */

static int expandedSize = 0, reducedSize = 0;

static void CollectorOn(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst, rootCount = 0;

  if (collectDiag >= 2)
    printf("Proc %d: CollectorOn\n", proc->procid); 
  
  procChangeState(proc, GCWork, 100);
  proc->segmentType |= (MajorWork | FlipOn);

  switch (GCStatus) {
  case GCOff:                   /* Signalling to other processors that collector is turning on */
    GCStatus = doAgressive ? GCPendingAgressive : GCPendingOn;
    StopAllThreads();
    break;
  case GCPendingOn:             /* Responding to signal that collector is turning on */
  case GCPendingAgressive: break;
  default: DIE("CollectorOn");
  }

  /* Check local stack empty; reset root lists */
  assert(SetIsEmpty(&proc->work.objs));
  assert(SetIsEmpty(&proc->work.roots));
  SetCopyRange(&proc->copyRange, proc, toSpace, NULL);

  isFirst = (weakBarrier(barriers, proc) == 0);
  if (isFirst) {
    int pages = Heap_ResetFreshPages(proc,fromSpace);

    /*    workAhead = 0; XXX */
    workAhead = (expandedSize - reducedSize) / 10; 
    Heap_Resize(fromSpace, relaxed ? Heap_GetMaximumSize(fromSpace) : expandedSize, 0);
    Heap_Resize(toSpace, relaxed ? Heap_GetMaximumSize(toSpace) : expandedSize, 1);
    paranoid_check_all(fromSpace, NULL, NULL, NULL, NULL);
    totalRequest = totalReplicated = totalUnused = 0;
    ResetJob();
  }
  resetSharedStack(workStack,&proc->work, 0);
  strongBarrier(barriers, proc);

  if (isFirst) 
    major_global_scan(proc);

  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  while ((curThread = NextJob()) != NULL) {
    initial_root_scan(proc,curThread);
    if (curThread->requestInfo >= 0) /* Allocation request */
      FetchAndAdd(&totalRequest, curThread->requestInfo);
  }
  procChangeState(proc, GCWork, 104);

  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);
  pushSharedStack(0,workStack,&proc->work);
  assert(SetIsEmpty(&proc->work.objs));
  GCStatus = doAgressive ? GCAgressive : GCOn;
  strongBarrier(barriers, proc);
}


static void CollectorTransition(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst, rootCount = 0;

  assert(proc->work.hasShared == 0);

  if (collectDiag >= 2)
    printf("Proc %d: CollectorTransition\n", proc->procid); 
  
  procChangeState(proc, GCWork, 105);
  proc->segmentType |= (MajorWork | FlipTransition);

  switch (GCStatus) {
  case GCAgressive:                 /* Signalling to other processors that collector is turning on */
    GCStatus = GCPendingOn;
    StopAllThreads();
    break;
  case GCPendingOn: break;         /* Responding to signal that collector is turning on */
  default: DIE("CollectorTransition");
  }

  isFirst = (weakBarrier(barriers, proc) == 0);
  if (isFirst) {
    ResetJob();
  }
  resetSharedStack(workStack,&proc->work, 0);
  strongBarrier(barriers, proc);

  if (isFirst) 
    major_global_scan(proc);

  /* Check local stack empty; reset root lists */
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  while ((curThread = NextJob()) != NULL) {
    discard_root_scan(proc,curThread);
    if (curThread->used == 0)
      continue;
    initial_root_scan(proc,curThread);
    if (curThread->requestInfo >= 0) /* Allocation request */
      FetchAndAdd(&totalRequest, curThread->requestInfo);
  }

  procChangeState(proc, GCWork, 109);
  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);
  /* Note the omission of popSharedStack since we used resetSharedStack */
  pushSharedStack(0,workStack,&proc->work);
  assert(SetIsEmpty(&proc->work.objs));
  GCStatus = GCOn;
  strongBarrier(barriers, proc);
}


static void CollectorOff(Proc_t *proc)
{
  extern int stkSize;
  Thread_t *curThread = NULL;
  int isFirst;
  int rootCount = 0;
  ploc_t rootLoc, globalLoc;

  if (collectDiag >= 2)
    printf("Proc %d: entered CollectorOff\n", proc->procid);
  assert(SetIsEmpty(&proc->work.objs));
  memBarrier();

  switch (GCStatus) {
  case GCOn:                   /* First to signal turning collector off */
    GCStatus = GCPendingOff;
    memBarrier();
    StopAllThreads();
    break;
  case GCPendingOff: break;   /* Responding to someone's signal to turn collector off */
  default: DIE("CollectorOff");
  }

  ClearCopyRange(&proc->copyRange);
  isFirst = (weakBarrier(barriers, proc) == 0);
  if (isFirst) 
    ResetJob();
  strongBarrier(barriers, proc);

  /* Replace all roots (global, local registers) with replica */
  assert(isEmptySharedStack(workStack));
  assert(isLocalWorkEmpty(&proc->work));
  if (isFirst) 
    minor_global_scan(proc);                /* Minor scan since the tenured roots are already flipped */
  while (globalLoc = (ploc_t) SetPop(&proc->work.globals)) {
    ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
    flipRootLoc(replicaLoc);
  }

  while ((curThread = NextJob()) != NULL) 
    complete_root_scan(proc, curThread);
  procChangeState(proc, GCWork, 112);
  proc->segmentType |= MajorWork | FlipOff;

  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);
  /* Flip stack slots */
  while (rootLoc = (ploc_t) SetPop(&proc->work.roots)) 
    flipRootLoc(rootLoc);
  FetchAndAdd(&totalReplicated, proc->segUsage.bytesReplicated + proc->cycleUsage.bytesReplicated);
  strongBarrier(barriers, proc);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    double liveRatio = 0.0;
    /* Check the fromspace and tospace heap - zero out all of fromspace */
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
    
    /* Resize heaps and do stats */
    HeapAdjust1(totalRequest,totalUnused,totalReplicated, 
		CollectionRate, doAgressive ? 2 : 1,
		fromSpace,toSpace);
    reducedSize = Heap_GetSize(toSpace);
    expandedSize = reducedToExpanded(reducedSize, CollectionRate, doAgressive ? 2 : 1);
    Heap_Resize(fromSpace, 0, 1);
    typed_swap(int, primaryGlobalOffset, replicaGlobalOffset);
    typed_swap(int, primaryStackletOffset, replicaStackletOffset);
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
    GCStatus = GCOff;
    memBarrier();
  }

  /* All system threads need to reset their limit pointer */
  ResetAllocation(proc, NULL);
  assert(proc->writelistCursor == proc->writelistStart);

  strongBarrier(barriers, proc);
}

void GCRelease_SemiConc(Proc_t *proc)
{
  mem_t allocCurrent;
  mem_t allocStop;
  ploc_t writelistCurrent;
  ploc_t writelistStop;
  int gcAgressive;
  int gcOn;
  int numWrites;
  assert(proc->work.hasShared == 0);

  allocCurrent = proc->allocStart;
  allocStop = proc->allocCursor;
  writelistCurrent = proc->writelistStart;
  writelistStop = proc->writelistCursor;
  gcAgressive = doAgressive ? (GCStatus == GCAgressive) || (GCStatus == GCPendingOn) : 0;
  gcOn = gcAgressive || (GCStatus == GCOn) || (GCStatus == GCPendingOff);

  proc->allocStart = proc->allocCursor;  /* allocation area is NOT reused */  
  numWrites = (proc->writelistCursor - proc->writelistStart) / 3;
  proc->writelistCursor = proc->writelistStart;  /* write list reused once processed */

  if (shouldDoubleAllocate()) {
    if (collectDiag >= 3)
      printf("Proc %d:   Double-allocating %d to %d\n",proc->procid,allocCurrent,allocStop);

    procChangeState(proc, GCReplicate, 114);
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
      if (ordering == StackOrder) {
	if (grayAsReplica)
	  alloc_copyCopySync_replicaSet(proc,obj);
	else
	  alloc_copyCopySync_primarySet(proc,obj);
      }
      else {
	assert(ordering == HybridOrder);
	alloc_copyCopySync(proc,obj);
      }
      objSize = proc->bytesCopied;
      if (objSize == 0) {
	mem_t dummy = NULL;
	objSize = objectLength(obj, &dummy);
	assert(dummy == allocCurrent);
      }
      allocCurrent += objSize / sizeof(val_t);
    }
  }

  if (collectDiag >= 3)
    printf("Proc %d:   Processing %d writes from %d to %d\n",proc->procid,numWrites,writelistCurrent,writelistStop);
  if (writelistCurrent < writelistStop) 
    procChangeState(proc, GCWrite, 115);
  assert(primaryArrayOffset == 0);
  while (writelistCurrent < writelistStop) {
    vptr_t primary = *writelistCurrent++, replica;
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
    /* If object has not been copied, we need to only gray the old pointer value.
       Snapshot-at-the-beginning (Yuasa) write barrier requires copying prevPtrVal 
       even if it might die to prevent the mutator from hiding live data */
    tag = primary[-1];
    switch (GET_TYPE(tag)) {
      case PTR_ARRAY_TYPE:
      case MIRROR_PTR_ARRAY_TYPE:
	if (ordering == StackOrder) {
	  if (grayAsReplica)
	    alloc1_copyCopySync_replicaSet(proc,possPrevPtrVal, fromSpace);
	  else
	    alloc1_copyCopySync_primarySet(proc,possPrevPtrVal, fromSpace);
	}
	else {
	  assert(ordering == HybridOrder);
	  alloc1_copyCopySync(proc,possPrevPtrVal, fromSpace);
	}
	continue;
      case WORD_ARRAY_TYPE: 
      case QUAD_ARRAY_TYPE: 
	continue;
    }
    /* Object must be copied or being copied */
    while ((replica = (ptr_t) primary[-1]) == (ptr_t)STALL_TAG)  
      ;
    tag = replica[-1];
    /* Backpointer present indicates object not yet scanned - can/must skip replica update */
    if (replica[0] == (val_t) primary) 
      continue;

    byteLen = GET_ANY_ARRAY_LEN(tag);
    if (byteLen <= arraySegmentSize)
      while (replica[-1] == SEGSTALL_TAG)
	; 
    else {
      int segment = DivideDown(byteDisp, arraySegmentSize);
      while (replica[-2-segment] == SEGSTALL_TAG)
	;
    }
    switch (GET_TYPE(tag)) {
    case PTR_ARRAY_TYPE: 
    case MIRROR_PTR_ARRAY_TYPE: {
      ptr_t primaryField = (ptr_t) primary[wordDisp];
      ptr_t replicaField = primaryField;
      if (ordering == StackOrder) {
	if (grayAsReplica)
	  locAlloc1_copyCopySync_replicaSet(proc,&replicaField, fromSpace);
	else 
	  locAlloc1_copyCopySync_primarySet(proc,&replicaField, fromSpace);
      }
      else {
	assert(ordering == HybridOrder);
	locAlloc1_copyCopySync(proc,&replicaField, fromSpace);
      }
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
    default: DIE("GCRelease_SemiConc");
    }
  } /* while */
  if (ordering == HybridOrder)   /* Add new gray objects */
    AddGrayCopyRange(&proc->copyRange);
  if (gcOn)
    pushSharedStack(1,workStack,&proc->work);
  assert(proc->work.hasShared == 0);
}


/* GCStatus is either GCOn or GCPendingOff.
   Upon entry and exit, the local work stack is empty.
   The routine will work up until either
     (1) workDone > maxWork_cur + additionalWork.  The excess should be slight.
     (2) Status is PendingOn or PendingOff and no local work is available after a fetch from the shared stack
   If the local flag is set, then the shared stack is not accessed.
*/
static void do_work(Proc_t *proc, int additionalWork)
{
  if (additionalWork <= 0)
    return;
  addMaxWork(proc, additionalWork);
  procChangeState(proc, GCWork, 116);
  proc->segmentType |= MajorWork; 

  assert(isLocalWorkEmpty(&proc->work));

  if (collectDiag >= 2)
    printf("GC %d Seg %d:  do_work(%d)  updatedWorkDone = %5d\n",
	   NumGC, proc->segmentNumber, additionalWork, updateGetWorkDone(proc));

  while (!reachMaxWork(proc)) {
    int start, end;
    int i, globalEmpty;
    Stacklet_t *stacklet;
    ploc_t rootLoc, globalLoc;
    ptr_t gray;
    
    popSharedStack(workStack, &proc->work);

    if ((GCStatus == GCPendingOn || GCStatus == GCPendingOff) &&
	isLocalWorkEmpty(&proc->work))
      break;

    if (ordering == StackOrder) {
      if (grayAsReplica) {
	while (!reachCheckWork(proc) && (rootLoc = (ploc_t) SetPop(&proc->work.roots)) != NULL) {
	  locAlloc1_copyCopySync_replicaSet(proc,rootLoc,fromSpace);
	  proc->segUsage.rootsProcessed++;
	}
	while (!reachCheckWork(proc) && (globalLoc = (ploc_t) SetPop(&proc->work.globals)) != NULL) {
	  ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	  locAlloc1_copyCopySync_replicaSet(proc,replicaLoc,fromSpace);  
	  proc->segUsage.globalsProcessed++;
	}
	/* segments are stored with primary gray even when grayAsReplica is true */
	while ((!updateReachCheckWork(proc)) &&
	       (gray = SetPop3(&proc->work.segments,(ptr_t *)&start,(ptr_t *)&end)) != NULL) 
	  transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_replicaSet(proc,gray,start,end,fromSpace); 
	while (!reachCheckWork(proc) && 
	       ((gray = SetPop(&proc->work.objs)) != NULL)) 
	  backTransferScanObj_copyWriteSync_locAlloc1_copyCopySync_replicaSet(proc,gray,fromSpace); 
      }
      else {
	while (!reachCheckWork(proc) && (rootLoc = (ploc_t) SetPop(&proc->work.roots)) != NULL) {
	  locAlloc1_copyCopySync_primarySet(proc,rootLoc,fromSpace);
	  proc->segUsage.rootsProcessed++;
	}
	while (!reachCheckWork(proc) && (globalLoc = (ploc_t) SetPop(&proc->work.globals)) != NULL) {
	  ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	  locAlloc1_copyCopySync_primarySet(proc,replicaLoc,fromSpace);  
	  proc->segUsage.globalsProcessed++;
	}
	while (!updateReachCheckWork(proc) &&
	       (gray = SetPop3(&proc->work.segments,(ptr_t *)&start,(ptr_t *)&end)) != NULL) 
	  transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primarySet(proc,gray,start,end,fromSpace); 
	while (!reachCheckWork(proc) && 
	       ((gray = SetPop(&proc->work.objs)) != NULL)) 
	  transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primarySet(proc,gray,fromSpace); 
      }
    }
    else if (ordering == HybridOrder) {
      assert(grayAsReplica);
      while (!reachCheckWork(proc) && (rootLoc = (ploc_t) SetPop(&proc->work.roots)) != NULL) {
	  locAlloc1_copyCopySync(proc,rootLoc,fromSpace);
	  proc->segUsage.rootsProcessed++;
      }
      while (!reachCheckWork(proc) && (globalLoc = (ploc_t) SetPop(&proc->work.globals)) != NULL) {
	ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	locAlloc1_copyCopySync(proc,replicaLoc,fromSpace);  
	proc->segUsage.globalsProcessed++;
      }
      /* segments are stored with primary gray even when grayAsReplica is true */
      while (!updateReachCheckWork(proc) &&
	     (gray = SetPop3(&proc->work.segments,(ptr_t *)&start,(ptr_t *)&end)) != NULL) 
	transferScanSegment_copyWriteSync_locAlloc1_copyCopySync(proc,gray,start,end,fromSpace); 
      assert(SetIsEmpty(&proc->work.objs));
      while (!updateReachCheckWork(proc)) {
	mem_t start, stop;
	if (start = (mem_t) SetPop2(&proc->work.grayRegion, &stop)) {
	  backTransferScanRegion_copyWriteSync_locAlloc1_copyCopySync(proc,start,stop,fromSpace);
	}
	AddGrayCopyRange(&proc->copyRange);
	if (SetIsEmpty(&proc->work.grayRegion))
	  break;
      }
    }
    else
      DIE("bad ordering");

    while (!updateReachCheckWork(proc) &&
	   ((stacklet = (Stacklet_t *) SetPop(&proc->work.stacklets)) != NULL)) {
      if (!work_root_scan(proc, stacklet))
	SetPush(&proc->work.stacklets, (ptr_t) stacklet);
    }

    if (pushSharedStack(0,workStack,&proc->work)) {
      if (GCStatus == GCAgressive) {
	GCStatus = GCPendingOn;
	if (collectDiag >= 2)
	  printf("Proc %d: GC %d: Transitioning collector to GCPendingOn\n", proc->procid, NumGC); 
      }
      else if (GCStatus == GCOn) {
	GCStatus = GCPendingOff;
	if (collectDiag >= 2)
	  printf("Proc %d: GC %d: Turning collector to off - GCPendingOff\n", proc->procid, NumGC); 
      }
    }

  }
  assert(SetIsEmpty(&proc->work.objs));
  assert(proc->work.hasShared == 0);
  if (collectDiag >= 2)
    printf("Proc %d: Completed do_work.  %d bytes allocated and %d bytes copied on current GC cycle.\n", 
	   proc->procid, 
	   proc->cycleUsage.bytesAllocated + proc->segUsage.bytesAllocated,
	   bytesCopied(&proc->cycleUsage) + bytesCopied(&proc->segUsage));
}

/* Satisfy space requirements by allocating space.  
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
  if (collectDiag >= 3)
    printf("Proc %d: GCSemiConcHelp %s in allocating %d bytes. %d bytes allocated this GC cycle\n",
	   proc->procid, (proc->allocStart != NULL) ? "succeeded" : "failed", roundSize,
	   proc->cycleUsage.bytesAllocated + proc->segUsage.bytesAllocated);
  return GCSatisfiable(proc,th);
}

void GC_SemiConc(Proc_t *proc, Thread_t *th)
{
  int requestInfo = th->requestInfo;
  int roundOffSize, roundOnSize, targetWork;
  assert(proc->writelistCursor + 3 <= proc->writelistEnd);
  memBarrier();

  assert(proc->work.hasShared == 0);
  assert(requestInfo != 0);
  if (requestInfo < 0) {
    roundOffSize = RoundUp(4, minOffRequest);
    roundOnSize = RoundUp(4, minOnRequest);
  }
  else {
    roundOffSize = RoundUp(requestInfo, minOffRequest);
    roundOnSize = RoundUp(requestInfo, minOnRequest);
  }
  /* include 1.0 for work already done in replicating primary */
  targetWork = (int) (((GCStatus == GCAgressive ? 0.0 : 1.0) + CollectionRate) * roundOnSize);    
  if (roundOnSize > minOnRequest) /* XXXXXXXXXXXXXX might fall behind XXXXXXXXX */
    targetWork = (int) (((GCStatus == GCAgressive ? 0.0 : 1.0) + CollectionRate) * (3 * minOnRequest));   


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
      assert(proc->work.hasShared == 0);
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
    case GCOn: {
      /* The default behavior is to do targetWork.  
	 Under certain conditions, we desire to do less work.
	   (1) utilization is too low
	   (2) the collection is particularly inefficient and would take too much time
	 To do less work though, we must borrow from the bank by decreementing from workAhead.
	 The inefficiency of the collection is detected by doing only a third of the collection
	 work and computing the efficiency at that point.
      */
      if (targetUtil < 1.0) {
	double lastUtil = proc->utilizationQuotient1.stat[0].last;
	/*
	double lastMutatorTime = Min(proc->mutatorTime, 0.5);
	double targetTime = Min(1.5, lastMutatorTime / targetUtil * (1.0 - targetUtil));
	*/
	double excessOnTime = 5.0 * (lastUtil - targetUtil);
	double targetTime = Min(1.5, get_prewindow(&proc->utilizationQuotient1, 0, excessOnTime));
	double typicalEfficiency = 15.0;
	int estimatedWork = targetTime * typicalEfficiency * 1000.0;
	int remainWork = estimatedWork / 3;
	int curWork = 0;
	double curTime, curEff;
	double lastTime = 0.0, lastEff = 0.0;
	int lastRemainWork = 0;

	FetchAndAdd(&workAhead, -targetWork);
	while (remainWork > 2048) {
	  do_work(proc, remainWork);
	  FetchAndAdd(&workAhead, remainWork);
	  curWork += remainWork;
	  curTime = nonMutatorTime(proc);
	  curEff = Min(18.0, (curWork / 1000.0) / curTime);
	  remainWork =  (int) (0.7 * (targetTime * curEff * 1000.0) - curWork);

	  if (curTime > 1.2 * targetTime)
	    sprintf(proc->delayMsg, 
		    "seg %d:  curT = %.2f    tarT = %.2f   lastT = %.2f    lastRemW = %d   actualW = %d      curEff = %.2f  lastEff = %.2f\n", 
		    proc->segmentNumber, curTime, targetTime, lastTime, lastRemainWork, updateGetWorkDone(proc), curEff, lastEff);

	  lastTime = curTime;
	  lastEff = curEff;
	  lastRemainWork = remainWork;
	}
      }
#ifdef SKIP
      else if (useLastUtil < 1.0) {
	double curUtil = proc->utilizationQuotient1.stat[0].last;
	if (curUtil < 0.05 + useLastUtil && 
	    (1 || workAhead > targetWork)) {
	  FetchAndAdd(&workAhead, -targetWork);
	}
	else {
	  double time, efficiency, finalEfficiency;
	  int prelimWork = targetWork / 4;
	  int remainWork = targetWork - prelimWork;
	  do_work(proc, prelimWork);
	  time = nonMutatorTime(proc);
	  efficiency = (prelimWork / 1000.0) / time;
	  
	  if ((curUtil < 0.10 + useLastUtil  && efficiency < 20.0) ||
	      (curUtil < 0.15 + useLastUtil  && efficiency < 15.0) ||
	      (curUtil < 0.20 + useLastUtil  && efficiency < 10.0)) {
	    if (1 || workAhead > remainWork) {
	      FetchAndAdd(&workAhead, -remainWork);
	      finalEfficiency = efficiency;
	    }
	    else {
	      do_work(proc, remainWork);
	      time = nonMutatorTime(proc);
	      finalEfficiency = ((prelimWork + remainWork) / 1000.0) / time;
	    }
	  }
	  else {
	    do_work(proc, remainWork);
	    time = nonMutatorTime(proc);
	    finalEfficiency = ((prelimWork + remainWork) / 1000.0) / time;
	  }
	  if (efficienct != finalEfficiency && finalEfficiency < 6.0) 
	    printf("***** efficiency = %.1f   finalEfficiency = %.1f *******\n",
		   efficiency, finalEfficiency);
	}
      }
#endif
      else {  /* Default scheduling */
	do_work(proc, targetWork); 
      }
  
       if (GC_SemiConcHelp(th,proc,roundOnSize))
	 goto satisfied;
       goto fail;	 
     }
     case GCPendingOff:
       do_work(proc, MAXINT);
       CollectorOff(proc);
       goto retry;
    default: 
      DIE("GC_SemiConc");
    }

 satisfied:
  assert(proc->work.hasShared == 0);
    return;
 fail:
    fprintf(stderr,"Proc %d: Concurrent collector fell too far behind  - check parameters\n", proc->procid);
    DIE("out of memory");
}

void GCPoll_SemiConc(Proc_t *proc)
{
  assert(proc->work.hasShared == 0);
  switch (GCStatus) {
  case GCOff:
    return;
  case GCOn:
  case GCAgressive: {
    int workToDo =  (int) (CollectionRate * minOnRequest);
    do_work(proc, workToDo);
    FetchAndAdd(&workAhead, workToDo);
    return;
  }
  case GCPendingOff:
    do_work(proc, MAXINT);
    CollectorOff(proc);
    return;
  case GCPendingAgressive:
    CollectorOn(proc);
    return;
  case GCPendingOn:
    assert(proc->work.hasShared == 0);
    if (doAgressive)
      CollectorTransition(proc);
    else
      CollectorOn(proc);
    return;
  }
}

void GCInit_SemiConc(void)
{
  if (ordering == DefaultOrder)
    ordering = StackOrder;
  if (ordering == HybridOrder)
    grayAsReplica = 1;
  GCInit_Help(0.1, 0.7, 512, 50 * 1024);   
  if (relaxed) {
    reducedSize = Heap_GetSize(fromSpace);
    expandedSize = reducedToExpanded(reducedSize, CollectionRate, doAgressive ? 2 : 1);
  }
  else {
    expandedSize = Heap_GetSize(fromSpace);
    reducedSize = expandedToReduced(expandedSize, CollectionRate, doAgressive ? 2 : 1);
    Heap_Resize(fromSpace, reducedSize, 1);
    Heap_Resize(toSpace, reducedSize, 1);
  }
  workStack = SharedStack_Alloc(0, 100, 16 * 1024, 12 * 1024, 128 * 1024, 16 * 1024, 0, 0);
  barriers = createBarriers(NumProc, 10);
  arraySegmentSize = 2 * 1024;
  mirrorGlobal = 1;
  pauseWarningThreshold = 10.0;
  addOldStackletOnUnderflow = 1;
}

