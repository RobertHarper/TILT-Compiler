#include <strings.h>
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "gc_para.h"
#include "gc_large.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "show.h"
#include "platform.h"

/* 
   BACK-POINTER MUTATIONS
   ----------------------
   During a minor collection, the primary continues to be allocated
   from the nursery even as a replica of the nursery-residing primary
   is replicated in the fromSpace.  The objects in fromSpace are
   considered primary-replica in the sense that both the primary and
   replica point to it.  At the beginning of the collection, there are
   no pointers back to the nursery and so the mutator continues to see
   only the primary or primary-replica.  Eventually, when the
   collector is turned off, it is possible to discard the nursery
   primary and switch to the replica without modifying the
   primary-replica at all.  However, when a fromSpace-residing
   pointer array is mutated, we may now have a primary-replica array
   pointing back to the primary.  This makes it impossible to discard
   the primary later.  (Almost) Alternatively, we could make the
   primary-replica array point to the replica but then we are in the
   worse situation that the mutator sees the replica.

   Solution 1:
   -----------
   The easiest thing to do is to leave the primary-replica array pointing
   at the primary.  Meanwhile, all backward-pointer locations are recorded
   during the collection.  When it is time to flip, we then replace these
   locations with the replica version.  Unfortunately, the flip is no longer
   constant-time but takes time proportional to the number of back-pointers.

   Solution 2:
   ----------- 
   Replicate the primary-replica pointer arrays.  The mutator
   continues to modify the primary version while the collector keeps
   the (unreachable) replica version up to date.  Note that the
   replicas are not reachable even from the primary-replica.  There
   are 2 mechansims that permit these pointer arrray replicas to be
   swapped in and be visible to the primary-replica and the replica.

   Solution 2a:
   Pointer arrays will now be replicated in place.  That is,
   every pair of actual array entries correspond to one logical
   entry from the program's point of view.  The program accesses 
   pa[0], pa[2], .... while the mutator keeps pa[1], pa[3], ...
   up-to-date with the corresponding replicas.  To perform a flip,
   the collector modifies a global value (actually kept in a register)
   that indicates that the alternate entries are now to be used.
   At this point, the collector must re-apply all the mutations
   to the discarded copy so that it is up-to-date before the next
   collection is begun.
   
   Solution 2b:
   An alternative way of replication is to represent pointer arrays
   as logical offsets into a pointer array heap.  We then keep two 
   separate heaps and flipping consists of changing a global heap
   variable.  This can be seen as the unzipped version of Solution 2a.

   Solution 3:
   -----------
   When we process an update to a primary-replica array, we
   create a new copy of the array.  We would like the new copy
   to be the one used by the mutator so that the original
   copy can point to the replica so that the flip can later occur.
   This (reversed) replica points must point back to the primary
   before it can be be updated with replica pointers so that 
   the mutator can traverse the backward pointer.

   Whenever the mutator encounters a reversed-replica, array access
   now passes through a "mutable read barrier" which recovers, if
   possible, the primary copy.  We must be sure that this
   mutable read barrier is cheap but does not interleave with the 
   collector's installation of the backward pointer which must occur when
   the 2 copies are identical.  A fine grain lock would be too expensive.

   Another way to make sure the conditions hold is to synchronize
   all the processors to perform pending writes to ensure the two copise
   are identical and then installing the backward-pointers.  To preserve
   the time bounds, multiple synchronizations may be required.  Once
   a backward-pointer is installed, the original copy is now a replica
   and its contents need to be replaced with replicas.  This is limited
   to cells that have been modified when the collection began.

   The implementation of the flip requires two extra indirection
   cells per pointer array.  A global variable is used to select which
   cell to use.  After the collection is stopped, and before the
   global variable is reset for the next collection, the unused set
   of cells must be reset.

   Comparison:
   -----------
   Solution 1
     (+) simple
     (+) no additional space required
     (-) not real-time
   Solution 2
     (-) complex - requires compiler support
     (-) always requires copies of all pointer arrays
     (+) real-time
   Solution 3
     (-) complex - requires compiler support
     (+) requires copies of modified pointer arrays during collection
     (+) real-time
     (-) requires multiple syncs 

   For now, we choose Solution 1 and store the pointer updates in the queue modifiedPrimaryReplicaLoc.

   A separate issue is backpointer mutations when there is no
   collection.  These backpointers must also be considered roots when
   collection starts.  Like the other backpointer mutations, they must
   be flipped when the collector is turned off.

   Another concern is the interaction between the back-pointers due
   and major collections.  In the absence of mutations, the fromSpace
   graph is closed (except for globals) and is replicated into toSpace
   during major GC work.  With mutations, the major collector may copy
   a fromSpace with backpointers, creating a toSpace object with
   backpointers to the nursery.  When the major collector scans this
   toSpace object, it will skip over the nursery pointers.  This is OK
   provided that these locations are fixed when we flip the spaces.
   Since we are using Solution 1 already, we can extend it easily to
   handle this.  Normally, the mutations are applied at the end of a
   minor GC.  We simply apply them to the toSpace replica as well when
   we reach the end of a major GC.

*/


/* weights can be reset in init routine */
static int reducedNurserySize = 0, expandedNurserySize = 0;
static int reducedTenuredSize = 0, expandedTenuredSize = 0;

/* ------------------  Parallel array allocation routines ------------------- */

mem_t AllocBigArray_GenConc(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj = NULL;
  /* Double length for PointerField to make MirrotPtrArray */
  int specByteLen = spec->byteLen;
  int i, segments = (specByteLen <= arraySegmentSize) ? 0 : DivideUp(specByteLen, arraySegmentSize);
  int tagByteLen = specByteLen + 4 + 4 * segments;
  Align_t align = (spec->type == DoubleField) ? 
                      ((segments & 1) ? EvenWordAlign : OddWordAlign) : 
                      NoWordAlign; 

  /* Allocate the space */
  assert(spec->type != PointerField);
  if (spec->type == MirrorPointerField) {
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
  proc->segUsage.bytesAllocated += tagByteLen;

  /* Allocate object; update stats; initialize */
  obj = region + 1 + segments;
  for (i=0; i<segments; i++)
    obj[-(2+i)] = SEGPROCEED_TAG;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : assert(0);
    case MirrorPointerField : 
      init_double_ptr_array(obj, spec->elemLen, spec->pointerVal); 
      if (GCType != Major)
	pushStack2(proc->backObjs, obj, 0);
      break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Concurrent collector --------------------- */
static long totalRequest = 0;     /* Total number of bytes requested by all threads */
static long totalReplicated = 0;  /* Total number of bytes replicated by all processors */
static long totalUnused = 0;      /* Total number of bytes unused but assigned to processors */

static void CollectorOn(Proc_t *proc)
{

  int i, len, isFirst = 0;
  Thread_t *threadIterator = NULL;
  Stack_t *objStack = (GCType == Minor) ? &proc->majorObjStack : &proc->majorObjStack;
  Stack_t *segmentStack = (GCType == Minor) ? &proc->minorSegmentStack : &proc->majorSegmentStack;
  CopyRange_t *copyRange = (GCType == Minor) ? &proc->minorRange : &proc->majorRange;
  Heap_t *srcRange = (GCType == Minor) ? nursery : fromSpace;

  switch (GCStatus) {
    case GCOff:                   /* Signalling to other processors that collector is turning on */
      if (collectDiag >= 2)
	printf("Proc %d: CollectorOn - stopping other processors\n", proc->procid); 
      GCStatus = GCPendingOn;
      StopAllThreads();
      break;
    case GCPendingOn:             /* Responding to signal that collector is turning on */
      if (collectDiag >= 2)
	printf("Proc %d: CollectorOn - stopped by another processor\n", proc->procid); 
      break;                      
    case GCOn:                    /* Collector already on */
    case GCPendingOff:            /* We are turning the collector on after someone has tried turning the collector off */
    default: 
      assert(0);
  }

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     prelimiary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  isFirst = (weakBarrier(barriers,0) == 0);
  if (isFirst) {
    Heap_ResetFreshPages(nursery);
    if (GCType == Major) {
      int pages = Heap_ResetFreshPages(fromSpace);
      proc->segUsage.pagesTouched += pages / 100;  /* These refer to pages of the pageMap itself */
    }
    if (GCType == Minor) {
      if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery)) {
	printf("Warning: fromSpace has less available space than total nursery size.\n"
	       "         Probably due to fromSpace pointer array allocation.\n");
	Heap_Resize(fromSpace, Heap_GetSize(fromSpace) + Heap_GetSize(nursery), 0);
	assert(Heap_GetAvail(fromSpace) >= Heap_GetSize(nursery));
      }
    }
    totalUnused = 0;
    totalRequest = 0;
    totalReplicated = 0;
    ResetJob();                               /* Reset counter so all user threads are scanned */
  }
  strongBarrier(barriers,1);

  /* Reset root lists, compute thread-specific roots in parallel,
     determine whether a major GC was explicitly requested. */
  procChangeState(proc, GCStack);
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  assert(isEmptyStack(proc->rootLocs));
  while ((threadIterator = NextJob()) != NULL) {
    Thread_t *thread = initial_root_scan(proc,threadIterator);
    if (thread != NULL)
      pushStack(&proc->threads, (ptr_t) thread);
    if (threadIterator->requestInfo >= 0)  /* Allocation request */
      FetchAndAdd(&totalRequest, threadIterator->requestInfo);
  }
  strongBarrier(barriers,2);

  /* The "first" processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. 
     The major GC does not take effect until the first minor GC is completed.
  */
  procChangeState(proc, GCGlobal);
  if (isFirst) {
    if (paranoid) /* Check heaps before starting GC */
      paranoid_check_all(nursery, fromSpace, NULL, NULL, largeSpace);
    resetSharedStack(workStack,NumProc);
    major_global_scan(proc);    /* Always a major_global_scan because we must flip all globals */
    if (GCType == Major) {
      Heap_Resize(fromSpace,expandedTenuredSize,0);
      Heap_Resize(toSpace,expandedTenuredSize,1);
      gc_large_startCollect();
    }
    else {
      Heap_Resize(nursery,expandedNurserySize,0);
    }
  }
  strongBarrier(barriers, 3);

  /* Check local stack empty, prepare copy range,
     forward all the roots (first proc handles backpointers),
     transfer work from local to shared work stack */
  procChangeState(proc, GC);
  if (GCType == Major)
    proc->gcSegment1 = MajorWork;

  assert(isEmptyStack(&proc->majorObjStack));
  if (GCType == Minor) {
    if (IsNullCopyRange(&proc->minorRange))   /* First minor GC (after a major GC) */
      SetCopyRange(&proc->minorRange, proc, fromSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
  }
  else
    SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);

  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);

  /* Omit popSharedObjStack */
  pushSharedStack(workStack, &proc->threads, proc->globalLocs, proc->rootLocs, objStack, segmentStack);
  GCStatus = GCOn;
  strongBarrier(barriers,4);
}

INLINE(flipRootLoc)
void flipRootLoc(int curGCType, ploc_t root)
{
  ptr_t primary = *root;
  if ((curGCType == Minor && inHeap(primary,nursery)) ||
      (curGCType == Major && inHeap(primary,fromSpace))) {
    ptr_t replica = (ptr_t) primary[-1];
    *root = replica;
  }
  else {
    fastAssert(IsGlobalData(primary) || 
	       IsTagData(primary) ||
	       inHeap(primary,(curGCType == Minor) ? fromSpace : toSpace) ||   /* Already flipped */
	       inHeap(primary,largeSpace));
  }
}

static void CollectorOff(Proc_t *proc)
{
  Thread_t *threadIterator = NULL;
  int isFirst;
  int curGCType = GCType;      /* GCType will be written to during this function for the next GC
				  and so we save its value here for reading */

  if (collectDiag >= 2)
    printf("Proc %d: entered CollectorOff\n", proc->procid);
  assert(isEmptyStack(&proc->majorObjStack));        /* Local stack must be empty */
  assert(GCStatus == GCPendingOff);
  flushStore();

  isFirst = (weakBarrier(barriers,5) == 0);
  if (isFirst) {
    if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery))   
      GCType = Major;                         /* The next GC needs to be a major GC so we must 
						 begin allocation in the fromSpace immediately. */
    else
      GCType = Minor;
    ResetJob();
  }
  strongBarrier(barriers,6);

  /* Local stacks must be empty. */
  assert(isEmptyStack(&proc->majorObjStack));
  assert(isEmptyStack(&proc->majorObjStack));
  assert(isEmptyStack(proc->backObjs));
  assert(isEmptyStack(proc->backLocs));

  /* Replace all roots with replica */
  procChangeState(proc, GCGlobal);
  if (isFirst) 
    minor_global_scan(proc);   /* Even for a major GC since we already flipped global locs tenured when GC started */
  procChangeState(proc, GCStack);
  while ((threadIterator = NextJob()) != NULL) {
    complete_root_scan(proc, threadIterator);
    if (threadIterator->request == MajorGCRequestFromC) /* Runtime explicitly requests major GC */
      GCType = Major;
  }

  procChangeState(proc, GC);
  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);
  while (!isEmptyStack(proc->rootLocs)) {
    ploc_t root = (ploc_t) popStack(proc->rootLocs);
    flipRootLoc(curGCType, root);
  }
  while (!isEmptyStack(proc->globalLocs)) {
    ptr_t global = popStack(proc->globalLocs);
    ploc_t replicaLoc = DupGlobal(global);
    flipRootLoc(curGCType, replicaLoc);
  }
  FetchAndAdd(&totalReplicated, proc->segUsage.bytesReplicated + proc->cycleUsage.bytesReplicated);
  strongBarrier(barriers,7);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    double liveRatio = 0.0;
    if (curGCType == Minor) {
      int i, copied = 0;
      paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
      minor_global_promote(proc);
      for (i=0; i<NumProc; i++) {
	Proc_t *p = getNthProc(i);;
	copied += bytesCopied(&p->cycleUsage) + bytesCopied(&p->segUsage);
      }
      liveRatio = (double) (copied) / (double) (nursery->cursor - nursery->bottom); 
      add_statistic(&proc->minorSurvivalStatistic, liveRatio);
    }
    else {
      paranoid_check_all(fromSpace, NULL, toSpace, NULL, largeSpace);
      gc_large_endCollect();
      liveRatio = HeapAdjust2(totalRequest, totalUnused, totalReplicated,  1.0/ (1.0 + majorCollectionRate),
			      nursery, fromSpace, toSpace);
      add_statistic(&proc->majorSurvivalStatistic, liveRatio);
      Heap_Resize(fromSpace, 0, 1);
      typed_swap(Heap_t *, fromSpace, toSpace);
      reducedTenuredSize = Heap_GetSize(fromSpace);
      expandedTenuredSize = reducedToExpanded(reducedTenuredSize, majorCollectionRate);
      Heap_Resize(fromSpace,reducedTenuredSize,0);  
      NumMajorGC++;                          
    }
    typed_swap(Stack_t *, proc->backObjs, proc->backObjsTemp);
    typed_swap(Stack_t *, proc->backLocs, proc->backLocsTemp);
    typed_swap(int, primaryGlobalOffset, replicaGlobalOffset);
    typed_swap(int, primaryArrayOffset, replicaArrayOffset);
    typed_swap(int, primaryStackletOffset, replicaStackletOffset);
    Heap_Resize(nursery,reducedNurserySize,1);
    NumGC++;
    GCStatus = GCOff;
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  proc->writelistCursor = proc->writelistStart;

  strongBarrier(barriers,8);
}

static void do_work(Proc_t *proc, int workToDo, int local)
{
  int done = 0;
  Stack_t *objStack = (GCType == Minor) ? &proc->majorObjStack : &proc->majorObjStack;
  Stack_t *segmentStack = (GCType == Minor) ? &proc->minorSegmentStack : &proc->majorSegmentStack;
  CopyRange_t *copyRange = (GCType == Minor) ? &proc->minorRange : &proc->majorRange;
  Heap_t *srcRange = (GCType == Minor) ? nursery : fromSpace;

  while (!done) {
    ploc_t rootLoc, globalLoc, backLoc;
    ptr_t backObj, largeObj, gray;
    int largeStart, largeEnd;
    /* Get shared work */
    if (!local)
      popSharedStack(workStack, &proc->threads, threadFetchSize, 
		     proc->globalLocs, globalLocFetchSize,
		     proc->rootLocs, rootLocFetchSize, 
		     objStack, objFetchSize, segmentStack, segFetchSize);
    /* Do the thread stacklets - we can afford to call updateWorkDone often */
    while (updateWorkDone(proc) < workToDo && 
	   !isEmptyStack(&proc->threads)) {
      Thread_t *thread = (Thread_t *) popStack(&proc->threads);
      int complete = work_root_scan(proc, thread, workToDo);
      if (!complete)
	pushStack(&proc->threads, (ptr_t) thread);
    }
    /* Do the rootLocs */
    if (GCType == Minor) 
      while (!recentWorkDone(proc, localWorkSize) && 
	     ((rootLoc = (ploc_t) popStack(proc->rootLocs)) != NULL)) {
	locSplitAlloc1_copyCopySync_primaryStack(proc,rootLoc,objStack,copyRange,srcRange);
	proc->segUsage.fieldsScanned++;
      }
    else 
      while (!recentWorkDone(proc, localWorkSize) && 
	     ((rootLoc = (ploc_t) popStack(proc->rootLocs)) != NULL)) {
	locSplitAlloc1L_copyCopySync_primaryStack(proc,rootLoc,objStack,copyRange,srcRange,largeSpace);
	proc->segUsage.fieldsScanned++;
      }
    /* Do the backObjs and backLocs */

    if (GCType == Minor) {
      int occur;
      while (updateWorkDone(proc) < workToDo && 
	     ((backObj = (ptr_t) popStack2(proc->backObjs, (ploc_t) &occur)) != NULL)) {
	selfTransferScanObj_locSplitAlloc1_copyCopySync_primaryStack(proc,backObj,objStack,segmentStack,copyRange,srcRange);
	if (occur == 0)
	  pushStack2(proc->backObjsTemp, backObj, (ptr_t) 1);
	else
	  fastAssert(occur == 1);
      }
      while (!recentWorkDone(proc, localWorkSize) && 
	     ((backLoc = (ploc_t) popStack2(proc->backLocs, (ploc_t) &occur)) != NULL)) {
	ploc_t mirrorBackLoc = backLoc + ((primaryArrayOffset == 0) ? 1 : -1);
	*mirrorBackLoc = *backLoc;
	locSplitAlloc1_copyCopySync_primaryStack(proc,mirrorBackLoc,objStack,copyRange,srcRange);
	proc->segUsage.fieldsScanned += 2;
	if (occur == 0)
	  pushStack2(proc->backLocsTemp, (ptr_t) mirrorBackLoc, (ptr_t) 1);
	else
	  fastAssert(occur == 1);
      }
    }
    else {
      resetStack(proc->backObjs);
      resetStack(proc->backLocs);
    }
    /* Do the globalLocs */
    if (GCType == Minor) 
      while (!recentWorkDone(proc, localWorkSize) && 
	     ((globalLoc = (ploc_t) popStack(proc->globalLocs)) != NULL)) {
	ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	locSplitAlloc1_copyCopySync_primaryStack(proc,replicaLoc,objStack,copyRange,srcRange);
	proc->segUsage.globalsProcessed++;
      }
    else       
      while (!recentWorkDone(proc, localWorkSize) && 
	     ((globalLoc = (ploc_t) popStack(proc->globalLocs)) != NULL)) {
	ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	locSplitAlloc1L_copyCopySync_primaryStack(proc,replicaLoc,objStack,copyRange,srcRange,largeSpace);
	proc->segUsage.globalsProcessed++;
      }
    /* Do the large objects - don't need to optimize loop */
    while (updateWorkDone(proc) < workToDo && !isEmptyStack(segmentStack)) {
      gray = popStack3(segmentStack,(ptr_t *)&largeStart,(ptr_t *)&largeEnd);
      if (GCType == Minor) {
	int isMirrorPtrArray = (GET_TYPE(gray[-1]) == MIRROR_PTR_ARRAY_TYPE);
	if (isMirrorPtrArray) {
	  fastAssert(inHeap(gray, fromSpace));
	  selfTransferScanSegment_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(proc,gray,largeStart,largeEnd,
											 objStack, segmentStack,
											 copyRange, srcRange);
	}
	else
	    transferScanSegment_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(proc,gray,largeStart,largeEnd,
										       objStack, segmentStack,
										       copyRange, srcRange);
      }
      else
	 transferScanSegment_copyWriteSync_locSplitAlloc1L_copyCopySync_primaryStack(proc,gray,largeStart,largeEnd,
										     objStack, segmentStack,
										     copyRange, srcRange, largeSpace);
    }
    /* Work on the gray objects */
    if (GCType == Minor)
      while (!recentWorkDone(proc, localWorkSize) &&
	     ((gray = popStack(objStack)) != NULL)) 
	transferScanObj_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(proc,gray,objStack,segmentStack,copyRange,srcRange);
    else
      while (!recentWorkDone(proc, localWorkSize) &&
	     ((gray = popStack(objStack)) != NULL))
	transferScanObj_copyWriteSync_locSplitAlloc1L_copyCopySync_primaryStack(proc,gray,objStack,segmentStack,copyRange,srcRange,largeSpace);
    /* Put work back on shared stack */
    if (!local) {
      int sharedWorkDone = pushSharedStack(workStack, &proc->threads, proc->globalLocs, proc->rootLocs, objStack, segmentStack);
      if (sharedWorkDone) {
	GCStatus = GCPendingOff;
	done = 1;
      }
    }
    else {
      if (isEmptyStack(objStack) && isEmptyStack(segmentStack) && 
	  isEmptyStack(proc->backObjs) && isEmptyStack(proc->backLocs))
	done = 1;
    }
    done |= (updateWorkDone(proc) >= workToDo);
  }


  assert(isEmptyStack(objStack));
  assert(isEmptyStack(segmentStack));
  if (collectDiag >= 2)
    printf("GC %d Segment %d:  do_work:  workToDo = %d segWork = %d   shared workStack has %d items %d segments\n",
	   NumGC, proc->numSegment, workToDo, getWorkDone(proc), workStack->obj.cursor, workStack->segment.cursor);

}


void GCRelease_GenConc(Proc_t *proc)
{
  mem_t allocCurrent = proc->allocStart;
  mem_t allocStop = proc->allocCursor;
  ploc_t writelistStart = proc->writelistStart;
  ploc_t writelistCurrent = proc->writelistStart;
  ploc_t writelistStop = proc->writelistCursor;
  int isGCOn = (GCStatus == GCOn) || (GCStatus == GCPendingOff);
  Stack_t *objStack = (GCType == Minor) ? &proc->majorObjStack : &proc->majorObjStack;
  Stack_t *segmentStack = (GCType == Minor) ? &proc->minorSegmentStack : &proc->majorSegmentStack;
  CopyRange_t *copyRange = (GCType == Minor) ? &proc->minorRange : &proc->majorRange;
  Heap_t *srcRange = (GCType == Minor) ? nursery : fromSpace;

  /* Update statistics, reset cursors */
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  proc->segUsage.bytesAllocated += sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->allocStart = proc->allocCursor;          
  proc->writelistCursor = proc->writelistStart;  

  /* Replicate objects allocated by mutator when the collector is on */
  if (isGCOn) {
    if (collectDiag >= 2)
      printf("Proc %d: Scanning/Replicating %d to %d\n",proc->procid,allocCurrent,allocStop);
    proc->segUsage.bytesReplicated += sizeof(val_t) * (allocStop - allocCurrent);
    while (allocCurrent + 1 <= allocStop) {  /* There may be no data for empty array */
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
      objSize = splitAlloc_copyCopySync_primaryStack(proc,obj,objStack,copyRange,srcRange);
      if (objSize == 0) {
	mem_t dummy = NULL;
	objSize = objectLength(obj, &dummy);
	fastAssert(dummy == allocCurrent);
      }
      allocCurrent += objSize / sizeof(val_t);
    }
  }
  
  /* Process writes */
  if (diag && writelistCurrent < writelistStop)
    printf("Proc %d: Processing writes from %d to %d     primaryArrayOffset = %d\n",
	   proc->procid,writelistCurrent,writelistStop, primaryArrayOffset);
  while (writelistCurrent < writelistStop) {
    ptr_t mutatorPrimary = *writelistCurrent++;          /* Global, nursery, or fromSpace */
    int byteDisp = (int) *writelistCurrent++;
    ptr_t possPrevPtrVal = (ptr_t) *writelistCurrent++;  /* Pointer value only if pointer array was modified */
    int wordDisp = byteDisp / sizeof(val_t);
    int mirrorWordDisp = (primaryArrayOffset == 0) ? wordDisp + 1 : wordDisp - 1;
    int doubleWordDisp = byteDisp / (sizeof(double));
    int byteLen;  /* Length of data portion of array */

    if (IsGlobalData(mutatorPrimary)) {
      add_global_root(proc,mutatorPrimary);
      continue;
    }
    if (inHeap(mutatorPrimary, largeSpace))
      continue;

    if (!isGCOn) {                                /* Record back-pointers when GC is off for roots of next GC */
      if (GCType == Minor &&                      /* If next GC is major, no backLocs needed */
	  inHeap(mutatorPrimary, fromSpace)) {
	tag_t tag = mutatorPrimary[-1];
	if (GET_TYPE(tag) == MIRROR_PTR_ARRAY_TYPE) {
	  pushStack2(proc->backLocs, (ptr_t) &mutatorPrimary[wordDisp], (ptr_t) 0);
	}
	else 
	  fastAssert((GET_TYPE(tag) == WORD_ARRAY_TYPE ||
		      GET_TYPE(tag) == QUAD_ARRAY_TYPE));
      }
    }
    else {
      ptr_t nurseryPrimary = NULL;           /* Used if primary was in nursery */
      ptr_t fromSpaceEither = NULL;          /* Used for primaryReplica or (minor replica/major primary) */
      ptr_t toSpaceReplica = NULL;           /* Used only during major GC*/
      tag_t tag;                             /* Actual tag deteremined after traversing forwarding pointers */

      /* Make a copy of the modified arrays and get tag */
      if (inHeap(mutatorPrimary, nursery)) {
	assert(GCType == Minor);
	nurseryPrimary = mutatorPrimary;
	splitAlloc1_copyCopySync_primaryStack(proc,nurseryPrimary,objStack,copyRange,srcRange);
	fromSpaceEither = (ptr_t) nurseryPrimary[-1];
	tag = fromSpaceEither[-1];
      }
      else if (inHeap(mutatorPrimary,fromSpace)) {
	fromSpaceEither = mutatorPrimary;
	if (GCType == Major) {
	  splitAlloc1_copyCopySync_primaryStack(proc,fromSpaceEither,objStack,copyRange,srcRange);
	  toSpaceReplica = (ptr_t) fromSpaceEither[-1];
	  tag = toSpaceReplica[-1];
	}
	else
	  tag = fromSpaceEither[-1];
      }
      else 
	assert(0);

      byteLen = GET_ANY_ARRAY_LEN(tag);
      if (byteLen <= arraySegmentSize) {
	if (nurseryPrimary)
	  while (nurseryPrimary[-1] == STALL_TAG)
	    ;
	if (toSpaceReplica)
	  while (fromSpaceEither[-1] == STALL_TAG)
	    ;
      }
      else {
	int segment = DivideUp(byteDisp, arraySegmentSize);
	if (nurseryPrimary)
	  while (nurseryPrimary[-2-segment] == SEGSTALL_TAG)
	    ;
	if (toSpaceReplica)
	  while (fromSpaceEither[-2-segment] == SEGSTALL_TAG)
	    ;
      }
      switch (GET_TYPE(tag)) {
      case WORD_ARRAY_TYPE:
	if (nurseryPrimary)
	  fromSpaceEither[wordDisp] = nurseryPrimary[wordDisp];
	if (toSpaceReplica)
	  toSpaceReplica[wordDisp] = fromSpaceEither[wordDisp];
	break;
      case QUAD_ARRAY_TYPE: 
	if (nurseryPrimary)
	  ((double *)fromSpaceEither)[doubleWordDisp] = ((double *)nurseryPrimary)[doubleWordDisp];
	if (toSpaceReplica)
	  ((double *)toSpaceReplica)[doubleWordDisp] = ((double *)fromSpaceEither)[doubleWordDisp];
	break;
      case PTR_ARRAY_TYPE: 
	assert(0); 
      case MIRROR_PTR_ARRAY_TYPE: {
	/* Snapshot-at-the-beginning (Yuasa) write barrier requires copying prevPtrVal 
	   even if it might die to prevent the mutator from hiding live data.

	   There are several cases:
	   (1) MinorGC: The original array is in nursery/fromSpace.
	       (i) (a) The field is in nursery.           Update new/existing array (replica entry) with new (fromSpace) field.
	           (b) The field is in fromSpace.         Update new/existing array (replica entry) with same field.
	           (c) The field is in global/largeSpace. Update new/existing array (replica entry) with same field.
	       (ii) If original array in fromSpace, add location to backLocs for replication. Same as when GC is off.
	   (3) MajorGC: The original array must be in fromSpace.
               (a) The field is in fromSpace. Update new array (replica entry) with new (toSpace) field.
	       (b) The field is in global/largeSpace. Update new array with field.  (toSpaceField is NULL).
	*/
	ptr_t mutatorField = (ptr_t) mutatorPrimary[wordDisp];
	
	fastAssert(((byteDisp - primaryArrayOffset) % (2 * sizeof(val_t))) == 0);

	/* Snapshot-at-the-beginning (Yuasa) write barrier requires copying prevPtrVal 
	   even if it might die to prevent the mutator from hiding live data */
	splitAlloc1_copyCopySync_primaryStack(proc,possPrevPtrVal,objStack,copyRange,srcRange);

	if (GCType == Minor) {
	    ptr_t newField = mutatorField;
	    if (inHeap(newField, nursery)) {
	      splitAlloc1_copyCopySync_primaryStack(proc,mutatorField,objStack,copyRange,srcRange);
	      newField = (ptr_t) mutatorField[-1];
	    }
	    fastAssert(!inHeap(newField, nursery));
	    fromSpaceEither[mirrorWordDisp] = (val_t) newField;
	    if (inHeap(mutatorPrimary, nursery))
	      fromSpaceEither[wordDisp] = (val_t) newField;
	    else {
	      fastAssert(inHeap(mutatorPrimary, fromSpace));
	      pushStack2(proc->backLocs, (ptr_t) &mutatorPrimary[wordDisp], (ptr_t) 0);
	    }
	}
	else {  
	  ptr_t newField = mutatorField;
	  assert(GCType == Major);
	  fastAssert(inHeap(mutatorPrimary,fromSpace));
	  if (inHeap(newField,fromSpace)) {
	    splitAlloc1L_copyCopySync_primaryStack(proc,mutatorField,objStack,copyRange,srcRange,largeSpace);
	    newField = (ptr_t) mutatorField[-1];
	  }
	  toSpaceReplica[wordDisp] = (val_t) newField;
	  toSpaceReplica[mirrorWordDisp] = (val_t) newField;
	}
	break;
      }
      default:
	assert(0);
      } /* else */
    } /* switch */
  } /* while */
  
  if (isGCOn) {
    condPushSharedStack(workStack, &proc->threads, proc->globalLocs, proc->rootLocs, objStack, segmentStack);
  }
  flushStore();

}

static int GC_GenConcHelp(Proc_t *proc, int roundSize)
{
  GetHeapArea((GCType == Minor) ? nursery : fromSpace,
	      roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  if (proc->allocStart)
    return 1;
  return 0;
}

/* Try to obatain space in proc to run th.  If th is NULL, an idle processor is signified */
void GC_GenConc(Proc_t *proc, Thread_t *th)
{
  int satisfied = 0;  /* satisfied = 1 means GC stayed off; 2 means it turned on or was on */
  int roundOffSize = minOffRequest;                            /* default work amount for idle processor */
  int roundOnSize = minOnRequest;    
  int minorWorkToDo, majorWorkToDo;

  assert(proc->writelistCursor + 2 <= proc->writelistEnd);
  flushStore();


  /* If space requirement satisfied and we are not turning collector off, do no work. */
  if (th != NULL) {
    assert(th->requestInfo != 0);
    if (th->requestInfo < 0) {
      if (GCStatus != GCPendingOff) {
	unsigned int writelistBytesAvailable = sizeof(val_t) * (proc->writelistEnd - proc->writelistCursor);
	assert((-th->requestInfo) <= writelistBytesAvailable);
	return;
      }
      roundOffSize = RoundUp(4, minOffRequest);
      roundOnSize = RoundUp(4, minOnRequest);
    }
    else {
      roundOffSize = RoundUp(th->requestInfo,minOffRequest);
      roundOnSize = RoundUp(th->requestInfo,minOnRequest);
    }
  }
  minorWorkToDo = (int) (1.0 + minorCollectionRate) * roundOnSize;  /* + 1 for the work done in replicating primary */
  majorWorkToDo = (int) (1.0 + majorCollectionRate) * roundOnSize;
  /* XXXXXXXXXXXXXX might fall behind XXXXXXXXX */
  if (roundOnSize > 2 * minOnRequest) {
    minorWorkToDo = (int) (1.0 + minorCollectionRate) * (2 * minOnRequest);
    majorWorkToDo = (int) (1.0 + majorCollectionRate) * (2 * minOnRequest);
  }

  /* If GCStatus is off but GCType is Major, then we immediately start a major collection */
  while (!satisfied) {
    switch (GCStatus) {
    case GCOff:                                          /* Off; Off, PendingOn */
    case GCPendingOn:
      if (th == NULL ||                                /* Don't allocate/return if not really a thread */
	  GC_GenConcHelp(proc,roundOffSize)) {
	if (!satisfied)
	  satisfied = 1;
	break;
      }
      procChangeState(proc, GC);
      proc->gcSegment1 = (GCType == Major) ? MajorWork : MinorWork;
      proc->gcSegment2 = FlipOn;
      CollectorOn(proc);                                 /* Off, PendingOn; On, PendingOff */
      assert(GCStatus == GCOn || GCStatus == GCPendingOff);
      if ((th == NULL) ||                                /* Idle processor done some work */
          (th != NULL && GCStatus == GCOn &&             /* Don't allocate/return if not really a thread */
           GC_GenConcHelp(proc,roundOnSize))) {       /* On, PendingOff; same */
        satisfied = 2;
        break;
      }
      if (GCStatus == GCPendingOff)                      /* If PendingOff, must loop */
        break;                    
      printf("Proc %d: Conc coll fell behind.  Could not allocate %d bytes\n", proc->procid, roundOnSize);
      printf("        GCType = %d.   GCStatus = %d.\n", GCType, GCStatus);
      assert(0);
      break;
    case GCOn:
      procChangeState(proc, GC);
      proc->gcSegment1 = (GCType == Major) ? MajorWork : MinorWork;
      do_work(proc, GCType == Minor ? minorWorkToDo : majorWorkToDo, 0);
      if (th != NULL)
	if (!GC_GenConcHelp(proc,roundOnSize)) {
	  printf("Proc %d: Conc coll fell behind.  Could not allocate %d bytes for thread %d\n", 
		 proc->procid, roundOnSize, th->tid);
	  printf("        GCType = %d.   GCStatus = %d.\n", GCType, GCStatus);
	  assert(0);
	}
      satisfied = 2;
      break;
    case GCPendingOff:                                  /* If PendingOff, shared work is completed. */
      procChangeState(proc, GC);
      proc->gcSegment1 = (GCType == Major) ? MajorWork : MinorWork;
      proc->gcSegment2 = FlipOff;
      do_work(proc, 
	      sizeof(val_t) * Heap_GetSize(nursery), 1); /* Actually bounded by the allocation of one processor */
      if (GCType == Minor) 
	PadCopyRange(&proc->minorRange);                 /* Pad so that paranoid check works */
      else if (GCType == Major) {                        /* Carry over minor ranges across minor GCs */
	if (proc->minorRange.discharge != NULL)         /* Might have 2 consecutive major GC */
	  ClearCopyRange(&proc->minorRange); 
	ClearCopyRange(&proc->majorRange);
      }
      CollectorOff(proc);                               /* PendingOff; Off, PendingOn */
      assert(GCStatus == GCOff || GCStatus == GCPendingOn);
      break;                                            /* Must repeat loop */
    default: 
      assert(0);
    }
  }
}

void GCPoll_GenConc(Proc_t *proc)
{
  (void) GC_GenConc(proc, NULL);
}

void GCInit_GenConc()
{
  int cache_size = GetBcacheSize();
  double nurseryFraction = 1.2 * NumProc;

  init_int(&YoungHeapByte, (int)(nurseryFraction * cache_size));
  init_int(&MaxHeap, 128 * 1024);
  init_int(&MinHeap, 2048);
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

  expandedNurserySize = YoungHeapByte;
  reducedNurserySize = expandedToReduced(expandedNurserySize, minorCollectionRate);
  reducedNurserySize -= (1 + NumProc) * pagesize;      /* One extra page per processor */
  expandedTenuredSize = Heap_GetSize(fromSpace);
  reducedTenuredSize = expandedToReduced(expandedTenuredSize, majorCollectionRate);
  Heap_Resize(nursery, reducedNurserySize, 0);
  Heap_Resize(fromSpace, reducedTenuredSize, 0);
  Heap_Resize(toSpace, reducedTenuredSize, 0);

  gc_large_init();
  workStack = SharedStack_Alloc(100, 16 * 1024, 8 * 1024, 64 * 1024, 16 * 1024);
  barriers = createBarriers(NumProc, 9);
  arraySegmentSize = 2 * 1024;
  mirrorGlobal = 1;
  mirrorArray = 1;
}

