#include <strings.h>
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>
#include <values.h>

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


int multiPhase = 1;  /* Allow the collector to flip on, work, flip off in same segment */

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


/* weights can be reset in init routine */
static int reducedNurserySize = 0, expandedNurserySize = 0, maximumNurserySize = 0;
static int reducedTenuredSize = 0, expandedTenuredSize = 0;
static int tenuredReserve = 256 * 1024;  /* Trigger major collection this much sooner due to possible large object allocations XXXX */

/* ------------------  Parallel array allocation routines ------------------- */

mem_t AllocBigArray_GenConc(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj = NULL;
  /* Double length for PointerField to make MirrorPtrArray */
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
  /* Should do work proportional to size of array */
  /*
    if (GCStatus != GCOff && GCStatus != GCPendingOn && GCStatus != GCPendingAgressive) 
    do_work(proc, CollectionRate * tagByteLen);
  */
  assert(region != NULL);
  proc->segUsage.bytesAllocated += tagByteLen;

  /* Allocate object; update stats; initialize */
  obj = region + 1 + segments;
  for (i=0; i<segments; i++)
    obj[-(2+i)] = SEGPROCEED_TAG;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : DIE("pointer array");
    case MirrorPointerField : 
      init_double_ptr_array(obj, spec->elemLen, spec->pointerVal); 
      SetPush(&proc->work.backObjs, obj);
      SetPush(&proc->work.nextBackObjs, obj);
      break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Concurrent collector --------------------- */
static long totalRequest = 0;     /* Total number of bytes requested by all threads */
static long totalReplicated = 0;  /* Total number of bytes replicated by all processors */
static long totalUnused = 0;      /* Total number of bytes unused but assigned to processors */

static void establishCopyRange(Proc_t *proc)
{
  if (GCType == Minor) {
    if (proc->copyRange.heap == fromSpace)
      ClearCopyRange(&proc->copyRange); 
    SetCopyRange(&proc->copyRange, proc, fromSpace, NULL);
  }
  else {                                      /* Next GC is major */
    if (proc->copyRange.heap == nursery)
      ClearCopyRange(&proc->copyRange); 
    SetCopyRange(&proc->copyRange, proc, toSpace, NULL);
  }
}

static void CollectorOn(Proc_t *proc)
{

  int i, len, isFirst = 0;
  Thread_t *threadIterator = NULL;
  Set_t *objStack = &proc->work.objs;
  Set_t *segmentStack = &proc->work.segments;
  Heap_t *srcRange = (GCType == Minor) ? nursery : fromSpace;

  /* Major vs Minor of current GC was determined at end of last GC */
  procChangeState(proc, GCWork, 600);
  proc->segmentType |= (FlipOn | ((GCType == Major) ? MajorWork : MinorWork));

  switch (GCStatus) {
    case GCOff:                   /* Signalling to other processors that collector is turning on */
      GCStatus = (GCType == Major ? doAgressive : doMinorAgressive) ? GCPendingAgressive : GCPendingOn;
      StopAllThreads();
      break;
    case GCPendingOn:             /* Responding to signal that collector is turning on */
    case GCPendingAgressive:
      break;
    default: 
      DIE("CollectorOn");
  }

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     preliminary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  isFirst = (weakBarrier(barriers,proc) == 0);
  if (isFirst) {
    Heap_ResetFreshPages(proc,nursery);
    if (GCType == Major) 
      Heap_ResetFreshPages(proc,fromSpace);
    if (GCType == Minor) {
      if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery)) {
	printf("Warning: fromSpace has less available space than total nursery size.\n"
	       "         Probably due to fromSpace pointer array allocation.\n");
	Heap_Resize(fromSpace, Heap_GetSize(fromSpace) + reducedNurserySize, 0);
	assert(Heap_GetAvail(fromSpace) >= Heap_GetSize(nursery));
      }
    }
    totalUnused = 0;
    totalRequest = 0;
    totalReplicated = 0;
    ResetJob();                               /* Reset counter so all user threads are scanned */
  }
  strongBarrier(barriers,proc);

  /* Reset root lists, compute thread-specific roots in parallel,
     determine whether a major GC was explicitly requested. */
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  assert(SetIsEmpty(&proc->work.roots));
  while ((threadIterator = NextJob()) != NULL) {
    initial_root_scan(proc,threadIterator);
    if (threadIterator->requestInfo >= 0)  /* Allocation request */
      FetchAndAdd(&totalRequest, threadIterator->requestInfo);
  }
  strongBarrier(barriers,proc);

  /* The "first" processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. 
     The major GC does not take effect until the first minor GC is completed.
  */
  if (isFirst) {
    paranoid_check_all(nursery, fromSpace, NULL, NULL, largeSpace);
    major_global_scan(proc);    /* Always a major_global_scan because we must flip all globals */
    if (GCType == Major) {
      Heap_Resize(fromSpace,expandedTenuredSize,0);
      Heap_Resize(toSpace,expandedTenuredSize,1);
      gc_large_startCollect();
    }
    else {
      /* Heap_Resize(nursery,expandedNurserySize,0); XXXXX */
       Heap_Resize(nursery,maximumNurserySize,0); /* Allocating large arrays makes this necesary */
    }
  }
  resetSharedStack(workStack,&proc->work, 1);
  strongBarrier(barriers, proc);

  /* Check local stack empty, prepare copy range,
     forward all the roots (first proc handles backpointers),
     transfer work from local to shared work stack */
  procChangeState(proc, GCWork, 603);

  assert(SetIsEmpty(&proc->work.objs));
  establishCopyRange(proc);

  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);

  /* Omit popSharedObjStack */
  pushSharedStack(0,workStack, &proc->work);
  GCStatus = (GCType == Major ? doAgressive : doMinorAgressive) ? GCAgressive : GCOn;
  strongBarrier(barriers, proc);
}

static void CollectorTransition(Proc_t *proc)
{

  int i, len, isFirst = 0;
  Thread_t *threadIterator = NULL;
  Set_t *objStack = &proc->work.objs;
  Set_t *segmentStack = &proc->work.segments;
  Heap_t *srcRange = (GCType == Minor) ? nursery : fromSpace;

  /* Major vs Minor of current GC was determined at end of last GC */
  procChangeState(proc, GCWork, 604);
  proc->segmentType |= (FlipTransition | ((GCType == Major) ? MajorWork : MinorWork));

  switch (GCStatus) {
    case GCAgressive:             /* Signalling to other processors that collector is turning on */
      GCStatus = GCPendingOn;
      StopAllThreads();
      break;
    case GCPendingOn:             /* Responding to signal that collector is turning on */
      break;
    default: 
      DIE("CollectorTransition");
  }

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     prelimiary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  isFirst = (weakBarrier(barriers,proc) == 0);
  if (isFirst) {
    ResetJob();                               /* Reset counter so all user threads are scanned */
  }
  resetSharedStack(workStack,&proc->work, 0);
  strongBarrier(barriers,proc);

  /* Reset root lists, compute thread-specific roots in parallel,
     determine whether a major GC was explicitly requested. */
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  assert(SetIsEmpty(&proc->work.roots));
  while ((threadIterator = NextJob()) != NULL) {
    discard_root_scan(proc,threadIterator);
    if (threadIterator->used == 0)
      continue; 
    initial_root_scan(proc,threadIterator);
    if (threadIterator->requestInfo >= 0)  /* Allocation request */
      FetchAndAdd(&totalRequest, threadIterator->requestInfo);
  }
  strongBarrier(barriers,proc);

  /* The "first" processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. 
     The major GC does not take effect until the first minor GC is completed.
  */

  if (isFirst) 
    major_global_scan(proc);    /* Always a major_global_scan because we must flip all globals */
  strongBarrier(barriers, proc);

  /* Check local stack empty, prepare copy range,
     forward all the roots (first proc handles backpointers),
     transfer work from local to shared work stack */
  procChangeState(proc, GCWork, 607);
  assert(SetIsEmpty(&proc->work.objs));
  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);

  /* Omit popSharedObjStack */
  pushSharedStack(0,workStack, &proc->work);
  GCStatus = GCOn;
  strongBarrier(barriers, proc);
}


static void CollectorOff(Proc_t *proc)
{
  Thread_t *threadIterator = NULL;
  int isFirst;
  int nextGCType = Minor;      /* GCType will be written to during this function for the next GC
				  and so we save its value here for reading */
  procChangeState(proc, GCWork, 608);
  proc->segmentType |= FlipOff;

  if (collectDiag >= 2)
    printf("Proc %d: entered CollectorOff\n", proc->procid);
  assert(SetIsEmpty(&proc->work.objs));        /* Local stack must be empty */
  assert(GCStatus == GCPendingOff);
  memBarrier();

  PadCopyRange(&proc->copyRange);              /* Pad so that paranoid check works */

  isFirst = (weakBarrier(barriers,proc) == 0);
  if (isFirst) {
    ResetJob();
  }
  strongBarrier(barriers,proc);

  /* Local stacks must be empty. */
  assert(isLocalWorkEmpty(&proc->work));

  /* Replace all roots with replica */
  if (isFirst) 
    minor_global_scan(proc);   /* Even for a major GC since we already flipped global locs tenured when GC started */
  while ((threadIterator = NextJob()) != NULL) {
    complete_root_scan(proc, threadIterator);
    if (threadIterator->request == MajorGCRequestFromC) /* Runtime explicitly requests major GC */
      nextGCType = Major;
  }

  procChangeState(proc, GCWork, 611);
  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);
  while (!SetIsEmpty(&proc->work.roots)) {
    ploc_t root = (ploc_t) SetPop(&proc->work.roots);
    flipRootLoc(GCType, root);
  }
  while (!SetIsEmpty(&proc->work.globals)) {
    ptr_t global = SetPop(&proc->work.globals);
    ploc_t replicaLoc = DupGlobal(global);
    flipRootLoc(GCType, replicaLoc);
  }
  FetchAndAdd(&totalReplicated, proc->segUsage.bytesReplicated + proc->cycleUsage.bytesReplicated);
  strongBarrier(barriers,proc);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    if (GCType == Minor) {
      double liveRatio = 0.0;
      int i, copied = 0;
      paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
      minor_global_promote(proc);
      for (i=0; i<NumProc; i++) {
	Proc_t *p = getNthProc(i);;
	copied += bytesCopied(&p->cycleUsage) + bytesCopied(&p->segUsage);
      }
      liveRatio = (double) (copied) / (double) Heap_GetUsed(nursery);
      add_statistic(&minorSurvivalStatistic, liveRatio);
    }
    else { /* Major */
      discardNextSharedStack(workStack); /* Discard nextBackObj/nextBackLocs on major GC */
      paranoid_check_all(nursery, fromSpace, toSpace, NULL, largeSpace);
      gc_large_endCollect();
      HeapAdjust2(totalRequest, totalUnused, totalReplicated,  
		  CollectionRate, doAgressive ? 2 : 1,
		  nursery, fromSpace, toSpace);
      reducedTenuredSize = Heap_GetSize(toSpace);
      expandedTenuredSize = reducedToExpanded(reducedTenuredSize, CollectionRate, doAgressive ? 2 : 1);
      Heap_Resize(fromSpace, 0, 1);
      typed_swap(Heap_t *, fromSpace, toSpace);
      NumMajorGC++;                          
    }
    typed_swap(int, primaryGlobalOffset, replicaGlobalOffset);
    typed_swap(int, primaryArrayOffset, replicaArrayOffset);
    typed_swap(int, primaryStackletOffset, replicaStackletOffset);
    Heap_Resize(nursery,reducedNurserySize,1);
    NumGC++;
    GCStatus = GCOff;
    if (Heap_GetAvail(fromSpace) < tenuredReserve + Heap_GetSize(nursery)) {
      /*  The next GC needs to be a major GC so we must begin allocation in the fromSpace immediately. 
	  We permit allocation to continue so we don't flip on again too soon.  However, allocation
	  is restricted so the major collection is started soon so that an accurate survival rate
	  can be computed. */
      GCType = Major;        
      fromSpace->top = fromSpace->cursor + (minOffRequest * NumProc) / sizeof(val_t);
    }
    else
      GCType = nextGCType;
  }

  /* All system threads need to reset their limit pointer */
  ResetAllocation(proc, NULL);
  proc->writelistCursor = proc->writelistStart;

  strongBarrier(barriers,proc);
  establishCopyRange(proc);    /* Called here to copyRanges are initialized for use in GCRelease */

  if (collectDiag >= 2)
    printf("Proc %d: leaving CollectorOff\n", proc->procid);
}

static void do_work(Proc_t *proc, int additionalWork)
{
  int done = 0;
  Heap_t *srcRange = (GCType == Minor) ? nursery : fromSpace;
  
  if (additionalWork <= 0)
    return;
  addMaxWork(proc, additionalWork);
  procChangeState(proc, GCWork, 612);
  proc->segmentType |= (GCType == Major) ? MajorWork : MinorWork;

  if (collectDiag >= 2)
    printf("GC %4d/%6d:  Entered do_work(%d)  updateWorkDone = %5d\n",
	   NumGC, proc->segmentNumber, additionalWork, updateGetWorkDone(proc));

  while (!reachMaxWork(proc)) {
    ploc_t rootLoc, globalLoc, backLoc;
    ptr_t backObj, largeObj, gray;
    int largeStart, largeEnd;
    Stacklet_t *stacklet;

    /* Get shared work */
    popSharedStack(workStack, &proc->work);

    if ((GCStatus == GCPendingOn || GCStatus == GCPendingOff) &&
	isLocalWorkEmpty(&proc->work))
      break;

    /* Do the thread stacklets - we can afford to call updateWorkDone often */
    while (!reachCheckWork(proc) &&
	   ((stacklet = (Stacklet_t *) SetPop(&proc->work.stacklets)) != NULL)) {
      if (!work_root_scan(proc, stacklet))
	SetPush(&proc->work.stacklets, (ptr_t) stacklet);
    }

    /* Do the rootLocs */
    if (GCType == Minor) {
      while (!reachCheckWork(proc) &&
	     ((rootLoc = (ploc_t) SetPop(&proc->work.roots)) != NULL)) {
	locAlloc1_copyCopySync_primarySet(proc,rootLoc,srcRange);
	proc->segUsage.fieldsScanned++;
      }
    }
    else {
      while (!reachCheckWork(proc) && 
	     ((rootLoc = (ploc_t) SetPop(&proc->work.roots)) != NULL)) {
	locAlloc1L_copyCopySync_primarySet(proc,rootLoc,srcRange,largeSpace);
	proc->segUsage.fieldsScanned++;
      }
    }

    /* Do the backObjs and backLocs */
    if (GCType == Minor) {
      int occur;
      while (!updateReachCheckWork(proc) &&
	     ((backObj = (ptr_t) SetPop(&proc->work.backObjs)) != NULL)) {
	selfTransferScanObj_locAlloc1_copyCopySync_primarySet(proc,backObj,srcRange);
      }
      while (!reachCheckWork(proc) &&
	     ((backLoc = (ploc_t) SetPop(&proc->work.backLocs)) != NULL)) {
	ploc_t mirrorBackLoc = backLoc + ((primaryArrayOffset == 0) ? 1 : -1);
	*mirrorBackLoc = *backLoc;
	locAlloc1_copyCopySync_primarySet(proc,mirrorBackLoc,srcRange);
	proc->segUsage.fieldsScanned += 2;
      }
    }
    else {
      SetReset(&proc->work.backObjs);
      SetReset(&proc->work.backLocs);
    }

    /* Do the globalLocs */
    if (GCType == Minor) 
      while (!reachCheckWork(proc) && 
	     ((globalLoc = (ploc_t) SetPop(&proc->work.globals)) != NULL)) {
	ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	locAlloc1_copyCopySync_primarySet(proc,replicaLoc,srcRange);
	proc->segUsage.globalsProcessed++;
      }
    else       
      while (!reachCheckWork(proc) &&
	     ((globalLoc = (ploc_t) SetPop(&proc->work.globals)) != NULL)) {
	ploc_t replicaLoc = (ploc_t) DupGlobal((ptr_t) globalLoc);
	locAlloc1L_copyCopySync_primarySet(proc,replicaLoc,srcRange,largeSpace);
	proc->segUsage.globalsProcessed++;
      }

    /* Do the large objects - don't need to optimize loop */
    while (!updateReachCheckWork(proc) &&
	   (gray = SetPop3(&proc->work.segments,(ptr_t *)&largeStart,(ptr_t *)&largeEnd))) {
      if (GCType == Minor) {
	int isMirrorPtrArray = (GET_TYPE(gray[-1]) == MIRROR_PTR_ARRAY_TYPE);
	if (isMirrorPtrArray) {
	  fastAssert(inHeap(gray, fromSpace));
	  selfTransferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primarySet(proc,gray,largeStart,largeEnd,srcRange);
	}
	else
	    transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primarySet(proc,gray,largeStart,largeEnd,srcRange);
      }
      else
	 transferScanSegment_copyWriteSync_locAlloc1L_copyCopySync_primarySet(proc,gray,largeStart,largeEnd,srcRange, largeSpace);
    }

    /* Work on the gray objects */
    if (GCType == Minor)
      while (!reachCheckWork(proc) &&
	     ((gray = SetPop(&proc->work.objs)) != NULL)) 
	transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primarySet(proc,gray,srcRange);
    else
      while (!reachCheckWork(proc) &&
	     ((gray = SetPop(&proc->work.objs)) != NULL))
	transferScanObj_copyWriteSync_locAlloc1L_copyCopySync_primarySet(proc,gray,srcRange,largeSpace);
    /* Put work back on shared stack */
    if (pushSharedStack(0,workStack,&proc->work)) {
      if (GCStatus == GCAgressive)
	GCStatus = GCPendingOn;
      else if (GCStatus == GCOn)
	GCStatus = GCPendingOff;
    }
  }

  assert(isLocalWorkEmpty(&proc->work));
  if (collectDiag >= 2)
    printf("GC %d Seg %d:  Completed do_work(%d)  segWork = %5d    sharedStack(%4d items %2d segs)     %d alloc  %d copied\n",
	   NumGC, proc->segmentNumber, additionalWork, updateGetWorkDone(proc), 
	   SetLength(&workStack->work.objs), SetLength(&workStack->work.segments),
	   proc->cycleUsage.bytesAllocated + proc->segUsage.bytesAllocated,
	   bytesCopied(&proc->cycleUsage) + bytesCopied(&proc->segUsage));

}



void GCRelease_GenConc(Proc_t *proc)
{
  mem_t allocCurrent = proc->allocStart;
  mem_t allocStop = proc->allocCursor;
  ploc_t writelistStart = proc->writelistStart;
  ploc_t writelistCurrent = proc->writelistStart;
  ploc_t writelistStop = proc->writelistCursor;
  int isGCAgressive = (GCType == Major ? doAgressive : doMinorAgressive) ? 
                      (GCStatus == GCAgressive) || (GCStatus == GCPendingOn) : 0;
  int isGCOn = isGCAgressive || (GCStatus == GCOn) || (GCStatus == GCPendingOff);
  Heap_t *srcRange = (GCType == Minor) ? nursery : fromSpace;
  int numWrites = (proc->writelistCursor - proc->writelistStart) / 3;

  /* Reset cursors */
  proc->allocStart = proc->allocCursor;          
  proc->writelistCursor = proc->writelistStart;  


  /* Replicate objects allocated by mutator when the collector is on */
  if (isGCOn && !isGCAgressive) {
    procChangeState(proc, GCReplicate, 613);
    if (collectDiag >= 2)
      printf("GC %4d/%6d:   Double-allocating %d to %d\n",
	     NumGC, proc->segmentNumber, allocCurrent,allocStop);
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
      alloc_copyCopySync_primarySet(proc,obj);
      objSize = proc->bytesCopied;
      if (objSize == 0) {
	mem_t dummy = NULL;
	objSize = objectLength(obj, &dummy);
	fastAssert(dummy == allocCurrent);
      }
      allocCurrent += objSize / sizeof(val_t);
    }
  }


  /* Process writes */
  if (collectDiag >= 2 && writelistCurrent < writelistStop)
    printf("Proc %d: Processing %d writes from %d to %d\n",
	   proc->procid,numWrites,writelistCurrent,writelistStop);
  procChangeState(proc, GCWrite, 614);
  while (writelistCurrent < writelistStop) {
    ptr_t mutatorPrimary = *writelistCurrent++;          /* Global, nursery, or fromSpace */
    int byteDisp = (int) *writelistCurrent++;
    ptr_t possPrevPtrVal = (ptr_t) *writelistCurrent++;  /* Pointer value only if pointer array was modified */
    int wordDisp = byteDisp / sizeof(val_t);
    int mirrorWordDisp = (primaryArrayOffset == 0) ? wordDisp + 1 : wordDisp - 1;
    int doubleWordDisp = byteDisp / (sizeof(double));
    int byteLen;  /* Length of data portion of array */
    int syncIndex;

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
	  SetPush(&proc->work.backLocs, (ptr_t) &mutatorPrimary[wordDisp]);
	  SetPush(&proc->work.nextBackLocs, (ptr_t) &mutatorPrimary[mirrorWordDisp]);
	}
	else 
	  fastAssert((GET_TYPE(tag) == WORD_ARRAY_TYPE ||
		      GET_TYPE(tag) == QUAD_ARRAY_TYPE));
      }
    }
    else {
      vptr_t nurseryPrimary = NULL;           /* Used if primary was in nursery */
      vptr_t fromSpaceEither = NULL;          /* Used for primaryReplica or (minor replica/major primary) */
      vptr_t toSpaceReplica = NULL;           /* Used only during major GC*/
      tag_t tag;                              /* Actual tag deteremined after traversing forwarding pointers */

      
      /* Make a copy of the modified arrays and get tag */
      if (inHeap(mutatorPrimary, nursery)) {
	fastAssert(GCType == Minor);
	nurseryPrimary = mutatorPrimary;
	tag = nurseryPrimary[-1];
	/* If object has not been copied, simply gray previous pointer value */
	switch (GET_TYPE(tag)) {
          case PTR_ARRAY_TYPE:
          case MIRROR_PTR_ARRAY_TYPE:
	    alloc1_copyCopySync_primarySet(proc,possPrevPtrVal, srcRange);
	    continue;
          case WORD_ARRAY_TYPE: 
          case QUAD_ARRAY_TYPE: 
	    continue;
	}
	while ((val_t)(fromSpaceEither = (ptr_t) nurseryPrimary[-1]) == STALL_TAG)
	  ;
	tag = fromSpaceEither[-1];
      }
      else if (inHeap(mutatorPrimary,fromSpace)) {
	fromSpaceEither = mutatorPrimary;
	if (GCType == Major) {
	  alloc1_copyCopySync_primarySet(proc,(ptr_t) fromSpaceEither,srcRange);
	  toSpaceReplica = (ptr_t) fromSpaceEither[-1];
	  tag = toSpaceReplica[-1];
	}
	else
	  tag = fromSpaceEither[-1];
      }
      else 
	DIE("mutatorPrimary in neither fromSpace nor nursery");


      /* Copy-Write synchronization on replica object */
      byteLen = GET_ANY_ARRAY_LEN(tag);
      syncIndex = (byteLen <= arraySegmentSize) ? -1 : -2-(DivideDown(byteDisp, arraySegmentSize));
      if (nurseryPrimary)
	while (fromSpaceEither[syncIndex] == SEGSTALL_TAG)
	    ;
      else if (toSpaceReplica)
	while (toSpaceReplica[syncIndex] == SEGSTALL_TAG)
	  ;

      switch (GET_TYPE(tag)) {
      case WORD_ARRAY_TYPE:
	if (nurseryPrimary) {
	  if (fromSpaceEither[0] == (val_t) nurseryPrimary) 	/* Backpointer present indicates object not yet scanned - can/must skip replica update */
	    break;
	  fromSpaceEither[wordDisp] = nurseryPrimary[wordDisp];
	}
	if (toSpaceReplica)
	  toSpaceReplica[wordDisp] = fromSpaceEither[wordDisp];
	break;
      case QUAD_ARRAY_TYPE: 
	if (nurseryPrimary) {
	  if (fromSpaceEither[0] == (val_t) nurseryPrimary) 	/* Backpointer present indicates object not yet scanned - can/must skip replica update */
	    break;
	  ((double *)fromSpaceEither)[doubleWordDisp] = ((double *)nurseryPrimary)[doubleWordDisp];
	}
	if (toSpaceReplica)
	  ((double *)toSpaceReplica)[doubleWordDisp] = ((double *)fromSpaceEither)[doubleWordDisp];
	break;
      case PTR_ARRAY_TYPE: 
	DIE("pointer array"); 
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
	alloc1_copyCopySync_primarySet(proc,possPrevPtrVal,srcRange);


	if (GCType == Minor) {
	    ptr_t newField = mutatorField;
	    if (inHeap(newField, nursery)) {
	      alloc1_copyCopySync_primarySet(proc,mutatorField,srcRange);
	      newField = (ptr_t) mutatorField[-1];
	    }
	    fastAssert(!inHeap(newField, nursery));
	    if (fromSpaceEither[0] == (val_t) mutatorPrimary) /* Backpointer present indicates object not yet scanned - can/must skip replica update */
		break;
	    fromSpaceEither[mirrorWordDisp] = (val_t) newField;
	    if (inHeap(mutatorPrimary, nursery))
	      fromSpaceEither[wordDisp] = (val_t) newField;
	    else {
	      fastAssert(inHeap(mutatorPrimary, fromSpace));
	      SetPush(&proc->work.backLocs, (ptr_t) &mutatorPrimary[wordDisp]);
	      SetPush(&proc->work.nextBackLocs, (ptr_t) &mutatorPrimary[mirrorWordDisp]);
	    }
	}
	else {  /* GCType == Major */
	  ptr_t newField = mutatorField;
	  fastAssert(inHeap(mutatorPrimary,fromSpace));
	  if (inHeap(newField,fromSpace)) {
	    alloc1L_copyCopySync_primarySet(proc,mutatorField,srcRange,largeSpace);
	    newField = (ptr_t) mutatorField[-1];
	  }
	  if (toSpaceReplica[0] == (val_t) mutatorPrimary) /* Backpointer present indicates object not yet scanned - can/must skip replica update */
	    break;
	  toSpaceReplica[wordDisp] = (val_t) newField;
	  toSpaceReplica[mirrorWordDisp] = (val_t) newField;
	}
	break;
      }
      default:
	DIE("bad tag type");
      } /* else */
    } /* switch */
  } /* while */
  
  if (isGCOn) 
    pushSharedStack(1,workStack, &proc->work);
}

static int GC_GenConcHelp(Thread_t *th, Proc_t *proc, int roundSize)
{
  if (th == NULL)
    return 1;
  GetHeapArea((GCType == Minor) ? nursery : fromSpace,
	      roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  if (proc->allocStart)
    return 1;
  return 0;
}

/* Try to obatain space in proc to run th.  If th is NULL, an idle processor is signified */
void GC_GenConc(Proc_t *proc, Thread_t *th)
{
  int roundOffSize = minOffRequest;        /* Effective request size */
  int roundOnSize = minOnRequest;    
  int minorWorkToDo, majorWorkToDo;

  assert(proc->writelistCursor + 2 <= proc->writelistEnd);
  memBarrier();

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
  /* include 1.0 for the work done in replicating primary */
  minorWorkToDo = (int) ((GCStatus == GCAgressive ? 0.0 : 1.0) + CollectionRate) * roundOnSize;  
  majorWorkToDo = (int) ((GCStatus == GCAgressive ? 0.0 : 1.0) + CollectionRate) * roundOnSize;
  /* XXXXXXXXXXXXXX might fall behind XXXXXXXXX */
  if (roundOnSize > 2 * minOnRequest) {
    minorWorkToDo = (int) ((GCStatus == GCAgressive ? 0.0 : 1.0) + CollectionRate * (2 * minOnRequest));
    majorWorkToDo = (int) ((GCStatus == GCAgressive ? 0.0 : 1.0) + CollectionRate * (2 * minOnRequest));
  }

  /* If GCStatus is off but GCType is Major, then we immediately start a major collection */
  retry:
  switch (GCStatus) {
    case GCOff:                                          /* Off; Off, PendingOn */
      if (GC_GenConcHelp(th, proc,roundOffSize)) 
	goto satisfied;
      if (GCType == Major ? doAgressive : doMinorAgressive)
	goto pendingAgressive;
      goto pendingOn;
    case GCPendingAgressive:
    pendingAgressive:
      CollectorOn(proc);                               
      if (GC_GenConcHelp(th,proc,roundOnSize))
	goto satisfied;
      if (GCStatus == GCPendingOn)
	goto retry;
      goto fail;
    case GCPendingOn:
    pendingOn:
      if (GCType == Major ? doAgressive : doMinorAgressive) 
	CollectorTransition(proc);
      else
	CollectorOn(proc);
      if (multiPhase)
	goto retry;  
      if (GC_GenConcHelp(th, proc,roundOffSize)) 
	goto satisfied;
      if (GCStatus == GCPendingOff)
	goto retry;
      goto fail;
    case GCAgressive:
    case GCOn:
      do_work(proc, GCType == Minor ? minorWorkToDo : majorWorkToDo);
      if (multiPhase &&
	  reachMaxWork(proc) &&
	  GCStatus == GCPendingOff)
	goto retry;
      if (GC_GenConcHelp(th,proc,roundOnSize)) 
	goto satisfied;
      goto fail;
    case GCPendingOff:                                  /* If PendingOff, shared work is completed. */
      do_work(proc, MAXINT);                            /* Actually bounded by the allocation of one processor */
      CollectorOff(proc);                               /* PendingOff; Off, PendingOn */
      goto retry;
    default: 
       DIE("GC_GenConc");
  }

 satisfied:
  return;

 fail:
  fprintf(stderr,"Proc %d: Collector behind.  Failed to allocate %d bytes for thread %d\n", proc->procid, roundOnSize, th->tid);
  fprintf(stderr,"        GCType = %d.   GCStatus = %d.\n", GCType, GCStatus);
  DIE("out of memory");
}

void GCPoll_GenConc(Proc_t *proc)
{
  (void) GC_GenConc(proc, NULL);
}

void GCInit_GenConc(void)
{
  int cache_size = GetBcacheSize();
  double nurseryFraction = 1.2 * NumProc;

  if (ordering == DefaultOrder)
    ordering = StackOrder;
  if (ordering == HybridOrder)
    grayAsReplica = 1;

  GCInit_Help(0.2, 0.8, 512, 50 * 1024);   
  if (relaxed) {
    reducedTenuredSize = Heap_GetSize(fromSpace);
    expandedTenuredSize = reducedToExpanded(reducedTenuredSize, CollectionRate, doAgressive ? 2 : 1);
  }
  else {
    expandedTenuredSize = Heap_GetSize(fromSpace);
    reducedTenuredSize = expandedToReduced(expandedTenuredSize, CollectionRate, doAgressive ? 2 : 1);
    Heap_Resize(fromSpace, reducedTenuredSize, 0);
    Heap_Resize(toSpace, reducedTenuredSize, 0);
  }
  init_int(&NurseryByte, (int)(nurseryFraction * cache_size));
  assert(MinHeapByte >= 1.2*NurseryByte);
  reducedNurserySize = NurseryByte;
  expandedNurserySize = reducedToExpanded(reducedNurserySize, CollectionRate, doAgressive ? 2 : 1);
  maximumNurserySize = 2 * expandedNurserySize; 
  nursery = Heap_Alloc(maximumNurserySize, maximumNurserySize);
  Heap_Resize(nursery, reducedNurserySize, 0);

  gc_large_init();
  workStack = SharedStack_Alloc(1, 100, 16 * 1024, 12 * 1024, 96 * 1024, 16 * 1024, 2048, 256 * 1024);
  barriers = createBarriers(NumProc, 14);
  arraySegmentSize = 2 * 1024;
  mirrorGlobal = 1;
  mirrorArray = 1;
  pauseWarningThreshold = 10.0;
  addOldStackletOnUnderflow = 1;
}

