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
static int minorCollectionRate = 4;   /* Ratio of minor coll rate to alloc rate */
static int majorCollectionRate = 4;   /* Ratio of major coll rate to alloc rate */


/* Compute reduced heap size by reserving area and rounding down to nearest page */
int expandedToReduced(int size, int rate)
{
  int newSize = size * rate / (1 + rate);
  return RoundDown(newSize, pagesize);
}

int reducedToExpanded(int size, int rate)
{
  int newSize = size * (1 + rate)  / rate;
  return RoundUp(newSize, pagesize);
}

/* ------------------  Parallel array allocation routines ------------------- */

mem_t AllocBigArray_GenConc(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj = NULL;
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
    case PointerField : 
      init_parray(obj, spec->elemLen, spec->pointerVal); 
      pushStack(proc->primaryReplicaObjRoots, obj);        /* Object is one word past tag */
      break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Concurrent collector --------------------- */
static long totalRequest = 0;  /* Total number of bytes requested by all threads */

/* These are barrier synchronizations which reset the previous one when completed.
   So, after barrier_n is passed, all processors reset barrier_(n-1).  For this
   cyclical scheme to work, there must be at least three barriers. 
*/
static long numWaitOnProc = 0;  /* waiting for mutators to stop; first proc does prelim work */
static long numStackOnProc = 0; /* waiting for roots to be computed; determine if GC is major and then resize heaps */
static long numDoneOnProc = 0;  /* waiting for roots to be forwarded and work to be shared */

static long numWaitOffProc = 0;        /* waiting for mutators to stop; first proc does prelim work */
static long numFlipOffProc = 0;        /* waiting for roots to be replaced with replica;
				   if starting a major GC, find major roots and move to shared stack */				   
static long numDoneOffProc = 0;        /* waiting for minor_global_promote; space resize/flip */

static void CollectorOn(Proc_t *proc)
{
  int i, len, isFirst = 0;
  Thread_t *threadIterator = NULL;
  Stack_t *objStack = (GCType == Minor) ? &proc->minorStack : &proc->majorStack;
  CopyRange_t *copyRange = (GCType == Minor) ? &proc->minorRange : &proc->majorRange;
  range_t *srcRange = (GCType == Minor) ? &nursery->range : &fromSpace->range;

  switch (GCStatus) {
    case GCOff:                   /* Signalling to other processors that collector is turning on */
      if (diag)
	printf("Proc %d: CollectorOn - stopping other processors\n", proc->procid); 
      GCStatus = GCPendingOn;
      StopAllThreads();
      break;
    case GCPendingOn:             /* Responding to signal that collector is turning on */
      if (diag)
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
  isFirst = (asynchReachBarrier(&numWaitOnProc)) == 0;
  if (isFirst) {
    if (GCType == Minor)
      assert(Heap_GetAvail(fromSpace) > Heap_GetSize(nursery));
    totalRequest = 0;
    ResetJob();                               /* Reset counter so all user threads are scanned */
    asynchReachBarrier(&numWaitOnProc);       /* First processor is counted twice. */
  }
  while (!(asynchCheckBarrier(&numWaitOnProc, NumProc + 1, &numDoneOnProc)))   /* The first thread is counted twice */
    ;

  /* Reset root lists, compute thread-specific roots in parallel,
     determine whether a major GC was explicitly requested. */
  procChangeState(proc, GCStack);
  resetStack(proc->roots);
  while ((threadIterator = NextJob()) != NULL) {
    assert(threadIterator->requestInfo >= 0);
    FetchAndAdd(&totalRequest, threadIterator->requestInfo);
    local_root_scan(proc,threadIterator);
  }
  asynchReachBarrier(&numStackOnProc);

  /* The "first" processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. 
     The major GC does not take effect until the first minor GC is completed.
  */
  procChangeState(proc, GCGlobal);
  if (isFirst) {
    if (paranoid) /* Check heaps before starting GC */
      paranoid_check_all(nursery, fromSpace, NULL, NULL, largeSpace);
    while (!(asynchCheckBarrier(&numStackOnProc, NumProc, &numWaitOnProc)))
      ;
    resetSharedObjStack(workStack,NumProc);
    if (GCType == Major) {
      major_global_scan(proc);
      Heap_Resize(fromSpace,expandedTenuredSize,0);
      Heap_Resize(toSpace,expandedTenuredSize,1);
      gc_large_startCollect();
    }
    else {
      minor_global_scan(proc);
      Heap_Resize(nursery,expandedNurserySize,0);
    }
    asynchReachBarrier(&numStackOnProc);            /* First processor is counted twice */
  }
  while (!(asynchCheckBarrier(&numStackOnProc, NumProc + 1, &numWaitOnProc)))
    ;

  /* Check local stack empty, prepare copy range,
     forward all the roots (first proc handles backpointers),
     transfer work from local to shared work stack */
  procChangeState(proc, GC);
  if (GCType == Major)
    proc->gcSegment1 = MajorWork;

  assert(isEmptyStack(&proc->minorStack));
  assert(isEmptyStack(&proc->majorStack));
  if (GCType == Minor) {
    if (IsNullCopyRange(&proc->minorRange))   /* First minor GC (after a major GC) */
      SetCopyRange(&proc->minorRange, proc, fromSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
  }
  else
    SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);

  /* modifiedPrimaryReplicaLoc have back pointer roots */
  len = lengthStack(proc->primaryReplicaLocRoots);
  if (GCType == Minor) {
    if (diag)
      printf("GC %d: %d back pointers when collector turned on.\n", NumGC, len);
    while (!isEmptyStack(proc->primaryReplicaLocRoots)) {
      ploc_t primaryReplicaLoc = (ploc_t) popStack(proc->primaryReplicaLocRoots);
      ptr_t root = *primaryReplicaLoc;
      pushStack(proc->primaryReplicaLocFlips, (void *) primaryReplicaLoc);
      alloc1_copyCopySync_primaryStack(proc,root,objStack,copyRange,srcRange);
    }
  }
  else
    assert(len == 0);

  /* Forward roots */
  proc->numRoot += lengthStack(proc->roots);
  while (!isEmptyStack(proc->roots)) {
    ploc_t root = (ploc_t) popStack(proc->roots);
    ptr_t obj = *root;
    alloc1_copyCopySync_primaryStack(proc,obj,objStack,copyRange,srcRange);
  }

  if (GCType == Minor)
    while (!isEmptyStack(proc->primaryReplicaObjRoots)) {
      /* Not transferScanObj_* since this object is a primaryReplica.
	 Not scanObj_locCopy1_* since this object is a primary-replica and we cannot modify it until we Flip. */
      ptr_t obj = popStack(proc->primaryReplicaObjRoots);
      scanObj_copy1_copyCopySync_primaryStack(proc, obj, objStack, copyRange, srcRange);
      pushStack(proc->primaryReplicaObjFlips, obj);
    }
  else
    resetStack(proc->primaryReplicaObjRoots);

  /* Omit popSharedObjStack */
  pushSharedObjStack(workStack, objStack);
  assert(isEmptyStack(objStack));
  GCStatus = GCOn;
  synchBarrier(&numDoneOnProc, NumProc, &numStackOnProc);
  flushStore();
}

static void CollectorOff(Proc_t *proc)
{
  Thread_t *threadIterator = NULL;
  int isFirst;
  int curGCType = GCType;      /* GCType will be written to during this function for the next GC
				  and so we save its value here for reading */

  if (diag)
    printf("Proc %d: entered CollectorOff\n", proc->procid);
  assert(isEmptyStack(&proc->minorStack));        /* Local stack must be empty */
  assert(GCStatus == GCPendingOff);
  flushStore();

  isFirst = (asynchReachBarrier(&numWaitOffProc)) == 0;
  if (isFirst) {
    if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery))   
      GCType = Major;                         /* A Major GC is needed if the tenured fromSpace is too low */
    else
      GCType = Minor;
    ResetJob();
    asynchReachBarrier(&numWaitOffProc);
  }
  while (!asynchCheckBarrier(&numWaitOffProc, NumProc+1, &numDoneOffProc))
    ;
  /* Local stacks must be empty. */
  assert(isEmptyStack(&proc->minorStack));
  assert(isEmptyStack(&proc->majorStack));

  /* Replace all roots with replica */
  procChangeState(proc, GCGlobal);
  if (isFirst) {
    if (curGCType == Major) 
      major_global_scan(proc);
    else 
      minor_global_scan(proc);   
  }
  if (curGCType == Major) {
    assert(isEmptyStack(proc->primaryReplicaObjFlips));
    assert(isEmptyStack(proc->primaryReplicaLocFlips));
  }
  else {
    while (!isEmptyStack(proc->primaryReplicaObjFlips)) {
      /* Not transferScanObj_* since this object is a primaryReplica that just needs to be flipped. No copying should actually occur. */
      ptr_t obj = popStack(proc->primaryReplicaObjFlips);
      scanObj_locCopy1_copyCopySync_primaryStack(proc, obj, &proc->minorStack,&proc->minorRange,&nursery->range);
    }
    while (!isEmptyStack(proc->primaryReplicaLocFlips)) {
      ploc_t primaryReplicaLoc = (ploc_t) popStack(proc->primaryReplicaLocFlips);  /* fromSpace */
      ptr_t field = *primaryReplicaLoc;                                          /* fromSpace or tag/global */
      
      if (inHeap(field,nursery)) {
	ptr_t fieldFrom = (ptr_t) field[-1];
	*primaryReplicaLoc = fieldFrom;  /* switching field to replica */
      }
    }
  }

  procChangeState(proc, GCStack);
  while ((threadIterator = NextJob()) != NULL) {
    local_root_scan(proc,threadIterator);
    if (threadIterator->request == MajorGCRequestFromC) /* Runtime explicitly requests major GC */
      GCType = Major;
  }

  procChangeState(proc, GC);
  proc->numRoot += lengthStack(proc->roots);
  while (!isEmptyStack(proc->roots)) {
    ploc_t root = (ploc_t) popStack(proc->roots);     /* Cannot dequeue from roots since this may be a globals list */
    ptr_t primary = *root;                            /* tag/global or nursery (if minor) or fromSpace or largeSpace */
    ptr_t replica;
    if (curGCType == Minor) {
      if (inHeap(primary,nursery)) {
	replica = (ptr_t) primary[-1];
	*root = replica;
      }
      else if (paranoid) {
	assert(IsGlobalData(primary) || 
	       IsTagData(primary) ||
	       inHeap(primary,fromSpace) ||
	       inHeap(primary,largeSpace));
      }
    }
    else {                            
      if (inHeap(primary,fromSpace)) {
	replica = (ptr_t) primary[-1];
	*root = replica;
      }
      else if (paranoid) {
	assert(IsGlobalData(primary) || 
	       IsTagData(primary) ||
	       inHeap(primary,toSpace) ||
	       inHeap(primary,largeSpace));
      }
    }
  }
  synchBarrier(&numFlipOffProc, NumProc, &numWaitOffProc);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    double liveRatio = 0.0;
    if (curGCType == Minor) {
      paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
      minor_global_promote();
      liveRatio = (double) (fromSpace->cursor - fromSpace->prevCursor) / (double) (nursery->cursor - nursery->bottom); 
      fromSpace->prevCursor = fromSpace->cursor;
      add_statistic(&proc->minorSurvivalStatistic, liveRatio);
    }
    else {
      paranoid_check_all(fromSpace, NULL, toSpace, NULL, largeSpace);
      gc_large_endCollect();
      liveRatio = HeapAdjust2(totalRequest, nursery, fromSpace, toSpace);
      add_statistic(&proc->majorSurvivalStatistic, liveRatio);
      Heap_Resize(fromSpace, 0, 1);
      typed_swap(Heap_t *, fromSpace, toSpace);
      reducedTenuredSize = Heap_GetSize(fromSpace);
      expandedTenuredSize = reducedToExpanded(reducedTenuredSize, majorCollectionRate);
      Heap_Resize(fromSpace,reducedTenuredSize,0);  
      NumMajorGC++;                          
    }
    Heap_Resize(nursery,reducedNurserySize,1);
    NumGC++;
    GCStatus = GCOff;
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  proc->writelistCursor = proc->writelistStart;
  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting to sync on completion of CollectorOff\n",proc->procid);
  flushStore();
  synchBarrier(&numDoneOffProc, NumProc, &numFlipOffProc);
}

static void do_work(Proc_t *proc, int workToDo, int local)
{
  int counter, done = 0;
  Stack_t *objStack = (GCType == Minor) ? &proc->minorStack : &proc->majorStack;
  CopyRange_t *copyRange = (GCType == Minor) ? &proc->minorRange : &proc->majorRange;
  range_t *srcRange = (GCType == Minor) ? &nursery->range : &fromSpace->range;
  while (!done) {
    if (!local)
      popSharedObjStack(workStack, objStack, fetchSize);
    for (counter = localWorkSize; counter > 0; counter--) {
      ptr_t gray = popStack(objStack);
      if (gray == NULL) 
	break;
      transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primaryStack(proc,gray,objStack,copyRange,srcRange);
    }
    if (!local) {
      int sharedWorkDone = pushSharedObjStack(workStack, objStack);
      if (sharedWorkDone) {
	GCStatus = GCPendingOff;
	done = 1;
      }
    }
    else {
      if (isEmptyStack(objStack))
	done = 1;
    }
    done |= (updateWorkDone(proc) >= workToDo);
  }
  assert(isEmptyStack(objStack));
  if (diag)
    printf("Proc %d: GC %d Segment %d:  do_minor_work:  workToDo = %d segWork = %d   shared workStack has %d items\n",
	   proc->procid, NumGC, proc->numSegment, workToDo, getWorkDone(proc), workStack->cursor);
}

 
void GCRelease_GenConc(Proc_t *proc)
{
  mem_t allocCurrent = proc->allocStart;
  mem_t allocStop = proc->allocCursor;
  ploc_t writelistCurrent = proc->writelistStart;
  ploc_t writelistStop = proc->writelistCursor;
  int isGCOn = (GCStatus == GCOn) || (GCStatus == GCPendingOff);
  Stack_t *objStack = (GCType == Minor) ? &proc->minorStack : &proc->majorStack;
  CopyRange_t *copyRange = (GCType == Minor) ? &proc->minorRange : &proc->majorRange;
  range_t *srcRange = (GCType == Minor) ? &nursery->range : &fromSpace->range;

  /* Update statistics, reset cursors */
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  proc->minorUsage.bytesAllocated += sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->allocStart = proc->allocCursor;          
  proc->writelistCursor = proc->writelistStart;  

  /* local stack must be empty between calls to GCRelease and do_minor_work */
  assert(isEmptyStack(&proc->minorStack)); 

  /* Replicate objects allocated by mutator when the collector is on */
  if (isGCOn) {
    if (diag)
      printf("Proc %d: Scanning/Replicating %d to %d\n",proc->procid,allocCurrent,allocStop);
    while (allocCurrent + 1 < allocStop) {
      tag_t tag = *allocCurrent;
      ptr_t obj = allocCurrent + 1;
      int objSize = alloc_unique_primaryStack(proc,obj,objStack,copyRange,srcRange);
      allocCurrent += objSize / sizeof(val_t);
    }
  }

  /* Process writes */
  if (diag && writelistCurrent < writelistStop)
    printf("Proc %d: Processing writes from %d to %d\n",proc->procid,writelistCurrent,writelistStop);
  while (writelistCurrent < writelistStop) {
    ptr_t mutatorPrimary = *writelistCurrent++;          /* Global, nursery, or fromSpace */
    int byteDisp = (int) *writelistCurrent++;
    ptr_t possPrevPtrVal = (ptr_t) *writelistCurrent++;  /* Pointer value only if pointer array was modified */
    int wordDisp = byteDisp / sizeof(val_t);
    int doubleWordDisp = byteDisp / (sizeof(double));

    if (IsGlobalData(mutatorPrimary) ||
	inHeap(mutatorPrimary, largeSpace))
      continue;

    if (!isGCOn) {                             /* Record back-pointers when GC is off for roots of next GC */
      if (inHeap(mutatorPrimary, fromSpace)) {
	tag_t tag = mutatorPrimary[-1];
	if (TAG_IS_FORWARD(tag))
	  tag = ((ptr_t) tag)[-1];
	if (GET_TYPE(tag) == PARRAY_TYPE) 
	  pushStack(proc->primaryReplicaLocRoots, (ptr_t) &mutatorPrimary[wordDisp]);
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
	alloc1_copyCopySync_primaryStack(proc,nurseryPrimary,objStack,copyRange,srcRange);
	  fromSpaceEither = (ptr_t) nurseryPrimary[-1];
	  tag = fromSpaceEither[-1];
      }
      else if (inHeap(mutatorPrimary,fromSpace)) {
	fromSpaceEither = mutatorPrimary;
	if (GCType == Major) {
	  alloc1_copyCopySync_primaryStack(proc,fromSpaceEither,objStack,copyRange,srcRange);
	  toSpaceReplica = (ptr_t) fromSpaceEither[-1];
	  tag = toSpaceReplica[-1];
	}
	else
	  tag = fromSpaceEither[-1];
      }
      else 
	assert(0);

      switch (GET_TYPE(tag)) {
      case IARRAY_TYPE:
	if (nurseryPrimary)
	  fromSpaceEither[wordDisp] = nurseryPrimary[wordDisp];
	if (toSpaceReplica)
	  toSpaceReplica[wordDisp] = fromSpaceEither[wordDisp];
	break;
      case RARRAY_TYPE: 
	if (nurseryPrimary)
	  ((double *)fromSpaceEither)[doubleWordDisp] = ((double *)nurseryPrimary)[doubleWordDisp];
	if (toSpaceReplica)
	  ((double *)toSpaceReplica)[doubleWordDisp] = ((double *)fromSpaceEither)[doubleWordDisp];
	break;
      case PARRAY_TYPE: {
	/* There are several cases:
	   (1) MinorGC: The original array is in the nursery.  
	       (a) The field is in nursery.   Update new array with new (fromSpace) field.
	       (b) The field is in fromSpace. Update new array with old (fromSpace) field.
	       (c) The field is in global/largeSpace. Update new array with field.  (fromSpaceField is NULL).
	   (2) MinorGC: The original array is in fromSpace.
	       Perform no updates.  Record modification if field is in nursery.  (backpointer)
	   (3) MajorGC: The original array is in fromSpace.
               (a) The field is in fromSpace. Update new array with new (toSpace) field.
	       (b) The field is in global/largeSpace. Update new array with field.  (toSpaceField is NULL).
	*/
	ptr_t mutatorField = (ptr_t) mutatorPrimary[wordDisp];
	ptr_t fromSpaceField = NULL;
	ptr_t toSpaceField = NULL;

	/* Snapshot-at-the-beginning (Yuasa) write barrier requires copying prevPtrVal 
	   even if it might die to prevent the mutator from hiding live data */
	alloc1_copyCopySync_primaryStack(proc,possPrevPtrVal,objStack,copyRange,srcRange);

	if (inHeap(mutatorField,nursery)) {
	  assert(GCType == Minor);
	  alloc1_copyCopySync_primaryStack(proc,mutatorField,objStack,copyRange,srcRange);
	  fromSpaceField = (ptr_t) mutatorField[-1];
	}
	else if (inHeap(mutatorField,fromSpace)) {
	  fromSpaceField = mutatorField;
	  if (GCType == Major)
	    alloc1_copyCopySync_primaryStack(proc,fromSpaceField,objStack,copyRange,srcRange);
	  toSpaceField = (ptr_t) fromSpaceField[-1];
	}
	if (GCType == Minor) {
	  if (inHeap(mutatorPrimary,nursery)) {
	    if (fromSpaceField != NULL)
	      fromSpaceEither[wordDisp] = (val_t) fromSpaceField;
	    else
	      fromSpaceEither[wordDisp] = (val_t) mutatorField;
	  }
	  else {
	    assert(inHeap(mutatorPrimary,fromSpace));
	    if (inHeap(mutatorField,nursery)) 
	      pushStack(proc->primaryReplicaLocFlips, (ptr_t) &mutatorPrimary[wordDisp]);
	  }
	}
	else {  /* GCType == Major */
	  if (toSpaceField != NULL)
	    toSpaceReplica[wordDisp] = (val_t) toSpaceField;
	  else
	    toSpaceReplica[wordDisp] = (val_t) mutatorField;
	}
	break;
      }
      default:
	assert(0);
      } /* else */
    } /* switch */
  } /* while */
  
  if (isGCOn) {
    condPushSharedObjStack(workStack, objStack);
  }
  flushStore();
}

static int GCTry_GenConcHelp(Proc_t *proc, int roundSize)
{
  assert(!(GCType == Major && GCStatus == GCOff));
  GetHeapArea((GCType == Minor) ? nursery : fromSpace,
	      roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  if (proc->allocStart)
    return 1;
  return 0;
}

/* Try to obatain space in proc to run th.  If th is NULL, an idle processor is signified */
int GCTry_GenConc(Proc_t *proc, Thread_t *th)
{
  int satisfied = 0;  /* satisfied = 1 means GC stayed off; 2 means it turned on or was on */
  int roundOffSize = minOffRequest;                            /* default work amount for idle processor */
  int roundOnSize = minOnRequest;    
  int minorWorkToDo, majorWorkToDo;

  assert(proc->writelistCursor + 2 <= proc->writelistEnd);
  flushStore();

  /* Make sure space requirement is not already fulfilled */
  if (th != NULL) {
    if (th->requestInfo < 0) {
      unsigned int writelistBytesAvailable = sizeof(val_t) * (proc->writelistEnd - proc->writelistCursor);
      assert((-th->requestInfo) <= writelistBytesAvailable);
      return 1;
    }
    assert(th->requestInfo > 0);
    roundOffSize = RoundUp(th->requestInfo,minOffRequest);
    roundOnSize = RoundUp(th->requestInfo,minOnRequest);
  }
  minorWorkToDo = (1 + minorCollectionRate) * roundOnSize;  /* + 1 for the work done in replicating primary */
  majorWorkToDo = (1 + majorCollectionRate) * roundOnSize;

  /* If GCStatus is off but GCType is Major, then we immediately start a major collection */
  while (!satisfied) {
    switch (GCStatus) {
    case GCOff:                                          /* Possible GC states: before; after */
    case GCPendingOn:
      if (GCStatus == GCOff && GCType != Major) {        /* Off, PendingOn; same */
	if (th == NULL ||                                /* Don't allocate/return if not really a thread */
	    GCTry_GenConcHelp(proc,roundOffSize)) {
	  if (!satisfied)
	    satisfied = 1;
	  break;
	}
      }
      procChangeState(proc, GC);
      proc->gcSegment1 = (GCType == Major) ? MajorWork : MinorWork;
      proc->gcSegment2 = FlipOn;
      CollectorOn(proc);                                 /* Off, PendingOn; On, PendingOff */
      assert(GCStatus == GCOn || GCStatus == GCPendingOff);
      if (GCType == Minor)                               /* Don't do work since we just finished a minor GC if starting a major GC */
	do_work(proc, minorWorkToDo, 0);
      if ((th == NULL) ||                                /* Idle processor done some work */
	  (th != NULL && GCStatus == GCOn &&             /* Don't allocate/return if not really a thread */
	   GCTry_GenConcHelp(proc,roundOnSize))) {       /* On, PendingOff; same */
	satisfied = 2;
	break;
      }
      if (GCStatus == GCPendingOff)                      /* If PendingOff, must loop */
	break;                    
      printf("Proc %d: Conc coll fell behind.  GCStatus = %d. Could not allocate %d bytes\n", proc->procid, GCStatus, roundOnSize);
      assert(0);
      break;
    case GCOn:
    case GCPendingOff:                                  /* If PendingOff, shared work is completed. */
      procChangeState(proc, GC);
      proc->gcSegment1 = (GCType == Major) ? MajorWork : MinorWork;
      if (GCType == Major)
	proc->gcSegment1 = MajorWork;
      if (GCStatus == GCOn)                             /* On, PendingOff; same */
	do_work(proc, GCType == Minor ? minorWorkToDo : majorWorkToDo, 0);
      if (GCStatus == GCOn &&                           /* On, PendingOff; same */
	  th != NULL &&                                 /* Don't allocate/return if not really a thread */
	  GCTry_GenConcHelp(proc,roundOnSize)) {
	satisfied = 2;
	break;
      }
      if (GCStatus == GCOn) {                           
	if (th == NULL) {                               /* Idle processor has done some work */
	 satisfied = 2;
	 break;
	}
	printf("Proc %d: Conc coll fell behind.  GCStatus = %d. Could not allocate %d bytes for thread %d\n", 
	       proc->procid, GCStatus, roundOnSize, th->tid);
	assert(0);
      }
      assert(GCStatus == GCPendingOff);
      do_work(proc, 
	      sizeof(val_t) * Heap_GetSize(nursery), 1); /* Actually bounded by the allocation of one processor */
      if (GCType == Minor) 
	PadCopyRange(&proc->minorRange);                 /* Pad so that paranoid check works */
      else if (GCType == Major) {                        /* Carry over minor ranges across minor GCs */
	ClearCopyRange(&proc->minorRange); 
	ClearCopyRange(&proc->majorRange);
      }
      proc->gcSegment2 = FlipOff;
      CollectorOff(proc);                               /* PendingOff; Off, PendingOn */
      assert(GCStatus == GCOff || GCStatus == GCPendingOn);
      break;                                            /* Must repeat loop */
    default: 
      assert(0);
    }
  }

  return 1;
}

void GCPoll_GenConc(Proc_t *proc)
{
  (void) GCTry_GenConc(proc, NULL);
}

void GCInit_GenConc()
{
  int cache_size = GetBcacheSize();
  double nurseryFraction = 0.85 + 0.2 * NumProc;

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
  reducedNurserySize -= (NumProc - 1) * pagesize;      /* One extra page per processor over one */
  expandedTenuredSize = Heap_GetSize(fromSpace);
  reducedTenuredSize = expandedToReduced(expandedTenuredSize, majorCollectionRate);
  Heap_Resize(nursery, reducedNurserySize, 0);
  Heap_Resize(fromSpace, reducedTenuredSize, 0);
  Heap_Resize(toSpace, reducedTenuredSize, 0);

  gc_large_init();
  workStack = SharedObjStack_Alloc(32 * 1024);
}

