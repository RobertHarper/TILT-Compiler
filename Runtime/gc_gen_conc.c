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


/* HEAP LAYOUT
   -----------
   The heap is set up with a nursery, a tenuredFrom, and a
   tenuredTo as in any 2-generational collector.  During a minor
   collection, the live parts of the nursery primary is replicated in
   tenuredFrom while the primary continues to be allocated
   from a reserved area of the nursery.  When the collection rate is k,
   then the reserved area must be at least 1/k times the size of the
   rest of the nursery.  A similar reserve area must be set up in the
   tenured area.

   Eventually, a major collection is required.  Unfortunately, the
   simple scheme of replicating the nursery and tenuredFrom into
   tenuredTo will fail.  The amount of reserved space in the nursery
   will only allow a certain amount of replication to occur.  When the
   reserved area is exhausted, the replication is only partially
   completed and a flip is impossible.  On the other hand, if the
   primary is allocated in tenuredFrom, then the collector is no
   longer generational for the duration of the major collection. 
   Instead, the replica of the nursery must continue to be made 
   in tenuredFrom.

   When a major collection is occurring, major collection work
   (replication of the tenuredFrom graph to tenuerdTo) occurs while
   minor collections are proceeding.  The tenuerdFrom graph is
   replicated starting with tenured roots obtained at the end of any
   minor collection.  During a major collection, the tenuredFrom area
   continues to fill up as the result of minor collections.  These
   "allocations" into tenuredFrom need to be replicated as well.
   Thus, objects allocated during a major GC that are live have 3
   copies.  Eventually, the major collection and we major-flip
   the roles of tenuredFrom and tenuredTo.  This flip will conincide
   with a minor-flip as the major collection cannot complete until
   the minor collection finishes.

   BACK-POINTER MUTATIONS
   ----------------------
   During a minor collection, the primary continues to be allocated
   from the nursery even as a replica of the nursery-residing primary
   is replicated in the tenuredFrom.  The objects in tenuredFrom are
   considered primary-replica in the sense that both the primary and
   replica point to it.  At the beginning of the collection, there are
   no pointers back to the nursery and so the mutator continues to see
   only the primary or primary-replica.  Eventually, when the
   collector is turned off, it is possible to discard the nursery
   primary and switch to the replica without modifying the
   primary-replica at all.  However, when a tenuredFrom-residing
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

   For now, we choose Solution 1 and store the pointer updates in hte queue modifiedTenuredPtrLoc.

   Another concern is the interaction between the presence of back-pointers due to mutation
   and major collection.  In the absence of mutations, the tenuredFrom space is a closed 
   graph and is replicated into tenuredTo during major GC work.  With mutations, the major collection
   may replicate an object into tenuredTo which points back to the nursery.  When this object
   is scanned, it is not clear how to forward these objects.  The major collector cannot proceed
   with a minor copy being made and mixing the work of the minor collection here seems too complicated.
   Since we are using Solution 1 already, we can extend it easily to handle this.
   Normally, the mutations are applied at the end of a minor GC.  We simply apply them
   to the tenuredTo replica as well when we reach the end of a major GC.

*/


static int reducedNurserySize = 0, expandedNurserySize = 0;
static int reducedTenuredSize = 0, expandedTenuredSize = 0;
static int minorCollectionRate = 4;   /* Ratio of minor coll rate to minor alloc rate */
static int majorCollectionRate = 2;   /* Ratio of major coll rate to major alloc rate(minor coll rate) */
static int fetchSize = 20;            /* Number of objects to fetch from global pool */
static int localWorkSize = 40;        /* Number of objects to work on from local pool */
static Queue_t *modifiedTenuredPtrLoc = NULL;


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

static mem_t alloc_big_GenConc(int byteLen, int hasPointers)
{
  Thread_t *curThread = getThread();
  Proc_t *proc = curThread->proc;
  unsigned long *saveregs = curThread->saveregs;
  mem_t region = NULL;
  int wordLen = byteLen / (sizeof (val_t));
  int request = RoundUp(byteLen, pagesize);

  /* Should be at least 0.5K to be considered big */
  assert(byteLen >= 512);                

  /* Get the large region - use tenured heap if object has pointers */
  if (hasPointers) {
    if (tenuredFrom->cursor + wordLen <= tenuredFrom->top) {
      region = tenuredFrom->cursor;
      tenuredFrom->cursor += wordLen;
    }
  }
  else
    region = gc_large_alloc(proc,byteLen);

  /* If allocation failed, perform GC and try again */
  if (region == NULL) {
    GCFromC(curThread, 4, 1);
    if (hasPointers) {
      if (tenuredFrom->cursor + wordLen <= tenuredFrom->top) {
	region = tenuredFrom->cursor;
	tenuredFrom->cursor += wordLen;
      }
    }
    else
      region = gc_large_alloc(proc,byteLen);
  }

  assert(region != NULL);
  assert(tenuredFrom->cursor < tenuredFrom->top);

  /* Update Statistics */
  gcstat_normal(proc,byteLen, 0, 0);
  return region;
}

ptr_t alloc_bigintarray_GenConc(int elemLen, int initVal, int ptag)
{
  /* elemements are byte-sized */
  int wordLen = 1 + (elemLen + 3) / 4;
  mem_t space = alloc_big_GenConc(4 * wordLen,0);
  ptr_t res = space + 1;
  init_iarray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigptrarray_GenConc(int elemLen, ptr_t initVal, int ptag)
{
  int wordLen = 1 + elemLen;
  mem_t space = alloc_big_GenConc(4 * wordLen,1);
  ptr_t res = space + 1;
  init_parray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigfloatarray_GenConc(int elemLen, double initVal, int ptag)
{
  ptr_t res = NULL;
  mem_t region = NULL;
  Thread_t *curThread = getThread();
  int wordLen = 2 + (elemLen << 1);  /* Includes one word for alignment */

  region = alloc_big_GenConc(4 * wordLen,0);
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


/* --------------------- Concurrent collector --------------------- */
static long req_size = 0;

static long numOn1 = 0;
static long numOn2 = 0;
static long numOn3 = 0;

static void regionSave(CopyRange_t *copyRange)
{
  /* The stack in LocalStack is too big.  XXX */
  LocalStack_t stack;
  if (copyRange->start >= copyRange->cursor)
    return;
  stack.stack[0] = copyRange->start;
  stack.stack[1] = copyRange->cursor;
  stack.cursor = 2;
  if (diag)
    printf("Proc %d: Saving region %d to %d\n", copyRange->sth->stid, copyRange->start, copyRange->cursor);
  SynchStart(majorRegionWorkStack);
  SynchMid(majorRegionWorkStack);
  moveToGlobalStack(majorRegionWorkStack, &stack);
  SynchEnd(majorRegionWorkStack,NULL);
  assert(stack.cursor == 0);
}

void expandWithPadSave(CopyRange_t *copyRange, int size)
{
  regionSave(copyRange);
  expandWithPad(copyRange, size);
}

void dischargeWithPadSave(CopyRange_t *copyRange)
{
  regionSave(copyRange);
  dischargeWithPad(copyRange);
}

static void CollectorOn(Proc_t *proc)
{
  int isFirst = 0;
  Thread_t *curThread;

  if (diag)
    printf("Proc %d: CollectorOn\n", proc->stid); 

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

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     prelimiary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  isFirst = (asynchReachBarrier(&numOn1)) == 0;
  if (isFirst) {
    if (diag)
      printf("Proc %d: Starting collection %d.  starting with  type %d\n", proc->stid, NumGC, GCType);
    if (GCType == Major)              /* A Major GC is ongoing */
      ;
    else if (GCType == EndMajor)      /* A Major GC will end at the end of this minor GC */
      ;
    else if (GCType == Minor) {
      /* A Major GC is forced if the tenured space is potentially too small */
      if (Heap_GetAvail(tenuredFrom) < 2 * Heap_GetSize(nursery)) {
	GCType = BeginMajor;             
      }
      else 
	GCType = Minor;
    }
    else                               /* GCType should not be BeginMajor */
      assert(0);
    req_size = 0;
    ResetJob();                        /* Reset counter so all user threads are scanned */
    asynchReachBarrier(&numOn1);       /* First processor is counted twice. */
    if (diag)
      printf("Proc %d: Starting collection %d of type %d\n", proc->stid, NumGC, GCType);
  }
  /* Wait for all threads to reach this point; note that the first thread is counted twice */
  if (verbose)
    printf("Proc %d: waiting for %d procs to stop mutator %s\n",
	   proc->stid, (NumProc + 1) - numOn1, isFirst ? "First" : "");
  while (!(asynchCheckBarrier(&numOn1, NumProc + 1, &numOn3)))
    ;

  /* Check local stack empty; reset root lists */
  assert(proc->localStack.cursor == 0);
  QueueClear(proc->root_lists);
  if (GCType == Minor)
    SetCopyRange(&proc->minorRange, proc, tenuredFrom, expandWithPad, dischargeWithPad);
  else if (GCType == BeginMajor) {
    SetCopyRange(&proc->minorRange, proc, tenuredFrom, expandWithPadSave, dischargeWithPadSave);
    SetCopyRange(&proc->majorRange, proc, tenuredTo, expandWithPad, dischargeWithPad);
  }
  else if (GCType == Major || GCType == EndMajor) {
    SetCopyRange(&proc->minorRange, proc, tenuredFrom, expandWithPadSave, dischargeWithPadSave);
  }

  /* All processors compute thread-specific roots in parallel
     and determine whether a major GC has been requested. */
  while ((curThread = NextJob()) != NULL) {
    assert(curThread->requestInfo >= 0);
    FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(proc,curThread);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC) 
      GCType = Major;      
    if (verbose)
      printf("Proc %d:    computed roots of userThread %d\n", proc->stid,curThread->tid);      
  }
  asynchReachBarrier(&numOn2);

  /* The "first" GC processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. 
     The major GC does not take effect until the first minor GC is completed.
  */
  if (isFirst) {
    while (!(asynchCheckBarrier(&numOn2, NumProc, &numOn1)))
      ;
    if (paranoid) /* Check heaps before starting GC */
      paranoid_check_all(nursery, tenuredFrom, NULL, NULL);
    minor_global_scan(proc);           /* Even if major GC */
    Heap_Resize(nursery,expandedNurserySize,0);
    if (GCType == BeginMajor) {
      Heap_Resize(tenuredFrom,expandedTenuredSize,0);
      Heap_Resize(tenuredTo,expandedTenuredSize,1);
    }
    asynchReachBarrier(&numOn2);            /* First processor is counted twice */
  }
  while (!(asynchCheckBarrier(&numOn2, NumProc + 1, &numOn1)))
    ;

  /* Now forward all the roots which initializes the local work stacks */
  synchBarrier(&numOn2, NumProc, &numOn1);
  while (!QueueIsEmpty(proc->root_lists)) {
    /* Cannot dequeue from roots since this may be a global */
    Queue_t *roots = (Queue_t *) Dequeue(proc->root_lists);
    int i, rootCount = 0, len = QueueLength(roots);  
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      ptr_t obj = *root;
      forward1_concurrent_stack(obj,&proc->minorRange,&nursery->range,proc);
      if (verbose)
	printf("GC %d: collector on %d root = %d   primary = %d, replica = %d\n",NumGC,++rootCount,root,obj,obj[-1]);
    }
    FetchAndAdd(&NumRoots, len);
  }

  /* Move to global stack */
  SynchStart(workStack);
  SynchMid(workStack);
  moveToGlobalStack(workStack, &proc->localStack);
  SynchEnd(workStack,NULL);

  assert(proc->localStack.cursor == 0);
  GCStatus = GCOn;
  synchBarrier(&numOn3, NumProc, &numOn2);
  flushStore();
}

static long numOff1 = 0;
static long numOff2 = 0;
static long numOff3 = 0;
static long numOff4 = 0;

int missingRoot = 0;

static void CollectorOff(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst;
  int rootCount = 0;

  if (diag)
    printf("Proc %d: entered CollectorOff\n", proc->stid);
  assert(proc->localStack.cursor == 0);        /* Local stack must be empty */
  flushStore();

  assert(GCStatus == GCPendingOff);

  isFirst = (asynchReachBarrier(&numOff1)) == 0;
  if (isFirst) {
    ResetJob();
    asynchReachBarrier(&numOff1);
  }
  while (!asynchCheckBarrier(&numOff1, NumProc+1, &numOff4))
    ;
  assert(isEmptyGlobalStack(workStack));            /* After all processors are here, shared work stack must be empty */
  if (GCType == EndMajor) {                         /* Major stack may contain some small amount of work until all threads stop */
    assert(isEmptyGlobalStack(majorWorkStack));
    assert(isEmptyGlobalStack(majorRegionWorkStack));
  }

  /* Replace all roots with replica; doing 2 replica indirection if end of major GC */
  if (isFirst) {
    if (GCType == BeginMajor || GCType == EndMajor)   
      major_global_scan(proc);
    else {
      minor_global_scan(proc);   
    }
    while (!QueueIsEmpty(modifiedTenuredPtrLoc)) {
      ptr_t primaryReplica = (ptr_t) Dequeue(modifiedTenuredPtrLoc);
      int byteDisp = (int) Dequeue(modifiedTenuredPtrLoc);
      ptr_t field = (ptr_t) primaryReplica[byteDisp / sizeof(ptr_t)];         /* could be primary or replica */
      ptr_t fieldTenuredFrom;
      
      if (inHeap(field,nursery)) {
	fieldTenuredFrom = (ptr_t) field[-1];
	primaryReplica[byteDisp / sizeof(ptr_t)] = (val_t) fieldTenuredFrom;  /* switching field to replica */
      }
      else
	fieldTenuredFrom = field;
      assert(IsTagData(fieldTenuredFrom) ||
	     IsGlobalData(fieldTenuredFrom) ||
	     inHeap(fieldTenuredFrom,tenuredFrom));
      
      if (GCType == EndMajor) {
	ptr_t objTenuredTo = (ptr_t) primaryReplica[-1];
	ptr_t fieldTenuredTo = (ptr_t) fieldTenuredFrom[-1];
	objTenuredTo[byteDisp / sizeof(ptr_t)] = (val_t) fieldTenuredTo;
	assert(inHeap(objTenuredTo, tenuredTo));
	assert(inHeap(fieldTenuredTo, tenuredTo));
      }
    }
  }
  while ((curThread = NextJob()) != NULL) {
    local_root_scan(proc,curThread);
  }
  while (!QueueIsEmpty(proc->root_lists)) {
    Heap_t *minorPrimary[2] = {nursery, NULL};
    Heap_t *minorReplica[2] = {tenuredFrom, NULL};
    Heap_t *majorPrimary[2] = {tenuredFrom, NULL};
    Heap_t *majorReplica[2] = {tenuredTo, NULL};
    Bitmap_t *starts[2] = {NULL, NULL};
    Queue_t *roots = (Queue_t *) Dequeue(proc->root_lists);
    int i, len = QueueLength(roots);  
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);      /* Cannot dequeue from roots since this may be a globals list */
      loc_t primary = *root, replica, replicaTemp;
      if (IsGlobalData(primary) || IsTagData(primary))  /* root is global data or small constructor */
	continue;
      if (inHeaps(primary,minorReplica, starts))        /* root was forwarded at beginning of GC */
	replica = primary;
      else 
	replica = (loc_t) primary[-1];
      replicaTemp = replica;
      if (GCType == BeginMajor) {                       /* These form the roots for the major collection */
	forward1_concurrent_stack(replica,&proc->majorRange,&tenuredFrom->range,proc);
      }
      if (GCType == EndMajor) 
	if (inHeaps(replica,majorPrimary, starts))      /* root may have been forwarded at beginning of major GC */
	  replica = (loc_t) replica[-1];
      if (!inHeaps(replica,(GCType==EndMajor)?majorReplica:minorReplica,starts)) {
	printf("Proc %d: GC %d: collector off   GCType = %d  root #%d = %d   primary = %d, replicaTemp = %d, replica = %d\n",
	       proc->stid, NumGC,GCType,++rootCount,root,primary,replicaTemp,replica);
	missingRoot = 1;
      }
      *root = replica;
    }
    FetchAndAdd(&NumRoots, len);
  }
  synchBarrier(&numOff2, NumProc, &numOff1);

  if (missingRoot)
    assert(0);

  if (isFirst)
    minor_global_promote();
  if (GCType == BeginMajor) {                           /* This initializes the shared majorGC work stack */
    SynchStart(majorWorkStack);
    SynchMid(majorWorkStack);
    moveToGlobalStack(majorWorkStack, &proc->localStack);
    SynchEnd(majorWorkStack,NULL);
    if (diag)
      printf("Proc %d: Started Major GC. CopyRange is %d - %d - %d.  majorSharedStack has %d items\n", 
	     proc->stid, proc->majorRange.start, proc->majorRange.cursor, proc->majorRange.stop,
	     majorWorkStack->cursor);
  }
  synchBarrier(&numOff3, NumProc, &numOff2);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    long alloc = (sizeof (val_t)) * (nursery->top - nursery->bottom); 

    nursery->cursor = nursery->bottom;        /* Always reset nursery */
    gcstat_normal(proc,alloc,0,0);       /* Is this right? XXX */
    if (paranoid && GCType != EndMajor)
      paranoid_check_all(nursery, tenuredFrom, tenuredFrom, NULL);  /* At the end of a minor collection, tenuerdFrom should be intact */
    Heap_Resize(nursery,reducedNurserySize,1);  /* Collection Off now */
    if (GCType == Minor)
      ;
    else if (GCType == BeginMajor) {
      if (diag)
	printf("Proc %d: Transitioning to major GC now\n", proc->stid);
      GCType = Major;
    }
    else if (GCType == Major) {               /* The Major -> EndMajor transition occurs elsewhere. */
      if (isEmptyGlobalStack(majorWorkStack)) {
	/* Note that majorRegionWorkStack does not need to be empty since that contains only 
	   the work of replicating objects copied into the tenuerdFrom area during a major collection. */
	if (diag)
	  printf("Proc %d: Most of major GC completed.  Will flip at end of next minor GC\n", proc->stid);
	GCType = EndMajor;
      }
    }
    else if (GCType == EndMajor) {
      Heap_t *froms[3] = {nursery, tenuredFrom, NULL};
      gc_large_flush();
      if (paranoid)
	paranoid_check_all(tenuredFrom, NULL, tenuredTo, NULL);      /* At the end of a major collection, tenuerdTo should be intact */
      HeapAdjust(0,req_size,froms,tenuredTo);
      typed_swap(Heap_t *, tenuredFrom, tenuredTo);
      reducedTenuredSize = Heap_GetSize(tenuredFrom);
      expandedTenuredSize = reducedToExpanded(reducedTenuredSize, majorCollectionRate);
      Heap_Resize(tenuredFrom,reducedTenuredSize,0);  
      Heap_Resize(tenuredTo,reducedTenuredSize,1);  
      GCType = Minor;
      NumMajorGC++;                           /* Increment major GC here */
      if (diag)
	printf("Proc %d: Major GC %d completed at minor GC %d\n", proc->stid, NumMajorGC, NumGC);
    }
    else 
      assert(0);
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
    printf("Proc %d: waiting to sync on completion of CollectorOff\n",proc->stid);
  flushStore();
  synchBarrier(&numOff4, NumProc, &numOff3);
}

static void do_local_minor_work(Proc_t *proc, int bytesToCopy)
{
  int bytesCopied = 0;
  while (bytesCopied < bytesToCopy && proc->localStack.cursor > 0) {
    loc_t grayCell = (loc_t)(proc->localStack.stack[--proc->localStack.cursor]);
    int bytesScanned = scan1_object_coarseParallel_stack(grayCell,&proc->majorRange,&nursery->range,&tenuredFrom->range,proc);
    bytesCopied += bytesScanned;
  }
  assert(proc->localStack.cursor == 0);    /* local stack must be empty when we exit */ 
}

void signalCollectorOff()
{
  if (diag)
    printf("Turning Collector Off\n");
  GCStatus = GCPendingOff;      /* Signals that the amount of work is now bounded and some processors may not participate */
  StopAllThreads();
  flushStore();
}

void GCRelease_GenConc(Proc_t *proc)
{
  mem_t allocCurrent = proc->allocStart;
  mem_t allocStop = proc->allocCursor;
  ploc_t writelistCurrent = proc->writelistStart;
  ploc_t writelistStop = proc->writelistCursor;
  int bytesCopied = 0;

  assert(proc->localStack.cursor == 0);    /* local stack must be empty between calls to GCReleas and do_minor_work */
  proc->allocStart = proc->allocCursor;  /* allocation area is NOT reused */
  proc->writelistCursor = proc->writelistStart;  /* write list reused once processed */

  switch (GCStatus) {
  case GCOff:
  case GCPendingOn:
    return;          /* We don't replicate or process the write list */
  case GCOn:
  case GCPendingOff:
    break;
  default:
    assert(0);
  }

  if (diag)
    printf("Proc %d: Scanning/Replicating %d to %d\n",proc->stid,allocCurrent,allocStop);
  while (allocCurrent + 1 < allocStop) {
    tag_t tag = *allocCurrent;
    ptr_t obj = allocCurrent + 1;
    int bytesCopied, objSize;
    if (IS_SKIP(tag)) {
      allocCurrent += GET_SKIP(tag);
      continue;
    }
    bytesCopied = forward1_concurrent_stack(obj,&proc->minorRange,&nursery->range,proc);
    objSize = (bytesCopied) ? bytesCopied : objectLength((ptr_t) obj[-1]);
    allocCurrent += objSize / sizeof(val_t);
  }

  if (diag)
    printf("Proc %d: Processing writes from %d to %d\n",proc->stid,writelistCurrent,writelistStop);
  while (writelistCurrent < writelistStop) {
    ptr_t primary = *writelistCurrent++;
    tag_t tag;
    int byteDisp = (int) *writelistCurrent++;

    forward1_concurrent_stack(primary,&proc->minorRange,&nursery->range,proc);
    tag = (tag_t) primary[-1];

    if (!IS_FORWARDPTR(tag)) {       /* primary-replica mutation */
      assert(!(IS_FORWARDPTR(tag)));
      switch (GET_TYPE(tag)) {
      case PARRAY_TAG: 
	Enqueue(modifiedTenuredPtrLoc, (void *) primary);
	Enqueue(modifiedTenuredPtrLoc, (void *) byteDisp);
	if (diag)
	  printf("Proc %d: primary-replica pointer mutation detected\n", proc->stid);
	break;
      case IARRAY_TAG:
      case RARRAY_TAG:
	break;
      default:
	assert(0);
      }
    }
    else {
      ptr_t replica = (ptr_t) tag;
      tag = replica[-1];
      while (tag == STALL_TAG) {
	flushStore();
	tag = replica[-1];
      }
      if (IS_FORWARDPTR(tag))        /* a tenuredFrom object that has been replicated during a major GC */
	tag = ((ptr_t) tag) [-1];
      switch (GET_TYPE(tag)) {
      case PARRAY_TAG: { 
	int wordDisp = byteDisp/ sizeof(val_t);
	ptr_t primaryField = (ptr_t) primary[wordDisp], replicaField;
	forward1_concurrent_stack(primaryField,&proc->minorRange,&nursery->range,proc);
	replicaField = IsTagData(primaryField) ? primaryField : (ptr_t) primaryField[-1];
	replica[wordDisp] = (val_t) replicaField;  /* update replica with replicated object */
	break;
      }
      case IARRAY_TAG: {
	int wordDisp = byteDisp/ sizeof(val_t);
	int primaryField = (int) primary[wordDisp];
	replica[wordDisp] = primaryField;           /* update replica with primary's non-pointer value */
	break;
      }
      case RARRAY_TAG: {
	int doublewordDisp = byteDisp / (sizeof(double));
	double primaryField = (int) primary[doublewordDisp];
	replica[doublewordDisp] = primaryField;     /* update replica with primary's non-pointer value */
	break;
      }
      default: 
	printf("Proc %d: Bad tag %d\n", proc->stid, tag);
	assert(0);
      }
    }
  }

  SynchStart(workStack);
  if (GCStatus == GCPendingOff) {
    SynchMid(workStack);
    SynchEnd(workStack, NULL);
    do_local_minor_work(proc, pagesize);
  }
  else {
    SynchMid(workStack);
    moveToGlobalStack(workStack, &proc->localStack);
    SynchEnd(workStack, &signalCollectorOff);  
  }
  flushStore();

  assert(proc->localStack.cursor == 0);    /* local stack must be empty between calls to GCReleas and do_minor_work */
}

static void do_minor_work(Proc_t *proc, int bytesToCopy)
{
  int i, bytesCopied = 0;

  assert(proc->localStack.cursor == 0); 
  while ((!(isEmptyGlobalStack(workStack)) || (proc->localStack.cursor>0)) && 
	 bytesCopied < bytesToCopy) {
    SynchStart(workStack);
    if (GCStatus == GCPendingOff) {
      SynchMid(workStack);
      SynchEnd(workStack, NULL);
      do_local_minor_work(proc, bytesToCopy);
    }
    else {
      fetchFromGlobalStack(workStack, &proc->localStack, fetchSize);
      for (i=0; i < localWorkSize && proc->localStack.cursor > 0; i++) {
	loc_t grayCell = (loc_t)(proc->localStack.stack[--proc->localStack.cursor]);
	int bytesScanned = scan1_object_coarseParallel_stack(grayCell,&proc->minorRange,&nursery->range,&tenuredFrom->range,proc);
	bytesCopied += bytesScanned;
      }
      SynchMid(workStack);
      moveToGlobalStack(workStack, &proc->localStack);
      SynchEnd(workStack, &signalCollectorOff);
    }
  }
  assert(proc->localStack.cursor == 0); 
}

static void do_major_work(Proc_t *proc, int bytesToCopy)
{
  int i, bytesCopied = 0, lastAndEmpty = 0;

  assert(proc->localStack.cursor == 0);                                        /* local stack must be empty */ 
  while (!(isEmptyGlobalStack(majorRegionWorkStack)) && bytesCopied < bytesToCopy) {
    /* the stack in LocalStack_t is way too bug */
    LocalStack_t stack;
    mem_t allocCurrent, allocStop;
    stack.cursor = 0;
    SynchStart(majorRegionWorkStack);
    fetchFromGlobalStack(majorRegionWorkStack, &stack, 2);
    SynchMid(majorRegionWorkStack);
    SynchEnd(majorRegionWorkStack,NULL);
    if (stack.cursor == 0)
      continue;
    assert(stack.cursor == 2);
    allocCurrent = stack.stack[1];
    allocStop = stack.stack[0];
    if (diag)
      printf("Proc %d: Major Replicating %d to %d\n", proc->stid, allocCurrent, allocStop);
    assert(allocCurrent <= allocStop);
    while (allocCurrent + 1 < allocStop) {
      tag_t tag = *allocCurrent;
      ptr_t obj = allocCurrent + 1;
      int objSize;
      if (IS_SKIP(tag)) {
	allocCurrent += GET_SKIP(tag);
	continue;
      }
      objSize = forward1_concurrent_stack(obj,&proc->majorRange,&tenuredFrom->range,proc);
      if (objSize==0 && !(IS_FORWARDPTR(obj[-1])))
	assert(0);
      objSize = (objSize) ? objSize : objectLength((ptr_t) obj[-1]);
      bytesCopied += objSize;
      allocCurrent += (objSize / sizeof(val_t));
    }
    SynchStart(majorWorkStack);
    SynchMid(majorWorkStack);
    moveToGlobalStack(majorWorkStack, &proc->localStack);
    SynchEnd(majorWorkStack,NULL);
  }
  assert(proc->localStack.cursor == 0);                                        /* local stack must be empty */ 
  while (!(isEmptyGlobalStack(majorWorkStack)) && bytesCopied < bytesToCopy) {
    SynchStart(majorWorkStack);
    fetchFromGlobalStack(majorWorkStack, &proc->localStack, fetchSize);
    for (i=0; i < localWorkSize && proc->localStack.cursor > 0; i++) {
      loc_t grayCell = (loc_t)(proc->localStack.stack[--proc->localStack.cursor]);
      /* The gray objects can have mostly pointers to tenuredFrom but also to nursery.  We skip pointers to the nursery
	 as these indicate primary-replica back-pointer mutations which are handled later. */
      int bytesScanned = scan1_object_coarseParallel_stack(grayCell,&proc->majorRange,&tenuredFrom->range,&tenuredTo->range,proc);
      bytesCopied += bytesScanned;
    }
    SynchMid(majorWorkStack);
    moveToGlobalStack(majorWorkStack, &proc->localStack);
    SynchEnd(majorWorkStack,NULL);
  }
  flushStore();
  assert(proc->localStack.cursor == 0);                                        /* local stack must be empty */ 
  if (diag)
    printf("Proc %d: do_major_work %d bytes copied. %d items in majorWorkStack and %d regions in majorRegionWorkStack\n",
	   proc->stid, bytesCopied, majorWorkStack->cursor, majorRegionWorkStack->cursor / 2);
}

static int GCTry_GenConcHelp(Proc_t *proc, int roundSize)
{
  mem_t tmp_alloc, tmp_limit;
  GetHeapArea(nursery,roundSize,&tmp_alloc,&tmp_limit);
  proc->allocStart = tmp_alloc;
  proc->allocCursor = tmp_alloc;
  proc->allocLimit = tmp_limit;
  if (tmp_alloc)
    return 1;
  return 0;
}

/* Try to obatain space in proc to run th.  If th is NULL, an idle processor is signified */
int GCTry_GenConc(Proc_t *proc, Thread_t *th)
{
  int satisfied = 0;
  int roundSize = pagesize;                              /* default work amount for idle processor */
  assert(proc->writelistCursor + 2 <= proc->writelistEnd);
  flushStore();

  /* Make sure space requirement is not already fulfilled */
  if (th != NULL) {
    if (th->requestInfo < 0) {
      unsigned int writelistBytesAvailable = (((unsigned int)proc->writelistEnd) - 
					      ((unsigned int)proc->writelistCursor));
      assert((-th->requestInfo) <= writelistBytesAvailable);
      return 1;
    }
    assert(th->requestInfo > 0);
    roundSize = RoundUp(th->requestInfo,pagesize);
  }

  start_timer(&proc->gctime);
  while (!satisfied) {
    switch (GCStatus) {
    case GCOff:                                          /* Possible GC states: before; after */
    case GCPendingOn:
      if (GCStatus == GCOff) {                           /* Off, PendingOn; same */
	if (th == NULL ||                                /* Don't allocate/return if not really a thread */
	    GCTry_GenConcHelp(proc,roundSize)) {
	  satisfied = 1;
	  break;
	}
      }
      CollectorOn(proc);                            /* Off, PendingOn; On, PendingOff */
      assert(GCStatus == GCOn || GCStatus == GCPendingOff);
      do_minor_work(proc, minorCollectionRate * roundSize);
      if (GCType == Major)
	do_major_work(proc, majorCollectionRate * minorCollectionRate * roundSize);
      if ((th == NULL) ||                                /* Idle processor done some work */
	  (th != NULL && GCStatus == GCOn &&             /* Don't allocate/return if not really a thread */
	   GCTry_GenConcHelp(proc,roundSize))) {    /* On, PendingOff; same */
	satisfied = 1;
	break;
      }
      if (GCStatus == GCPendingOff)                      /* If PendingOff, must loop */
	break;                    
      printf("Proc %d: Conc coll fell behind.  GCStatus = %d. Could not allocate %d bytes\n", proc->stid, GCStatus, roundSize);
      assert(0);
      break;
    case GCOn:
    case GCPendingOff:                                  /* If PendingOff, shared work is completed. */
      if (GCStatus == GCOn)                             /* On, PendingOff; same */
	do_minor_work(proc, minorCollectionRate * roundSize);
      if (GCType == Major)                              /* On, PendingOff; same */
	do_major_work(proc, majorCollectionRate * minorCollectionRate * roundSize);
      if (GCStatus == GCOn &&                           /* On, PendingOff; same */
	  th != NULL &&                                 /* Don't allocate/return if not really a thread */
	  GCTry_GenConcHelp(proc,roundSize)) {
	satisfied = 1;
	break;
      }
      if (GCStatus == GCOn) {                           
	if (th == NULL) {                               /* Idle processor has done some work */
	 satisfied = 1;
	 break;
	}
	printf("Proc %d: Conc coll fell behind.  GCStatus = %d. Could not allocate %d bytes for %d\n", proc->stid, GCStatus, roundSize, th);
	assert(0);
      }
      assert(GCStatus == GCPendingOff);
      do_minor_work(proc, 
		    sizeof(val_t) * Heap_GetSize(nursery)); /* Actually bounded by the allocation of one processor */
      ClearCopyRange(&proc->minorRange);
      if (GCType == Major || GCType == EndMajor)
	do_major_work(proc, majorCollectionRate * minorCollectionRate * sizeof(val_t) * Heap_GetSize(nursery));
      if (GCType == EndMajor)
	ClearCopyRange(&proc->majorRange);         /* This field usually persists across multiple minor collections */
      CollectorOff(proc);                          /* PendingOff; Off, PendingOn */
      assert(GCStatus == GCOff || GCStatus == GCPendingOn);
      break;                                            /* Must repeat loop */
    default: 
      assert(0);
    }
  }
  stop_timer(&proc->gctime);
  return 1;
}

void gc_poll_GenConc(Proc_t *proc)
{
  (void) GCTry_GenConc(proc, NULL);
}

void gc_init_GenConc()
{
  int cache_size = GetBcacheSize();
  double nurseryFraction = 0.85 + 0.2 * NumProc;

  init_int(&YoungHeapByte, (int)(nurseryFraction * cache_size));
  init_int(&MaxHeap, 80 * 1024);
  init_int(&MinHeap, 2048);
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

  expandedNurserySize = YoungHeapByte;
  reducedNurserySize = expandedToReduced(expandedNurserySize, minorCollectionRate);
  reducedNurserySize -= (NumProc - 1) * pagesize;      /* One extra page per processor over one */
  expandedTenuredSize = Heap_GetSize(tenuredFrom);
  reducedTenuredSize = expandedToReduced(expandedTenuredSize, majorCollectionRate);
  Heap_Resize(nursery, reducedNurserySize, 0);
  Heap_Resize(tenuredFrom, reducedTenuredSize, 0);
  Heap_Resize(tenuredTo, reducedTenuredSize, 0);

  modifiedTenuredPtrLoc = QueueCreate(1, 1024);
  gc_large_init(0);
  workStack = SharedStack_Alloc(4096);
  majorWorkStack = SharedStack_Alloc(32768);
  majorRegionWorkStack = SharedStack_Alloc(1024);
}

void gc_finish_GenConc()
{
  Thread_t *th = getThread();
  Proc_t *sth = th->proc;
  int allocsize = (unsigned int) th->saveregs[ALLOCPTR] - (unsigned int) nursery->cursor;
  gcstat_normal(sth,allocsize,0,0);
}
