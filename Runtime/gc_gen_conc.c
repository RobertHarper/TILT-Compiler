#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "platform.h"
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
   rest of the nursery.

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

   POINTER MUTATIONS
   -----------------
   During a minor collection, the primary continues to be allocated
   from the nursery even as a replica of the nursery-residing primary
   is replicated in the tenuredFrom.  The objects in tenuredFrom are
   considered primary-replica in the sense that both the primary and
   replica point to it.  At the beginning of the collection, there are
   no pointers back to the nursery and so the mutator continues to see
   only the primary or primary-replica.  Eventually, when the
   collector is turned off, it is possible to discard the nursery
   primary and switch to the replica without modifynig the
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
   Replicate the primary-replica pointer arrays.  The nutator
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
     (-) requires multiple syncs */


static int reducedNurserySize = 0, expandedNurserySize = 0;
static int minorCollectionRate = 4;   /* Ratio of minor coll rate to minor alloc rate */
static int majorCollectionRate = 2;   /* Ratio of major coll rate to major alloc rate(minor coll rate) */
static int fetchSize = 20;            /* Number of objects to fetch from global pool */
static int localWorkSize = 40;        /* Number of objects to work on from local pool */
static Queue_t *modifiedTenuredPtrLoc = NULL;


/* ------------------  Parallel array allocation routines ------------------- */

static mem_t alloc_big_GenConc(int byteLen, int hasPointers)
{
  Thread_t *curThread = getThread();
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
    region = gc_large_alloc(byteLen);

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
      region = gc_large_alloc(byteLen);
  }

  assert(region != NULL);
  assert(tenuredFrom->cursor < tenuredFrom->top);

  /* Update Statistics */
  gcstat_normal(byteLen, 0, 0);
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
  ptr_t stack[2];
  int cursor = 2;
  stack[0] = copyRange->start;
  stack[1] = copyRange->cursor;
  SynchStart(majorWorkStack2);
  SynchMid(majorWorkStack2);
  moveToGlobalStack(majorWorkStack2, &(stack[0]), &cursor);
  SynchEnd(majorWorkStack2);
  assert(cursor == 0);
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

static void CollectorOn(SysThread_t *sysThread)
{
  int isFirst = 0;
  Thread_t *curThread;
  CopyRange_t copyRange;

  if (diag)
    printf("Proc %d: CollectorOn\n", sysThread->stid); 

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
    /* A Major GC is forced if the tenured space is potentially too small */
    if (Heap_GetAvail(tenuredFrom) < 2 * Heap_GetSize(nursery))
      GCType = BeginMajor;             
    else 
      GCType = Minor;
    req_size = 0;
    ResetJob();                        /* Reset counter so all user threads are scanned */
    asynchReachBarrier(&numOn1);       /* First processor is counted twice. */
  }
  /* Wait for all threads to reach this point; note that the first thread is counted twice */
  if (diag)
    printf("Proc %d: waiting for %d systhreads to stop mutator %s\n",
	   sysThread->stid, (NumSysThread + 1) - numOn1, isFirst ? "First" : "");
  while (!(asynchCheckBarrier(&numOn1, NumSysThread + 1, &numOn3)))
    ;

  /* Check local stack empty; reset root lists */
  assert(sysThread->LocalCursor == 0);
  QueueClear(sysThread->root_lists);
  if (GCType == Minor || GCType == BeginMajor)
    SetCopyRange(&copyRange, sysThread->stid, toSpace, expandWithPad, dischargeWithPad);
  else
    SetCopyRange(&copyRange, sysThread->stid, toSpace, expandWithPadSave, dischargeWithPadSave);

  /* All processors compute thread-specific roots in parallel
     and determine whether a major GC has been requested. */
  while ((curThread = NextJob()) != NULL) {
    assert(curThread->requestInfo >= 0);
    FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(sysThread,curThread);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC) 
      GCType = Major;      
    if (diag)
      printf("Proc %d:    computed roots of userThread %d\n", sysThread->stid,curThread->tid);      
  }
  asynchReachBarrier(&numOn2);

  /* The "first" GC processor is in charge of the globals but
     must wait until all threads are processed before knowing if GC is major. 
     The major GC does not take effect until the first minor GC is completed.
  */
  if (isFirst) {
    while (!(asynchCheckBarrier(&numOn2, NumSysThread, &numOn1)))
      ;
    if (paranoid) /* Check heaps before starting GC */
      paranoid_check_all(nursery, tenuredFrom, NULL, NULL);
    minor_global_scan(sysThread);           /* Even if major GC */
    Heap_Resize(nursery,expandedNurserySize);
    asynchReachBarrier(&numOn2);            /* First processor is counted twice */
  }
  while (!(asynchCheckBarrier(&numOn2, NumSysThread + 1, &numOn1)))
    ;

  /* Now forward all the roots which initializes the local work stacks */
  synchBarrier(&numOn2, NumSysThread, &numOn1);
  while (!QueueIsEmpty(sysThread->root_lists)) {
    /* Cannot dequeue from roots since this may be a global */
    Queue_t *roots = (Queue_t *) Dequeue(sysThread->root_lists);
    int i, rootCount = 0, len = QueueLength(roots);  
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      ptr_t obj = *root;
      forward1_concurrent_stack(obj,&copyRange,&nursery->range,sysThread);
      if (verbose)
	printf("GC %d: collector on %d root = %d   primary = %d, replica = %d\n",NumGC,++rootCount,root,obj,obj[-1]);
    }
  }

  /* XXX wastage of space */
  copyRange.discharge(&copyRange);

  /* Move to global stack */
  SynchStart(workStack);
  SynchMid(workStack);
  moveToGlobalStack(workStack, sysThread->LocalStack, &(sysThread->LocalCursor));
  SynchEnd(workStack);

  assert(sysThread->LocalCursor == 0);
  GCStatus = GCOn;
  synchBarrier(&numOn3, NumSysThread, &numOn2);
  flushStore();
}

static long numOff1 = 0;
static long numOff2 = 0;
static long numOff3 = 0;
static long numOff4 = 0;

static void CollectorOff(SysThread_t *sysThread)
{
  Thread_t *curThread = NULL;
  int isFirst;
  int rootCount = 0;

  if (diag)
    printf("Proc %d: entered CollectorOff\n", sysThread->stid);
  assert(sysThread->LocalCursor == 0); 
  flushStore();

  switch (GCStatus) {
  case GCOff: assert(0);       /* Collector already off! */
  case GCPendingOn: assert(0); /* Someone is turning the collector off while we are turning it on! */
  case GCOn:                   /* First to signal turning collector off */
    GCStatus = GCPendingOff;
    StopAllThreads();
    break;
  case GCPendingOff: break;   /* Responding to someone's signal to turn collector off */
  default: assert(0);
  }

  isFirst = (asynchReachBarrier(&numOff1)) == 0;
  if (isFirst) {
    assert(isEmptyGlobalStack(workStack));
    ResetJob();
    asynchReachBarrier(&numOff1);
  }
  while (!asynchCheckBarrier(&numOff1, NumSysThread+1, &numOff4))
    ;

  if (isFirst) 
    if (isEmptyGlobalStack(majorWorkStack1) &&
	isEmptyGlobalStack(majorWorkStack2))
      GCType = EndMajor;

  /* Replace all roots (global, local registers, local stack) with minor replica */
  if (isFirst) 
    minor_global_scan(sysThread);   /* always do minor first even if major is occurring */
  while ((curThread = NextJob()) != NULL)
    local_root_scan(sysThread,curThread);
  while (!QueueIsEmpty(sysThread->root_lists)) {
    Heap_t *legalPrimaryHeaps[3];
    Heap_t *legalReplicaHeaps[3];
    Bitmap_t *legalStarts[3] = {NULL, NULL, NULL};
    Queue_t *roots;
    int i, len;
    legalPrimaryHeaps[0] = nursery;
    legalPrimaryHeaps[1] = tenuredFrom;
    legalPrimaryHeaps[2] = NULL;
    legalReplicaHeaps[0] = tenuredFrom;
    legalReplicaHeaps[1] = NULL;
    roots = (Queue_t *) Dequeue(sysThread->root_lists);
    len = QueueLength(roots);  
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);     /* Cannot dequeue from roots since this may be a global */
      loc_t primary = *root, replica;
      if (!inHeaps(primary,legalPrimaryHeaps, legalStarts))  /* root was not in heap; could be global or small constructor */
	continue;
      replica = (loc_t) primary[-1];
      if (!inHeaps(replica,legalReplicaHeaps, legalStarts) || verbose) {
	printf("GC %d: collector off %d root = %d   primary = %d, replica = %d",NumGC, ++rootCount,root,primary,replica);
	if (!verbose)
	  printf("   ERROR replica not in to heap\n");
	else printf("\n");
      }
      *root = replica;
    }
  }
  synchBarrier(&numOff2, NumSysThread, &numOff1);

  /* Replace all roots (global, local registers, local stack) with minor replica */
  if (GCType == EndMajor) {
    if (isFirst) 
      major_global_scan(sysThread);   
    while ((curThread = NextJob()) != NULL)
      local_root_scan(sysThread,curThread);
    while (!QueueIsEmpty(sysThread->root_lists)) {
      Heap_t *legalPrimaryHeaps[2];
      Heap_t *legalReplicaHeaps[2];
      Bitmap_t *legalStarts[2] = {NULL, NULL};
      Queue_t *roots;
      int i, len;
      legalPrimaryHeaps[0] = tenuredFrom;
      legalPrimaryHeaps[1] = NULL;
      legalReplicaHeaps[0] = tenuredTo;
      legalReplicaHeaps[1] = NULL;
      roots = (Queue_t *) Dequeue(sysThread->root_lists);
      len = QueueLength(roots);
      for (i=0; i<len; i++) {
	ploc_t root = (ploc_t) QueueAccess(roots,i);         /* Cannot dequeue from roots since this may be a global */
	loc_t primary = *root, replica;
	if (!inHeaps(primary,legalPrimaryHeaps, legalStarts))  /* root was not in heap; may be global or small ptr */
	  continue;
	replica = (loc_t) primary[-1];
	if (!inHeaps(replica,legalReplicaHeaps, legalStarts) || verbose) {
	  printf("GC %d: collector off %d root = %d   primary = %d, replica = %d",NumGC, ++rootCount,root,primary,replica);
	  if (!verbose)
	    printf("   ERROR replica not in to heap\n");
	  else printf("\n");
	}
	*root = replica;
      }
    }
  }
  synchBarrier(&numOff3, NumSysThread, &numOff2);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    long alloc = (sizeof (val_t)) * (nursery->top - nursery->bottom); 

    /* Check the heaps after collection */
    if (paranoid) {
      if (GCType == Minor)
	paranoid_check_all(nursery, tenuredFrom, tenuredFrom, NULL);
      else
	assert(0); /* different if major GC just finished */
    }
    
    /* Resize heaps and do stats */
    gcstat_normal(alloc,0,0);
    nursery->cursor = nursery->bottom;
    Heap_Resize(nursery,reducedNurserySize);
    if (GCType != Minor) {
      if (GCType == BeginMajor)
	GCType = Major;
      /* The Major -> EndMajor transition occurs elsewhere. */
      if (GCType == EndMajor) {
	Heap_t *froms[3];
	froms[0] = nursery;
	froms[1] = tenuredFrom;
	froms[2] = NULL;
	GCType = Minor;
	gc_large_flush();
	HeapAdjust(0,req_size,froms,tenuredTo);
	typed_swap(Heap_t *, tenuredFrom, tenuredTo);
	NumMajorGC++;
      }
      assert(0);
    }
    NumGC++;
    GCStatus = GCOff;
  }

  /* All system threads need to reset their limit pointer */
  sysThread->allocStart = StartHeapLimit;
  sysThread->allocCursor = StartHeapLimit;
  sysThread->allocLimit = StartHeapLimit;
  sysThread->writelistCursor = sysThread->writelistStart;

  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting to sync on completion of CollectorOff\n",sysThread->stid);
  flushStore();
  synchBarrier(&numOff4, NumSysThread, &numOff3);
}

void GCRelease_GenConc(SysThread_t *sysThread)
{
  mem_t allocCurrent = sysThread->allocStart;
  mem_t allocStop = sysThread->allocCursor;
  ploc_t writelistCurrent = sysThread->writelistStart;
  ploc_t writelistStop = sysThread->writelistCursor;
  CopyRange_t copyRange;
  int bytesCopied = 0;

  sysThread->allocStart = sysThread->allocCursor;  /* allocation area is NOT reused */
  sysThread->writelistCursor = sysThread->writelistStart;  /* write list reused once processed */
  if (GCType == Minor || GCType == BeginMajor)
    SetCopyRange(&copyRange, sysThread->stid, toSpace, expandWithPad, dischargeWithPad);
  else
    SetCopyRange(&copyRange, sysThread->stid, toSpace, expandWithPadSave, dischargeWithPadSave);


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

  assert(sysThread->LocalCursor == 0);

  if (diag)
    printf("Proc %d: Scanning/Replicating %d to %d\n",sysThread->stid,allocCurrent,allocStop);
  while (allocCurrent + 1 < allocStop) {
    tag_t tag = *allocCurrent;
    ptr_t obj = allocCurrent + 1;
    int bytesCopied, objSize;
    if (IS_SKIP(tag)) {
      allocCurrent += GET_SKIP(tag);
      continue;
    }
    bytesCopied = forward1_concurrent_stack(obj,&copyRange,&fromSpace->range,sysThread);
    objSize = (bytesCopied) ? bytesCopied : objectLength((ptr_t) obj[-1]);
    allocCurrent += objSize / sizeof(val_t);
  }

  if (diag)
    printf("Proc %d: Processing writes from %d to %d\n",sysThread->stid,writelistCurrent,writelistStop);
  while (writelistCurrent < writelistStop) {
    ptr_t primary = *writelistCurrent++, replica;
    tag_t tag;
    int byteDisp = (int) *writelistCurrent++;

    forward1_concurrent_stack(primary,&copyRange,&nursery->range,sysThread);
    replica = (ptr_t) primary[-1];
    tag = replica[-1];

    switch (GET_TYPE(tag)) {
    case PARRAY_TAG: {
      int wordDisp = byteDisp/ sizeof(val_t);
      ptr_t primaryField = (ptr_t) primary[wordDisp], replicaField;
      forward1_concurrent_stack(primaryField,&copyRange,&nursery->range,sysThread);
      replicaField = IsTagData(primaryField) ? primaryField : (ptr_t) primaryField[-1];
      replica[wordDisp] = (val_t) replicaField;  /* update replica with replicated object */
      break;
    }
    case IARRAY_TAG: {
      int wordDisp = byteDisp/ sizeof(val_t);
      int primaryField = (int) primary[wordDisp];
      replica[wordDisp] = primaryField;       /* update replica with primary's non-pointer value */
      break;
    }
    case RARRAY_TAG: {
      int doublewordDisp = byteDisp / (sizeof(double));
      double primaryField = (int) primary[doublewordDisp];
      replica[doublewordDisp] = primaryField;  /* update replica with primary's non-pointer value */
      break;
    }
    default: assert(0);
    }
  }

  SynchStart(workStack);
  SynchMid(workStack);
  moveToGlobalStack(workStack,sysThread->LocalStack, &(sysThread->LocalCursor));
  SynchEnd(workStack);
  
  /* XXX wastage of space */
  copyRange.discharge(&copyRange);
  flushStore();
}


static void do_minor_work(SysThread_t *sysThread, int bytesToCopy)
{
  CopyRange_t copyRange;
  int i, bytesCopied = 0, lastAndEmpty = 0;

  assert(sysThread->LocalCursor == 0);                                        /* local stack must be empty */ 
  if (GCType == Minor || GCType == BeginMajor)
    SetCopyRange(&copyRange, sysThread->stid, toSpace, expandWithPad, dischargeWithPad);
  else
    SetCopyRange(&copyRange, sysThread->stid, toSpace, expandWithPadSave, dischargeWithPadSave);
  while (!(isEmptyGlobalStack(workStack)) && bytesCopied < bytesToCopy) {
    SynchStart(workStack);
    fetchFromGlobalStack(workStack,sysThread->LocalStack, &(sysThread->LocalCursor), fetchSize);
    for (i=0; i < localWorkSize && sysThread->LocalCursor > 0; i++) {
      loc_t grayCell = (loc_t)(sysThread->LocalStack[--sysThread->LocalCursor]);
      int bytesScanned = scan1_object_coarseParallel_stack(grayCell,&copyRange,&nursery->range,&tenuredFrom->range,sysThread);
      bytesCopied += bytesScanned;
    }
    SynchMid(workStack);
    moveToGlobalStack(workStack,sysThread->LocalStack, &(sysThread->LocalCursor));
    lastAndEmpty = SynchEnd(workStack);
  }
  copyRange.discharge(&copyRange);     /* XXX wastage of space */
  if (lastAndEmpty)                    /* If shared stack is empty after SynchEnd, then everyone's stack is empty */
    CollectorOff(sysThread);
  flushStore();
}

static void do_major_work(SysThread_t *sysThread, int bytesToCopy)
{
  CopyRange_t copyRange;
  int i, bytesCopied = 0, lastAndEmpty = 0;

  assert(sysThread->LocalCursor == 0);                                        /* local stack must be empty */ 
  SetCopyRange(&copyRange, sysThread->stid, tenuredTo, expandWithPad, dischargeWithPad);       /* Get local ranges ready for use */
  while (!(isEmptyGlobalStack(majorWorkStack2)) && bytesCopied < bytesToCopy) {
    int cursor = 0;
    ptr_t stack[2];
    mem_t allocCurrent, allocStop;
    SynchStart(majorWorkStack2);
    fetchFromGlobalStack(majorWorkStack2,stack, &cursor, 2);
    SynchMid(majorWorkStack2);
    SynchEnd(majorWorkStack2);
    assert(cursor == 2);
    allocCurrent = stack[0];
    allocStop = stack[1];
    while (allocCurrent + 1 < allocStop) {
      tag_t tag = *allocCurrent;
      ptr_t obj = allocCurrent + 1;
      int objSize;
      if (IS_SKIP(tag)) {
	allocCurrent += GET_SKIP(tag);
	continue;
      }
      objSize = forward1_concurrent_stack(obj,&copyRange,&fromSpace->range,sysThread);
      objSize = (objSize) ? objSize : objectLength((ptr_t) obj[-1]);
      bytesCopied += objSize;
    }
    SynchStart(majorWorkStack2);
    SynchMid(majorWorkStack2);
    moveToGlobalStack(majorWorkStack2,sysThread->LocalStack, &(sysThread->LocalCursor));
    lastAndEmpty = SynchEnd(majorWorkStack2);
  }
 while (!(isEmptyGlobalStack(majorWorkStack1)) && bytesCopied < bytesToCopy) {
    SynchStart(majorWorkStack1);
    fetchFromGlobalStack(majorWorkStack1,sysThread->LocalStack, &(sysThread->LocalCursor), fetchSize);
    for (i=0; i < localWorkSize && sysThread->LocalCursor > 0; i++) {
      loc_t grayCell = (loc_t)(sysThread->LocalStack[--sysThread->LocalCursor]);
      int bytesScanned = scan1_object_coarseParallel_stack(grayCell,&copyRange,&nursery->range,&tenuredFrom->range,sysThread);
      bytesCopied += bytesScanned;
    }
    SynchMid(majorWorkStack1);
    moveToGlobalStack(majorWorkStack1,sysThread->LocalStack, &(sysThread->LocalCursor));
    lastAndEmpty &= SynchEnd(majorWorkStack1);
  }
  copyRange.discharge(&copyRange);     /* XXX wastage of space */
  flushStore();
}

static int GCTry_GenConcHelp(SysThread_t *sysThread, int roundSize)
{
  mem_t tmp_alloc, tmp_limit;
  switch (GCStatus) {
    case GCOff: 
    case GCOn: 
    case GCPendingOn: 
       GetHeapArea(nursery,roundSize,&tmp_alloc,&tmp_limit);
       break;
    case GCPendingOff: 
      tmp_alloc = tmp_limit = 0;
      break;
    default : 
       assert(0);
  }
  if (tmp_alloc) {
    sysThread->allocStart = tmp_alloc;
    sysThread->allocCursor = tmp_alloc;
    sysThread->allocLimit = tmp_limit;
    return 1;
  }
  return 0;
}


int GCTry_GenConc(SysThread_t *sysThread, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo,pagesize);
  assert(sysThread->writelistCursor + 2 <= sysThread->writelistEnd);
  flushStore();

  if (th->requestInfo < 0) {
    unsigned int bytesAvailable = (((unsigned int)sysThread->writelistEnd) - 
				   ((unsigned int)sysThread->writelistCursor));
    assert((-th->requestInfo) <= bytesAvailable);
    return 1;
  }
  assert(th->requestInfo > 0);
  switch (GCStatus) {
  case GCOff:
    if (GCTry_GenConcHelp(sysThread,roundSize))
      return 1;
    CollectorOn(sysThread);
    do_minor_work(sysThread, minorCollectionRate * roundSize);
    if (GCTry_GenConcHelp(sysThread,roundSize))
      return 1;
    printf("Concurrent collector fell too far behind just as it is activated - check parameters\n");
    assert(0);
    break;
  case GCPendingOn:
    CollectorOn(sysThread);
    do_minor_work(sysThread, minorCollectionRate * roundSize);
    if (GCTry_GenConcHelp(sysThread,roundSize))
      return 1;
    printf("Concurrent collector fell too far behind while GC pending - check parameters\n");
    assert(0);
    break;
  case GCOn:
    do_minor_work(sysThread, minorCollectionRate * roundSize);
    if (GCTry_GenConcHelp(sysThread,roundSize))
      return 1;
    printf("Concurrent collector fell too far behind with collector fully on - check parameters\n");
    assert(0);
    break;
  case GCPendingOff:
    /* this is actually bounded by the size of objects allocated by all processors since CollectorOff is triggered */
    do_minor_work(sysThread,sizeof(val_t) * Heap_GetSize(nursery));
    if (GCStatus == GCPendingOff)                                  /* do_work may trigger CollectorOff */
      CollectorOff(sysThread);
    return GCTry_GenConc(sysThread, th);
  default: 
    assert(0);
  }
  assert(0);
}

void gc_poll_GenConc(SysThread_t *sth)
{
  switch (GCStatus) {
  case GCOff:
    return;
  case GCPendingOn:
    CollectorOn(sth);
    return;
  case GCOn:
    do_minor_work(sth, minorCollectionRate * pagesize);  /* collect as though one page had been allocated */
    return;
  case GCPendingOff:
    CollectorOff(sth);
    return;
  }
}

void gc_init_GenConc()
{
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
  expandedNurserySize = YoungHeapByte;
  reducedNurserySize = expandedNurserySize * minorCollectionRate / (1 + minorCollectionRate);
  reducedNurserySize = RoundDown(reducedNurserySize, pagesize);

  nursery = Heap_Alloc(YoungHeapByte, YoungHeapByte);
  Heap_Resize(nursery, reducedNurserySize);
  tenuredFrom = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  tenuredTo = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  modifiedTenuredPtrLoc = QueueCreate(0, 1024);
  gc_large_init(0);
  workStack = SharedStack_Alloc();
  majorWorkStack1 = SharedStack_Alloc();
  majorWorkStack2 = SharedStack_Alloc();
}

void gc_finish_GenConc()
{
  Thread_t *th = getThread();
  int allocsize = (unsigned int) th->saveregs[ALLOCPTR] - (unsigned int) nursery->cursor;
  gcstat_normal(allocsize,0,0);
}
