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

/* To make sure the garbage collection does not fall behind, we observe that
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

static int minOffRequest, minOnRequest;  /* Mutator handed multiples of this amount of space when collector is Off/On */
static double copyWeight = 3.0 / 4.0;
static double scanWeight = 1.0 / 4.0;
static double rootWeight = 10.0;
static int collectionRate = 2;   /* Ratio of collection rate to allocation rate */
static int fetchSize = 20;       /* Number of objects to fetch from global pool */
static int localWorkSize = 50;   /* Number of objects to work on from local pool */

/* ------------------  Parallel array allocation routines ------------------- */

static mem_t alloc_big(int byteLen, int hasPointers)
{
  Thread_t *curThread = getThread();
  Proc_t *proc = curThread->proc;
  unsigned long *saveregs = curThread->saveregs;
  mem_t res = NULL;
  int request = RoundUp(byteLen, (GCStatus == GCOff || GCStatus == GCPendingOn) ? minOffRequest : minOnRequest);

  /* Should be at least 0.5K to be considered big */
  assert(byteLen >= 512);                

  /* Get the large region */
  if (saveregs[ALLOCPTR] + byteLen > saveregs[ALLOCLIMIT]) 
    GCFromC(curThread, request, 0);
  assert(saveregs[ALLOCPTR] + byteLen <= saveregs[ALLOCLIMIT]);

  /* Perform actual allocation */
  res = (mem_t) saveregs[ALLOCPTR];
  saveregs[ALLOCPTR] = (unsigned long) (res + (byteLen / (sizeof (val_t))));

  gcstat_normal(proc,byteLen, 0, 0);
  return res;
}

ptr_t alloc_bigintarray_SemiConc(int elemLen, int initVal, int ptag)
{
  /* elemements are byte-sized */
  int wordLen = 1 + (elemLen + 3) / 4;
  mem_t space = alloc_big(4 * wordLen,0);
  ptr_t res = space + 1;
  init_iarray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigptrarray_SemiConc(int elemLen, ptr_t initVal, int ptag)
{
  int wordLen = 1 + elemLen;
  mem_t space = alloc_big(4 * wordLen,1);
  ptr_t res = space + 1;
  init_parray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigfloatarray_SemiConc(int elemLen, double initVal, int ptag)
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
    region[wordLen-1] = SKIP_TAG | (1 << SKIPLEN_OFFSET);
    res = region;
  }
  init_farray(res, elemLen, initVal);
  return res;
}


/* --------------------- Concurrent collector --------------------- */
static long numOff = 0;
static long numFlip = 0;
static long numOffDone = 0;
static long req_size = 0;

static void CollectorOff(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst;
  int rootCount = 0;

  if (diag)
    printf("Proc %d: entered CollectorOff\n", proc->stid);
  assert(proc->localStack.cursor == 0); 
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
  isFirst = (asynchReachBarrier(&numOff)) == 0;
  if (isFirst) {
    if (!(isEmptyGlobalStack(workStack))) {
      printf("Proc %d: Global Stack is not empty in CollectorOff\n", proc->stid);
      assert(0);
    }
    ResetJob();
    asynchReachBarrier(&numOff);
  }
  while (!asynchCheckBarrier(&numOff, NumProc+1, &numOffDone))
    ;

  /* Replace all roots (global, local registers, local stack) with replica */
  if (isFirst) 
    major_global_scan(proc);
  while ((curThread = NextJob()) != NULL)
    local_root_scan(proc,curThread);
  /* Now forward all the roots which initializes the local work stacks */
  while (!QueueIsEmpty(proc->root_lists)) {
    /* Cannot dequeue from roots since this may be a global */
    Heap_t *primaryHeaps[2] = {fromSpace, NULL};
    Heap_t *replicaHeaps[2] = {toSpace, NULL};
    Bitmap_t *legalStarts[2] = {NULL, NULL};
    Queue_t *roots = (Queue_t *) Dequeue(proc->root_lists);
    int i, len = QueueLength(roots);  
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      loc_t primary = *root;
      loc_t replica;
      if (IsGlobalData(primary) || IsTagData(primary))        /* root is global data or small constructor */
	continue;
      if (inHeaps(primary,replicaHeaps, legalStarts))         /* root was forwarded at beginning of GC */
	continue;
      replica = (loc_t) primary[-1];
      if (!inHeaps(replica,replicaHeaps, legalStarts) || verbose) {
	printf("GC %d: collector off %d root = %d   primary = %d, replica = %d",NumGC, ++rootCount,root,primary,replica);
	if (!verbose)
	  printf("   ERROR replica not in to heap\n");
	else printf("\n");
      }
      *root = replica;
    }
    proc->numRoot += len;
  }

  synchBarrier(&numFlip, NumProc, &numOff);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    Heap_t *froms[2] = {NULL, NULL};
    froms[0] = fromSpace;

    /* Check the fromspace and tospace heap - zero out all of fromspace */
    if (paranoid) 
      paranoid_check_all(fromSpace, NULL, toSpace,NULL);
    
    /* Resize heaps and do stats */
    HeapAdjust(0,req_size,froms,toSpace);
    Heap_Resize(fromSpace, 0, 1);
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
    printf("Proc %d: waiting to sync on completion of CollectorOff\n",proc->stid);
  flushStore();
  synchBarrier(&numOffDone, NumProc, &numFlip);
  flushStore();

}

void GCRelease_SemiConc(Proc_t *proc)
{
  mem_t allocCurrent = proc->allocStart;
  mem_t allocStop = proc->allocCursor;
  ploc_t writelistCurrent = proc->writelistStart;
  ploc_t writelistStop = proc->writelistCursor;
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  int write = (proc->writelistCursor - proc->writelistStart) / 2;
  int bytesCopied = 0;

  proc->allocStart = proc->allocCursor;  /* allocation area is NOT reused */  
  proc->writelistCursor = proc->writelistStart;  /* write list reused once processed */
  gcstat_normal(proc, alloc, bytesCopied, write);

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

  /* Get local ranges ready for use */
  assert(proc->localStack.cursor == 0);

  if (diag)
    printf("Proc %d: Scanning/Replicating %d to %d\n",proc->stid,allocCurrent,allocStop);
  while (allocCurrent + 1 < allocStop) {
    tag_t tag = *allocCurrent;
    ptr_t obj = allocCurrent + 1;
    int objSize, localBytesCopied;
    if (IS_SKIP(tag)) {
      allocCurrent += GET_SKIP(tag);
      continue;
    }
    localBytesCopied = forward1_concurrent_stack(obj,&proc->majorRange,&fromSpace->range,proc);
    bytesCopied += localBytesCopied;
    objSize = localBytesCopied ? localBytesCopied : objectLength((ptr_t)obj[-1]);
    allocCurrent += objSize / sizeof(val_t);
  }

  if (diag)
    printf("Proc %d: Processing writes from %d to %d\n",proc->stid,writelistCurrent,writelistStop);
  while (writelistCurrent < writelistStop) {
    ptr_t primary = *writelistCurrent++, replica;
    tag_t tag;
    int byteDisp = (int) *writelistCurrent++;

    forward1_concurrent_stack(primary,&proc->majorRange,&fromSpace->range,proc);
    replica = (ptr_t) primary[-1];
    tag = replica[-1];

    switch (GET_TYPE(tag)) {
    case PARRAY_TAG: {
      int wordDisp = byteDisp/ sizeof(val_t);
      ptr_t primaryField = (ptr_t) primary[wordDisp], replicaField;
      forward1_concurrent_stack(primaryField,&proc->majorRange,&fromSpace->range,proc);
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
  moveToGlobalStack(workStack,&proc->localStack);
  SynchEnd(workStack,NULL);
  
  flushStore();
}


/* GCStatus is either GCOn or GCPendingOff.
   Upon entry and exit, the local work stack is empty.
   The routine will copy at least bytesToCopy bytes before returning.  It may copy slightly more.
   If the local flag is set, then the shared stack is not accessed.
*/
static int do_work(Proc_t *proc, int workToDo, int local)
{
  int i, bytesCopied = 0, bytesScanned = 0, lastAndEmpty = 0;

  assert(proc->localStack.cursor == 0); 

  if (local) {
    int counter = localWorkSize;
    while (proc->localStack.cursor > 0) {
      loc_t grayCell = (loc_t)(proc->localStack.stack[--proc->localStack.cursor]);
      bytesScanned += scan1_object_coarseParallel_stack(grayCell,&proc->majorRange,&fromSpace->range,&toSpace->range,proc, &bytesCopied);
      counter--;
      if (!counter) {
	if (bytesCopied * copyWeight + bytesScanned * scanWeight >= workToDo)
	  break;
	else counter = localWorkSize;
      }
    }
  }
  else {
    while (!(isEmptyGlobalStack(workStack)) && 
	   (bytesCopied * copyWeight + bytesScanned * scanWeight < workToDo)) {
      SynchStart(workStack);
      fetchFromGlobalStack(workStack,&proc->localStack, fetchSize);
      for (i=0; i < localWorkSize && proc->localStack.cursor > 0; i++) {
	loc_t grayCell = (loc_t)(proc->localStack.stack[--proc->localStack.cursor]);
	bytesScanned += scan1_object_coarseParallel_stack(grayCell,&proc->majorRange,
							  &fromSpace->range,&toSpace->range,proc, &bytesCopied);
      }
      SynchMid(workStack);
      moveToGlobalStack(workStack,&proc->localStack);
      lastAndEmpty = SynchEnd(workStack,NULL);
    }
  }
  if (lastAndEmpty) {
    if (diag)
      printf("Proc %d: Turning Collector Off\n", proc->stid); 
    GCStatus = GCPendingOff;
  }
  flushStore();
  assert(proc->localStack.cursor == 0);
  gcstat_normal(proc, 0, bytesCopied, 0);
  return (int) (bytesCopied * copyWeight + bytesScanned * scanWeight);
}



static long numOn1 = 0;
static long numOn2 = 0;
static long numOn3 = 0;

static int CollectorOn(Proc_t *proc)
{
  Thread_t *curThread = NULL;
  int isFirst, rootCount = 0;
  int bytesCopied = 0, rootWork = 0;

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

  /* Check local stack empty; reset root lists */
  assert(proc->localStack.cursor == 0);
  QueueClear(proc->root_lists);
  SetCopyRange(&proc->majorRange, proc, toSpace, expandWithPad, dischargeWithPad);

  req_size = 0;
  ResetJob();
  isFirst = (synchBarrier(&numOn1, NumProc, &numOn3)) == 0;
  if (paranoid) {
    if (isFirst) {
      paranoid_check_all(fromSpace, NULL, NULL, NULL);
      synchBarrier(&numOn1, NumProc+1, &numOn3);
    }
    else {
      while (!asynchCheckBarrier(&numOn1, NumProc+1, &numOn3))
	;
    }
  }

  if (isFirst) {
    int neededSize = Heap_GetSize(fromSpace);
    neededSize += neededSize / collectionRate;
    Heap_Resize(fromSpace,neededSize,0);
    Heap_Resize(toSpace,neededSize,1);
    rootWork = major_global_scan(proc);
  }

  while ((curThread = NextJob()) != NULL) {
    assert(curThread->requestInfo >= 0);
    FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(proc,curThread);
  }

  /* Now forward all the roots which initializes the local work stacks */
  synchBarrier(&numOn2, NumProc, &numOn1);
  while (!QueueIsEmpty(proc->root_lists)) {
    /* Cannot dequeue from roots since this may be a global */
    Queue_t *roots = (Queue_t *) Dequeue(proc->root_lists);
    int i, len = QueueLength(roots);  
    rootWork += len;
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      ptr_t obj = *root;
      bytesCopied += forward1_concurrent_stack(obj,&proc->majorRange,&fromSpace->range,proc);
      if (verbose)
	printf("GC %d: collector on %d root = %d   primary = %d, replica = %d\n",NumGC,++rootCount,root,obj,obj[-1]);
    }
    proc->numRoot += len;
  }

  /* Move to global stack */
  SynchStart(workStack);
  SynchMid(workStack);
  moveToGlobalStack(workStack,&proc->localStack);
  SynchEnd(workStack,NULL);
  assert(proc->localStack.cursor == 0);
  GCStatus = GCOn;
  synchBarrier(&numOn3, NumProc, &numOn2);
  flushStore();
  gcstat_normal(proc, 0, bytesCopied, 0);
  return (int) (bytesCopied * copyWeight + rootWork * rootWeight);
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
  int satisfied = 0;
  int hasGC = 0;
  int roundOffSize = RoundUp(th->requestInfo, minOffRequest);
  int roundOnSize = RoundUp(th->requestInfo, minOnRequest);
  int workDone = 0, workToDo = collectionRate * roundOnSize;

  assert(proc->writelistCursor + 2 <= proc->writelistEnd);
  flushStore();

  if (th->requestInfo < 0) {
    unsigned int bytesAvailable = (((unsigned int)proc->writelistEnd) - 
				   ((unsigned int)proc->writelistCursor));
    assert((-th->requestInfo) <= bytesAvailable);
    return 1;
  }
  assert(th->requestInfo > 0);

  while (!satisfied) {
    switch (GCStatus) {
    case GCOff:                                         /* Possible GC states: before; after */
    case GCPendingOn:
      if (GCStatus == GCOff &&
	  GCTry_SemiConcHelp(proc,roundOffSize)) {    /* Off, PendingOn; same */
	satisfied = 1;
	break;
      }
      hasGC = 1;
      workDone += CollectorOn(proc);                 /* Off, PendingOn; On, PendingOff */
      assert(GCStatus == GCOn || GCStatus == GCPendingOff);
      if (workDone < workToDo)
	workDone += do_work(proc, workToDo - workDone, 0); /* On, PendingOff; same */
      if ((th == NULL) ||
	  ((th != NULL) && GCStatus == GCOn &&
	   GCTry_SemiConcHelp(proc,roundOnSize))) {       /* On, PendingOff; same */
	satisfied = 1;
	break;
      }
      if (GCStatus == GCPendingOff)                     /* On, PendingOff */
	break;
      printf("Proc %d: Concurrent collector fell too far behind  - check parameters\n", proc->stid);
      assert(0);
      break;
    case GCOn:                                       
    case GCPendingOff:
      hasGC = 1;
      if (workDone < workToDo)
	workDone += do_work(proc, workToDo - workDone, 0); /* On, PendingOff; same */
      if (GCStatus == GCOn &&
	  GCTry_SemiConcHelp(proc,roundOnSize)) {    /* On, PendingOff; same */
	satisfied = 1;
	break;
      }
      workDone += do_work(proc,                                     /* On, PendingOff; same */                 
			  sizeof(val_t) * Heap_GetSize(fromSpace),  /* Actually bounded by allocation of all procs since GCOff triggered. */
			  1);                                       
      CollectorOff(proc);                               /* On, PendingOff; Off, PendingOn */
      assert(GCStatus == GCOff || GCStatus == GCPendingOn);
      break;
    default: 
      assert(0);
    }
  }

  return 1;
}

void gc_poll_SemiConc(Proc_t *sth)
{
  switch (GCStatus) {
  case GCOff:
    return;
  case GCPendingOn:
    CollectorOn(sth);
    return;
  case GCOn:
    do_work(sth,collectionRate * minOnRequest, 0);  /* collect as though one page had been allocated */
    return;
  case GCPendingOff:
    CollectorOff(sth);
    return;
  }
}

void gc_init_SemiConc()
{
  minOffRequest = 4 * pagesize;
  minOnRequest = pagesize;
  init_int(&MaxHeap, 80 * 1024);
  init_int(&MinHeap, 256);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  init_double(&MinRatio, 0.1);
  init_double(&MaxRatio, 0.7);
  init_int(&MinRatioSize, 512);         
  init_int(&MaxRatioSize, 50 * 1024);
  fromSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  workStack = SharedStack_Alloc(8192);
}

