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
#include "show.h"

mem_t AllocBigArray_SemiPara(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj;
  int tagByteLen = spec->byteLen + 4;
  Align_t align = (spec->type == DoubleField) ? OddWordAlign : NoWordAlign; /* Since there is one tag word */

  /* Allocate the space */
  region = AllocFromThread(thread, tagByteLen, align);
  if (region == NULL) 
    region = AllocFromHeap(fromSpace, thread, tagByteLen, align);
  if (region == NULL) {
    GCFromC(thread, tagByteLen + 4, 0);
    region = AllocFromThread(thread, tagByteLen, align);
  }
  assert(region != NULL);

  /* Allocate object; update stats; initialize */
  obj = region + 1;
  proc->majorUsage.bytesAllocated += tagByteLen;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : init_parray(obj, spec->elemLen, spec->pointerVal); break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}


/* --------------------- Parallel collector --------------------- */
/* These are barrier synchronizations which reset the previous one when completed.
   So, after barrier_n is passed, all processors reset barrier_(n-1).  For this
   cyclical scheme to work, there must be at least three barriers. 
*/
static long numWaitProc = 0;   /* waiting for mutators to stop; first procsesor does prelim work */
static long numWorkProc = 0;   /* waiting for root computation, root forwarding, and shared work to complete */
static long numDoneProc = 0;   /* waiting for heap resize/space flip to complete */

void GCRelease_SemiPara(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->minorUsage.bytesAllocated += alloc;
}

static void stop_copy(Proc_t *proc)
{
  int i;
  int isFirst = 0;
  mem_t to_alloc_start;         /* Designated thread records this initially */
  Thread_t *curThread = NULL;
  static long req_size;            /* These are shared across processors. */
  
  assert(isEmptyStack(&proc->majorStack));
  assert(isEmptySharedObjStack(workStack));

  /* Using asynchronous version, we detect the first thread and permit
     it to do some preliminary work while other processors have not reached the barrier.
     Note that we prevent other threads from proceeding until the first thread has done its work
     by counting the first thread twice in the barrier size. */
  isFirst = (asynchReachBarrier(&numWaitProc)) == 0;
  if (isFirst) {
    Heap_Resize(toSpace, Heap_GetSize(fromSpace) + NumProc * minOffRequest, 1);
    resetSharedObjStack(workStack,NumProc);
    ResetJob();                        /* Reset counter so all user threads are scanned */
    req_size = 0;
    asynchReachBarrier(&numWaitProc);
  }
  while (!asynchCheckBarrier(&numWaitProc,  NumProc + 1, &numDoneProc))
    ;
  if (diag)
    printf("Proc %d: mutators stopped; proceeding to collection\n", proc->procid);


  proc->gcSegment2 = FlipBoth;

  /* All threads get local structures ready */
  resetStack(proc->roots);
  SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);

  /* Write list can be ignored */
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  discard_writelist(proc);

  /* The "first" processor is in charge of the globals. */
  if (isFirst) {
    procChangeState(proc, GCGlobal);
    major_global_scan(proc);
    if (diag)
      printf("Proc %d:    computed global roots\n", proc->procid);
  }
  /* All other processors compute thread-specific roots in parallel */
  procChangeState(proc, GCStack);
  while ((curThread = NextJob()) != NULL) {
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(proc,curThread);
    if (diag)
      printf("Proc %d:    computed roots of userThread %d\n",
	     proc->procid,curThread->tid);      
  }

  procChangeState(proc, GC);
  /* Now forward all the roots which initializes the local work stacks */
  proc->numRoot += lengthStack(proc->roots);
  while (!isEmptyStack(proc->roots)) {
    ploc_t root = (ploc_t) popStack(proc->roots);
    (void) locCopy1_copyCopySync_primaryStack(proc,root,&proc->majorStack,&proc->majorRange,&fromSpace->range); 
  }
  pushSharedObjStack(workStack,&proc->majorStack);  /* We must call this even if local stack is empty */

  while (1) {
    int i, globalEmpty;
    popSharedObjStack(workStack,&proc->majorStack, fetchSize);
    for (i=0; i < localWorkSize; i++) {
      ptr_t gray = popStack(&proc->majorStack);
      if (gray != NULL)
	(void) transferScanObj_locCopy1_copyCopySync_primaryStack(proc,gray,&proc->majorStack,&proc->majorRange,&fromSpace->range);
    }
    globalEmpty = pushSharedObjStack(workStack,&proc->majorStack);  /* We must call this even if local stack is empty */
    if (globalEmpty)
      break;
  }
  assert(isEmptyStack(&proc->majorStack));
  assert(isEmptySharedObjStack(workStack));
  ClearCopyRange(&proc->majorRange);

  /* Wait for all active threads to reach this point so all forwarding is complete */
  if (diag)
    printf("Proc %d: waiting for %d procs to finish collecting\n",proc->procid, 
	 NumProc - numWorkProc);
  synchBarrier(&numWorkProc, NumProc, &numWaitProc);


  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    long alloc = (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom);
    double liveRatio = 0.0;

    /* Check the tospace heap */
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
    /* Resize heaps and do stats */
    liveRatio = HeapAdjust1(req_size,fromSpace,toSpace);
    add_statistic(&proc->majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace, 0, 1);
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  assert(proc->writelistCursor == proc->writelistStart);

  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting for %d threads to sync on space flip\n",
	   proc->procid, NumProc - numDoneProc);
  synchBarrier(&numDoneProc, NumProc, &numWorkProc);
}

void GCPoll_SemiPara(Proc_t *proc)
{
  if (numWaitProc)
    stop_copy(proc);
}


int GCTry_SemiPara(Proc_t *proc, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo,minOffRequest);

  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  discard_writelist(proc);
  if (th->requestInfo > 0) {
    GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
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

void GCStop_SemiPara(Proc_t *proc)
{
  assert(proc->userThread == NULL);
  assert(proc->writelistCursor <= proc->writelistEnd);

  if (diag)
    printf("Proc %d: Invoking stop-and-copy\n",proc->procid);
  stop_copy(proc);
}


void GCInit_SemiPara()
{
  minOffRequest = 4 * pagesize;
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
  workStack = SharedObjStack_Alloc(16384);
}
