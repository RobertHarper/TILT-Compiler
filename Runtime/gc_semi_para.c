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


/* ------------------  Parallel array allocation routines ------------------- */

static mem_t alloc_big(int byteLen, int hasPointers)
{
  Thread_t *curThread = getThread();
  Proc_t *proc = curThread->proc;
  unsigned long *saveregs = curThread->saveregs;
  mem_t res = NULL;
  int request = RoundUp(byteLen, pagesize);

  /* Should be at least 0.5K to be considered big */
  assert(byteLen >= 512);                

  /* Get the large region */
  if (saveregs[ALLOCPTR] + byteLen > saveregs[ALLOCLIMIT]) 
    GCFromC(curThread, request, 0);
  assert(saveregs[ALLOCPTR] + byteLen <= saveregs[ALLOCLIMIT]);

  /* Perform actual allocation */
  res = (mem_t) saveregs[ALLOCPTR];
  saveregs[ALLOCPTR] = (unsigned long) (res + (byteLen / (sizeof (val_t))));

  gcstat_normal(proc, byteLen, 0, 0);
  return res;
}

ptr_t alloc_bigintarray_SemiPara(int elemLen, int initVal, int ptag)
{
  /* elemements are byte-sized */
  int wordLen = 1 + (elemLen + 3) / 4;
  mem_t space = alloc_big(4 * wordLen,0);
  ptr_t res = space + 1;
  init_iarray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigptrarray_SemiPara(int elemLen, ptr_t initVal, int ptag)
{
  int wordLen = 1 + elemLen;
  mem_t space = alloc_big(4 * wordLen,1);
  ptr_t res = space + 1;
  init_parray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigfloatarray_SemiPara(int elemLen, double initVal, int ptag)
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


/* --------------------- Parallel collector --------------------- */
static long numWaitThread = 0;   /* threads waiting for mutators to stop */
static long numGlobalThread = 0; /* threads completed local work; all work in shared area */
static long numReadyThread = 0;  /* threads waiting for collection to complete */
static long numDoneThread = 0;   /* threads waiting for heap resize/space flip to complete */

void GCRelease_SemiPara(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  gcstat_normal(proc, alloc, 0, 0);
}

static void stop_copy(Proc_t *proc)
{
  int i;
  int isFirst = 0;
  mem_t to_alloc_start;         /* Designated thread records this initially */
  Thread_t *curThread = NULL;
  static long req_size;            /* These are shared across processors. */
  int copied = 0, write = (proc->writelistCursor - proc->writelistStart) / 2;
  
  /* start timer */ 
  GCStatus = GCOn;

  /* Using asynchronous version, we detect the first thread and permit
     it to do some preliminary work while other threads have not reached the barrier.
     Note that we prevent other threads from proceeding until the first thread has done its work
     by counting the first thread twice in the barrier size. */
  isFirst = (asynchReachBarrier(&numWaitThread)) == 0;
  if (isFirst) {
    Heap_Resize(toSpace,
		((sizeof (val_t)) * (fromSpace->top - fromSpace->bottom)) + NumProc * pagesize, 1);
    ResetJob();                        /* Reset counter so all user threads are scanned */
    req_size = 0;
    asynchReachBarrier(&numWaitThread);
  }
  while (!asynchCheckBarrier(&numWaitThread,  NumProc + 1, &numDoneThread))
    ;
  if (diag)
    printf("Proc %d: mutators stopped; proceeding to collection\n", proc->stid);


  /* All threads get local structures ready */
  assert(proc->localStack.cursor == 0);
  QueueClear(proc->root_lists);
  SetCopyRange(&proc->majorRange, proc, toSpace, expandWithPad, dischargeWithPad);

  /* Write list can be ignored */
  discard_writelist(proc);

  /* The "first" processor is in charge of the globals. */
  if (isFirst) {
    major_global_scan(proc);
    if (diag)
      printf("Proc %d:    computed global roots\n", proc->stid);
  }
  /* All other processors compute thread-specific roots in parallel */
  while ((curThread = NextJob()) != NULL) {
    assert(curThread->requestInfo >= 0);
    FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(proc,curThread);
    if (diag)
      printf("Proc %d:    computed roots of userThread %d\n",
	     proc->stid,curThread->tid);      
  }

  /* Now forward all the roots which initializes the local work stacks */
  while (!QueueIsEmpty(proc->root_lists)) {
    /* Cannot dequeue from roots since this may be a global queue */
    Queue_t *roots = (Queue_t *) Dequeue(proc->root_lists);
    int i, len = QueueLength(roots);  
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      copied += forward1_coarseParallel_stack(root,&proc->majorRange,&fromSpace->range,proc);
    }
    proc->numRoot += len;
  }

  /* Move everything from local stack to global stack to balance work */
  SynchStart(workStack);
  SynchMid(workStack);
  moveToGlobalStack(workStack,&proc->localStack);
  SynchEnd(workStack,NULL);
  

  /* Reaching this barrier indicates all work is now shared.  
     Get work from global stack; operate on local stack; put work back on global stack 
     If global stack is empty between SynchEnd and SynchStart, then we are done. 
     We have to check that all procssors have reached this point before proceeding. */
  asynchReachBarrier(&numGlobalThread);
  if (diag)
    printf("Proc %d: Entering global state\n",proc->stid);
  while (1) {
    int i, numToFetch = 10, numToWork = 20;
    if (asynchCheckBarrier(&numGlobalThread, NumProc, &numWaitThread) &&
	isEmptyGlobalStack(workStack))
      break;
    /* Stack may be empty at this point */
    SynchStart(workStack);
    fetchFromGlobalStack(workStack,&proc->localStack, numToFetch);
    for (i=0; i < numToWork && proc->localStack.cursor > 0; i++) {
      loc_t grayCell = (loc_t)(proc->localStack.stack[--proc->localStack.cursor]);
      (void) scan1_object_coarseParallel_stack(grayCell,&proc->majorRange,&fromSpace->range,&toSpace->range,proc,&copied);
    }
    SynchMid(workStack);
    moveToGlobalStack(workStack,&proc->localStack);
    SynchEnd(workStack,NULL);
  }
  assert(isEmptyGlobalStack(workStack));
  ClearCopyRange(&proc->majorRange);

  /* Wait for all active threads to reach this point so all forwarding is complete */
  if (diag)
    printf("Proc %d: waiting for %d procs to finish collecting\n",proc->stid, 
	 NumProc - numReadyThread);
  synchBarrier(&numReadyThread, NumProc, &numGlobalThread);
  gcstat_normal(proc, 0, copied, write);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    long alloc = (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom);
    Heap_t *froms[2] = {NULL, NULL};
    froms[0] = fromSpace;

    /* Check the tospace heap - zero out all of fromspace */
    if (paranoid) {
      bzero((char *)fromSpace->bottom, (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom));
      paranoid_check_all(fromSpace, NULL, toSpace, NULL);
    }
    
    /* Resize heaps and do stats */
    HeapAdjust(0,req_size,froms,toSpace);
    Heap_Resize(toSpace, Heap_GetSize(fromSpace) + alloc, 0); 
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
	   proc->stid, NumProc - numDoneThread);
  synchBarrier(&numDoneThread, NumProc, &numReadyThread);

  /* stop timer */
  GCStatus = GCOff;
}

void gc_poll_SemiPara(Proc_t *sth)
{
  if (numWaitThread) 
    stop_copy(sth);
}


int GCTry_SemiPara(Proc_t *proc, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo,pagesize);
  int write = (proc->writelistCursor - proc->writelistStart) / 2;

  gcstat_normal(proc, 0, 0, write);
  discard_writelist(proc);
  if (th->requestInfo > 0) {
    GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
    if (proc->allocStart) {
      if (diag) 
	printf("Proc %d: Grabbed %d page(s) at %d\n",proc->stid,roundSize/pagesize,proc->allocStart);
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
    printf("Proc %d: Invoking stop-and-copy\n",proc->stid);
  stop_copy(proc);
}


void gc_init_SemiPara()
{
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


