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

  gcstat_normal(byteLen, 0, 0);
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

static void stop_copy(SysThread_t *sysThread)
{
  int i;
  int isFirst = 0;
  mem_t to_alloc_start;         /* Designated thread records this initially */
  Thread_t *curThread = NULL;
  static long req_size;            /* These are shared across processors. */
  CopyRange_t copyRange;
  
  /* start timer */
  start_timer(&(sysThread->gctime));
  GCStatus = GCOn;

  /* Using asynchronous version, we detect the first thread and permit
     it to do some preliminary work while other threads have not reached the barrier.
     Note that we prevent other threads from proceeding until the first thread has done its work
     by counting the first thread twice in the barrier size. */
  isFirst = (asynchReachBarrier(&numWaitThread)) == 0;
  if (isFirst) {
    Heap_Unprotect(toSpace,
		((sizeof (val_t)) * (fromSpace->top - fromSpace->bottom)) + NumSysThread * pagesize);
    ResetJob();                        /* Reset counter so all user threads are scanned */
    req_size = 0;
    asynchReachBarrier(&numWaitThread);
  }
  while (!asynchCheckBarrier(&numWaitThread,  NumSysThread + 1, &numDoneThread))
    ;
  if (diag)
    printf("Proc %d: mutators stopped; proceeding to collection\n", sysThread->stid);


  /* All threads get local structures ready */
  assert(sysThread->LocalCursor == 0);
  QueueClear(sysThread->root_lists);
  SetCopyRange(&copyRange, sysThread->stid, toSpace, expandWithPad, dischargeWithPad);

  /* Write list can be ignored */
  discard_writelist(sysThread);

  /* The "first" processor is in charge of the globals. */
  if (isFirst) {
    major_global_scan(sysThread);
    if (diag)
      printf("Proc %d:    computed global roots\n", sysThread->stid);
  }
  /* All other processors compute thread-specific roots in parallel */
  while ((curThread = NextJob()) != NULL) {
    assert(curThread->requestInfo >= 0);
    FetchAndAdd(&req_size, curThread->requestInfo);
    local_root_scan(sysThread,curThread);
    if (diag)
      printf("Proc %d:    computed roots of userThread %d\n",
	     sysThread->stid,curThread->tid);      
  }

  /* Now forward all the roots which initializes the local work stacks */
  while (!QueueIsEmpty(sysThread->root_lists)) {
    /* Cannot dequeue from roots since this may be a global queue */
    Queue_t *roots = (Queue_t *) Dequeue(sysThread->root_lists);
    int i, len = QueueLength(roots);  
    for (i=0; i<len; i++) {
      ploc_t root = (ploc_t) QueueAccess(roots,i);
      forward1_coarseParallel_stack(root,&copyRange,&fromSpace->range,sysThread);
    }
  }

  /* Move everything from local stack to global stack to balance work */
  SynchStart(workStack);
  SynchMid(workStack);
  moveToGlobalStack(workStack,sysThread->LocalStack, &(sysThread->LocalCursor));
  SynchEnd(workStack);
  

  /* Reaching this barrier indicates all work is now shared.  
     Get work from global stack; operate on local stack; put work back on global stack 
     If global stack is empty between SynchEnd and SynchStart, then we are done. 
     We have to check that all procssors have reached this point before proceeding. */
  asynchReachBarrier(&numGlobalThread);
  if (diag)
    printf("Proc %d: Entering global state\n",sysThread->stid);
  while (1) {
    int i, numToFetch = 10, numToWork = 20;
    if (asynchCheckBarrier(&numGlobalThread, NumSysThread, &numWaitThread) &&
	isEmptyGlobalStack(workStack))
      break;
    /* Stack may be empty at this point */
    SynchStart(workStack);
    fetchFromGlobalStack(workStack,sysThread->LocalStack, &(sysThread->LocalCursor), numToFetch);
    for (i=0; i < numToWork && sysThread->LocalCursor > 0; i++) {
      loc_t grayCell = (loc_t)(sysThread->LocalStack[--sysThread->LocalCursor]);
	scan1_object_coarseParallel_stack(grayCell,&copyRange,&fromSpace->range,&toSpace->range,sysThread);
    }
    SynchMid(workStack);
    moveToGlobalStack(workStack,sysThread->LocalStack, &(sysThread->LocalCursor));
    SynchEnd(workStack);
  }
  assert(isEmptyGlobalStack(workStack));
  copyRange.discharge(&copyRange);

  /* Wait for all active threads to reach this point so all forwarding is complete */
  if (diag)
    printf("Proc %d: waiting for %d systhreads to finish collecting\n",sysThread->stid, 
	 NumSysThread - numReadyThread);
  synchBarrier(&numReadyThread, NumSysThread, &numGlobalThread);


  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    long alloc = (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom);
    long copied = (sizeof (val_t)) * (toSpace->cursor - toSpace->bottom);
    Heap_t *froms[2] = {NULL, NULL};
    froms[0] = fromSpace;

    /* Check the tospace heap - zero out all of fromspace */
    if (paranoid) {
      bzero((char *)fromSpace->bottom, (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom));
      paranoid_check_all(fromSpace, NULL, toSpace, NULL);
    }
    
    /* Resize heaps and do stats */
    gcstat_normal(alloc,copied,0);
    HeapAdjust(0,req_size,froms,toSpace);
    Heap_Unprotect(toSpace, fromSpace->top - fromSpace->bottom + alloc); 
    fromSpace->cursor = fromSpace->bottom;
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  sysThread->allocStart = StartHeapLimit;
  sysThread->allocCursor = StartHeapLimit;
  sysThread->allocLimit = StartHeapLimit;
  assert(sysThread->writelistCursor == sysThread->writelistStart);

  /* Resume normal scheduler work and start mutators */
  if (diag)
    printf("Proc %d: waiting for %d threads to sync on space flip\n",
	   sysThread->stid, NumSysThread - numDoneThread);
  synchBarrier(&numDoneThread, NumSysThread, &numReadyThread);

  /* stop timer */
  GCStatus = GCOff;
  stop_timer(&(sysThread->gctime));
}

void gc_poll_SemiPara(SysThread_t *sth)
{
  if (numWaitThread) 
    stop_copy(sth);
}


int GCTry_SemiPara(SysThread_t *sysThread, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo,pagesize);
  mem_t tmp_alloc, tmp_limit;

  discard_writelist(sysThread);
  if (th->requestInfo > 0) {
    GetHeapArea(fromSpace,roundSize,&tmp_alloc,&tmp_limit);
    if (tmp_alloc) {
      if (diag) 
	printf("Proc %d: Grabbed %d page(s) at %d\n",sysThread->stid,roundSize/pagesize,tmp_alloc);
      sysThread->allocStart = tmp_alloc;
      sysThread->allocCursor = tmp_alloc;
      sysThread->allocLimit = tmp_limit;
      return 1;
    }
  }
  else if (th->requestInfo < 0) {
    unsigned int bytesAvailable = (((unsigned int)sysThread->writelistEnd) - 
				   ((unsigned int)sysThread->writelistCursor));
    return ((-th->requestInfo) <= bytesAvailable);
  }
  else 
    assert(0);
  return 0;
}

void GCStop_SemiPara(SysThread_t *sysThread)
{
  assert(sysThread->userThread == NULL);
  assert(sysThread->writelistCursor <= sysThread->writelistEnd);

  if (diag)
    printf("Proc %d: Invoking stop-and-copy\n",sysThread->stid);
  stop_copy(sysThread);
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
  workStack = SharedStack_Alloc();
}


void gc_finish_SemiPara()
{
  Thread_t *th = getThread();
  int allocsize = (unsigned int) th->saveregs[ALLOCPTR] - (unsigned int) fromSpace->cursor;
  gcstat_normal(allocsize,0,0);
}
