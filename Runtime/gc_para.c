
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "show.h"


extern int NumGC;
extern Queue_t *ScanQueue;
extern value_t MUTABLE_TABLE_BEGIN_VAL;
extern value_t MUTABLE_TABLE_END_VAL;
extern int module_count;
extern int pagesize;

static Heap_t *fromheap = NULL, *toheap = NULL;


/* ------------------  Parallel array allocation routines ------------------- */

static value_t* alloc_big(int wordLen, int oddAlign) 
{
  Thread_t *curThread = getThread();
  long *saveregs = curThread->saveregs;
  value_t *bottom = NULL, *top;
  int byteLen = 4 * wordLen;
  int byteLenPad = byteLen + (oddAlign ? 4 : 0);
  int request = RoundUp(byteLenPad, pagesize);

  /* Get the large region */
  if (saveregs[ALLOCPTR] + request > saveregs[ALLOCLIMIT]) 
    GCFromC(curThread, request, 0);
  assert(saveregs[ALLOCPTR] + request <= saveregs[ALLOCLIMIT]);
  bottom = saveregs[ALLOCPTR];
  saveregs[ALLOCPTR] += request;
  
  /* Align by inserting Skip Tag */
  if ((((value_t)bottom) & 7) == 0) {
      *bottom = SKIP_TAG;
      bottom++;
    }
  
  /* Discard the rest of the allocated region */

  return bottom;
}

value_t alloc_bigintarray_SemiPara(int byteLen, value_t initVal, int ptag)
{
  int wordLen = 4 + (byteLen + 3) / 4;
  value_t *space = alloc_big(wordLen,0);
  value_t *res = space + 1;
  init_iarray(res, byteLen, initVal);
  return (value_t) res;
}

value_t alloc_bigptrarray_SemiPara(int wordLen, value_t initVal, int ptag)
{
  value_t *space = alloc_big(4 + wordLen,0);
  value_t *res = space + 1;
  init_parray(res, wordLen, initVal);
  return (value_t) res;
}

value_t alloc_bigfloatarray_SemiPara(int doubleLen, double initVal, int ptag)
{
  int wordLen = 2 * doubleLen;
  value_t *space = alloc_big(4 + wordLen,1);
  value_t *res = space + 1;
  init_farray(res, doubleLen, initVal);
  return (value_t) res;
}


/* --------------------- Parallel collector --------------------- */
static long numWaitThread = 0;      /* threads waiting for mutators to stop */
static long numGlobalThread = 0;    /* threads completed local work; all work in shared area */
static long numReadyThread = 0;     /* threads waiting for collection to complete */
static long numDoneThread = 0;      /* threads waiting for heap resize/space flip to complete */
static value_t SharedStack[4096];
static long SharedCursor;
static long Gate = 0, Turn1 = 0, Turn2 = 0;

static void SynchStart(SysThread_t *sth)
{
  while (Gate);
  FetchAndAdd(&Turn1, 1);
  while (Gate)
    {
      FetchAndAdd(&Turn1, -1);
      while (Gate);
      FetchAndAdd(&Turn1, 1);
    }
}

static void SynchMid(void)
{
  Gate = 1;
  FetchAndAdd(&Turn2, 1);
  FetchAndAdd(&Turn1, -1);
  while (Turn1 > 0);
}

static void SynchEnd(void)
{
  int i = FetchAndAdd(&Turn2, -1);
  if (i==1) {
    Gate = 0;
  }
}



static void stop_copy(SysThread_t *sysThread)
{
  int i;
  int isFirst = 0;
  value_t  *tmp1,*tmp2;
  value_t to_alloc_start;         /* Designated thread records this initially */
  value_t *to_alloc = 0;
  value_t *to_limit = 0;
  range_t from_range, to_range;
  static long req_size;           /* This is shared across processors. */

  /* Indicate we have reached this point;
     if we are the first thread to reach this point, we do some preliminary work */
  isFirst = !(FetchAndAdd(&numWaitThread,1));
  if (isFirst) {
    Heap_Resize(toheap,fromheap->top - fromheap->bottom + NumSysThread * pagesize);
    Heap_Unprotect(toheap);    
    ResetJob();                        /* Reset counter so all user threads are scanned */
    FetchAndAdd(&numWaitThread,1);
    numGlobalThread = 0;
    req_size = 0;
  }

  /* Wait for all threads to reach this point; note that the first thread is counted twice */
  if (diag)
    printf("Proc %d: waiting for %d systhreads to stop mutator %s\n",
	 sysThread->stid, (NumSysThread + 1) - numWaitThread, isFirst ? "First" : "");
  while (numWaitThread < (NumSysThread + 1)) 
    ;
  if (diag)
    printf("Proc %d: proceeding to collection\n", sysThread->stid);
  numDoneThread = 0;


  /* Get local ranges ready for use */
  SetRange(&from_range, fromheap->bottom, fromheap->top);
  SetRange(&to_range, toheap->bottom, toheap->top);
  sysThread->LocalCursor = 0;

  /* The "first" GC processor is in charge of the globals */
  if (isFirst)
    {
      int i, qlen;
      /* Since it's semispace, we must consider all the global roots each time */
      Queue_t *tenuredGlobalRoots = major_global_scan(sysThread);
      to_alloc_start = toheap->alloc_start;
      qlen = QueueLength(tenuredGlobalRoots);
      for (i=0; i<qlen; i++) {
	value_t *root = QueueAccess(tenuredGlobalRoots, i);
	value_t temp = *root;
	forward_minor_stack(root,to_alloc,to_limit,toheap,&from_range,sysThread);
      }
    }

  /* For each user thread, forward its stack and registers */
  {
    Thread_t *curThread = NULL;
    while ((curThread = NextJob()) != NULL) {
      assert(curThread->requestInfo >= 0);
      FetchAndAdd(&req_size, curThread->requestInfo);

      /* Compute the roots from the stack and register set */
      local_root_scan(sysThread,curThread,fromheap);
      /* Also add in the locative roots */
      QueueClear(curThread->loc_roots);
      for (i=0; i<QueueLength(ScanQueue); i++)
	{
	  value_t first = (value_t) (QueueAccess(ScanQueue,i));
	  value_t data_addr = *((value_t *)first);
	  value_t data = *((value_t *)data_addr);
	  if (data > 255)
	    Enqueue(curThread->loc_roots,(int *)first);
	  else
	    {
	      int offset = ((255 - data) * sizeof(value_t));
	      int obj_start = data_addr - offset;
	      forward_minor_stack((value_t *)(&obj_start),to_alloc,to_limit,toheap,&from_range,sysThread);
	      *((int *)first) = obj_start + offset;
	    }
	}
      Enqueue(curThread->root_lists, curThread->loc_roots); 

      while (!QueueIsEmpty(curThread->root_lists))
	{
	  Queue_t *roots = (Queue_t *)Dequeue(curThread->root_lists);
	  int len = QueueLength(roots);
	  while (!(QueueIsEmpty(roots))) {
	    value_t *root = Dequeue(roots);
	    value_t temp = *root;
	    forward_minor_stack(root,to_alloc,to_limit,toheap,&from_range,sysThread);
	  }
	}

      if (paranoid)
	paranoid_check_stack(curThread,fromheap);

      if (diag)
	printf("Proc %d:    forwarded local roots of userThread %d\n",
	       sysThread->stid,curThread->tid);      
    }
  }

  start_timer(&(sysThread->gctime));

  /* Move everything from local stack to global stack to balance work */
  {
    int i,cursor;
    SynchStart(sysThread);
    SynchMid();
    cursor = FetchAndAdd(&SharedCursor,sysThread->LocalCursor);
    for (i=0; i<sysThread->LocalCursor; i++)
      SharedStack[cursor+i] = sysThread->LocalStack[i];
    sysThread->LocalCursor = 0;
    SynchEnd();
    FetchAndAdd(&numGlobalThread,1);
  }
  if (diag)
    printf("Proc %d: Entering global state\n",sysThread->stid);

  /* Get work from global stack; operate on local stack; put work back on global stack 
     If global stack is empty between SynchEnd and SynchStart, then we are done. 
     We have to check that numGlobalThread is high enough so we know all threads have reached
     the point where all work is shared.  */
  while (numGlobalThread < NumSysThread || (SharedCursor != 0)) {
    int num = 10;
    int i, cursor;
    SynchStart(sysThread);
    /* Try to grab 10 items; might have to fix cursor */
    cursor = FetchAndAdd(&SharedCursor,-num);
    if (cursor < num) {     /* check for overreach; SharedCursor is unreliable */
      num = cursor;
      SharedCursor = 0;
    }
    assert(sysThread->LocalCursor == 0);
    for (i=0; i<num; i++)
      sysThread->LocalStack[sysThread->LocalCursor++] = SharedStack[cursor-i-1];
    /* Work on up to 10 items */
    for (i=0; i < 10 && sysThread->LocalCursor > 0; i++) {
      value_t *gray = (value_t *)(sysThread->LocalStack[--sysThread->LocalCursor]);
      scan_minor_stack(gray,&to_alloc,&to_limit,toheap,&from_range,&to_range,sysThread);
    }
    SynchMid();
    /* Move work from local to shared stack */
    cursor = FetchAndAdd(&SharedCursor,sysThread->LocalCursor);
    for (i=0; i<sysThread->LocalCursor; i++)
      SharedStack[cursor+i] = sysThread->LocalStack[i];
    sysThread->LocalCursor = 0;
    SynchEnd();
  }

  if (diag)
    printf("Proc %d: emptied work stack\n",sysThread->stid);

  /* Zero out rest of to-space page */
  if (paranoid) 
    while (to_alloc < to_limit)
      *(to_alloc++) = 15;


  /* Wait for all active threads to reach this point so all forwarding is complete */
  FetchAndAdd(&numReadyThread,1);
  if (diag)
    printf("Proc %d: waiting for %d systhreads to finish collecting\n",sysThread->stid, 
	 NumSysThread - numReadyThread);
  while (numReadyThread < NumSysThread)
    ;
  numWaitThread = 0;

  /* All system threads need to reset their limit pointer */
  sysThread->alloc = StartHeapLimit;
  sysThread->limit = StartHeapLimit;

  /* Only the designated thread needs to perform the following */
  if (isFirst)
    {
      /* Check the tospace heap - zero out all of fromspace */
      if (paranoid) {
	value_t *from_alloc = (value_t *)fromheap->bottom;
	Heap_t *legalHeaps[2];
	legalHeaps[0] = fromheap;
	legalHeaps[1] = NULL;
	paranoid_check_heap_global(fromheap,legalHeaps);
	while (from_alloc < (value_t *)fromheap->top)
	  *(from_alloc++) = 0;
      }

      /* Resize the tospace by using the oldspace size and liveness ratio */
      {
	long alloc = fromheap->top - fromheap->alloc_start;
	long copied = toheap->alloc_start - to_alloc_start;
	long used = fromheap->top - fromheap->bottom + req_size;
	long live = copied + req_size;
	double ratio = (double)(live) / used;
	long new = ComputeHeapSize(live, ratio);
	if (new < live) {
	    fprintf(stderr,"FATAL ERROR: failure new = %d < copied = %d\n",new,copied);
	    assert(0);
	  }
	gcstat_normal(alloc,copied);
	Heap_Resize(toheap,new);
	Heap_Unprotect(toheap); 
      }

      /* Flip space and discard tospace */
      {
	Heap_Protect(fromheap);
	typed_swap(Heap_t *, fromheap, toheap);
	toheap->alloc_start = toheap->bottom;
	NumGC++;
	assert(fromheap->alloc_start >= fromheap->bottom);
	assert(fromheap->alloc_start < fromheap->top);
      }
    }

  /* Resume normal scheduler work and start mutators */
  FetchAndAdd(&numDoneThread,1);
  if (diag)
    printf("Proc %d: waiting for %d threads to sync on space flip\n",sysThread->stid, 
	   NumSysThread - numDoneThread);
  while (numDoneThread < NumSysThread)
    ;
  numReadyThread = 0;

  stop_timer(&(sysThread->gctime));

}

void gc_poll_SemiPara(SysThread_t *sth)
{
  if (numWaitThread) 
    stop_copy(sth);
}


int GCAllocate_SemiPara(SysThread_t *sysThread, int req_size)
{
  int roundSize = RoundUp(req_size,pagesize);
  value_t *tmp1, *tmp2;

  /* Check for first time heap value needs to be initialized */
  GetHeapArea(fromheap,roundSize,&tmp1,&tmp2);
  if (tmp1) {
    if (diag) 
      printf("Proc %d: Grabbed %d page at %d\n",sysThread->stid,roundSize/pagesize,tmp1);
    sysThread->alloc = (int)tmp1;
    sysThread->limit = (int)tmp2;
    return 1;
  }
  return 0;
}

void GC_SemiPara(SysThread_t *sysThread, int req_size)
{
  int pageAllocated = 0;
  int stid = sysThread->stid;
  value_t *tmp1, *tmp2;

  assert(sysThread->userThread == NULL);
  assert(writelist_cursor >= writelist_start);
  assert(writelist_cursor <= writelist_end);   /* It's okay not to synchornize since */
  writelist_cursor = writelist_start;          /* Write list irrelevant in a semispace collector */

  if (diag)
    printf("Proc %d: Invoking stop-and-copy\n",stid);
  stop_copy(sysThread);

}



#define INT_INIT(x,y) { if (x == 0) x = y; }
#define DOUBLE_INIT(x,y) { if (x == 0.0) x = y; }

void gc_init_SemiPara()
{
  INT_INIT(MaxHeap, 80 * 1024);
  INT_INIT(MinHeap, 256);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  DOUBLE_INIT(MinRatio, 0.1);
  DOUBLE_INIT(MaxRatio, 0.7);
  DOUBLE_INIT(MinRatioSize, 512);         
  DOUBLE_INIT(MaxRatioSize, 50 * 1024);
  fromheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  SharedCursor = 0;
}


void gc_finish_SemiPara()
{
  Thread_t *th = getThread();
  int allocsize = th->saveregs[ALLOCPTR] - fromheap->alloc_start;
  gcstat_finish(allocsize);
#ifdef HEAPPROFILE
  gcstat_heapprofile_beforecollect((value_t *)fromheap->alloc_start,
				   allocptr);
  gcstat_heapprofile_aftercollect((value_t *)fromheap->bottom,
				  allocptr);
  gcstat_show_heapprofile("full",0.0,0.0);
  printf("\n\n\n\n");
  gcstat_show_heapprofile("short",1.0,1.0);
#endif
}
