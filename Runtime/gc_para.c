/* Not thread-safe */
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "show.h"
#include "forward.h"
#include "gc.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"




extern int TotalGenBytesCollected;
extern int TotalBytesAllocated;
extern int NumGC;
extern Queue_t *ScanQueue;
extern value_t MUTABLE_TABLE_BEGIN_VAL;
extern value_t MUTABLE_TABLE_END_VAL;
extern int module_count;
extern int pagesize;

static Heap_t *fromheap = NULL, *toheap = NULL;
static Queue_t *global_roots = 0;


/* ------------------  Parallel array allocation routines ------------------- */


value_t alloc_bigintarray_para(int log_len, value_t init_val, int ptag)
{
  assert(0);
  return 0;
}

value_t alloc_bigptrarray_para(int log_len, value_t init_val, int ptag)
{
  assert(0);
  return 0;
}

value_t alloc_bigfloatarray_para(int log_len, double init_val, int ptag)
{
  assert(0);
  return 0;
}




/* --------------------- Parallel collector --------------------- */
static int numWaitThread = 0;      /* threads waiting for mutators to stop */
static int numGlobalThread = 0;    /* threads completed local work; all work in shared area */
static int numReadyThread = 0;     /* threads waiting for collection to complete */
static int numDoneThread = 0;      /* threads waiting for heap resize/space flip to complete */
static value_t SharedStack[4096];
static int SharedCursor;
static int Gate = 0, Turn1 = 0, Turn2 = 0;

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
  value_t *to_alloc = 0;
  value_t *to_limit = 0;
  range_t from_range, to_range;

  /* Indicate we have reached this point;
     if we are the first thread to reach this point, we do some preliminary work */
  isFirst = !(FetchAndAdd(&numWaitThread,1));
  if (isFirst) {
    Heap_Resize(toheap,fromheap->top - fromheap->bottom + NumSysThread * pagesize);
    Heap_Unprotect(toheap);    
    ResetJob();                        /* Reset counter so all user threads are scanned */
    FetchAndAdd(&numWaitThread,1);
    numGlobalThread = 0;
  }

  /* Wait for all threads to reach this point; note that the first thread is counted twice */
  if (diag)
    printf("Proc %d: waiting for %d systhreads to stop mutator %s\n",
	 sysThread->stid, (NumSysThread + 1) - numWaitThread, isFirst ? "First" : "");
  while (numWaitThread < (NumSysThread + 1)) 
    ;
  if (diag)
    printf("Proc %d: proceeding to colection\n", sysThread->stid);
  numDoneThread = 0;



  /* Get local ranges ready for use */
  SetRange(&from_range, fromheap->bottom, fromheap->top);
  SetRange(&to_range, toheap->bottom, toheap->top);
  sysThread->LocalCursor = 0;

  /* The "first" GC thread is in charge of the globals */
  if (isFirst)
    {
      /* Since it's semispace, we must consider the global_roots each time */
      global_root_scan(sysThread,global_roots,fromheap);
      while (!(QueueIsEmpty(global_roots))) {
	value_t *root = Dequeue(global_roots);
	value_t temp = *root;
	forward_minor_stack(root,to_alloc,to_limit,toheap,&from_range,sysThread);
      }
    }

  /* For each remaining user thread, forward its stack and registers */
  {
    Thread_t *curThread = NULL;
    while ((curThread = NextJob()) != NULL) {


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
      /* Check the heap */
      if (paranoid) {
	value_t *from_alloc = (value_t *)fromheap->bottom;
	paranoid_check_heap(fromheap,toheap);
	while (from_alloc < (value_t *)fromheap->top)
	  *(from_alloc++) = 13;
      }

      /* Resize the tospace by using the oldspace size and liveness ratio */
      {
	long alloc = fromheap->top - fromheap->alloc_start;
	long old = fromheap->top - fromheap->bottom;
	long copied = ((value_t) toheap->alloc_start) - toheap->bottom;
	double oldratio = (double)(copied) / old;
	long new = ComputeHeapSize(copied, oldratio);
	if (new < copied)
	  {
	    fprintf(stderr,"FATAL ERROR: failure new = %d < copied = %d\n",new,copied);
	    exit(-1);
	  }
	gcstat_normal(alloc,old,oldratio,new,copied);
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

  scheduler(sysThread);
  assert(0);
}

void poll_para()
{
  if (numWaitThread)
    stop_copy(getSysThread());
}

void gc_para(SysThread_t *sysThread)
{
  int stid = sysThread->stid;
  value_t *tmp1, *tmp2;

  assert(sysThread->userThread == NULL);
  assert(writelist_cursor >= writelist_start);
  assert(writelist_cursor <= writelist_end);   /* It's okay not to synchornize since */
  writelist_cursor = writelist_start;          /* Write list is irrelevnat in a semispace collector */

  /* See if we can grab another page from the fromspace; if not, then it's time to stop and copy */
  GetHeapArea(fromheap,pagesize,&tmp1,&tmp2);
  if (tmp1) {
    sysThread->alloc = (int)tmp1;
    sysThread->limit = (int)tmp2;
    if (diag) {
      printf("Proc %d: Grabbed a page at %d\n",stid,tmp1);
    }
    scheduler(sysThread);
    assert(0);
  }

  if (diag)
    printf("Proc %d: Invoking stop-and-copy\n",stid);
  stop_copy(sysThread);
  assert(0);
}



#define INT_INIT(x,y) { if (x == 0) x = y; }
#define DOUBLE_INIT(x,y) { if (x == 0.0) x = y; }

void gc_init_para()
{
  INT_INIT(MaxHeap, 32 * 1024);
  INT_INIT(MinHeap, 256);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  DOUBLE_INIT(TargetRatio, 0.08);
  DOUBLE_INIT(MaxRatio, 0.8);
  DOUBLE_INIT(UpperRatioReward, 1.5);
  DOUBLE_INIT(LowerRatioReward, 0.75);
  DOUBLE_INIT(TargetSize, 8192.0);
  DOUBLE_INIT(SizePenalty, 0.1);
  fromheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  global_roots = QueueCreate(0,100);
  SharedCursor = 0;
}


void gc_finish_para()
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
