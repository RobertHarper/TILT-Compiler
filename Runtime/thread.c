#include "general.h"
#include "tag.h"
#include "thread.h"
#include "stats.h"
#include "memobj.h"
#include <assert.h>
#include <stdio.h>
#include "til-signal.h"
#include <sys/types.h>
#include <sys/procset.h>
#include <sys/processor.h>
#include <unistd.h>
#include <pthread.h>
#include "gc.h"



Thread_t    *Threads;                         /* array of NumUserThread user threads */
static SysThread_t *SysThreads;               /* array of NumSystemThread system threads */

static long totalThread = 0;                  /* Number of create user threads */
static long maxThread = 0;                    /* Maximum number of threads in work queue */

static Thread_t **JobQueue;                   /* Work list for user threads: array of pointers to user threads */
static pthread_cond_t EmptyCond;              /* Signals no more jobs */
static pthread_mutex_t EmptyLock;             /*   Lock associated with EmptyCond */
static int topThread = 0;                     /* points just beyond last job on work list */
static int curThread = 0;                     /* cursor for iterating over all jobs */
static const int ThreadDone = -1;
static const int ThreadNew = -1;

extern void start_client(Thread_t *, ptr_t *, int);
extern void context_restore(Thread_t *);

/* ------------------ Manipulating the job queue ----------------- */
static long local_lock = 0;

void LocalLock(void)
{
  while (!TestAndSet(&local_lock)) /* No need to flush */
    ;
  assert(local_lock == 1);
  flushStore();
}

void LocalUnlock(void)
{
  flushStore();
  local_lock = 0;
}

int NumTotalJob(void)
{
  return topThread;
}

int NumReadyJob(void)
{
  int i, count = 0;
  LocalLock();
  for (i=0; i<topThread; i++) 
    if (JobQueue[i]->status >= 0) {
      count++;
    }
  LocalUnlock();
  return count;
}

void ResetJob(void)
{
  LocalLock();
  curThread = 0;
  LocalUnlock();
}

Thread_t *NextJob(void)
{
  Thread_t *this = NULL;
  LocalLock();
  while (curThread < NumThread && this == NULL) {
    this = JobQueue[curThread];
    curThread++;
  }
  LocalUnlock();
  return this;
}


void AddJob(Thread_t *th)
{
  LocalLock();
  th->status = 0;
  if (th->parent) {
    int i, j;
    FetchAndAdd(&th->parent->status,1);
  }
  JobQueue[topThread] = th;
  topThread++;
  maxThread = (topThread > maxThread) ? topThread : maxThread;
  flushStore();
  LocalUnlock();
}

Thread_t *FetchJob(void)
{
  int i;
  LocalLock();
  for (i=topThread-1; i>=0; i--) {
    Thread_t *th = JobQueue[i];
    if (th->status == 0) {
      FetchAndAdd(&th->status,1);
      LocalUnlock();
      return th;
    }
  }
  LocalUnlock();
  return 0;
}

void ReleaseJob(SysThread_t *sth)
{
  int i;
  Thread_t *th = sth->userThread;
  mem_t sth_allocCursor = sth->allocCursor;
  mem_t sth_allocLimit = sth->allocLimit;
  mem_t sth_writelistCursor = (mem_t) sth->writelistCursor;

  /* Check that thread's allocation pointers are consistent and update processor's version */
  if (((mem_t) th->saveregs[ALLOCLIMIT] != sth_allocLimit &&
       (mem_t) th->saveregs[ALLOCLIMIT] != StopHeapLimit) ||
      (mem_t) th->saveregs[ALLOCPTR] > sth_allocLimit) {
    printf("sth->allocCursor = %d\n",sth_allocCursor);
    printf("sth->allocLimit = %d\n",sth_allocLimit);
    printf("th->saveregs[ALLOC] = %d\n",th->saveregs[ALLOCPTR]);
    printf("th->saveregs[LIMIT] = %d\n",th->saveregs[ALLOCLIMIT]);
    assert(0);
  }

  /* Update processor's version of allocation range and write list and process */
  sth->allocCursor = (mem_t) th->saveregs[ALLOCPTR];
  sth->writelistCursor = th->writelistAlloc;
  GCRelease(sth);

  /* Null out thread's version of allocation and write-list */
  th->saveregs[ALLOCPTR] = 0;
  th->saveregs[ALLOCLIMIT] = 0;
  th->writelistAlloc = 0;
  th->writelistLimit = 0;

  /* Break association between thread and processor */
  sth->userThread = NULL;
  th->sysThread = NULL;
  
  flushStore();                          /* make sure thread info is flushed to memory */
  assert(th->status >= 1);               /* Thread was mapped and running so could not be ready or done */
  FetchAndAdd(&(th->status),-1);         /* Release after flush; note that FA always goes to mem */

  if (diag)
    printf("Proc %d: Released thread %d (status = %d) with request = %d.  Used %d to %d and %d to %d.\n",
	   sth->stid,th->tid,th->status,th->requestInfo, sth_allocCursor,sth->allocCursor,
	   sth_writelistCursor, sth->writelistCursor);
}


void DeleteJob(SysThread_t *sth)
{
  int i, j;
  Thread_t *th = sth->userThread;
  assert(th->sysThread == sth);
  FetchAndAdd(&(th->status),1);  /* We increment status so it doesn't get scheduled when released */
  ReleaseJob(sth);
  LocalLock();
  for (i=0; i<NumThread; i++) {
    if (JobQueue[i] == th) {
      if (th->parent) {
	FetchAndAdd(&(th->parent->status),-1);
	assert(th->parent->status >= 0); /* Parent must not have already finished */
      }
      for (j=i; j<NumThread-1; j++)
	JobQueue[j] = JobQueue[j+1];
      topThread--;
      if (diag)
	printf("DeleteJob: topThread = %d.  Signalling EmptyCond.\n", topThread); 
      th->status = ThreadDone;            /* Now, we put it back in the free pool */
      LocalUnlock();
      if (topThread == 0)
	pthread_cond_signal(&EmptyCond);  /* Wake up main thread if there are no more jobs */
      return;
    }
  }
  printf("Error deleteing user thread %d (%d)\n",th->tid,th->id);
  assert(0);
}


void StopAllThreads()
{
  int i;
  LocalLock();
  for (i=0; i<NumThread; i++) {
    Thread_t *th = JobQueue[i];
    if (th != NULL)
      th->saveregs[ALLOCLIMIT] = (val_t) StopHeapLimit;
  }
  LocalUnlock();
}


/* --------------------- Helpers ---------------------- */
void check(char *str, SysThread_t *sth)
{
  int i, j;
  return;
  for (i=0; i<NumThread; i++) {
    if (Threads[i].status < 0)
      continue;
    for (j=Threads[i].nextThunk; j<Threads[i].numThunk; j++) {
      ptr_t t = Threads[i].thunks[j];
      if (t != 0 && *t > 1000000) {
	printf("Proc %d: check at %s failed: Threads[%d].oneThunk = %d mapped to sysThread %d\n",
	       (sth == 0) ? -1 : sth->stid,
	       str,i,*t,Threads[i].sysThread);
	assert(0);
      }
    }
  }
}


SysThread_t *getSysThread()
{
  int i;
  pthread_t sys = pthread_self();
  for (i=0; i<NumSysThread; i++) {
    if (SysThreads[i].pthread == sys) 
      return &(SysThreads[i]);
  }
  return NULL;
}


Thread_t *getThread()
{
  SysThread_t *sth = getSysThread();
  if (sth == NULL) 
    return NULL;
  return sth->userThread;
}


void fillThread(Thread_t *th, int i)
{
  th->id = i;
  th->status = ThreadNew;
  th->tid = -1;
  th->parent = NULL;
  th->request = StartRequest;
  th->maxSP = 0;
  th->last_snapshot = -1;
  th->snapshots = (StackSnapshot_t *)malloc(NUM_STACK_STUB * sizeof(StackSnapshot_t));
  for (i=0; i<NUM_STACK_STUB; i++)
    {
      th->snapshots[i].saved_ra = (val_t) stub_error;
      th->snapshots[i].saved_sp = (val_t) 0;
      th->snapshots[i].saved_regstate = 0;
      th->snapshots[i].roots = (i == 0) ? QueueCreate(0,50) : NULL;
    }
  th->saveregs[THREADPTR] = (long) th;
  th->saveregs[ALLOCLIMIT] = 0;
  th->retadd_queue = QueueCreate(0,2000);
  th->stackchain = NULL;
  th->reg_roots = QueueCreate(0,32);
  th->thunks = (ptr_t *) 69;
}

void resetThread(Thread_t *th, Thread_t *parent, ptr_t *thunks, int numThunk)
{
  assert(&(Threads[th->id]) == th);
  th->tid = FetchAndAdd(&totalThread,1);
  th->parent = parent;
  th->request = StartRequest;
  th->last_snapshot = -1;
  th->saveregs[THREADPTR] = (long) th;
  th->saveregs[ALLOCLIMIT] = 0;
  if (numThunk == 0) { 
    /* thunks actually is a thunk */
    th->thunks = &(th->oneThunk);
    th->oneThunk = (ptr_t) thunks;
    th->nextThunk = 0;
    th->numThunk = 1;
  }
  else 
    { th->thunks = thunks;
      th->oneThunk = 0;
      th->nextThunk = 0;
      th->numThunk = numThunk;
    }
  check("threadcreate_mid",getSysThread());
  if (th->stackchain == NULL)
    th->stackchain = StackChain_Alloc(); 
  QueueClear(th->snapshots[0].roots);
  QueueClear(th->retadd_queue);
  QueueClear(th->reg_roots);
}

void thread_init()
{
  int i, j;
  assert(intSz == sizeof(int));
  assert(longSz == sizeof(long));
  assert(CptrSz == sizeof(int *));
  assert(doubleSz == sizeof(double));
  assert(sizeof(tag_t) == 4);
  assert(sizeof(val_t) == 4);
  assert(sizeof(ptr_t) == 4);
  assert(sizeof(loc_t) == 4);
  assert(sizeof(ploc_t) == 4);
  assert(sizeof(mem_t) == 4);

  Threads = (Thread_t *)malloc(sizeof(Thread_t) * NumThread);
  SysThreads = (SysThread_t *)malloc(sizeof(SysThread_t) * NumSysThread);
  JobQueue = (Thread_t **)malloc(sizeof(Thread_t *) * NumThread);
  for (i=0; i<NumThread; i++) 
    JobQueue[i] = NULL; 
  for (i=0; i<NumThread; i++)
    fillThread(&Threads[i], i);
  for (i=0; i<NumSysThread; i++) {
    char temp[40];
    SysThread_t *sth = &(SysThreads[i]); /* Structures are by-value in C */
    sth->stid = i;
    sth->LocalCursor = 0;
    sth->allocStart = StartHeapLimit;
    sth->allocCursor = StartHeapLimit;
    sth->allocLimit = StartHeapLimit;
    reset_timer(temp,&(sth->majorgctime));
    sth->writelistStart = &(sth->writelist[0]);
    sth->writelistCursor = sth->writelistStart;
    sth->writelistEnd = &(sth->writelist[(sizeof(sth->writelist) / sizeof(ptr_t)) - 2]);
    for (j=0; j<(sizeof(sth->writelist) / sizeof(ptr_t)); j++)
      sth->writelist[j] = 0;
    sth->root_lists = QueueCreate(0,50);
    sth->largeRoots = QueueCreate(0,50);
    sprintf(temp, "stacktime_%d", i);
    reset_timer(temp,&(sth->stacktime));
    sprintf(temp, "gctime_%d", i);
    reset_timer(temp,&(sth->gctime));
    sprintf(temp, "majorgcime_%d", i);
  }
  pthread_cond_init(&EmptyCond,NULL);
  pthread_mutex_init(&EmptyLock,NULL);
  setbuf(stdout,NULL);
  setbuf(stderr,NULL);
}

int thread_total() 
{
  return totalThread;
}
 
int thread_max() 
{
  return maxThread;
}

/* If numThunk is zero, then thunks IS the thunk rather than an array of thunks */
Thread_t *thread_create(Thread_t *parent, ptr_t *thunks, int numThunk)
{
  int i;
  Thread_t *th = NULL;

  for (i=0; i<NumThread; i++) {
    Thread_t *temp = &(Threads[i]);
    if (temp->status == ThreadNew) {
      LocalLock();
      if (temp->status == ThreadNew) 
	temp->status = 0;
      else {
	LocalUnlock();
	continue;
      }
      LocalUnlock();
      th = temp;
      break;
    }
  }

  if (th == NULL) {
    printf("Work list has %d threads\n",NumTotalJob());
    printf("thread_create failed\n");
    assert(0);
  }
  assert(th->status == 0);
  resetThread(th, parent, thunks, numThunk);
  return th;
}


void load_iregs_fail(int memValue, int regValue)
{
  printf("load_iregs_fail with memValue = %d  regValue = %d\n",memValue,regValue);
  assert(0);
}

void save_iregs_fail(int memValue, int regValue)
{
  printf("save_iregs_fail with memValue = %d  regValue = %d\n",memValue,regValue);
  assert(0);
}

void load_regs_fail(int memValue, int regValue)
{
  printf("load_regs_fail with memValue = %d  regValue = %d\n",memValue,regValue);
  assert(0);
}

void save_regs_fail(int memValue, int regValue)
{
  printf("save_regs_fail with memValue = %d  regValue = %d\n",memValue,regValue);
  assert(0);
}


static void mapThread(SysThread_t *sth, Thread_t *th)
{
  assert(sth->userThread == NULL);
  assert(th->sysThread == NULL);
  sth->userThread = th;
  th->sysThread = sth;
  th->saveregs[ALLOCPTR] = (reg_t) sth->allocCursor;
  th->saveregs[ALLOCLIMIT] = (reg_t) sth->allocLimit;
  th->writelistAlloc = sth->writelistCursor;
  th->writelistLimit = sth->writelistEnd;
}

static void work(SysThread_t *sth)
{
  Thread_t *th = sth->userThread;

  /* To make visible changes other processors have made */
  flushStore();

  /* System thread should not be mapped */
  assert(th == NULL);

  /* Wait for next user thread and remove from queue. Map system thread. */
  while (th == NULL) {
    if (NumReadyJob() == 0)
#ifdef alpha_osf
      sched_yield();
#endif
#ifdef solaris
      thr_yield();
#endif
    th = FetchJob();  /* Provisionally grab thread but don't map onto processor yet */
    if (th == NULL)
      gc_poll(sth);
  }

  if (th->status != 1)
    printf("Proc %d: thread %d  has status = %d\n", sth->stid,th->tid,th->status);
  assert(th->status == 1); /* FetchJob increments status from 0 to 1 */
  switch (th->request) {
  
    case NoRequest : assert(0);

    case YieldRequest : {
#ifdef solaris
      th->saveregs[16] = (long) th;  /* load_regs_forC on solaris expects thread pointer in %l0 */
#endif	
      mapThread(sth,th);
      returnFromYield(th);
    }

    case StartRequest : {              /* Starting thread for the first time */
      mem_t stack_top = th->stackchain->stacks[0]->top;
      assert(th->nextThunk == 0);
      th->saveregs[THREADPTR] = (long)th;
      th->saveregs[SP] = (long)stack_top - 128;       /* Get some initial room; Sparc requires at least 68 byte for the save area */
      mapThread(sth,th);
      if (diag) {
	printf("Proc %d: starting user thread %d (%d) with %d <= %d\n",
	       sth->stid, th->tid, th->id, th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT]);
	assert(th->saveregs[ALLOCPTR] <= th->saveregs[ALLOCLIMIT]);
      }
      flushStore();  /* make visible changes to other processors */
      start_client(th,th->thunks, th->numThunk);
      assert(0);
    }

    case GCRequestFromML:
    case GCRequestFromC: 
    case MajorGCRequestFromC: {
      /* Allocate space or check write buffer to see if we have enough space */
      int satisfied = GCFromScheduler(sth, th);
      while (!satisfied) {
	printf("Warning: Proc %d: could not resume thread %d after calling GCFromScheduler.  Retrying...\n",
	       sth->stid, th->tid);
	satisfied = GCFromScheduler(sth, th);
      }
      /* Note that another processor can change th->saveregs[ALLOCLIMIT] to Stop at any point */
      if (th->requestInfo > 0)
	assert(th->requestInfo + (val_t) sth->allocCursor <= (val_t) sth->allocLimit);
      else if (th->requestInfo < 0)
	assert((-th->requestInfo) + (val_t) sth->writelistCursor <= (val_t) sth->writelistEnd);
      else 
	assert(0);
      mapThread(sth,th);
      if (th->request == GCRequestFromML) {
	if (diag)
	  printf("Proc %d: Resuming thread %d from GCRequestFromML of %d bytes with allocation region %d < %d and writelist %d < %d\n",
		 sth->stid, th->tid, th->requestInfo, 
		 th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT],
		 th->writelistAlloc, th->writelistLimit);
	flushStore();  /* make visible changes to other processors */
	returnFromGCFromML(th);	
	assert(0);
      }
      else if (th->request == GCRequestFromC ||
	       th->request == MajorGCRequestFromC) {
	if (diag)
	  printf("Proc %d: Resuming thread %d from GCRequestFromML of %d bytes with allocation region %d < %d and writelist %d < %d\n",
		 sth->stid, th->tid, th->requestInfo, sth->allocCursor, sth->allocLimit,
		 th->writelistAlloc, th->writelistLimit);
	flushStore();  /* make visible changes to other processors */
	returnFromGCFromC(th);	
	assert(0);
      }
      else
	assert(0);
    }
   default: {
      printf("Odd request %d\n",th->request);
      assert(0);
    }
  } /* end of switch */
  assert(0);
}


static void* systhread_go(void* unused)
{
  SysThread_t *st = getSysThread();
#ifdef solaris
  int status = processor_bind(P_LWPID, P_MYID, st->processor, NULL);
  if (status != 0)
    printf("processor_bind failed with %d\n",status);
#else
  if (diag)
    printf("Cannot find processors on non-sparc: assuming uniprocessor\n");
#endif
  install_signal_handlers(0);
  st->stack = (int)(&st) & (~255);
  work(st);
  assert(0);
  return 0;
}


void thread_go(ptr_t *thunks, int numThunk)
{
  int curproc = -1;
  int i;

  Thread_t *th = thread_create(NULL,thunks,numThunk);
  AddJob(th);

  /* Create system threads that run off the user thread queue */
  for (i=0; i<NumSysThread; i++) {
    pthread_attr_t attr;
#ifdef solaris
    processor_info_t infop;
    while (1) {
      int status = processor_info(++curproc,&infop);
      if (status == 0 && infop.pi_state == P_ONLINE) {
	SysThreads[i].processor = curproc;
	break;
      }
      if (curproc > 1024) {
	printf("Cannot find enough online processors: curproc = %d\n",curproc);
	assert(0);
      }
    }
#else
    if (diag)
      printf("Cannot find processors on non-sparc: assuming uniprocessor\n");
#endif
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr,256 * 1024);
    pthread_attr_setscope(&attr,PTHREAD_SCOPE_SYSTEM); 
    pthread_create(&(SysThreads[i].pthread),&attr,systhread_go,NULL);
    if (diag)
      printf("Proc %d:  processor %d and pthread = %d\n",
	     SysThreads[i].stid, SysThreads[i].processor, SysThreads[i].pthread);
  }
  install_signal_handlers(1);
  /* Wait until the work stack is empty;  work stack contains running jobs too */
  while ((i = NumTotalJob()) > 0) {
    if (diag)
      printf("Main thread found %d jobs.\n", i);
    pthread_cond_wait(&EmptyCond,&EmptyLock);
  }
  /* Now collect the GC times of the system threads */
  for (i=0; i<NumSysThread; i++) {
    SysThread_t *sth = &(SysThreads[i]);
    stats_finish_thread(&sth->stacktime,&sth->gctime,&sth->majorgctime);
  }
}


/* ------------------ Mutator interface ----------------- */
int showIntList(ptr_t v)
{
  printf("%d: %d %d\n",v, v[0], v[1]);
  return 256; /* ML unit */
}

int showInt(val_t v)
{
  printf("%d",v); 
  return 256; /* ML unit */
}

int showIntRef(ptr_t v)
{
  printf("%d: %d", v, *v); 
  return 256; /* ML unit */
}

int threadID()
{
  SysThread_t *sth = getSysThread();
  Thread_t *th = sth->userThread;
  int temp = sth->stid;
  temp = 1000 * temp + th->tid;
  temp = 1000 * temp + th->id;
  return temp;
}

Thread_t *SpawnRest(ptr_t thunk)
{
  SysThread_t *sth = getSysThread();
  Thread_t *parent = sth->userThread;
  Thread_t *child = NULL;

  assert(sth->stack - ((int) &sth) < 1024);   /* stack frame for this function should be < 1K */
  assert(parent->sysThread == sth);
  check("Spawnstart",sth);
  if (thunk < (mem_t) 1000000) {
    printf("Proc %d: Thread %d: Spawn given bad thunk %d\n",
	   sth->stid, parent->tid, thunk);
  }
  child = thread_create(parent,(ptr_t *)thunk, 0);    /* zero indicates passing one actual thunk */
  check("Spawnmid",sth);

  if (collector_type != SemispaceParallel &&
      collector_type != GenerationalParallel &&
      collector_type != SemispaceConcurrent &&
      collector_type != GenerationalConcurrent) {
    printf("!!! Spawn called in a sequential collector\n");
    assert(0);
  }
  AddJob(child);
  if (diag)
    printf("Proc %d: user thread %d spawned user thread %d (status = %d)\n",
	   sth->stid,parent->tid,child->tid,child->status);
  check("Spawnend",sth);
  return parent;
}

/* This should not be called by the mutator directly.  
   Rather start_client returns here after swithcing to system thread stack. */
void Finish()
{
  SysThread_t *sth = getSysThread();
  Thread_t *th = sth->userThread;
  assert(((int)sth->stack - (int)(&sth)) < 1024); /* THis function's frame should not be more than 1K. */
  check("Finishstart",sth);
  if (diag) printf("Proc %d: finished user thread %d\n",sth->stid,th->tid);
  /*
  gc_finish();
  */
  DeleteJob(sth);
  check("Finishend",sth);
  work(sth);
  assert(0);
}

/* Mutator calls Yield which is defined in the service_platform_asm.s assembly file */
Thread_t *YieldRest()
{
  SysThread_t *sth = getSysThread();
  check("YieldReststart",sth);
  sth->userThread->request = YieldRequest;  /* Record why this thread pre-empted */
  sth->userThread->saveregs[RESULT] = 256;  /* ML representation of unit */
  ReleaseJob(sth);
  check("YieldRestend",sth);
  work(sth);
  assert(0);
  return 0;
}

/* Should be called from the timer handler.  Causes the current user thread to GC soon. */ 
void Interrupt(struct ucontext *uctxt)
{
  Thread_t *th = getThread();
  if (!th->notInML)
    {
      mem_t pc = GetPc(uctxt);
      SetIReg(uctxt, ALLOCLIMIT, (reg_t) StopHeapLimit);
      printf("      setting heap limit to %d while at %d\n",StopHeapLimit, pc);
    }
  return;
}

/* Releases current user thread if mapped */
void schedulerRest(SysThread_t *sth)
{
  SysThread_t *self = getSysThread();
  Thread_t *th = self->userThread;
  assert((self->stack - (int) (&self)) < 1024); /* Check that we are running on own stack */
  assert(sth == self);
  if (th != NULL)
    ReleaseJob(sth);
  work(sth);
  assert(0);
}



