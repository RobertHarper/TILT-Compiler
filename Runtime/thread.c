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



Thread_t    *Threads;                  /* array of NumUserThread user threads */
static SysThread_t *SysThreads;               /* array of NumSystemThread system threads */

static int totalThread = 0;                   /* Number of create user threads */
static int maxThread = 0;                     /* Maximum number of threads in work queue */

static Thread_t **JobQueue;                   /* Work list for user threads: array of pointers to user threads */
static pthread_cond_t EmptyCond;              /* Signals no more jobs */
static pthread_mutex_t EmptyLock;             /*   Lock associated with EmptyCond */
static int topThread = 0;                     /* points just beyond last job on work list */
static int curThread = 0;                     /* cursor for iterating over all jobs */
static const int ThreadDone = -1;
static const int ThreadNew = -1;

extern void start_client(Thread_t *, value_t *, int);
extern void context_restore(Thread_t *);

/* ------------------ Manipulating the job queue ----------------- */
static int local_lock = 0;

void LocalLock(void)
{
  while (!TestAndSet(&local_lock)) /* No need to flush */
    ;
  assert(local_lock == 1);
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

  assert(th->saveregs[ALLOCPTR] <= th->saveregs[ALLOCLIMIT]);
  sth->alloc = th->saveregs[ALLOCPTR];
  if (sth->limit != th->saveregs[ALLOCLIMIT]) {
    printf("sth->limit = %d\n",sth->limit);
    printf("th->saveregs[LIMIT] = %d\n",th->saveregs[ALLOCLIMIT]);
    assert(0);
  }

  if (diag)
    printf("Proc %d: unmapping with %d (< %d)\n",sth->stid,sth->alloc,th->saveregs[ALLOCLIMIT]);
  th->saveregs[ALLOCPTR] = 0;
  th->saveregs[ALLOCLIMIT] = 0;
  sth->userThread = NULL;
  th->sysThread = NULL;
  flushStore();                          /* make sure thread info is flushed to memory */
  FetchAndAdd(&(th->status),-1);         /* Release after flush; note that FA always goes to mem */
  if (diag)
    printf("Thread %d released; status = %d\n",th->tid,th->status);
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
	assert(th->parent >= 0); /* Parent must not have already finished */
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



/* --------------------- Helpers ---------------------- */

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
  th->request = 0;
  th->maxSP = 0;
  th->last_snapshot = -1;
  th->snapshots = (StackSnapshot_t *)malloc(NUM_STACK_STUB * sizeof(StackSnapshot_t));
  for (i=0; i<NUM_STACK_STUB; i++)
    {
      th->snapshots[i].saved_ra = (value_t) stub_error;
      th->snapshots[i].saved_sp = (value_t) 0;
      th->snapshots[i].saved_regstate = 0;
      th->snapshots[i].roots = (i == 0) ? QueueCreate(0,50) : NULL;
    }
  th->saveregs[THREADPTR] = (long) th;
  th->saveregs[ALLOCLIMIT] = 0;
  th->retadd_queue = QueueCreate(0,2000);
  th->stackchain = NULL;
  th->reg_roots = QueueCreate(0,32);
  th->root_lists = QueueCreate(0,200);
  th->loc_roots = QueueCreate(0,10);
  th->thunks = (value_t *)69;
}

void resetThread(Thread_t *th, Thread_t *parent, value_t *thunks, int numThunk)
{
  th->tid = FetchAndAdd(&totalThread,1);
  th->parent = parent;
  th->request = 0;
  th->last_snapshot = -1;
  th->saveregs[THREADPTR] = (long) th;
  th->saveregs[ALLOCLIMIT] = 0;
  if (numThunk == 0) { 
    /* thunks actually is a thunk */
    th->thunks = &(th->oneThunk);
    th->oneThunk = (value_t) thunks;
    th->nextThunk = 0;
    th->numThunk = 1;
  }
  else 
    { th->thunks = thunks;
      th->oneThunk = 0;
      th->nextThunk = 0;
      th->numThunk = numThunk;
    }
  if (th->stackchain == NULL)
    th->stackchain = StackChain_Alloc(); 
  QueueClear(th->snapshots[0].roots);
  QueueClear(th->retadd_queue);
  QueueClear(th->reg_roots);
  QueueClear(th->root_lists);
  QueueClear(th->loc_roots);
}

void thread_init()
{
  int i;
  Threads = (Thread_t *)malloc(sizeof(Thread_t) * NumThread);
  SysThreads = (SysThread_t *)malloc(sizeof(SysThread_t) * NumSysThread);
  JobQueue = (Thread_t **)malloc(sizeof(Thread_t *) * NumThread);
  for (i=0; i<NumThread; i++) 
    JobQueue[i] = NULL; 
  for (i=0; i<NumThread; i++)
    fillThread(&Threads[i], i);
  for (i=0; i<NumSysThread; i++) {
    char *temp;
    SysThreads[i].stid = i;
    SysThreads[i].alloc = StartHeapLimit;
    SysThreads[i].limit = StartHeapLimit;
    temp = malloc(20 * sizeof(char));
    sprintf(temp, "stacktime_%d", i);
    reset_timer(temp,&(SysThreads[i].stacktime));
    temp = malloc(20 * sizeof(char));
    sprintf(temp, "gctime_%d", i);
    reset_timer(temp,&(SysThreads[i].gctime));
    temp = malloc(20 * sizeof(char));
    sprintf(temp, "majorgcime_%d", i);
    reset_timer(temp,&(SysThreads[i].majorgctime));
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

/* If num_add is zero, then start_adds is a thunk */
Thread_t *thread_create(Thread_t *parent, value_t *thunks, int numThunk)
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


void work(SysThread_t *sth)
{
  Thread_t *th = sth->userThread;

  /* System thread should not be mapped */
  assert(th == NULL);

  if (diag)
    printf("Proc %d: entered work\n", sth->stid);

  /* Wait for next user thread and remove from queue. Map system thread. */
  while (th == NULL) {
    if (NumReadyJob() == 0)
#ifdef alpha_osf
      sched_yield();
#endif
#ifdef solaris
      thr_yield();
#endif
    /* We _might_ have some work now */
    th = FetchJob();
    if (th != NULL) {
      sth->userThread = th;
      th->sysThread = sth;
    }
    else
      poll();
  }

  if (th->status != 1)
    printf("Proc %d: thread %d  has status = %d\n", sth->stid,th->tid,th->status);
  assert(th->status == 1); /* FetchJob increments status from 0 to 1 */
  if (th->nextThunk == 0)  /* Starting thread for the first time */
    {
      value_t stack_top = th->stackchain->stacks[0]->top;
      th->saveregs[THREADPTR] = (long)th;
      th->saveregs[SP] = (long)stack_top - 64; /* Get some initial room so gdb won't freak */
      th->saveregs[ALLOCPTR] = sth->alloc;
      th->saveregs[ALLOCLIMIT] = sth->limit;

      if (diag) {
	printf("Proc %d: starting user thread %d (%d) with %d <= %d\n",
	       sth->stid, th->tid, th->id, 
	       th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT]);
	assert(th->saveregs[ALLOCPTR] <= th->saveregs[ALLOCLIMIT]);
      }

      assert(th->thunks[0] >= 100000);
      start_client(th,(value_t *)th->thunks, th->numThunk);

      assert(0);
    }
  else  /* Thread not starting for first time */
    {
      int request = th->request;
      value_t stack_top = th->stackchain->stacks[0]->top;
      if (request == -1) { /* Yield put us here */
#ifdef solaris
	th->saveregs[16] = (long) th;  /* load_regs_forC on solairs expects thread pointer in %l0 */
#endif	
      }
      else if (request >= 0) { /* GC request */
	if (request + sth->alloc >= sth->limit) {
	  if (diag)
	    printf("Proc %d: cannot resume user thread %d; need %d, only have %d; calling GC\n",
		 sth->processor, th->tid, request, sth->limit - sth->alloc);
	  gc(th); /* this call will not return if real gc */
	  if (!(request + sth->alloc < sth->limit))
	    printf("request = %d  alloc = %d  limit = %d\n",
		   request, sth->alloc, sth->limit);
	  assert(request + sth->alloc < sth->limit);
	}
      }
      else {
	printf("Odd request %d\n",request);
	assert(0);
      }
      th->saveregs[ALLOCPTR] = sth->alloc;
      th->saveregs[ALLOCLIMIT] = sth->limit;
      if (diag)
	printf("Proc %d: resuming user thread %d (%d) with request = %d  and %d < %d\n",
	       sth->processor, th->tid,th->id,
	       request, th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT]);
      context_restore(th);
    }

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
}

void thread_go(value_t *thunks, int numThunk)
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
}


/* ------------------ Mutator interface ----------------- */
int showInt(value_t v)
{
  /*  printf("%d",v); */
  return 256; /* ML unit */
}

int showIntRef(value_t v)
{
  /*  printf("%d",v); */
  return 256; /* ML unit */
}

int threadID()
{
  return getSysThread()->userThread->tid;
}

void Spawn(value_t thunk)
{
  SysThread_t *sth = getSysThread();
  Thread_t *parent = sth->userThread;
  Thread_t *child = NULL;

  assert(parent->sysThread == sth);
  child = thread_create(parent,(value_t *)thunk, 0);    /* zero indicates passing one actual thunk */

  if (collector_type != Parallel) {
    printf("!!! Spawn called in a sequential collector\n");
    assert(0);
  }
  AddJob(child);
  if (diag)
    printf("Proc %d: user thread %d spawned user thread %d (status = %d)\n",
	   sth->stid,parent->tid,child->tid,child->status);
}

void Finish()
{
  SysThread_t *sth = getSysThread();
  Thread_t *th = sth->userThread;
  if (diag) printf("Proc %d: finished user thread %d\n",sth->stid,th->tid);
  gc_finish();
  stats_finish_thread(&sth->stacktime,&sth->gctime,&sth->majorgctime);
  DeleteJob(sth);
  work(sth);
  assert(0);
}

/* Mutator calls Yield which is defined in the service_platform_asm.s assembly file */
Thread_t *YieldRest()
{
  SysThread_t *sth = getSysThread();
  sth->userThread->request = -1;               /* Record why this thread pre-empted */
  sth->userThread->saveregs[RESULT] = 256; /* ML representation of unit */
  ReleaseJob(sth);
  work(sth);
  assert(0);
}

/* Should be called from the timer handler.  Causes the current user thread to GC soon. */ 
void Interrupt(struct ucontext *uctxt)
{
  Thread_t *th = getThread();
  if (!th->notInML)
    {
      long pc = GetPc(uctxt);
      SetIReg(uctxt, ALLOCLIMIT, StopHeapLimit);
      printf("      setting heap limit to %d while at %d\n",StopHeapLimit, pc);
    }
  return;
}

void scheduler()
{
  SysThread_t *sth = getSysThread();
  Thread_t *th = sth->userThread;
  if (th != NULL) {
    assert(th->status >= 1);
    ReleaseJob(sth);
  }
  work(sth);
  assert(0);
}


