#include "general.h"
#include "tag.h"
#include "thread.h"
#include "stats.h"
#include "memobj.h"
#include <assert.h>
#include <stdio.h>
#include "til-signal.h"
#include <pthread.h>
#include "gc.h"



static Thread_t    *Threads;                  /* array of NumUserThread user threads */
static SysThread_t *SysThreads;               /* array of NumSystemThread system threads */

static int totalThread = 0;                   /* Number of create user threads */
static int maxThread = 0;                     /* Maximum number of threads in work queue */

static Thread_t **JobQueue;                   /* Work list for user threads: array of pointers to user threads */
static pthread_cond_t EmptyCond;              /* Signals no more jobs */
static pthread_mutex_t EmptyLock;             /*   Lock associated with EmptyCond */
static int topThread = 0;                     /* points just beyond last job on work list */
static int curThread = 0;                     /* cursor for iterating over all jobs */
static const int ThreadDone = -1;

extern void start_client(Thread_t *, value_t *, int);
extern void context_restore(Thread_t *);

/* ------------------ Manipulating the job queue ----------------- */
static int local_lock = 0;

void LocalLock(void)
{
  while (!TestAndSet(&local_lock))
    ;
}

void LocalUnlock(void)
{
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

  assert(th->saveregs[ALLOCPTR_REG] <= th->saveregs[ALLOCLIMIT_REG]);
  sth->alloc = th->saveregs[ALLOCPTR_REG];
  if (sth->limit != th->saveregs[ALLOCLIMIT_REG]) {
    printf("sth->limit = %d",sth->limit);
    printf("th->saveregs[LIMIT] = %d",th->saveregs[ALLOCLIMIT_REG]);
  }
  assert(sth->limit == th->saveregs[ALLOCLIMIT_REG]);
  if (diag)
    printf("SysThread %d: unmapping with %d (< %d)\n",sth->stid,sth->alloc,th->saveregs[ALLOCLIMIT_REG]);
  if (paranoid)
    for (i=0; i<4; i++) {
      int *addr = ((int *)sth->alloc) + i;
      int v = *addr;
      if (v != 0)
	printf("   unmap WARNING: *%d = %d\n",addr,v);
    }
  th->saveregs[ALLOCPTR_REG] = 0;
  th->saveregs[ALLOCLIMIT_REG] = 0;
  FetchAndAdd(&(th->status),-1);
  sth->userThread = NULL;
  th->sysThread = NULL;
}


void DeleteJob(SysThread_t *sth)
{
  int i, j;
  Thread_t *th = sth->userThread;
  FetchAndAdd(&(th->status),1);  /* We increment status so it doesn't get scheduled when released */
  ReleaseJob(sth);
  LocalLock();
  for (i=0; i<NumThread; i++) 
    if (JobQueue[i] == th) {
      if (th->parent)
	FetchAndAdd(&(th->parent->status),-1);
      for (j=i; j<NumThread-1; j++)
	JobQueue[j] = JobQueue[j+1];
      topThread--;
      /*      printf("topThread = %d.  Signalling EmptyCond.\n", topThread); */
      pthread_cond_signal(&EmptyCond);
      LocalUnlock();
      th->status = ThreadDone;  /* Now, we put it back in the free pool */
      return;
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



void thread_init()
{
  int i;
  Threads = (Thread_t *)malloc(sizeof(Thread_t) * NumThread);
  SysThreads = (SysThread_t *)malloc(sizeof(SysThread_t) * NumSysThread);
  JobQueue = (Thread_t **)malloc(sizeof(Thread_t *) * NumThread);
  for (i=0; i<NumThread; i++) 
    JobQueue[i] = NULL; 
  for (i=0; i<NumThread; i++)
    {
      Threads[i].status = ThreadDone;
      Threads[i].id = i;
      Threads[i].tid = -1;
      reset_timer("stacktime",&(Threads[i].stacktime));
      reset_timer("gctime",&(Threads[i].gctime));
      reset_timer("majorgctime",&(Threads[i].majorgctime));
    }
  for (i=0; i<NumSysThread; i++) {
    SysThreads[i].stid = i;
    SysThreads[i].alloc = StartHeapLimit;
    SysThreads[i].limit = StartHeapLimit;
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
Thread_t *thread_create(Thread_t *parent, value_t start_adds, int num_add)
{
  int i;
  Thread_t *th = NULL;

  for (i=0; i<NumThread; i++) {
    Thread_t *temp = &(Threads[i]);
    if (temp->status == ThreadDone) {
      th = temp;
      th->status = 0;
      break;
    }
  }
  if (th == NULL)
    printf("Work list has %d threads\n",NumTotalJob());
  assert(th != NULL);
  assert(th->status == 0);

  th->parent = parent;
  th->maxSP = 0;
  th->snapshots = (StackSnapshot_t *)malloc(NUM_STACK_STUB * sizeof(StackSnapshot_t));
  th->last_snapshot = -1;
  for (i=0; i<NUM_STACK_STUB; i++)
    {
      th->snapshots[i].saved_ra = (value_t) stub_error;
      th->snapshots[i].saved_sp = (value_t) 0;
      th->snapshots[i].saved_regstate = 0;
      th->snapshots[i].roots = (i == 0) ? QueueCreate(0,50) : NULL;
    }
  th->saveregs[ALLOCLIMIT_REG] = 0;
  th->retadd_queue = QueueCreate(0,2000);
  th->tid = totalThread++;
  if (th->stackchain == NULL)
    th->stackchain = StackChain_Alloc();
  if (num_add == 0) {
    th->thunks = &(th->oneThunk);
    th->oneThunk[0] = start_adds;
    th->nextThunk = 0;
    th->numThunk = 1;
  }
  else 
    { th->thunks = start_adds;
      th->oneThunk[0] = 0;
      th->nextThunk = 0;
      th->numThunk = num_add;
    }
  th->reg_roots = QueueCreate(0,32);
  th->root_lists = QueueCreate(0,200);
  th->loc_roots = QueueCreate(0,10);
  return th;
}



void work(SysThread_t *sth)
{
  Thread_t *th = sth->userThread;

  /* System thread should not be mapped */
  assert(th == NULL);

  if (diag)
    printf("SysThread %d: entered work\n", sth->stid);

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


  assert(th->status == 1);
  if (th->nextThunk == 0)  /* Starting thread for the first time */
    {
      value_t stack_top = th->stackchain->stacks[0]->top;
      th->saveregs[THREADPTR_REG] = (long)th;
      th->saveregs[SP_REG] = (long)stack_top - 64; /* Get some initial room so gdb won't freak */
      th->saveregs[ALLOCPTR_REG] = sth->alloc;
      th->saveregs[ALLOCLIMIT_REG] = sth->limit;

      if (diag)
	printf("SysThread %d: starting user thread %d (%d) with %d < %d\n",
	     sth->stid, th->tid, th->id, 
	     th->saveregs[ALLOCPTR_REG], th->saveregs[ALLOCLIMIT_REG]);

      printf ("alloc_ptr = %d\n", sth->alloc);
      printf ("limit_ptr = %d\n", sth->limit);
      th->nextThunk = 1;
      start_client(th,(value_t *)th->thunks, th->numThunk);

      assert(0);
    }
  else  /* Thread not starting for first time */
    {
      int request = th->saveregs[ASMTMP_REG] - th->saveregs[ALLOCPTR_REG];
      value_t stack_top = th->stackchain->stacks[0]->top;
      if (request + sth->alloc >= sth->limit) {
	gc(th); /* this call will not return if real gc */
	if (!(request + sth->alloc < sth->limit))
	  printf("request = %d  alloc = %d  limit = %d\n",
		 request, sth->alloc, sth->limit);
	assert(request + sth->alloc < sth->limit);
      }
      th->saveregs[ALLOCPTR_REG] = sth->alloc;
      th->saveregs[ALLOCLIMIT_REG] = sth->limit;
      if (diag)
	printf("SysThread %d: resuming user thread %d (%d) with %d < %d\n",
	       sth->stid, th->tid,th->id,
	       th->saveregs[ALLOCPTR_REG], th->saveregs[ALLOCLIMIT_REG]);
      context_restore(th);
    }

  assert(0);
}


static void* systhread_go(void* addr)
{
  SysThread_t *st = getSysThread();
  st->stack = (int)(&st) & (~255);
  work(st);
  assert(0);
}

void thread_go(value_t start_adds, int num_add)
{
  int i;
  Thread_t *th = thread_create(0,start_adds,num_add);
  AddJob(th);

  /* Create system threads that run off the user thread queue */
  for (i=0; i<NumSysThread; i++)
    pthread_create(&(SysThreads[i].pthread),NULL,systhread_go,(void *)i);
  /* Wait until the work stack is empty;  work stack contains running jobs too */
  while ((i = NumTotalJob()) > 0) {
      printf("Main thread found %d jobs.\n", i);
      pthread_cond_wait(&EmptyCond,&EmptyLock);
  }
}


/* ------------------ Mutator interface ----------------- */
void Spawn(value_t thunk)
{
  SysThread_t *sth = getSysThread();
  Thread_t *parent = sth->userThread;
  Thread_t *child = thread_create(parent,thunk, 0);    /* zero indicates closure */

  if (collector_type != Parallel) {
    printf("!!! Spawn called in a sequential collector\n");
    assert(0);
  }
  AddJob(child);
  if (diag)
    printf("SysThread %d: user thread %d spawned user thread %d (status = %d)\n",
	   sth->stid,parent->tid,child->tid,child->status);
}

void Finish()
{
  SysThread_t *sth = getSysThread();
  Thread_t *th = sth->userThread;
  if (diag) printf("SysThread %d: finished user thread %d\n",sth->stid,th->tid);
  gc_finish();
  stats_finish_thread(&th->stacktime,&th->gctime,&th->majorgctime);
  DeleteJob(sth);
  work(sth);
}

Thread_t *YieldRest()
{
  SysThread_t *sth = getSysThread();
  ReleaseJob(sth);
  work(sth);
  return sth->userThread;
}

/* Should be called from the timer handler.  Causes the current user thread to GC soon. */ 
void Interrupt(struct ucontext *uctxt)
{
  Thread_t *th = getThread();
  if (!th->notInML)
    {
      long pc = GetPc(uctxt);
      SetIReg(uctxt, ALLOCLIMIT_REG, StopHeapLimit);
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
}


