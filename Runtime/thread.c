#include "general.h"
#include "tag.h"
#include "thread.h"
#include "stats.h"
#include "memobj.h"
#include <assert.h>
#include "queue.h"
#include <stdio.h>
#include "til-signal.h"
#include <pthread.h>

/* Little allocation area for data allocated by the runtime. */
char RuntimeGlobalData[RuntimeGlobalDataSize];

#define NumThread 100
#define NumSysThread 5

static const int ThreadInvalid = 0;
static const int ThreadStarting = 1;
static const int ThreadRunning = 2;
static const int ThreadDone = 3;

Thread_t Threads[NumThread];               /* User threads */
SysThread_t SysThreads[NumSysThread];      /* System threads */
static Queue_t *Thread_Queue;              /* User threads that need to be scheduled */

static pthread_mutex_t ScheduleLock;

static int usedSysThread;                  /* Number of system threads that are mapped to user threads */
static pthread_cond_t usedSysThreadCond;   /* Used to signal main thread for possible termination */
static pthread_mutex_t usedSysThreadMutex;   /* Associated with usdeSysThreadCond */

long NOTINML;

extern HeapObj_t *fromheap, *toheap;

int InScheduler = 0;



void thread_init()
{
  int i;
  for (i=0; i<NumThread; i++)
    Threads[i].status = ThreadInvalid;
  for (i=0; i<NumSysThread; i++)
    SysThreads[i].stid = i;
  Thread_Queue = QueueCreate(1,10);
  pthread_mutex_init(&ScheduleLock,NULL);
  pthread_mutex_init(&usedSysThreadMutex,NULL);
  pthread_cond_init(&usedSysThreadCond, NULL);
  usedSysThread = 0;
}

SysThread_t *getSysThread()
{
  int i;
  pthread_t sys = pthread_self();
  for (i=0; i<NumSysThread; i++) {
//    printf("pthread = %d, self = %d\n",SysThreads[i].pthread,sys);
    if (SysThreads[i].pthread == sys) {
//      printf("getSysThread: returning %d\n", &(SysThreads[i]));
      return &(SysThreads[i]);
    }
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

Thread_t *thread_create(value_t start_adds, int num_add)
{
  static int count = 1;
  int start = count;
  Thread_t *th = NULL;
  while (Threads[count].status != ThreadInvalid)
    {
      count = (count + 1) % NumThread;
      if (start == count)
	assert(FALSE);
    }
  th = &(Threads[count]);
  th->savedHeapLimit = 0L;
  th->stackchain = StackChainObj_Alloc();
  th->tid = count;
  th->status = ThreadStarting;
  th->start_address = start_adds;
  th->num_add = num_add;
  return th;
}



void thread_insert(Thread_t *th)
{
  printf("thread_insert with th->status = %d\n",th->status);
  Enqueue(Thread_Queue,th);
}


void spawn(value_t thunk)
{
  /* zero indicates closure */
  Thread_t *th = thread_create(thunk, 0);
  thread_insert(th);
}


/* Should be called from the timer handler.  Causes the current user thread to GC soon. */ 
void thread_scheduler(struct sigcontext *scp)
{
  static int curThread = 0;
  if (InScheduler)
    return;
  InScheduler = 1;
  if (!NOTINML)
    {
      long *the_pc = GetPc(scp);
      long *the_iregs = GetIRegs(scp);
      Thread_t *th = getThread();
      printf("      setting heap limit while at %d\n",*the_pc);
      the_iregs[ALLOCLIMIT_REG] = LowHeapLimit;
    }
  InScheduler = 0;
  return;
}

Thread_t *tid2thread(int i)
{
  return &(Threads[i]);
}

void thread_finish(long *saveregs)
{
  Thread_t *th = getThread();
  th->status = ThreadDone;
  printf("thread_finish\n");
  thread_scheduler_clean(saveregs,0);
}

void* systhread(void* addr)
{
  thread_scheduler_clean();
  return NULL;
}

void thread_go()
{
  int i;
  printf("**** thread_go *****\n");
  /* Create system threads that run off the user thread queue */
  for (i=0; i<NumSysThread; i++)
    pthread_create(&(SysThreads[i].pthread),NULL,systhread,(void *)i);
  /* Wait until the thread queue is empty AND there are no running system threads */
  while (1) {
    int done = 0;
    /* Don't busy wait */
    printf("MAIN: thread waiting for empty user thread queue\n");
    QueueWaitEmpty(Thread_Queue);
    printf("MAIN: waiting for usedsysthread = %d to drop to zero\n",usedSysThread);
    if (usedSysThread > 0)
      pthread_cond_wait(&usedSysThreadCond, &usedSysThreadMutex);
    /* It _might_ be time to quit now */
    printf("MAIN: checking for termination\n");
    pthread_mutex_lock(&ScheduleLock);
    if (QueueIsEmpty(Thread_Queue) && usedSysThread == 0)
      done = 1;
    pthread_mutex_unlock(&ScheduleLock);
    if (done)
      return;
  }
}

void thread_scheduler_clean()
{
  int i;
  SysThread_t *sth = getSysThread();
  Thread_t *th = sth->userThread;

  printf("systhread %d: before unmap, usedSysThread = %d\n",sth->stid,usedSysThread);

  InScheduler = 1;

  /* If this is a currently running user thread, unmap it. */
  if (th != NULL) {
    /* If user thread is not finished, put back onto work queue. */
    if (th->status == ThreadRunning)
      Enqueue(Thread_Queue,th);
    /* Unmap system thread */
    sth->userThread = NULL;
    /* Reduce the used thread count */
    printf("  systhread %d: acquiring schedule lock\n",sth->stid);
    pthread_mutex_lock(&ScheduleLock);
    printf("  systhread %d: acquired schedule lock; usedSysThread: %d --> %d\n", sth->stid,usedSysThread, usedSysThread-1);
    usedSysThread--;
    pthread_mutex_unlock(&ScheduleLock);
    printf("  systhread %d: released schedule lock\n", sth->stid);
  }

  /* Signal possible termination to main thread */
  printf(" systhread %d: after unmap, usedSysThread = %d",sth->stid,usedSysThread);
  if (usedSysThread == 0)
    pthread_cond_signal(&usedSysThreadCond);

  /* Wait for next user thread and remove from queue. Map system thread. */
  th = NULL;
  while (th == NULL) {
    printf("  systhread %d: waiting for non-empty queue\n",sth->stid);
    QueueWaitNonEmpty(Thread_Queue);
    printf("  systhread %d: queue non empty\n",sth->stid);
    /* We _might_ have some work now */
    pthread_mutex_lock(&ScheduleLock);
    printf("  systhread %d: acquired lock\n",sth->stid);
    if (!QueueIsEmpty(Thread_Queue)) {
      printf("  systhread %d: got a user thread\n",sth->stid);
      th = QueuePop(Thread_Queue);
      sth->userThread = th;
      printf("  systhread %d: found user thread %d; usedSysThread: %d --> %d\n", 
	     sth->stid, th->tid, usedSysThread, usedSysThread+1);
      usedSysThread++;
    }
    pthread_mutex_unlock(&ScheduleLock);
    printf("  systhread %d: released lock\n",sth->stid);
  }

  /* where should this go?
    {
      gcstat_finish(saveregs[ALLOCPTR_REG]);
      stats_finish();
      exit(-1);
    }
  */

  /* Thread has already been running */
  if (th->status == ThreadRunning)
    {
      th->saveregs[ALLOCLIMIT_REG] = th->savedHeapLimit;
      fprintf(stderr,"RESTORING %d\n",th->tid);
      InScheduler = 0;
      context_restore(th->saveregs);
    }
  /* Starting thread for the first time */
  else if (th->status == ThreadStarting)
    {
      value_t top;
      top = th->stackchain->stacks[0]->top;
      th->status = ThreadRunning;
      th->saveregs[THREADPTR_REG] = (long)th;
      th->saveregs[ALLOCPTR_REG] = fromheap->bottom;
      th->saveregs[ALLOCLIMIT_REG] = fromheap->top;
      th->savedHeapLimit = fromheap->top;
#ifdef DEBUG
      printf("top,bottom,top = %d %d %d\nstart_address, num_add = %d %d\n",
	     top,fromheap->bottom,fromheap->top,
	     th->start_address, th->num_add);
#endif
      InScheduler = 0;
      if (th->num_add == 0)
	start_client(top,fromheap->bottom,fromheap->top - 128,
		     &(th->start_address), 1,th);
      else
	start_client(top,fromheap->bottom,fromheap->top - 128,
		     th->start_address, th->num_add,th);
      assert(FALSE);
    }
  else {
    printf("Found thread %d in thread queue whose status is not Starting or Running = %d\n", 
	   th->tid, th->status);
  }
  InScheduler = 0;
  return;
}




