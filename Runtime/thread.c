#include "tag.h"

#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif

#include "thread.h"
#include "stats.h"
#include "memobj.h"
#include <assert.h>
#include "queue.h"
#include <stdio.h>
#include "til-signal.h"

void thread_scheduler(struct sigcontext *scp);
void thread_scheduler_clean(long *saveregs, long __sp, value_t ret_add);


#define NumThreadObj 100
ThreadObj_t Threads[NumThreadObj];

double FPTOFROMINT;
long NOTINML;
extern long CurHeapLimit;
extern HeapObj_t *fromheap, *toheap;
int InScheduler = 0;
static int SavedHeapLimit = 0;
static Queue_t *Thread_Queue;
static ThreadObj_t *CurrentThread = NULL;

void thread_init()
{
  int i;
  for (i=0; i<NumThreadObj; i++)
    Threads[i].valid = 0;
  Thread_Queue = QueueCreate(10);
}

ThreadObj_t *thread_create(value_t start_adds, int num_add)
{
  static int count = 0;
  int start = count;
  ThreadObj_t *th = NULL;
  while (Threads[count].valid)
    {
      count = (count + 1) % NumThreadObj;
      if (start == count)
	assert(FALSE);
    }
  th = &(Threads[count]);
  th->running = 0;
  th->start_address = start_adds;
  th->num_add = num_add;
  th->valid = 1;
  th->tid = count;
  th->stackchain = StackChainObj_Alloc();
  th->done = 0;
  return th;
}

void thread_destroy(ThreadObj_t *th)
{
  th->valid = 0;
}

void thread_insert(ThreadObj_t *th)
{
  Enqueue(Thread_Queue,th);
}


void thread_spawn(ThreadObj_t *th)
{
}


/* should be called from the timer handler 
   all this does is to cause a timer-induced GC soon */
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
      printf("      setting heap limit while at %d\n",*the_pc);
      SavedHeapLimit = the_iregs[ALLOCLIMIT_REG];  
      the_iregs[ALLOCLIMIT_REG] = LowHeapLimit;  
      CurHeapLimit = LowHeapLimit;
    }
  InScheduler = 0;
  return;
}

ThreadObj_t *tid2thread(int i)
{
  return &(Threads[i]);
}

void thread_finish(long *saveregs)
{
  CurrentThread->done = 1;
  thread_scheduler_clean(saveregs,0,0);
}

void thread_go()
{
  long dummy[32];
  thread_scheduler_clean(dummy,0,0);
}

void thread_scheduler_clean(long *saveregs, long __sp, value_t ret_add)
{
  int i;
  ThreadObj_t *oldth = CurrentThread;
  ThreadObj_t *newth = NULL;

  InScheduler = 1;

  if (oldth != NULL)
    {
      for (i=0; i<31; i++)
	{
	  oldth->saveregs[i] = saveregs[i];
	  oldth->ret_add = ret_add;
	}
    }

  if (oldth != NULL && !oldth->done)
    Enqueue(Thread_Queue,oldth);
  if (!IsEmpty(Thread_Queue))
    newth = Dequeue(Thread_Queue);
  else
    {
#ifdef DEBUG
      fprintf(stderr,"Empty Thread_Queue...exiting\n");
#endif
      gcstat_finish(saveregs[ALLOCPTR_REG]);
      stats_finish();
      exit(-1);
    }
  if (newth == NULL || newth->done)
    {
      printf("ERROR: newth is null or done thread in queue...exiting\n");
      exit(-1);
    }

  saveregs = (long *)(newth->saveregs);
  ret_add = newth->ret_add;

  saveregs[ALLOCLIMIT_REG] = SavedHeapLimit;
  SavedHeapLimit = 0;
#ifdef DEBUG
  fprintf(stderr,"      thread_scheduler_clean restoring sp=%d ret_add=%d  ",
	 saveregs[30],ret_add);
#endif
  if (newth->running)
    {
      fprintf(stderr,"RESTORING %d\n",newth->tid);
      InScheduler = 0;
      CurrentThread = newth;
      context_restore(saveregs,ret_add);
    }
  else
    {
      value_t top;
      top = newth->stackchain->stacks[0]->top;
#ifdef DEBUG
      fprintf(stderr,"STARTING %d...\n",newth->tid);
      fprintf(stderr,"top is %d %d %d\n",top,fromheap->bottom,fromheap->top);
#endif
      newth->running = 1;
      InScheduler = 0;
      {
	value_t top = newth->stackchain->stacks[0]->top;
	newth->running = 1;
	CurrentThread = newth;
/* XXX this is not right */
	CurHeapLimit = fromheap->top;
#ifdef DEBUG
	printf("top,bottom,top = %d %d %d\nstart_address, num_add = %d %d\n",
	       top,fromheap->bottom,fromheap->top,
	       newth->start_address, newth->num_add);
#endif
	start_client(top,fromheap->bottom,fromheap->top - 128,
		     newth->start_address, newth->num_add);
      }
      assert(FALSE);
    }
  InScheduler = 0;
  return;
}
