#include "general.h"
#include "tag.h"
#include "thread.h"
#include "stack.h"
#include "stats.h"
#include "memobj.h"
#include <assert.h>
#include <stdio.h>
#include "til-signal.h"
#include <sys/types.h>
#include <sys/procset.h>
#include <sys/processor.h>
#include <unistd.h>
#ifdef solaris
#include <thread.h>
#endif
#include <pthread.h>
#include "gc.h"
#include "forward.h"
#include "platform.h"

int    threadDiag = 0;

Thread_t    *Threads;                         /* array of NumUserThread user threads */
static Proc_t *Procs;                         /* array of NumSystemThread system threads */

static long totalThread = 0;                  /* Number of create user threads */
static long maxThread = 0;                    /* Maximum number of threads in work queue */

static Thread_t **JobQueue;                   /* Work list for user threads: array of pointers to user threads */
static long NumActiveProc = 0;                 /* Number of processor threads active */
static int EmptyPlain = 0;                    /* Normal shared variable set to true when there are no more jobs */
static pthread_cond_t EmptyCond;              /* Signals no more jobs */
static pthread_mutex_t EmptyLock;             /* Lock associated with EmptyCond */
static int activeThread = 0;                  /* number of threads in work list */
static int curThread = 0;                     /* cursor for iterating over all jobs */

extern void start_client(Thread_t *);
extern void context_restore(Thread_t *);

Thread_t *mainThread = NULL;


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
  return activeThread;
}

int NumReadyJob(void)
{
  int i, count = 0;
  LocalLock();
  for (i=0; i<NumThread; i++) {
    Thread_t *th = JobQueue[i];
    if (th != NULL && th->status >= 0) 
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
  int i, j;
  LocalLock();
  th->status = 0;
  if (th->parent) 
    FetchAndAdd(&th->parent->status,1);
  for (i=0; i<NumThread; i++)
    if (JobQueue[i] == NULL) {
      JobQueue[i] = th;
      break;
    }
  assert(i < NumThread);
  activeThread++;
  maxThread = (activeThread > maxThread) ? activeThread : maxThread;
  flushStore();
  LocalUnlock();
}

Thread_t *FetchJob(void)
{
  int i;
  LocalLock();
  for (i=NumThread-1; i>=0; i--) {
    Thread_t *th = JobQueue[i];
    if (th != NULL && th->status == 0) {
      FetchAndAdd(&th->status,1);
      LocalUnlock();
      return th;
    }
  }
  LocalUnlock();
  return NULL;
}

void ReleaseJob(Proc_t *proc)
{
  int i;
  Thread_t *th = proc->userThread;
  Stacklet_t *stacklet;
  mem_t proc_allocCursor = proc->allocCursor;
  mem_t proc_allocLimit = proc->allocLimit;
  mem_t proc_writelistCursor = (mem_t) proc->writelistCursor;

  procChangeState(proc, Scheduler);

  /* Check that thread's allocation pointers are consistent and update processor's version */
  if (((mem_t) th->saveregs[ALLOCLIMIT] != proc_allocLimit &&
       (mem_t) th->saveregs[ALLOCLIMIT] != StopHeapLimit) ||
      (mem_t) th->saveregs[ALLOCPTR] > proc_allocLimit) {
    printf("proc->allocCursor = %d\n",proc_allocCursor);
    printf("proc->allocLimit = %d\n",proc_allocLimit);
    printf("th->saveregs[ALLOC] = %d\n",th->saveregs[ALLOCPTR]);
    printf("th->saveregs[LIMIT] = %d\n",th->saveregs[ALLOCLIMIT]);
    assert(0);
  }

  if (threadDiag)
    printf("Proc %d: Releasing thread %d.   Request = %d.\n"
	   "         Allocated %d to %d.    Writlist %d to %d.\n",
	   proc->procid,th->tid,th->requestInfo, 
	   proc_allocCursor, th->saveregs[ALLOCPTR],
	   proc_writelistCursor, th->writelistAlloc);
  
  /* Update processor's version of allocation range and write list and process */
  proc->allocCursor = (mem_t) th->saveregs[ALLOCPTR];
  proc->writelistCursor = th->writelistAlloc;
  GCRelease(proc);
  procChangeState(proc, Scheduler);

  /* Null out thread's version of allocation and write-list */
  stacklet = EstablishStacklet(th->stack, (mem_t) th->saveregs[SP]); /* updates stacklet->cursor */
#ifdef solaris
  stacklet->retadd = (mem_t) th->saveregs[LINK];
#else
  assert(0);
#endif
  th->saveregs[ALLOCPTR] = 0;
  th->saveregs[ALLOCLIMIT] = 0;
  th->writelistAlloc = 0;
  th->writelistLimit = 0;

  /* Break association between thread and processor */
  proc->userThread = NULL;
  th->proc = NULL;
  
  flushStore();                          /* make sure thread info is flushed to memory */
  assert(th->status >= 1);               /* Thread was mapped and running so could not be ready or done */
  FetchAndAdd(&(th->status),-1);         /* Release after flush; note that FA always goes to mem */

}



/* Does not do locking to prevent multiple access.  Assumes caller does it. */
void Thread_Create(Thread_t *th, Thread_t *parent, ptr_t thunk)
{
  assert(th->used == 0);
  assert(th->status == -1);
  assert(!th->pinned);
  assert(&(Threads[th->id]) == th);
  th->used = 1;
  th->status = 0;
  th->tid = FetchAndAdd(&totalThread,1);
  th->parent = parent;
  th->request = StartRequest;
  th->saveregs[THREADPTR] = (long) th;
  th->saveregs[ALLOCLIMIT] = 0;
  th->globalOffset = 0;
  th->arrayOffset = 0;
  th->stackletOffset = 0;
  th->thunk = thunk;
  if (th->stack == NULL)
    th->stack = StackChain_Alloc(); 
  th->snapshot = NULL;
}

void Thread_Pin(Thread_t *th)
{
  assert(th->pinned == 0);
  th->pinned = 1;
}

/* Provisionally delete thread if unpinned and unused */
void Thread_Free(Thread_t *th)
{
  int i, j, done;
  LocalLock();
  for (i=0; i<NumThread; i++) {
    if (JobQueue[i] == th) {
      assert(th->status < 0 || !th->pinned);
      if ((th->status < 0) && !th->pinned) {
	JobQueue[i] = NULL;
	activeThread--;
	StackChain_Dealloc(th->stack);
	th->stack = NULL;
	if (th->snapshot != NULL)
	  StackChain_Dealloc(th->snapshot);
	th->snapshot = NULL;
	th->used = 0;
      }
      break;
    }
  }
  assert(i < NumThread);              /* Thread must have been in scheduler */
  done = activeThread == 0;
  LocalUnlock();
  if (done) {
    EmptyPlain = 1;                   /* Causes all worker processors to terminate */
    pthread_cond_signal(&EmptyCond);  /* Wake up main thread if there are no more jobs */
  }
}

void Thread_Unpin(Thread_t *th)
{
  th->pinned = 0;
  Thread_Free(th);
}

void DeleteJob(Proc_t *proc)
{
  int i, j;
  Thread_t *th = proc->userThread;
  assert(th->proc == proc);
  FetchAndAdd(&(th->status),1);  /* We increment status so it doesn't get scheduled when released */
  ReleaseJob(proc);
  if (th->parent) {
    FetchAndAdd(&(th->parent->status),-1);
    assert(th->parent->status >= 0); /* Parent must not have already finished */
  }
  assert(th->status == 1);
  th->status = -1; 
  Thread_Free(th); 
  return;
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

Proc_t *getNthProc(int i)
{
  assert(i < NumProc);
  return &(Procs[i]);
}

Proc_t *getProc()
{
  int i;
  pthread_t sys = pthread_self();
  for (i=0; i<NumProc; i++) {
    if (Procs[i].pthread == sys) 
      return &(Procs[i]);
  }
  return NULL;
}


Thread_t *getThread()
{
  Proc_t *proc = getProc();
  if (proc == NULL) 
    return NULL;
  return proc->userThread;
}

void resetUsage(Usage_t *u)
{
  u->bytesAllocated = 0;
  u->fieldsCopied = 0;
  u->fieldsScanned = 0;
  u->ptrFieldsScanned = 0;
  u->objsCopied = 0;
  u->objsScanned = 0;
  u->pagesTouched = 0 ;
  u->globalsProcessed = 0;
  u->stackSlotsProcessed = 0;
  u->workDone = 0;
  u->lastWorkDone = 0;
  u->counter = 0;
}

long updateWorkDone(Proc_t *proc)
{
  Usage_t *u = &proc->segUsage;
  u->workDone = (long) (u->fieldsCopied * fieldCopyWeight + 
			u->fieldsScanned * fieldScanWeight +
			u->ptrFieldsScanned * ptrFieldScanWeight +
			u->objsCopied * objCopyWeight + 
			u->objsScanned * objScanWeight +
			u->pagesTouched * pageWeight +
			u->globalsProcessed * globalWeight +
			u->stackSlotsProcessed * stackSlotWeight);
  return u->workDone;
}

static void attributeUsage(Usage_t *from, Usage_t *to)
{
  to->bytesAllocated += from->bytesAllocated;
  to->fieldsCopied += from->fieldsCopied;
  to->fieldsScanned += from->fieldsScanned;
  to->ptrFieldsScanned += from->ptrFieldsScanned;
  to->objsCopied += from->objsCopied;
  to->objsScanned += from->objsScanned;
  to->pagesTouched += from->pagesTouched;
  to->globalsProcessed += from->globalsProcessed;
  to->stackSlotsProcessed += from->stackSlotsProcessed;
  resetUsage(from);
}


void fillThread(Thread_t *th, int i)
{
  th->id = i;
  th->tid = -1;
  th->status = -1;
  th->used = 0;
  th->pinned = 0;
  th->parent = NULL;
  th->request = StartRequest;
  th->saveregs[THREADPTR] = (long) th;
  th->saveregs[ALLOCLIMIT] = 0;
  th->stack = NULL;
  th->snapshot = NULL;
  th->thunk = NULL;
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
  Procs = (Proc_t *)malloc(sizeof(Proc_t) * NumProc);
  JobQueue = (Thread_t **)malloc(sizeof(Thread_t *) * NumThread);
  for (i=0; i<NumThread; i++) 
    JobQueue[i] = NULL; 
  for (i=0; i<NumThread; i++)
    fillThread(&Threads[i], i);
  for (i=0; i<NumProc; i++) {
    Proc_t *proc = &(Procs[i]); /* Structures are by-value in C */
    proc->procid = i;
    allocStack(&proc->minorObjStack, 16384);
    allocStack(&proc->majorObjStack, 1024);
    allocStack(&proc->minorSegmentStack, 16384);
    allocStack(&proc->majorSegmentStack, 1024);
    allocStack(&proc->majorRegionStack, 1024);
    proc->allocStart = (mem_t) StartHeapLimit;
    proc->allocCursor = (mem_t) StartHeapLimit;
    proc->allocLimit = (mem_t) StartHeapLimit;
    proc->writelistStart = &(proc->writelist[0]);
    proc->writelistCursor = proc->writelistStart;
    proc->writelistEnd = &(proc->writelist[(sizeof(proc->writelist) / sizeof(ptr_t)) - 2]);
    for (j=0; j<(sizeof(proc->writelist) / sizeof(ptr_t)); j++)
      proc->writelist[j] = 0;
    proc->globalLocs = createStack(8192);
    proc->rootLocs = createStack(4096);
    proc->backObjs = createStack(1024);
    proc->backObjsTemp = createStack(1024);
    proc->backLocs = createStack(64 * 1024);
    proc->backLocsTemp = createStack(64 * 1024);
    allocStack(&proc->threads, 100);
    reset_timer(&(proc->totalTimer));
    reset_timer(&(proc->currentTimer));
    proc->state = Scheduler;
    proc->segmentNumber = 0;
    proc->gcSegment1 = NoWork;
    proc->gcSegment2 = Continue;
    proc->gcTime = 0;
    proc->schedulerTime = 0;
    resetUsage(&proc->segUsage);
    resetUsage(&proc->minorUsage);
    resetUsage(&proc->majorUsage);
    reset_statistic(&proc->bytesAllocatedStatistic);
    reset_statistic(&proc->bytesCopiedStatistic);
    reset_statistic(&proc->minorSurvivalStatistic);
    reset_statistic(&proc->heapSizeStatistic);
    reset_statistic(&proc->majorSurvivalStatistic);
    reset_statistic(&proc->schedulerStatistic);
    reset_histogram(&proc->mutatorHistogram);
    reset_statistic(&proc->gcNoneStatistic);
    reset_histogram(&proc->gcWorkHistogram);
    reset_histogram(&proc->gcMajorWorkHistogram);
    reset_histogram(&proc->gcFlipOffHistogram);
    reset_histogram(&proc->gcFlipOnHistogram);
    reset_statistic(&proc->gcStackStatistic);
    reset_statistic(&proc->gcGlobalStatistic);
    SetCopyRange(&proc->minorRange, proc, NULL, NULL, NULL, NULL, 0);
    SetCopyRange(&proc->majorRange, proc, NULL, NULL, NULL, NULL, 0);
    proc->numCopied = proc->numShared = proc->numContention = 0;
    proc->numSegment = 0;
    proc->numWrite = 0;
    proc->numRoot = 0;
    proc->numLocative = 0;
    proc->lastHashKey = 0;
    proc->lastHashData = NULL;
  }
  pthread_cond_init(&EmptyCond,NULL);
  pthread_mutex_init(&EmptyLock,NULL);
  setbuf(stdout,NULL);
  setbuf(stderr,NULL);
}

static char* state2str(ProcessorState_t procState)
{
  switch (procState) {
  case Scheduler: return "Scheduler";
  case Mutator: return "Mutator";
  case GC: return "GC";
  case GCStack: return "GCStack";
  case GCGlobal: return "GCGlboal";
  case GCWork: return "GCWork";
  default : return "unknownProcState";
  }
}

void procChangeState(Proc_t *proc, ProcessorState_t procState)
{
  /* Get the time spent since last here and attribute it */
  double diff = 0.0;
  if (proc->currentTimer.on) {
    restart_timer(&proc->currentTimer);
    diff = proc->currentTimer.last;
  }
  else
    start_timer(&proc->currentTimer);

  /* Accumulate time for all GC substates */
  if (proc->state != Scheduler && proc->state != Mutator && proc->state != Done)
    proc->gcTime += diff;

  switch (proc->state) {
  case Scheduler: {
    int flipOn = (proc->gcSegment2 == FlipOn);
    int flipOff = (proc->gcSegment2 == FlipOff || proc->gcSegment2 == FlipBoth);

    proc->schedulerTime += diff;
    if (procState == Mutator || procState == Done) {  /* Reset GC-related info once we enter mutator */
      /* First do statistics dependent on whether GC is major or minor */
      if (proc->gcSegment1 == MinorWork) {
	add_histogram(&proc->gcWorkHistogram, proc->gcTime);
	attributeUsage(&proc->segUsage, &proc->minorUsage);
	if (flipOff) {
	  add_statistic(&proc->bytesAllocatedStatistic, proc->minorUsage.bytesAllocated);
	  add_statistic(&proc->bytesCopiedStatistic, 4 * proc->minorUsage.fieldsCopied);
	  proc->majorUsage.bytesAllocated += 4 * proc->minorUsage.fieldsCopied;
	  resetUsage(&proc->minorUsage);
	}
      }
      else if (proc->gcSegment1 == MajorWork) {
	add_histogram(&proc->gcWorkHistogram, proc->gcTime);
	add_histogram(&proc->gcMajorWorkHistogram, proc->gcTime);
	attributeUsage(&proc->segUsage, &proc->majorUsage);
	if (flipOff) {
	  add_statistic(&proc->bytesCopiedStatistic, 4 * proc->majorUsage.fieldsCopied);
	  resetUsage(&proc->majorUsage);
	}
      }
      else {
	add_statistic(&proc->gcNoneStatistic, proc->gcTime);
	resetUsage(&proc->segUsage);
      }
      /* Add statistics related to flipping on/off collector - independent of whether GC is major or minor */
      if (flipOn) {
	add_histogram(&proc->gcFlipOnHistogram, proc->gcTime);     
	if (timeDiag)
	  printf("FlipOn  %5d:  %3.1f ms %s\n\n", proc->segmentNumber, proc->gcTime, proc->gcTime > 4.0 ? "******" :"");
      }
      else if (flipOff) {
	add_histogram(&proc->gcFlipOffHistogram, proc->gcTime);
	add_statistic(&proc->heapSizeStatistic, Heap_GetSize(fromSpace) / 1024);
	if (timeDiag)
	  printf("FlipOff %5d: %3.1f ms %s\n\n", proc->segmentNumber, proc->gcTime, proc->gcTime > 4.0 ? "******" :"");
      }
      add_statistic(&proc->schedulerStatistic, proc->schedulerTime);
      proc->segmentNumber++;
      proc->gcSegment1 = NoWork;
      proc->gcSegment2 = Continue;
      proc->gcTime = 0.0;
      proc->schedulerTime = 0.0;
      proc->numSegment++;
    }
    break;
  }
  case Mutator:
    assert(procState == Scheduler);
    add_histogram(&proc->mutatorHistogram, diff);
    break;
  case GC:
  case GCWork:
    assert(procState != Mutator);
    break;
  case GCStack:
    add_statistic(&proc->gcStackStatistic, diff);
    assert(procState != Mutator && procState != Scheduler);
    break;
  case GCGlobal:
    add_statistic(&proc->gcGlobalStatistic, diff);
    assert(procState != Mutator && procState != Scheduler);
    break;
  }
  proc->state = procState;
}

int thread_total() 
{
  return totalThread;
}
 
int thread_max() 
{
  return maxThread;
}

Thread_t *thread_create(Thread_t *parent, ptr_t thunk)
{
  int i;
  for (i=0; i<NumThread; i++) {
    Thread_t *th = &(Threads[i]);
    if (th->used == 0) {    /* Test first without locking */
      LocalLock();
      if (th->used == 0) {  /* Should still be free, most likely */
	Thread_Create(th, parent, thunk);
	LocalUnlock();
	return th;
      }
      LocalUnlock();
    }
  }
  printf("Work list has %d threads\n",NumTotalJob());
  printf("thread_create failed\n");
  assert(0);
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


static void mapThread(Proc_t *proc, Thread_t *th)
{
  Stacklet_t *stacklet = CurrentStacklet(th->stack);

  Stacklet_KillReplica(stacklet);
  assert(proc->userThread == NULL);
  assert(th->proc == NULL);
  proc->userThread = th;
  th->proc = proc;
  th->saveregs[ALLOCPTR] = (reg_t) proc->allocCursor;
  th->saveregs[ALLOCLIMIT] = (reg_t) proc->allocLimit;
  th->writelistAlloc = proc->writelistCursor;
  th->writelistLimit = proc->writelistEnd;
  th->globalOffset = primaryGlobalOffset;
  th->arrayOffset = primaryArrayOffset;
  th->stackletOffset = primaryStackletOffset;
  th->saveregs[THREADPTR] = (val_t) th;
  th->saveregs[SP] = (val_t) StackletPrimaryCursor(stacklet);
  th->stackLimit = StackletPrimaryBottom(stacklet);
}

/* Processor should not be mapped to any user thread */
static void work(Proc_t *proc)
{
  Thread_t *th = NULL;

  /* To get changes other processors have made */
  flushStore();

  /* Processor should not be mapped */
  assert(proc->userThread == NULL);

  /* Wait for next user thread and remove from queue. Map processor. */
  while (th == NULL) {
    if (NumReadyJob() == 0)
#if   defined(alpha_osf)
      sched_yield();
#elif defined(solaris)
      thr_yield();
#else
      assert(0);
#endif
    th = FetchJob();  /* Provisionally grab thread but don't map onto processor yet */
    if (th == NULL) {
      procChangeState(proc, Scheduler);
      GCPoll(proc);
      procChangeState(proc, Scheduler);
      procChangeState(proc, Mutator);    /* So that segment counter increases */
      procChangeState(proc, Scheduler);
    }
    if (EmptyPlain) {
      procChangeState(proc,Done);
      stop_timer(&proc->totalTimer);
      FetchAndAdd(&NumActiveProc, -1);
      if (diag)
	printf("Processor exiting\n");
#ifdef alpha_osf
      /* The Alpha-OSF does not like pthread_exit.  It complains with:
%DECthreads bugcheck (version V3.15-413), terminating execution.
% Reason:  Termination exception reached last chance handler in normal thread.
% Running on OSF1 V4.0 on AlphaStation 250 4/266, 96Mb; 1 CPUs
Abort
      */
      while (1)
	sched_yield();
#else
      pthread_exit(NULL); 
#endif
    }
  }

  assert(th->status == 1); /* FetchJob increments status from 0 to 1 */

  switch (th->request) {

    case NoRequest: 
      assert(0);
    case YieldRequest: {
#ifdef solaris
      th->saveregs[16] = (long) th;  /* load_regs_forC on solaris expects thread pointer in %l0 */
#endif	
      mapThread(proc,th);
      returnFromYield(th);
    }
    case StartRequest: {              /* Starting thread for the first time */
      assert(th->thunk != NULL);
      mapThread(proc,th);
      if (threadDiag) {
	printf("Proc %d: starting user thread %d (%d) with %d <= %d\n",
	       proc->procid, th->tid, th->id, th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT]);
	assert(th->saveregs[ALLOCPTR] <= th->saveregs[ALLOCLIMIT]);
      }
      flushStore();  /* make visible changes to other processors */
      procChangeState(proc, Mutator);
      start_client(th);
      assert(0);
    }

    case GCRequestFromML:
    case GCRequestFromC: 
    case MajorGCRequestFromC: {
      /* Allocate space or check write buffer to see if we have enough space */
      int satisfied = GCFromScheduler(proc, th);
      procChangeState(proc, Scheduler);
      while (!satisfied) {
	printf("Warning: Proc %d: could not resume thread %d after calling GCFromScheduler.  Retrying...\n",
	       proc->procid, th->tid);
	satisfied = GCFromScheduler(proc, th);
	procChangeState(proc, Scheduler);
      }
      /* Note that another processor can change th->saveregs[ALLOCLIMIT] to Stop at any point */
      if (th->requestInfo > 0)
	assert(th->requestInfo + (val_t) proc->allocCursor <= (val_t) proc->allocLimit);
      else if (th->requestInfo < 0)
	assert((-th->requestInfo) + (val_t) proc->writelistCursor <= (val_t) proc->writelistEnd);
      else 
	assert(0);
      mapThread(proc,th);
      if (th->request == GCRequestFromML) {
	if (threadDiag)
	  printf("Proc %d: Resuming thread %d from GCRequestFromML of %d bytes with allocation region %d < %d and writelist %d < %d\n",
		 proc->procid, th->tid, th->requestInfo, 
		 th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT],
		 th->writelistAlloc, th->writelistLimit);
	flushStore();  /* make visible changes to other processors */
	procChangeState(proc, Mutator);
	returnFromGCFromML(th);	
	assert(0);
      }
      else if (th->request == GCRequestFromC ||
	       th->request == MajorGCRequestFromC) {
	if (threadDiag)
	  printf("Proc %d: Resuming thread %d from Major/C GCRequestFromML of %d bytes with allocation region %d < %d and writelist %d < %d\n",
		 proc->procid, th->tid, th->requestInfo, proc->allocCursor, proc->allocLimit,
		 th->writelistAlloc, th->writelistLimit);
	flushStore();  /* make visible changes to other processors */
	procChangeState(proc, Mutator);
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


static void* proc_go(void* unused)
{
  Proc_t *proc = getProc();
#ifdef solaris
  int status = processor_bind(P_LWPID, P_MYID, proc->processor, NULL);
  if (status != 0)
    printf("processor_bind failed with %d\n",status);
#else
  if (threadDiag)
    printf("Cannot find processors on non-sparc: assuming uniprocessor\n");
#endif
#ifdef solaris
  initializePerfMon();
#endif
  install_signal_handlers(0);
  proc->stack = (int)(&proc) & (~255);
  FetchAndAdd(&NumActiveProc, 1);
  start_timer(&proc->totalTimer);
  start_timer(&proc->currentTimer);
  work(proc);
  assert(0);
  return 0;
}

void thread_go(ptr_t thunk)
{
  int curproc = -1;
  int i, status;

  mainThread = thread_create(NULL,thunk);
  AddJob(mainThread);

  /* Create system threads that run off the user thread queue */
  for (i=0; i<NumProc; i++) {
    pthread_attr_t attr;
    struct sched_param schedParam;
#ifdef solaris
    processor_info_t infop;
    while (1) {
      int status = processor_info(++curproc,&infop);
      if (status == 0 && infop.pi_state == P_ONLINE) {
	Procs[i].processor = curproc;
	break;
      }
      if (curproc > 1024) {
	printf("Cannot find enough online processors: curproc = %d\n",curproc);
	assert(0);
      }
    }
#else
    if (threadDiag)
      printf("Cannot find processors on non-sparc: assuming uniprocessor\n");
#endif
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr,256 * 1024);
    pthread_attr_setscope(&attr,PTHREAD_SCOPE_SYSTEM); 
    /* Only the SCHED_OTHER scheduling policy is supported on Solaris. */
    /* Scheduling priority seems to work with pthread_attr_setschedparam. */
    schedParam.sched_priority = 30;
    status = pthread_attr_setschedparam(&attr,&schedParam);
    if (status)
      printf("pthread_attr_setschedparam returned status = %d\n", status);
    pthread_create(&(Procs[i].pthread),&attr,proc_go,NULL);
    if (threadDiag)
      printf("Proc %d:  processor %d and pthread = %d\n",
	     Procs[i].procid, Procs[i].processor, Procs[i].pthread);
  }
  install_signal_handlers(1);
  /* Wait until the work stack is empty;  work stack contains running jobs too */
  while ((i = NumTotalJob()) > 0) {
    if (threadDiag)
      printf("Main thread found %d jobs.\n", i);
    pthread_cond_wait(&EmptyCond,&EmptyLock);
  }
  
  /* Wait until all the processors have stopped */
  while (NumActiveProc > 0) {
#if   defined(alpha_osf)
      sched_yield();
#elif defined(solaris)
      thr_yield();
#else
      assert(0);
#endif
  }

  if (diag)
    printf("Main thread returning\n");
}


/* ------------------ Mutator interface ----------------- */


Thread_t *SpawnRest(ptr_t thunk)
{
  Proc_t *proc = getProc();
  Thread_t *parent = proc->userThread;
  Thread_t *child = NULL;

  switch (collector_type) {
  case Semispace:
  case Generational:
    assert(NumProc == 1);
  case SemispaceParallel:
  case GenerationalParallel:
  case SemispaceConcurrent:
  case GenerationalConcurrent:
    break;
  default:
    assert(0);
  }

  assert(proc->stack - ((int) &proc) < 1024);   /* stack frame for this function should be < 1K */
  assert(parent->proc == proc);

  child = thread_create(parent,thunk);    /* zero indicates passing one actual thunk */
  AddJob(child);
  if (threadDiag)
    printf("Proc %d: user thread %d spawned user thread %d (status = %d)\n",
	   proc->procid,parent->tid,child->tid,child->status);
  return parent;
}

/* This should not be called by the mutator directly.  
   Rather start_client returns here after swithcing to system thread stack. */
void Finish()
{
  Proc_t *proc = getProc();
  Thread_t *th = proc->userThread;
  assert(((int)proc->stack - (int)(&proc)) < 1024); /* THis function's frame should not be more than 1K. */
  if (threadDiag) printf("Proc %d: finished user thread %d\n",proc->procid,th->tid);
  DeleteJob(proc);
  work(proc);
  assert(0);
}

/* Mutator calls Yield which is defined in the service_platform_asm.s assembly file */
Thread_t *YieldRest()
{
  Proc_t *proc = getProc();
  proc->userThread->request = YieldRequest;  /* Record why this thread pre-empted */
  proc->userThread->saveregs[RESULT] = 256;  /* ML representation of unit */
  ReleaseJob(proc);
  work(proc);
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

/* Processor must be unmapped */
void schedulerRest(Proc_t *proc)
{
  int local;
  assert((proc->stack - (int) (&local)) < 1024); /* Check that we are running on own stack */
  assert(proc->userThread == NULL);
  work(proc);
  assert(0);
}


double segmentTime(Proc_t *proc)
{
  return proc->gcTime + lap_timer(&proc->currentTimer);
}
