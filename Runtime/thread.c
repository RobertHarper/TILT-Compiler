#if (defined alpha_osf)
#include <float.h>
#elif (defined solaris)
#include <ieeefp.h>
#endif
#include "errno.h"
#include "general.h"
#include "tag.h"
#include "thread.h"
#include "stack.h"
#include "stats.h"
#include "memobj.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
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

int NumThread     = 150;
int NumProc       = 1;
int RotateProc = 0;
int threadDiag = 0;
#if defined(solaris)
int accountingLevel = 1;
#elif defined(alpha_osf)
int accountingLevel = 0;
#endif
extern int usageCount;

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

extern void start_client(Thread_t *, ptr_t thunk);
extern void context_restore(Thread_t *);

Thread_t *mainThread = NULL;


/* ------------------ Manipulating the job queue ----------------- */
static long local_lock = 0;

void LocalLock(void)
{
  while (!TestAndSet(&local_lock)) /* No need to flush */
    ;
  assert(local_lock == 1);
  memBarrier();
}

void LocalUnlock(void)
{
  memBarrier();
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
  memBarrier();
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


void UpdateJob(Proc_t *proc)
{
  int i;
  Thread_t *th = proc->userThread;
  Stacklet_t *stacklet;
  mem_t proc_allocCursor = proc->allocCursor;
  mem_t proc_allocLimit = proc->allocLimit;
  mem_t proc_writelistCursor = (mem_t) proc->writelistCursor;

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

  /* Update processor's version of allocation range and write list.  Update stats */
  proc->allocCursor = (mem_t) th->saveregs[ALLOCPTR];
  proc->writelistCursor = th->writelistAlloc;
  PadHeapArea(proc->allocCursor, proc->allocLimit);
  proc->segUsage.numWrites += (proc->writelistCursor - proc->writelistStart) / 3;
  proc->segUsage.bytesAllocated += (sizeof(val_t) * (proc->allocCursor - proc->allocStart));

  if (threadDiag)
    printf("Proc %d: Releasing thread %d.   Request = %d (%d).\n"
	   "         %d bytes allocated.    Writes %d.\n",
	   proc->procid,th->tid,th->requestInfo, 
	   (sizeof(val_t) * (proc->allocCursor - proc->allocStart)),
	   (proc->writelistCursor - proc->writelistStart) / 3);

  procChangeState(proc, Scheduler, 5);
  GCReleaseThread(proc);
  procChangeState(proc, Scheduler, 6);

  /* Null out thread's version of allocation and write-list */
  stacklet = EstablishStacklet(th->stack, (mem_t) th->saveregs[SP]); /* updates stacklet->cursor */
#ifdef solaris
  stacklet->retadd = (mem_t) th->saveregs[LINK];
#else
  stacklet->retadd = (mem_t) th->saveregs[RA];
#endif
  th->saveregs[ALLOCPTR] = 0;
  th->saveregs[ALLOCLIMIT] = 0;
  th->writelistAlloc = 0;
  th->writelistLimit = 0;

}

void ReleaseJob(Proc_t *proc)
{
  Thread_t *th = proc->userThread;
  UpdateJob(proc);

  /* Break association between thread and processor */
  proc->userThread = NULL;
  th->proc = NULL;
  
  memBarrier();                          /* make sure thread info is flushed to memory */
  assert(th->status >= 1);               /* Thread was mapped and running so could not be ready or done */
  FetchAndAdd(&(th->status),-1);         /* Release after flush; note that FA always goes to mem */
}


/* Does not do locking to prevent multiple access.  Assumes caller does it. */
void Thread_Create(Thread_t *th, Thread_t *parent, ptr_t thunk)
{
  int i;
  assert(th->used == 0);
  assert(th->status == -1);
  assert(!th->pinned);
  assert(&(Threads[th->id]) == th);
  for (i=0; i<arraysize(th->rootLocs); i++) {
    th->rootVals[i] = NULL;
    th->rootLocs[i] = NULL;
  }
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
    th->stack = StackChain_Alloc(th); 
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
  procChangeState(proc, Scheduler, 11);
  if (th->parent) {
    FetchAndAdd(&(th->parent->status),-1);
    assert(th->parent->status >= 0); /* Parent must not have already finished */
  }
  assert(th->status == 1);
  th->status = -1; 
  Thread_Free(th); 
  return;
}


void StopAllThreads(void)
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

Proc_t *getProcPthread(void)
{
  int i;
  pthread_t sys = pthread_self();
  for (i=0; i<NumProc; i++) {
    if (Procs[i].pthread == sys) 
      return &(Procs[i]);
  }
  return NULL;
}

Thread_t *getThread(void)
{
  ui_t localVar;
  Stacklet_t *stacklet = GetStacklet(&localVar);
  Thread_t *thread = NULL;

  if (stacklet == NULL)
    return NULL;
  thread = (Thread_t *) stacklet->parent->thread;
  if (thread != NULL) {
    Proc_t *proc2 = getProcPthread();
    if (thread->proc != proc2) {
      printf("thread->proc  %d  %d\n", thread->proc->procid, thread->proc);
      if (proc2 != NULL) {
	printf("getProcPthread()  %d %d\n", proc2->procid, proc2);
      } else {
	printf("getProcPthread()  NULL\n");
      }
    }
    assert(thread->proc == proc2);
  }
  return thread;
}

Proc_t *getProc(void)
{
  Thread_t *thread = getThread();
  if (thread != NULL)
    return (Proc_t *) (thread->proc);
  return getProcPthread();
}

void resetUsage(Usage_t *u)
{
  u->numWrites = 0;
  u->bytesAllocated = 0;
  u->bytesReplicated = 0;
  u->fieldsCopied = 0;
  u->fieldsScanned = 0;
  u->ptrFieldsScanned = 0;
  u->objsCopied = 0;
  u->objsScanned = 0;
  u->pagesTouched = 0 ;
  u->rootsProcessed = 0;
  u->globalsProcessed = 0;
  u->stackSlotsProcessed = 0;
  u->workDone = 0;
  u->checkWork = 0;
  u->maxWork = 0;
  u->counter = usageCount;
}

long updateUsage(Usage_t *u)
{
  u->workDone = (long) (u->fieldsCopied * fieldCopyWeight + 
			u->fieldsScanned * fieldScanWeight +
			u->ptrFieldsScanned * ptrFieldScanWeight +
			u->objsCopied * objCopyWeight + 
			u->objsScanned * objScanWeight +
			u->pagesTouched * pageWeight +
			u->rootsProcessed * rootWeight +
			u->globalsProcessed * globalWeight +
			u->stackSlotsProcessed * stackSlotWeight);
  return u->workDone;
}

void updateWorkDone(Proc_t *proc)
{
  updateUsage(&proc->segUsage);
}

long bytesCopied(Usage_t *u)
{
  return 4 * (u->fieldsCopied + u->objsCopied);
}

static void attributeUsage(Usage_t *from, Usage_t *to)
{
  to->numWrites += from->numWrites;
  to->bytesAllocated += from->bytesAllocated;
  to->bytesReplicated += from->bytesReplicated;
  to->fieldsCopied += from->fieldsCopied;
  to->fieldsScanned += from->fieldsScanned;
  to->ptrFieldsScanned += from->ptrFieldsScanned;
  to->objsCopied += from->objsCopied;
  to->objsScanned += from->objsScanned;
  to->pagesTouched += from->pagesTouched;
  to->rootsProcessed += from->rootsProcessed;
  to->globalsProcessed += from->globalsProcessed;
  to->stackSlotsProcessed += from->stackSlotsProcessed;
  resetUsage(from);
}


void fillThread(Thread_t *th, int id)
{  
  int i;
  for (i=0; i<arraysize(th->rootLocs); i++) {
    th->rootVals[i] = NULL;
    th->rootLocs[i] = NULL;
  }
  th->id = id;
  th->tid = -1;
  th->status = -1;
  th->used = 0;
  th->pinned = 0;
  th->parent = NULL;
  th->request = StartRequest;
  th->saveregs[THREADPTR] = (long) th;
  th->saveregs[ALLOCLIMIT] = 0;
  th->stack = NULL;
  th->thunk = NULL;
}



void init_localWork(LocalWork_t *lw, int objSize, int segSize, int globalSize, int rootSize, int stackletSize, int backObjSize, int backLocSize)
{
  SetInit(&lw->objs, objSize);
  SetInit(&lw->grayRegion, 2048); /* XXX */
  SetInit(&lw->segments, segSize);
  SetInit(&lw->globals, globalSize);
  SetInit(&lw->roots, rootSize);
  SetInit(&lw->stacklets, stackletSize);
  SetInit(&lw->backObjs, backObjSize);
  SetInit(&lw->backLocs, backLocSize);
  SetInit(&lw->nextBackObjs, backObjSize);
  SetInit(&lw->nextBackLocs, backLocSize);
  lw->hasShared = 0;
} 

int isLocalWorkAlmostEmpty(LocalWork_t *lw)
{
  return (SetIsEmpty(&lw->stacklets) &&
	  SetIsEmpty(&lw->globals) &&
	  SetIsEmpty(&lw->roots) &&
	  SetIsEmpty(&lw->objs) &&
	  SetIsEmpty(&lw->grayRegion) &&
	  SetIsEmpty(&lw->segments) &&
	  lw->hasShared == 0);
}

int isLocalWorkEmpty(LocalWork_t *lw)
{
  return (SetIsEmpty(&lw->stacklets) &&
	  SetIsEmpty(&lw->globals) &&
	  SetIsEmpty(&lw->roots) &&
	  SetIsEmpty(&lw->objs) &&
	  SetIsEmpty(&lw->grayRegion) &&
	  SetIsEmpty(&lw->segments) &&
	  SetIsEmpty(&lw->backObjs) &&
	  SetIsEmpty(&lw->backLocs) &&
	  lw->hasShared == 0);
}


void thread_init(void)
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
    int tabSize = 20;
    Proc_t *proc = &(Procs[i]); /* Structures are by-value in C */
    proc->procid = i;
    proc->barrierPhase = 0;

    proc->allocStart = (mem_t) StartHeapLimit;
    proc->allocCursor = (mem_t) StartHeapLimit;
    proc->allocLimit = (mem_t) StartHeapLimit;
    proc->writelistStart = &(proc->writelist[0]);
    proc->writelistCursor = proc->writelistStart;
    proc->writelistEnd = &(proc->writelist[arraysize(proc->writelist) - 2]);
    for (j=0; j<arraysize(proc->writelist); j++)
      proc->writelist[j] = 0;
    init_localWork(&proc->work, 16384, 16384, 8192, 4096, 128, 2048, 4096);  
    reset_timer(&(proc->totalTimer));
    reset_timer(&(proc->currentTimer));
    proc->state = Scheduler;
    proc->segmentNumber = 0;
    proc->segmentType = 0;
    proc->mutatorTime = 0.0;
    proc->nonMutatorTime = 0.0;
    proc->nonMutatorCount = -1;
    proc->firstHistory = proc->lastHistory = 0;
    resetUsage(&proc->segUsage);
    resetUsage(&proc->cycleUsage);
    reset_statistic(&proc->numWritesStatistic);
    reset_statistic(&proc->bytesAllocatedStatistic);
    reset_statistic(&proc->bytesReplicatedStatistic);
    reset_statistic(&proc->bytesCopiedStatistic);
    reset_statistic(&proc->workStatistic);
    reset_statistic(&proc->schedulerStatistic);
    reset_statistic(&proc->accountingStatistic);
    reset_statistic(&proc->idleStatistic);
    reset_histogram(&proc->mutatorHistogram);
    reset_statistic(&proc->gcStatistic);
    reset_histogram(&proc->gcPauseHistogram);
    reset_histogram(&proc->timeDivWorkHistogram);
    reset_histogram(&proc->gcFlipOffHistogram);
    reset_histogram(&proc->gcFlipOnHistogram);
    reset_histogram(&proc->gcFlipBothHistogram);
    reset_histogram(&proc->gcFlipTransitionHistogram);
    reset_statistic(&proc->gcIdleStatistic);
    reset_statistic(&proc->gcWorkStatistic);
    reset_statistic(&proc->gcStackStatistic);
    reset_statistic(&proc->gcGlobalStatistic);
    reset_statistic(&proc->gcWriteStatistic);
    reset_statistic(&proc->gcReplicateStatistic);
    reset_statistic(&proc->gcOtherStatistic);
    reset_windowQuotient(&proc->utilizationQuotient1,0);
    reset_windowQuotient(&proc->utilizationQuotient2,1);
    SetCopyRange(&proc->copyRange, proc, NULL, NULL);
    proc->numCopied = proc->numShared = proc->numContention = 0;
    proc->segmentNumber = 0;
    proc->segmentType = 0;
    proc->numRoot = 0;
    proc->numLocative = 0;
    proc->lastHashKey = 0;
    proc->lastHashData = NULL;
    proc->lastCallinfoCursor.callinfo = NULL;
    proc->tab = (char *) malloc(i * tabSize + 1);
    memset(proc->tab, ' ', i * tabSize);
    proc->tab[i * tabSize] = (char) 0;
    proc->delayMsg[0] = 0;
  }
  pthread_cond_init(&EmptyCond,NULL);
  pthread_mutex_init(&EmptyLock,NULL);
  setbuf(stdout,NULL);
  setbuf(stderr,NULL);
}

static char* state2string(ProcessorState_t procState)
{
  switch (procState) {
  case Scheduler: return "Scheduler";
  case Mutator: return "Mutator";
  case Idle: return "Idle";
  case GC: return "GC";
  case GCStack: return "GCStack";
  case GCGlobal: return "GCGlboal";
  case GCWrite: return "GCWrite";
  case GCReplicate: return "GCReplicate";
  case GCWork: return "GCWork";
  case GCIdle: return "GCIdle";
  default : return "unknownProcState";
  }
}

extern int workAhead;

void captureSummary(Proc_t *proc, double diff, Summary_t *s) 
{
  s->time = diff;
  s->segment = proc->segmentNumber;
  s->state = proc->state;
  s->type = proc->segmentType;
  s->util1 = proc->utilizationQuotient1.stat[0].count ? proc->utilizationQuotient1.stat[0].last : 1.0;
  s->util2 = proc->utilizationQuotient1.stat[2].count ? proc->utilizationQuotient1.stat[2].last : 1.0;
  s->util3 = proc->utilizationQuotient2.stat[5].count ? proc->utilizationQuotient2.stat[5].last : 1.0;
  switch (proc->state) {
  case Scheduler:
  case Idle: 
    s->data1 = 0;
    s->data2 = 0;
    break;
  case Mutator:
    s->data1 = proc->segUsage.bytesAllocated;
    s->data2 = proc->segUsage.numWrites;
    break;
  case GC:
  case GCStack:
  case GCGlobal:
  case GCWrite: 
  case GCReplicate:
  case GCWork:
  case GCIdle: {
      int work = proc->segUsage.workDone;
      if (proc->segUsage.workDone >= proc->lastSegWorkDone)
	work = proc->segUsage.workDone - proc->lastSegWorkDone;
      proc->lastSegWorkDone = 
      s->data1 = proc->segUsage.workDone;
      s->data2 = workAhead;
      s->data3 = proc->segUsage.stackSlotsProcessed;
      break;
    }
  }
}


/* If howMany is zero, then show all history */
void showHistory(Proc_t *proc, int howMany, char *file)
{
  double mutTime = 0.0, gcTime = 0.0, skipTime = 0.0, minUtil = 1.0;
  int on = 0, skip = 0;
  int gc = 0;
  int cur = howMany ? proc->lastHistory - howMany : proc->firstHistory;
  int maxCount = (sizeof(proc->history) / sizeof(Summary_t));
  FILE *f = stdout;

  if (file != NULL) {
    f = fopen(file,"w");
    if (f == NULL) {
      printf("Could not open history file %s.  errno = %d\n", file, errno);
      return;
    }
  }

  if (cur < 0)
    cur += arraysize(proc->history);

  while (cur != proc->lastHistory) {
    Summary_t *s = &proc->history[cur];
    Summary_t *s2 = &proc->history[((cur+1) == maxCount) ? 0 : cur+1];
    int suppress;
    double utilCutoff = 0.8;

    if (s->type & FlipOn) {
      on = 1;
      mutTime = gcTime = 0.0;
    }
    suppress = (on == 0) && (s->util1 >= utilCutoff) && (s2->util1 >= utilCutoff);
    if (suppress) {
      skip++;
      skipTime += s->time;
    }
    else {
      if (skip > 0) {
	fprintf(f,"%3d %6d: %5.2f ms  %12s       utils >= %.3f       Merged\n",
		gc, s->segment, skipTime,  "Composite", utilCutoff);
      }
      skip = 0;
      skipTime = 0.0;
      fprintf(f,"%3d %6d: %5.2f ms  %12s       utils = %.3f  %.3f  %.3f",
	     gc, s->segment, s->time,  state2string(s->state), s->util1, s->util2, s->util3);
    }
    minUtil = Min(minUtil, s->util1);
    switch (s->state) {
    case Mutator:
      mutTime += s->time;
      if (!suppress)
	fprintf(f,"  alloc = %5d   writes = %4d rate = %4.1f", s->data1, s->data2, (s->data1 / 1000.0) / s->time); break;
    case Scheduler:
    case Idle: 
      gcTime += s->time;
      break;
    case GC:
    case GCStack:
    case GCGlobal:
    case GCWrite: 
    case GCReplicate:
    case GCWork:
    case GCIdle: 
      gcTime += s->time;
      if (!suppress) 
	fprintf(f,"  work  = %5d   eff  = %4.1f    slots = %4d   workAhead = %d", 
		s->data1, (s->data1 / 1000.0) / s->time, s->data3, s->data2); break;
    }
    if (!suppress) {
      if (s->type & FlipOn) 
	fprintf(f,"  FlipOn");
      if (s->type & FlipTransition)
	fprintf(f,"  FlipTransition");
      if (s->type & FlipOff) 
	fprintf(f,"  FlipOff");
      fprintf(f,"\n");
    }
    if (s->type & FlipOff) {
      if (howMany == 0)
	fprintf(f,"********** GC cycle:  mut = %5.1f   gc = %5.1f       avg util = %.3f     min util = %.3f\n",
	       mutTime, gcTime, mutTime / (mutTime + gcTime), minUtil);
      on = 0;
      mutTime = gcTime = 0.0;
      minUtil = 1.0;
      gc++;
    }
    cur++;
    if (cur >= arraysize(proc->history))
      cur = 0;
  }

  if (f != stdout)
    fclose(f);
}

void procChangeState(Proc_t *proc, ProcessorState_t newState, int discardedSubstate)
{
  int i, segWork;
  double diff = 0.0;             /* Time just spent in current segment in ms */
  int switchToMutator = (newState == Mutator || 
			 newState == Done ||
			 newState == Idle); /* Switching to mutator or effectievly so? */
  int hasAccess = 0;

  if (proc->segmentNumber < 0)
    return;
  if (proc->state == newState)  /* No state change */
    return;
  if (proc->currentTimer.on) {
    restart_timer(&proc->currentTimer);
    diff = proc->currentTimer.last;
  }
  else
    start_timer(&proc->currentTimer);

  if (proc->delayMsg[0] != 0)
    printf("%s\n", proc->delayMsg);
  proc->delayMsg[0] = 0;

  segWork = updateGetWorkDone(proc);  /* Grab this before attributeUsage */

  /* Accumulate info across segments since mutator segment */
  if (proc->state != Mutator && proc->nonMutatorCount >= 0) {
    proc->nonMutatorTime += diff;
    proc->nonMutatorSegmentType |= proc->segmentType;
    proc->nonMutatorCount++;
  }

  /* Add times to the segment that just ended */
  switch (proc->state) {
  case Mutator:
    add_histogram(&proc->mutatorHistogram, diff);
    proc->mutatorTime = diff;
    proc->nonMutatorCount = 0;
    proc->nonMutatorSegmentType = 0;
    proc->nonMutatorTime = 0.0;
    break;
  case Scheduler:
    add_statistic(&proc->schedulerStatistic, diff);
    break;
  case Idle:
    add_statistic(&proc->idleStatistic, diff);
    proc->nonMutatorCount = -1;  
    break;
  case GC:
  case GCWork:
  case GCIdle:
  case GCStack:
  case GCGlobal:
  case GCWrite:
  case GCReplicate:
    assert(newState != Mutator);
    add_statistic(&proc->gcStatistic, diff);
    switch (proc->state) {
    case GC:
      add_statistic(&proc->gcOtherStatistic, diff);
      break;
    case GCIdle:
      add_statistic(&proc->gcIdleStatistic, diff);
      break;
    case GCWork:
      add_statistic(&proc->gcWorkStatistic, diff);
      break;
    case GCStack:
      add_statistic(&proc->gcStackStatistic, diff);
      break;
    case GCGlobal:
      add_statistic(&proc->gcGlobalStatistic, diff);
      break;
    case GCWrite:
      add_statistic(&proc->gcWriteStatistic, diff);
      break;
    case GCReplicate:
      add_statistic(&proc->gcReplicateStatistic, diff);
      break;
    }
  case Done:
    break;
  default:
    assert(0);
  }

  /* A mutator is considered to have access unless a GC is active not triggered by an Idle state */
  if (accountingLevel >= 1) {
    hasAccess = (proc->state == Mutator || proc->state == Done || proc->state == Scheduler || proc->state == Idle ||
		 proc->nonMutatorCount == -1);
    add_windowQuotient(&proc->utilizationQuotient1, diff, hasAccess);
    add_windowQuotient(&proc->utilizationQuotient2, diff, hasAccess);
  }

  /* ---------------- Record History (skip short non-mutator segments) --------------- */
  if (accountingLevel >= 1 && diff >= 0.01 || proc->state == Mutator) {
    captureSummary(proc, diff, &proc->history[proc->lastHistory]);
    proc->lastHistory++;
    if (proc->lastHistory >= arraysize(proc->history))
      proc->lastHistory = 0;
    if (proc->lastHistory == proc->firstHistory) {
      proc->firstHistory++;
      if (proc->firstHistory >= arraysize(proc->history))
	proc->firstHistory = 0;
    }
  }

  /* -------------- Report History -------------------- */
  if (accountingLevel >= 1 &&
      ((warnThreshold && pauseWarningThreshold > 0.0) ||   
       (warnUtil > 0.0 && proc->utilizationQuotient1.stat[0].count && proc->utilizationQuotient1.stat[0].last < warnUtil))) {  
      int exceedThreshold = proc->nonMutatorTime > pauseWarningThreshold;
      int lowUtil = proc->utilizationQuotient1.stat[0].count && proc->utilizationQuotient1.stat[0].last < warnUtil;
      int diag = timeDiag && proc->nonMutatorTime > 0.2;
      if (exceedThreshold || lowUtil) 
	printf("Proc %d: Total time = %.2f ms.  Work = %d    Util = %.3f    %s %s\n",
	       proc->procid, proc->nonMutatorTime, updateGetWorkDone(proc),
	       proc->utilizationQuotient1.stat[0].last, 
	       exceedThreshold ? "  --> Exceeded Threshold <--  " : "",
	       lowUtil ? "  --> Low Utilization <--  " : "");
      if (exceedThreshold || lowUtil || diag) {
	printf("   objCopy    objScan  fieldsCopy  fieldsScan   ptrFieldScan   globals  stack   pages  rep  Time  Work\n");
	printf("   %5d        %5d       %5d       %5d          %5d        %3d      %3d     %2d    %5d   %.2f  %5d ;\n",
	       proc->segUsage.objsCopied, proc->segUsage.objsScanned, 
	       proc->segUsage.fieldsCopied, proc->segUsage.fieldsScanned, proc->segUsage.ptrFieldsScanned, 
	       proc->segUsage.globalsProcessed, proc->segUsage.stackSlotsProcessed, 
	       proc->segUsage.pagesTouched, proc->segUsage.bytesReplicated,
	       proc->nonMutatorTime, updateGetWorkDone(proc));
      }
      if (exceedThreshold || lowUtil) 
	showHistory(proc, 40, NULL);
  }


  if (proc->segmentNumber > 0 && switchToMutator) {

    int flipOn = (proc->nonMutatorSegmentType & FlipOn);
    int flipOff = (proc->nonMutatorSegmentType & FlipOff);
    int flipTransition = (proc->nonMutatorSegmentType & FlipTransition);

    /* First do statistics dependent on whether GC is major or minor */
    attributeUsage(&proc->segUsage, &proc->cycleUsage);
    if (flipOff || newState == Done) {
      add_statistic(&proc->numWritesStatistic, proc->cycleUsage.numWrites);
      add_statistic(&proc->bytesAllocatedStatistic, proc->cycleUsage.bytesAllocated);
      add_statistic(&proc->bytesReplicatedStatistic, proc->cycleUsage.bytesReplicated);
      add_statistic(&proc->bytesCopiedStatistic, bytesCopied(&proc->cycleUsage));
      add_statistic(&proc->workStatistic, updateUsage(&proc->cycleUsage));
      resetUsage(&proc->cycleUsage);
    }

    /* Attribute time since mutator last run and reset */
    if (flipOn && flipOff)
      add_histogram(&proc->gcFlipBothHistogram, proc->nonMutatorTime);
    else if (flipOff) 
      add_histogram(&proc->gcFlipOffHistogram, proc->nonMutatorTime);
    else if (flipOn) 
      add_histogram(&proc->gcFlipOnHistogram, proc->nonMutatorTime);
    if (flipTransition)
      add_histogram(&proc->gcFlipTransitionHistogram, proc->nonMutatorTime);
    add_histogram(&proc->gcPauseHistogram, proc->nonMutatorTime);
    if (proc->nonMutatorTime > 0.2 && segWork != 0) {
      double timeDivWork = proc->nonMutatorTime / (segWork / 1000.0);
      add_histogram(&proc->timeDivWorkHistogram, timeDivWork);
    }
    if (diag && (proc->mutatorTime / proc->nonMutatorTime) < 0.2) {
      printf("segmentNumber = %d    mutatorTime = %.3f    nonMutatorTime = %.3f   util = %.3f   segType = %d\n", 
	     proc->segmentNumber, proc->mutatorTime, proc->nonMutatorTime,
	     proc->mutatorTime / proc->nonMutatorTime,
	     proc->nonMutatorSegmentType);
    }
  }
  
  /* Perform state change */
  proc->segmentNumber++;
  proc->segmentType = 0;
  proc->state = newState;

  restart_timer(&proc->currentTimer);
  add_statistic(&proc->accountingStatistic, proc->currentTimer.last);
}

int thread_total(void)
{
  return totalThread;
}
 
int thread_max(void) 
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


/* Map thread onto processor.  Processor must be unmapped or already mapped to thread. */
static void mapThread(Proc_t *proc, Thread_t *th)
{
  Stacklet_t *stacklet = CurrentStacklet(th->stack);

  Stacklet_KillReplica(stacklet);
  if (proc->userThread == NULL) {
    assert(th->proc == NULL);
    proc->userThread = th;
    th->proc = proc;
  }
  else {
    assert(proc->userThread == th);
    assert(th->proc == proc);
  }
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

/* Processor might be mapped */
static void work(Proc_t *proc)
{
  Thread_t *th = proc->userThread;

  procChangeState(proc, Scheduler, 20);

  /* To get changes other processors have made */
  memBarrier();

  if (th == NULL)
    procChangeState(proc, Scheduler, 21);
  else
    procChangeState(proc, Scheduler, 22);

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
      procChangeState(proc, Idle, 23);
      GCPoll(proc);
    }
    if (EmptyPlain) {
      procChangeState(proc,Done, 25);
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

  procChangeState(proc, Scheduler, 26);

  assert(th->status == 1); /* FetchJob increments status from 0 to 1 */

  switch (th->request) {

    case NoRequest: 
      assert(0);
    case YieldRequest: {
#ifdef solaris
      th->saveregs[16] = (long) th;  /* load_regs_forC on solaris expects thread pointer in %l0 */
#endif	
      mapThread(proc,th);
      procChangeState(proc, Mutator, 27);
      returnFromYield(th);
    }
    case StartRequest: {              /* Starting thread for the first time */
      ptr_t thunk = th->thunk;
      mapThread(proc,th);
      th->thunk = NULL;
      assert(thunk != NULL);
      if (threadDiag) {
	printf("Proc %d: starting user thread %d (%d) with %d <= %d\n",
	       proc->procid, th->tid, th->id, th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT]);
	assert(th->saveregs[ALLOCPTR] <= th->saveregs[ALLOCLIMIT]);
      }
      memBarrier();  /* make visible changes to other processors */
      procChangeState(proc, Mutator, 28);
      start_client(th,thunk);
      assert(0);
    }

    case GCRequestFromML:
    case GCRequestFromC: 
    case MajorGCRequestFromC: {
      /* Allocate space or check write buffer to see if we have enough space */
      GCFromScheduler(proc, th);
      procChangeState(proc, Scheduler, 29);
      /* Note that another processor can change th->saveregs[ALLOCLIMIT] to Stop at any point */
      assert(GCSatisfiable(proc,th));
      mapThread(proc,th);
      if (th->request == GCRequestFromML) {
	if (threadDiag)
	  printf("Proc %d: Resuming thread %d from GCRequestFromML of %d bytes with allocation region %d < %d and writelist %d < %d\n",
		 proc->procid, th->tid, th->requestInfo, 
		 th->saveregs[ALLOCPTR], th->saveregs[ALLOCLIMIT],
		 th->writelistAlloc, th->writelistLimit);
	memBarrier();  /* make visible changes to other processors */
	procChangeState(proc, Mutator, 30);
	returnFromGCFromML(th);	
	assert(0);
      }
      else if (th->request == GCRequestFromC ||
	       th->request == MajorGCRequestFromC) {
	if (threadDiag)
	  printf("Proc %d: Resuming thread %d from Major/C GCRequestFromML of %d bytes with allocation region %d < %d and writelist %d < %d\n",
		 proc->procid, th->tid, th->requestInfo, proc->allocCursor, proc->allocLimit,
		 th->writelistAlloc, th->writelistLimit);
	memBarrier();  /* make visible changes to other processors */
	procChangeState(proc, Mutator, 31);
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


static void* proc_go(void* untypedProc)
{
  Proc_t *proc = (Proc_t *) untypedProc;
  /* pds: Do no binding for uniprocessor applications.  This avoids
     contention for the first active processor (so does @rotateProc,
     but this is automatic).  */
  if (NumProc > 1) {
#ifdef solaris
    int status = processor_bind(P_LWPID, P_MYID, proc->processor, NULL);
    if (status != 0)
      printf("processor_bind on %d failed with %d\n", proc->processor, status);
#else
    if (threadDiag)
      printf("Cannot find processors on non-sparc: assuming uniprocessor\n");
#endif
  }
  proc->pthread = pthread_self();
  initializePerfMon(proc);
  install_signal_handlers(0);
  proc->stack = (int)(&proc) & (~255);
  FetchAndAdd(&NumActiveProc, 1);
  start_timer(&proc->totalTimer);
  start_timer(&proc->currentTimer);

  proc->segmentNumber = -1; /* Disable procChangeState */
  GCReleaseThread(proc);    /* Fault in the code to avoid later Icache miss */
  proc->segmentNumber = 0;  /* Enable procChangeState */
  work(proc);
  assert(0);
  return 0;
}

void thread_go(ptr_t thunk)
{
  int curActive = RotateProc;
  int i, status;
  pthread_t discard;

#ifdef solaris
  int curproc = -1, active = 0;
  int activeProcs[64];
  processor_info_t infop;
  while (++curproc < 64) {
    int status = processor_info(curproc,&infop);
    if (status == 0 && infop.pi_state == P_ONLINE)
      activeProcs[active++] = curproc;
  }
  if (threadDiag) {
    printf("Found %d active processors:", active);
    for (i=0; i<active; i++) 
      printf(" %d ", activeProcs[i]);
    printf("\n");
  }
  if (active < NumProc) {
    printf("Needed %d processors.\n", NumProc);
    assert(0);
  }
#endif

  mainThread = thread_create(NULL,thunk);
  AddJob(mainThread);

  /* Create system threads that run off the user thread queue */
#ifdef solaris
  if (curActive >= active)
    curActive = 0;
#endif
  for (i=0; i<NumProc; i++) {
    pthread_attr_t attr;
    struct sched_param schedParam;
#ifdef solaris
    Procs[i].processor = activeProcs[curActive++];
    if (curActive >= active)
      curActive = 0;
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
    status = pthread_create(&discard,&attr,proc_go,&Procs[i]);
    if (status) {
      printf("pthread_create returned status = %d (%s)\n", status, strerror(status));
      assert(0);
    }
    if (threadDiag)
      printf("Proc %d:  processor %d and pthread = %d\n",
	     Procs[i].procid, Procs[i].processor, Procs[i].pthread);
  }

#ifdef solaris
  if (threadDiag) {
    printf("Using processors ");
    for (i=0; i<NumProc; i++) 
      printf(" %d", Procs[i].processor);
    printf(".\n");
  }
#endif

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
void Finish(void)
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
Thread_t *YieldRest(void)
{
  Proc_t *proc = getProc();
  proc->userThread->request = YieldRequest;  /* Record why this thread pre-empted */
  proc->userThread->saveregs[RESULT] = 256;  /* ML representation of unit */
  ReleaseJob(proc);
  procChangeState(proc, Scheduler, 11);
  work(proc);
  assert(0);
  return 0;
}

/* Processor might be mapped but is at least up-to-date */
void schedulerRest(Proc_t *proc)
{
  int local;
  assert((proc->stack - (int) (&local)) < 1024); /* Check that we are running on own stack */
  work(proc);
  assert(0);
}


double nonMutatorTime(Proc_t *proc)
{
  return proc->nonMutatorTime + lap_timer(&proc->currentTimer);
}

double segmentTime(Proc_t *proc)
{
  return lap_timer(&proc->currentTimer);
}

ptr_t registerThunk(ptr_t mlString)
{
  /*
    char buffer[256];
    mlstring2cstring_buffer(mlString, sizeof(buffer), buffer);
    printf("registerThunk: %20s   Threads[0].saveregs[ALLOC] = %dd\n", buffer, Threads[0].saveregs[ALLOCPTR]);
  */
  return empty_record;
}
