#ifdef alpha_osf
#define intSz        4
#define longSz       8
#define CptrSz       8
#define MLptrSz      4
#define doubleSz     8
#endif
#ifdef solaris
#define intSz        4
#define longSz       4
#define CptrSz       4
#define MLptrSz      4
#define doubleSz     8
#endif

#define MLsaveregs_disp     0
#define maxsp_disp          longSz*32+8*32
#define snapshot_disp       longSz*32+8*32+longSz
#define proc_disp           longSz*32+8*32+longSz+CptrSz
#define notinml_disp        longSz*32+8*32+longSz+CptrSz+CptrSz
#define scratch_disp        longSz*32+8*32+longSz+CptrSz+CptrSz+longSz
#define thunk_disp          longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz
#define nextThunk_disp      longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz+CptrSz
#define numThunk_disp       longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz+CptrSz+longSz
#define request_disp        longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz+CptrSz+longSz+longSz
#define requestInfo_disp    longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz+CptrSz+longSz+longSz+longSz
#define Csaveregs_disp      longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz+CptrSz+longSz+longSz+longSz+longSz+longSz
#define writelistAlloc_disp longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz+CptrSz+longSz+longSz+longSz+longSz+longSz+32*longSz+32*doubleSz
#define writelistLimit_disp longSz*32+8*32+longSz+CptrSz+CptrSz+longSz+doubleSz+CptrSz+longSz+longSz+longSz+longSz+longSz+32*longSz+32*doubleSz+MLptrSz
#if    defined(solaris)
#define snapshot_size       16
#elif  defined(alpha_osf)
#define snapshot_size       20
#endif
#define NoRequest 0
#define YieldRequest 1
#define StartRequest 2
#define GCRequestFromML 3
#define GCRequestFromC 4
#define MajorGCRequestFromC 5

#ifndef _thread_h
#define _thread_h
#ifndef _asm_

#include "memobj.h"
#include "stats.h"
#include "queue.h"
#include <signal.h>


typedef struct Stack__t
{
  ptr_t *data;
  long cursor;
  long size;
} Stack_t;

/* These types should be in stack.h but mutually recusrive headers are broken. */
typedef struct StackSnapshot   /* the size and layout of this structure affects the code in stack_asm.s */
{
  val_t saved_ra;              /* Real return address for this stub */
  val_t saved_sp;              /* Stack pointer position where stub was inserted */
  unsigned int saved_regstate; /* Register state (mask) at this point */
  Stack_t *roots;              /* Roots between this stub and the one above it */
} StackSnapshot_t;


/* These types should be in forward.h but mutually recusrive headers are broken. */
struct RegionStack__t;
struct Proc__t;
struct CopyRange__t;
typedef void discharge_t(struct CopyRange__t *);
typedef void expand_t(struct CopyRange__t *, int size);
typedef struct CopyRange__t   /* This is essentially an object clumsily expressed in C */
{
  mem_t start;
  mem_t cursor;
  mem_t stop;
  Heap_t *heap;
  expand_t *expand;
  discharge_t *discharge; 
  struct Proc__t *proc;
  Stack_t *regionStack;
} CopyRange_t;

typedef struct Usage__t
{
  long bytesAllocated;
  long bytesCopied;
  long bytesScanned;
  long rootsProcessed;
  long workDone;         /* Weighted average of bytesCopied, bytesScanned, and rootProcessed */
} Usage_t;

/* Finally, these types actually do belong here */
/* Thread Status:
     -1 : Thread is done
     0 : Thread is ready to be scheduled
     1 or more : Thread is running already or is blocked; not eligible to be run
*/
     
/* decalpha.sml, sparc.sml, and the ***_disp above have to be changed if the fields are modified 
   Note that long is used to avoid padding problems.
*/
typedef struct Thread__t
{
  /* ---- These fields are accessed by assembly code ---- */
  unsigned long      saveregs[32];     /* Register set; compiler relied on this being first */
  double             fregs[32];        /* Register set; compiler relied on this being second */
  long               maxSP;            /* Used by mutator exn handler; compiler relies on this third */
  StackSnapshot_t    *snapshots;       /* Used by stack.c and stack_asm.s */
  struct Proc__t     *proc;            /* of type Proc_t * - relied on by service_alpha_osf.s  */
  long               notInML;          /* set to true whenever mutator calls a normal external function */
  double             scratch;
  ptr_t              *thunks;          /* Array of num_add unit -> unit */
  long               nextThunk;        /* Index of next unstarted thunk.  Initially zero. */
  long               numThunk;         /* Number of thunks.  At least one. */
  long               request;          /* Why were we stoppped and how do we resume? */
  long               requestInfo;      /* If positive, how many bytes needed for allocation.
					  If negative, how many bytes of write buffer needed. */
  long               filler;           /* must double align here */
  long               Csaveregs[32];    /* C register saved when we need to de-schedule while in a C function */
  double             Cfregs[32];        
  ploc_t             writelistAlloc;
  ploc_t             writelistLimit;

  /* ---- The remaining fields not accessed by assembly code ---- */
  long               last_snapshot;    /* Index of last used snapshot */
  StackChain_t       *stackchain;      /* Stack */
  unsigned long      lastHashKey;     /* Last hash entry key/data */
  void               *lastHashData; 
  Stack_t            *callinfoStack;   /* Corresponds to stack frames */
  long                tid;             /* Thread ID */
  long                id;              /* Structure ID */
  long                status;          /* Thread status */
  struct Thread__t   *parent;
  ptr_t              oneThunk;         /* Avoid allocation by optimizing for common case */
} Thread_t;

void copyStack(Stack_t *from, Stack_t *to);     /* non-destructive operation on from */
void transferStack(Stack_t *from, Stack_t *to); /* destructive operation on from */
void resizeStack(Stack_t *ostack, int newSize);
void allocStack(Stack_t *ostack, int size);     /* allocate the data portion of an existing stack */
Stack_t *createStack(int size);                 /* make a stack from scratch */

INLINE1(resetStack)
INLINE2(resetStack)
void resetStack(Stack_t *oStack)
{
  oStack->cursor = 0;
}

INLINE1(lengthStack)
INLINE2(lengthStack)
int lengthStack(Stack_t *oStack)
{
  return oStack->cursor;
}

INLINE1(pushStack)
INLINE2(pushStack)
void pushStack(Stack_t *oStack, ptr_t item)
{
  /* assert(item != NULL); */
  oStack->data[oStack->cursor++] = item;
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);  /* Enough to maintain invariant that it is not full */
}

INLINE1(pushStack2)
INLINE2(pushStack2)
void pushStack2(Stack_t *oStack, ptr_t item1, ptr_t item2)
{
  /* assert(item != NULL); */
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
  oStack->data[oStack->cursor++] = item1;
  oStack->data[oStack->cursor++] = item2;
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
}

INLINE1(pushStack3)
INLINE2(pushStack3)
void pushStack3(Stack_t *oStack, ptr_t item1, ptr_t item2, ptr_t item3)
{
  /* assert(item != NULL); */
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
  oStack->data[oStack->cursor++] = item1;
  oStack->data[oStack->cursor++] = item2;
  oStack->data[oStack->cursor++] = item3;
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
}

/* Returns NULL if stack is empty */
INLINE1(popStack)
INLINE2(popStack)
ptr_t popStack(Stack_t *oStack)
{
  if (oStack->cursor) {
    return oStack->data[--oStack->cursor];
  }
  return NULL;
}

/* Returns NULL if stack is empty */
INLINE1(peekStack)
INLINE2(peekStack)
ptr_t peekStack(Stack_t *oStack)
{
  if (oStack->cursor) {
    return oStack->data[oStack->cursor-1];
  }
  return NULL;
}

/* Returns NULL if stack is empty */
INLINE1(popStack2)
INLINE2(popStack2)
ptr_t popStack2(Stack_t *oStack, ptr_t *item2Ref, ptr_t *item3Ref)
{
  if (oStack->cursor > 1) {
    *item2Ref = oStack->data[--oStack->cursor];  /* In reverse order of push */
    return oStack->data[--oStack->cursor];
  }
  return NULL;
}

/* Returns NULL if stack is empty */
INLINE1(popStack3)
INLINE2(popStack3)
ptr_t popStack3(Stack_t *oStack, ptr_t *item2Ref, ptr_t *item3Ref)
{
  if (oStack->cursor > 2) {
    *item3Ref = oStack->data[--oStack->cursor];  /* In reverse order of push */
    *item2Ref = oStack->data[--oStack->cursor];  /* In reverse order of push */
    return oStack->data[--oStack->cursor];
  }
  return NULL;
}

INLINE1(isEmptyStack)
INLINE2(isEmptyStack)
int isEmptyStack(Stack_t *oStack)
{
  return oStack->cursor == 0;
}

/* The states Scheduler, Mutator, GC,and  Done are disjoint.
   The remaining GC* states are substates of GC.
 */
typedef enum ProcessorState__t {Scheduler, Mutator, GC, Done,
				GCStack, GCGlobal, GCWork} ProcessorState_t;
/* Each segment might be no collection, minor, or major. 
   Independently, it migth invole flipping the collector on or off */
typedef enum GCSegment1__t {NoWork, MinorWork, MajorWork} GCSegment1_t;
typedef enum GCSegment2__t {Continue, FlipOn, FlipOff, FlipBoth} GCSegment2_t;

typedef struct Proc__t
{
  int                stack;        /* address of system thread stack that can be used to enter scheduler */
  int                procid;         /* sys thread id */
  mem_t              allocStart;     /* allocation range */
  mem_t              allocCursor;
  mem_t              allocLimit;      
  ploc_t             writelistStart;  /* write list range */
  ploc_t             writelistCursor;
  ploc_t             writelistEnd;
  ptr_t              writelist[4096];
  int                processor;      /* processor id that this pthread is bound to */
  pthread_t          pthread;        /* pthread that this system thread is implemented as */
  Thread_t           *userThread;    /* current user thread mapped to this system thread */
  Stack_t            minorStack;     /* Used by parallel/concurrent generational collector */
  Stack_t            minorSegmentStack; 
  Stack_t            majorStack;     /* Used by parallel/concurrent collector */
  Stack_t            majorSegmentStack;  
  Stack_t            majorRegionStack; /* Used by generational concurrent collector */
  Timer_t            totalTimer;     /* Time spent in entire processor */
  Timer_t            currentTimer;   /* Time spent running any subtask */
  int                segmentNumber;  /* Counts the Number of times we are in a GC since running a mutator */
  GCSegment1_t       gcSegment1;     /* Did GC work get done and was it a minor or major GC */
  GCSegment2_t       gcSegment2;     /* Did GC turn on, turn off, or continue? */
  double             gcTime;         /* How much time spent on current GC/Scheduler segment? */
  double             schedulerTime;  /* How much time spent on current GC/Scheduler segment? */
  ProcessorState_t   state;          /* What the processor is working on */
  long               numSegment;     /* Number of current segment, incremented each time we switch to a mutator */
  Usage_t            segUsage;       /* Info for current segment which will be added to a cycle */
  Usage_t            minorUsage;     /* Info for current GC cycle */
  Usage_t            majorUsage;
  Statistic_t        bytesAllocatedStatistic;  /* just minor - won't this exclude large objects? XXXX */
  Statistic_t        bytesCopiedStatistic;     /* both minor and major */
                                               /* XXX Should the next 3 be program-wide */
  Statistic_t        heapSizeStatistic;        /* in Kb */
  Statistic_t        minorSurvivalStatistic;
  Statistic_t        majorSurvivalStatistic;
  Statistic_t        schedulerStatistic;
  Histogram_t        mutatorHistogram;
  Statistic_t        gcStackStatistic;
  Statistic_t        gcGlobalStatistic;
  Statistic_t        gcNoneStatistic;
  Histogram_t        gcWorkHistogram;
  Histogram_t        gcMajorWorkHistogram;
  Histogram_t        gcFlipOffHistogram;
  Histogram_t        gcFlipOnHistogram;

  Stack_t            *roots;                  /* Stack, global root locations containing root values */
  Stack_t            *rootVals;               /* Used by incremental collector for breaking up initial root processing 
						      and by generational for ptr array allocation */
  Stack_t            *primaryReplicaObjFlips; /* PR objects whose locs must be flipped when minor collector goes off - allocated ptr array */
  Stack_t            *primaryReplicaLocRoots; /* Locations of PRs whose values serve as roots - modified ptr array loc */
  Stack_t            *primaryReplicaLocFlips; /* Locations of PRs which need to be flipped when collectors goes off - modified ptr array loc */

  CopyRange_t        minorRange;   /* Used only by generational collector */
  CopyRange_t        majorRange;

  long               numCopied;        /* Number of objects copied */
  long               numShared;        /* Number of times an object is reached after it's already been forwarded */
  long               numContention;    /* Number of failed (simultaneous) attempts to copy an object */
  long               numWrite;
  long               numRoot;
  long               numLocative;
} Proc_t;

void procChangeState(Proc_t *, ProcessorState_t);

long updateWorkDone(Proc_t *proc);
long getWorkDone(Proc_t *proc);
Thread_t *getThread(void);
Proc_t *getProc(void);
Proc_t *getNthProc(int);

extern pthread_mutex_t ScheduleLock;       /* locks (de)scheduling of sys threads */
void ResetJob(void);                       /* For iterating over all jobs in work list */
Thread_t *NextJob(void);
void StopAllThreads(void);                 /* Change all user thread's limit to StopHeapLimit */

void thread_init(void);
void thread_go(ptr_t *thunks, int numThunk);
void Interrupt(struct ucontext *);
void scheduler(Proc_t *);                  /* Unmap user thread of Proc if mapped */
void Finish(void);
Thread_t *YieldRest(void);
void ReleaseJob(Proc_t *);

int thread_total(void);
int thread_max(void);

extern Thread_t    *Threads;
Thread_t *mainThread;

#endif
#endif
