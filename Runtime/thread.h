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


/* These types should be in stack.h but mutually recusrive headers are broken. */
struct StackSnapshot           /* the size and layout of this structure affects the code in stack_asm.s */
{
  val_t saved_ra;              /* Real return address for this stub */
  val_t saved_sp;              /* Stack pointer position where stub was inserted */
  unsigned int saved_regstate; /* Register state (mask) at this point */
  Queue_t *roots;              /* Roots between this stub and the one above it */
};

typedef struct StackSnapshot StackSnapshot_t;


/* These types should be in forward.h but mutually recusrive headers are broken. */
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
  struct Proc__t *sth;
} CopyRange_t;


/* Finally, these types actually do belong here */
/* Thread Status:
     -1 : Thread is done
     0 : Thread is ready to be scheduled
     1 or more : Thread is running already or is blocked; not eligible to be run
*/
     
/* decalpha.sml, sparc.sml, and the ***_disp above have to be changed if the fields are modified 
   Note that long is used to avoid padding problems.
*/
struct Thread__t
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
  Queue_t            *retadd_queue;
  long                tid;             /* Thread ID */
  long                id;              /* Structure ID */
  long                status;          /* Thread status */
  struct Thread__t   *parent;
  ptr_t              oneThunk;         /* Avoid allocation by optimizing for common case */
  Queue_t            *reg_roots;
};

typedef struct Thread__t Thread_t;

typedef struct LocalStack__t
{
  ptr_t stack[1024];       /* stack contains gray objects */
  long cursor;
} LocalStack_t;


/* Note that Scheduler, Mutator, and GC are disjoint whereas GCStack, GCGlobal, and GCMajor are all within GC */
enum ProcessorState {Scheduler, Mutator, GC, GCStack, GCGlobal, GCMajor};

struct Proc__t
{
  int                stack;        /* address of system thread stack that can be used to enter scheduler */
  int                stid;         /* sys thread id */
  mem_t              allocStart;     /* allocation range */
  mem_t              allocCursor;
  mem_t              allocLimit;      
  ploc_t             writelistStart;  /* write list range */
  ploc_t             writelistCursor;
  ploc_t             writelistEnd;
  ptr_t              writelist[4096];
  int                processor;    /* processor id that this pthread is bound to */
  pthread_t          pthread;      /* pthread that this system thread is implemented as */
  Thread_t           *userThread;  /* current user thread mapped to this system thread */
  LocalStack_t       localStack;   /* Used by parallel collector */
  int temp;

  Timer_t            totalTimer;     /* Time spent in entire processor */
  Timer_t            currentTimer;   /* Time spent running any subtask */
  int                lastGCStatus;   /* Needed to distinguish between gcOff and gcOn */
  double             lastGCdiff;     /* Needed to remember time in one GC across substates like GCStack, GCGlobal, GCMajor */
  enum ProcessorState state;         /* What the processor is working on */
  Statistic_t        schedulerStatistic;
  Statistic_t        mutatorStatistic;
  Statistic_t        gcOffStatistic;
  History_t          gcOnHistory;
  Histogram_t        gcOnHistogram;
  Statistic_t        gcStackStatistic;
  Statistic_t        gcGlobalStatistic;
  Statistic_t        gcMajorStatistic;

  Queue_t            *root_lists;
  Queue_t            *largeRoots;  /* contains pointers into large-pointerless-object area */

  CopyRange_t        minorRange;   /* Used only by generational collector */
  CopyRange_t        majorRange;

  long               numCopied;        /* Number of objects copied */
  long               numShared;        /* Number of times an object is reached after it's already been forwarded */
  long               numContention;    /* Number of failed (simultaneous) attempts to copy an object */
  long               numWrite;
  long               numRoot;
  long               numLocative;
  long               bytesAllocated;
  long               kbytesAllocated;
  long               bytesCopied;
  long               kbytesCopied;
};

typedef struct Proc__t Proc_t;

void procChangeState(Proc_t *, enum ProcessorState);

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
void ReleaseJob(Proc_t *sth);

int thread_total(void);
int thread_max(void);

extern Thread_t    *Threads;

#endif
#endif
