#define reg_disp       0
#ifdef alpha_osf
#define doublesize     8
#define longsize       8
#define ptrsize        4  /* We use -xtaso_short to accomplish this */
#endif
#ifdef solaris
#define doublesize     8
#define longsize       4
#define ptrsize        4
#endif
#define MLsaveregs_disp 0
#define maxsp_disp     longsize*32+8*32
#define snapshot_disp  longsize*32+8*32+longsize
#define sysThread_disp longsize*32+8*32+longsize+ptrsize
#define notinml_disp   longsize*32+8*32+longsize+ptrsize+ptrsize
#define scratch_disp   longsize*32+8*32+longsize+ptrsize+ptrsize+longsize
#define thunk_disp     longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize
#define nextThunk_disp longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize
#define numThunk_disp  longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize+longsize
#define request_disp   longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize+longsize+longsize
#define requestInfo_disp longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize+longsize+longsize+longsize
#define Csaveregs_disp  longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize+longsize+longsize+longsize+longsize+longsize
#define writelistAlloc_disp  longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize+longsize+longsize+longsize+longsize+longsize+32*longsize+32*doublesize
#define writelistLimit_disp  longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize+longsize+longsize+longsize+longsize+longsize+32*longsize+32*doublesize+ptrsize

#define NoRequest 0
#define YieldRequest 1
#define StartRequest 2
#define GCRequestFromML 3
#define GCRequestFromC 4
#define MajorGCRequestFromC 5

#ifndef _inside_stack_h
#ifndef _asm_
#include "stack.h"
#endif
#endif

#ifndef _thread_h
#define _thread_h
#ifndef _asm_

#include "memobj.h"
#include "stats.h"
#include <signal.h>


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
  struct SysThread__t *sysThread;      /* of type SysThread_t * - relied on by service_alpha_osf.s  */
  long               notInML;          /* set to true whenever mutator calls a normal external function */
  double             scratch;
  ptr_t              *thunks;          /* Array of num_add unit -> unit */
  long               nextThunk;        /* Index of next unstarted thunk.  Initially zero. */
  long               numThunk;         /* Number of thunks.  At least one. */
  long               request;          /* Why were we stoppped and how do we resume? */
  long               requestInfo;      /* If GC, how many bytes do we want? */
  long               filler;           /* must double align here */
  long               Csaveregs[32];    /* C register saved when we need to de-schedule while in a C function */
  double             Cfregs[32];        
  ploc_t             writelistAlloc;
  ploc_t             writelistLimit;

  /* ---- The remaining fields not accessed by assembly code ---- */
  long               last_snapshot;    /* Index of last used snapshot */
  StackChain_t       *stackchain;      /* Stack */
  Queue_t            *retadd_queue;
  long                tid;              /* Thread ID */
  long                id;               /* Structure ID */
  long                status;           /* Thread status */
  struct Thread__t   *parent;
  ptr_t              oneThunk;         /* Avoid allocation by optimizing for common case */
  Queue_t            *reg_roots;
};

typedef struct Thread__t Thread_t;

struct SysThread__t
{
  int                stack;        /* address of system thread stack that can be used to enter scheduler */
  int                stid;         /* sys thread id */
  mem_t              alloc;        /* allocation pointer */
  mem_t              limit;        /* allocation limit */
  int                processor;    /* processor id that this pthread is bound to */
  pthread_t          pthread;      /* pthread that this system thread is implemented as */
  Thread_t           *userThread;  /* current user thread mapped to this system thread */
  ptr_t              LocalStack[1024];  /* Used by parallel collector */
  int                LocalCursor;
  int temp;
  timer_mt           gctime;
  timer_mt           stacktime;
  timer_mt           majorgctime;
  ploc_t             writelistStart;
  ploc_t             writelistCursor;
  ploc_t             writelistEnd;
  ptr_t              writelist[1024];
  Queue_t            *root_lists;
  Queue_t            *largeRoots; /* contains pointers into large-pointerless-object area */
};

typedef struct SysThread__t SysThread_t;

Thread_t *getThread(void);
SysThread_t *getSysThread(void);

extern pthread_mutex_t ScheduleLock;       /* locks (de)scheduling of sys threads */
void ResetJob(void);                       /* For iterating over all jobs in work list */
Thread_t *NextJob(void);
void StopAllThreads();                     /* Change all user thread's limit to StopHeapLimit */

void thread_init(void);
void thread_go(ptr_t *thunks, int numThunk);
void Interrupt(struct ucontext *);
void scheduler(SysThread_t *); /* Unmap systhread if mapped */
void Finish(void);
Thread_t *YieldRest(void);
void ReleaseJob(SysThread_t *sth);

int thread_total(void);
int thread_max(void);

extern Thread_t    *Threads;

#endif
#endif
