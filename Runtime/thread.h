#define reg_disp       0
#ifdef alpha_osf
#define doublesize     8
#define longsize       8
#define ptrsize        8
#endif
#ifdef solaris
#define doublesize     8
#define longsize       4
#define ptrsize        4
#endif
#define maxsp_disp     longsize*32+8*32
#define snapshot_disp  longsize*32+8*32+longsize
#define sysThread_disp longsize*32+8*32+longsize+ptrsize
#define notinml_disp   longsize*32+8*32+longsize+ptrsize+ptrsize
#define scratch_disp   longsize*32+8*32+longsize+ptrsize+ptrsize+longsize
#define thunk_disp     longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize
#define nextThunk_disp longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize
#define numThunk_disp  longsize*32+8*32+longsize+ptrsize+ptrsize+longsize+doublesize+ptrsize+longsize

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
   Request:
     0 or more: allocating specified number of bytes;l; 
                must check that this is satisfied before resumption
     -1 : Yield was called		
*/

/* decalpha.sml, sparc.sml, and the ***_disp above have to be changed if the fields are modified 
   Note that long is used to avoid padding problems.
*/
struct Thread__t
{
  long               saveregs[32];     /* Register set; compiler relied on this being first */
  double             fregs[32];        /* Register set; compiler relied on this being second */
  long               maxSP;            /* Used by mutator exn handler; compiler relies on this third */
  StackSnapshot_t    *snapshots;       /* Used by stack.c and stack_asm.s */
  struct SysThread__t *sysThread;      /* of type SysThread_t * - relied on by service_alpha_osf.s  */
  long                notInML;          /* set to true whenever mutator calls a normal external function */
  double             scratch;
  value_t            *thunks;          /* Array of num_add unit -> unit */
  long               nextThunk;        /* Index of next unstarted thunk.  Initially zero. */
  long               numThunk;         /* Number of thunks.  At least one. */

  /* ---- The remaining fields not accessed by assembly code ---- */

  long               last_snapshot;    /* Index of last used snapshot */
  StackChain_t       *stackchain;      /* Stack */
  Queue_t            *retadd_queue;
  long                tid;              /* Thread ID */
  long                id;               /* Structure ID */
  long                status;           /* Thread status */
  long                request;          /* Why pre-empted? */
  struct Thread__t   *parent;
  value_t            oneThunk;         /* Avoid allocation by optimizing for common case */
  Queue_t            *reg_roots;
  Queue_t            *root_lists;
  Queue_t            *loc_roots;
};

typedef struct Thread__t Thread_t;

struct SysThread__t
{
  int                stack;        /* address of system thread stack that can be used to enter scheduler */
  int                stid;         /* sys thread id */
  int                alloc;        /* allocation pointer */
  int                limit;        /* allocation limit */
  int                processor;    /* processor id that this pthread is bound to */
  pthread_t          pthread;      /* pthread that this system thread is implemented as */
  Thread_t           *userThread;  /* current user thread mapped to this system thread */
  value_t            LocalStack[1024];  /* Used by parallel collector */
  int                LocalCursor;
  int temp;
  timer_mt           gctime;
  timer_mt           stacktime;
  timer_mt           majorgctime;
};

typedef struct SysThread__t SysThread_t;

Thread_t *getThread(void);
SysThread_t *getSysThread(void);

extern pthread_mutex_t ScheduleLock;       /* locks (de)scheduling of sys threads */
void ResetJob(void);     /* For iterating over all jobs in work list */
Thread_t *NextJob(void);

void thread_init(void);
void thread_go(value_t *thunks, int numThunk);
void Interrupt(struct ucontext *);
void scheduler(void);
void Finish(void);
Thread_t *YieldRest(void);

int thread_total(void);
int thread_max(void);

extern Thread_t    *Threads;

#endif
#endif
