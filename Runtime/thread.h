#define reg_disp       0
#ifdef alpha_osf
#define longsize       8
#define ptrsize        8
#endif
#ifdef solaris
#define longsize       4
#define ptrsize        4
#endif
#define maxsp_disp     longsize*32+8*32
#define snapshot_disp  longsize*32+8*32+longsize
#define sysThread_disp longsize*32+8*32+longsize+ptrsize
#define notinml_disp   longsize*32+8*32+longsize+ptrsize+ptrsize

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


struct Thread__t
{
  long               saveregs[32];     /* Register set; compiler relied on this being first */
  double             fregs[32];        /* Register set; compiler relied on this being second */
  long               maxSP;            /* Used by mutator exn handler; compiler relies on this third */
  StackSnapshot_t    *snapshots;       /* Used by stack.c and stack_asm.s */
  struct SysThread__t *sysThread;      /* of type SysThread_t * - relied on by service_alpha_osf.s  */
  int                notInML;          /* set to true whenever mutator calls a normal external function */
  int                last_snapshot;    /* Index of last used snapshot */
  StackChain_t       *stackchain;      /* Stack */
  Queue_t            *retadd_queue;
  int                tid;              /* Thread ID */
  int                id;               /* Structure ID */
  int                status;           /* Thread status */
  struct Thread__t   *parent;
  value_t            start_address;    /* Array of num_add unit -> unit */
  int                num_add;          /* IF zero, then start_address is a thunk */
  Queue_t            *reg_roots;
  Queue_t            *root_lists;
  Queue_t            *loc_roots;
  timer_mt           gctime;
  timer_mt           stacktime;
  timer_mt           majorgctime;
  int pad;
};

typedef struct Thread__t Thread_t;

struct SysThread__t
{
  int                stack;        /* address of system thread stack that can be used to enter scheduler */
  int                stid;         /* sys thread id */
  int                alloc;        /* allocation pointer */
  int                limit;        /* allocation limit */
  pthread_t          pthread;      /* pthread that this system thread is implemented as */
  Thread_t           *userThread;  /* current user thread mapped to this system thread */
  value_t            LocalStack[1024];  /* Used by parallel collector */
  int                LocalCursor;
};

typedef struct SysThread__t SysThread_t;

Thread_t *getThread(void);
SysThread_t *getSysThread(void);

extern pthread_mutex_t ScheduleLock;       /* locks (de)scheduling of sys threads */
void ResetJob(void);     /* For iterating over all jobs in work list */
Thread_t *NextJob(void);

void thread_init(void);
void thread_go(value_t start_adds, int num_add);
void Interrupt(struct ucontext *);
void scheduler(void);
void Finish(void);
Thread_t *YieldRest(void);

int thread_total(void);
int thread_max(void);

#endif
#endif
