#ifndef _thread_h
#define _thread_h

#include "tag.h"
#include "memobj.h"
#include <signal.h>

#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif

extern char RuntimeGlobalData[];


struct Thread__t
{
  long               saveregs[32];     /* Register set; compiler relied on this being first */
  double             fregs[32];        /* Register set; compiler relied on this being second */
  long               savedHeapLimit;   /* Contains real heap limit when hap limit is used for preemption */
  StackChainObj_t   *stackchain;       /* Stack */
  int                tid;              /* Thread ID */
  int                status;           /* Thread status */
  value_t            start_address;    /* Array of num_add unit -> unit */
  int                num_add;          /* IF zero, then start_address is a thunk */
};

typedef struct Thread__t Thread_t;

struct SysThread__t
{
  pthread_t          pthread;      /* pthread that this system thread is implemented as */
  Thread_t           *userThread;  /* current user thread mapped to this system thread */
  int                stid;         /* sys thread id */
};

typedef struct SysThread__t SysThread_t;


Thread_t *thread_create(value_t start_adds, int num_add);
Thread_t *getThread();
void thread_insert(Thread_t *th);
void thread_go();

void thread_scheduler(struct sigcontext *scp);
void thread_scheduler_clean();



#endif

