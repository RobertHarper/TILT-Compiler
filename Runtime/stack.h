#ifndef _stack_h
#define _stack_h

#include "tag.h"
#include "queue.h"
#include "thread.h"
#include "gc.h"

#define TRACE_NO              0
#define TRACE_YES             1
#define TRACE_CALLEE          2
#define TRACE_SPECIAL         3
#define IS_TRACE_NO(trace)      ((trace) == TRACE_NO)
#define IS_TRACE_YES(trace)     ((trace) == TRACE_YES)
#define IS_TRACE_CALLEE(trace)  ((trace) == TRACE_CALLEE)
#define IS_TRACE_SPECIAL(trace) ((trace) == TRACE_SPECIAL)

/* These numbers must match up with tracetable.sml. */
#define GET_SPECIAL_TYPE(x)         (((x) >> 30) & 3)
#define IS_SPECIAL_STACK_REC(type)  ((type) == 0)
#define IS_SPECIAL_LABEL_REC(type)  ((type) == 1)
#define IS_SPECIAL_GLOBAL_REC(type) ((type) == 2)
#define IS_SPECIAL_UNSET(type)      ((type) == 3)

void show_stack(mem_t sp, mem_t cur_retadd, mem_t top);
void stack_init(void);
void add_global_root(Proc_t *proc, volatile mem_t global);
void minor_global_scan(Proc_t *);    /* Return all initialized global locs not in the tenured list */
void minor_global_promote(Proc_t *);         /* Move initialized global locs into the tenured list */
void major_global_scan(Proc_t *);
void stub_error(void);
void NullGlobals(int globalOffset);           /* For debugging - pass in primary or replica globalOffset */


void thread_root_scan(Proc_t *, Thread_t *);  /* Scan all root locations in primaryStack */

void initial_root_scan(Proc_t *, Thread_t *); /* Initialize snaphosts - returns 1 if thread as started */
int work_root_scan(Proc_t *, Stacklet_t *); /* Obtain roots from startStack; returns one when stacklet complete */
void discard_root_scan(Proc_t *, Thread_t *);  /* Clean up thread but don't do flip - for CollectorTransition */
void complete_root_scan(Proc_t *, Thread_t *); /* Perform flip */

extern int useGenStack;
extern long GlobalTableSize;
extern long MutableTableSize;
extern long CodeSize;
extern long GCTableSize;
extern long SMLGlobalSize;
extern long TotalStackDepth, MaxStackDepth, TotalStackSize, TotalNewStackDepth;

#pragma INLINEP(DupGlobal)
static INLINE
ploc_t DupGlobal(vptr_t global)
{
  ploc_t primaryLoc = (ploc_t) &global[primaryGlobalOffset / sizeof(val_t)];
  ploc_t replicaLoc = (ploc_t) &global[replicaGlobalOffset / sizeof(val_t)];
  *replicaLoc = *primaryLoc;
  return replicaLoc;
}

void installThreadRoot(Thread_t *th, vploc_t rootLoc);
void uninstallThreadRoot(Thread_t *th, vploc_t rootLoc);

#endif
