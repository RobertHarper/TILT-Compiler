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

#define IS_SPECIAL_STACK_REC(type)  ((type & 3) == 0)
#define IS_SPECIAL_LABEL_REC(type)  ((type & 3) == 1)
#define IS_SPECIAL_GLOBAL_REC(type) ((type & 3) == 2)
#define IS_SPECIAL_UNSET(type)      ((type & 3) == 3)

/* A pos of zero indicates end of projection; otherwise, subtract one to get true word offset */
#define GET_SPECIAL_REC_POS(type) ((type >> 2) & 63)
#define GET_SPECIAL_REC_POS2(type) ((type >> (2 + 6)) & 255)
#define GET_SPECIAL_REC_POS3(type) ((type >> (2 + 6 + 8)) & 255)
#define GET_SPECIAL_REC_POS4(type) ((type >> (2 + 6 + 8 + 8)) & 255)

void show_stack(mem_t sp, mem_t cur_retadd, mem_t top);

void stack_init(void);
void do_global_work(Proc_t *proc, int workToDo);  /* Do some global work */
void minor_global_scan(Proc_t *);    /* Return all initialized global locs not in the tenured list */
void minor_global_promote(Proc_t *);         /* Move initialized global locs into the tenured list */
void major_global_scan(Proc_t *);
void stub_error(void);
void NullGlobals(int globalOffset);           /* For debugging - pass in primary or replica globalOffset */
val_t GetGlobal(ptr_t);

void thread_root_scan(Proc_t *, Thread_t *);  /* Scan all root locations in primaryStack */

Thread_t *initial_root_scan(Proc_t *, Thread_t *); /* Initialize snaphosts */
int work_root_scan(Proc_t *, Thread_t *, int workToDo); /* Obtain roots from startStack; returns one when stack complete */
void complete_root_scan(Proc_t *, Thread_t *); /* Perform flip */

extern int useGenStack;
extern long GlobalTableSize;
extern long MutableTableSize;
extern long CodeSize;
extern long GCTableSize;
extern long SMLGlobalSize;
extern long TotalStackDepth, MaxStackDepth, TotalStackSize, TotalNewStackDepth;

INLINE1(DupGlobal)
INLINE2(DupGlobal)
ploc_t DupGlobal(ptr_t global)
{
  ploc_t primaryLoc = (ploc_t) &global[primaryGlobalOffset / sizeof(val_t)];
  ploc_t replicaLoc = (ploc_t) &global[replicaGlobalOffset / sizeof(val_t)];
  *replicaLoc = *primaryLoc;
  return replicaLoc;
}

#endif
