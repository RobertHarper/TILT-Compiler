#ifndef _stack_h
#define _stack_h

#include "tag.h"
#include "queue.h"
#include "thread.h"

#define TRACE_NO              0
#define TRACE_YES             1
#define TRACE_CALLEE          2
#define TRACE_SPECIAL         3
#define IS_TRACE_NO(trace)      ((trace) == TRACE_NO)
#define IS_TRACE_YES(trace)     ((trace) == TRACE_YES)
#define IS_TRACE_CALLEE(trace)  ((trace) == TRACE_CALLEE)
#define IS_TRACE_SPECIAL(trace) ((trace) == TRACE_SPECIAL)

#define IS_SPECIAL_STACK(type)      ((type) == 0)
#define IS_SPECIAL_GLOBAL(type)     ((type) == 1)
#define IS_SPECIAL_STACK_REC(type)  ((type & 3) == 2)
#define IS_SPECIAL_GLOBAL_REC(type) ((type & 3) == 3)
#define IS_SPECIAL_UNSET(type)      (((signed int)type) == -1)


#define GET_SPECIAL_STACK_REC_POS(type) ((type >> 2) & 63)
#define GET_SPECIAL_STACK_REC_POS2(type) ((type >> (2 + 6)) & 255)
#define GET_SPECIAL_STACK_REC_POS3(type) ((type >> (2 + 6 + 8)) & 255)
#define GET_SPECIAL_STACK_REC_POS4(type) ((type >> (2 + 6 + 8 + 8)) & 255)

#define GET_SPECIAL_STACK_GLOBAL_POS(type)  GET_SPECIAL_STACK_REC_POS(type)
#define GET_SPECIAL_STACK_GLOBAL_POS2(type) GET_SPECIAL_STACK_REC_POS2(type)
#define GET_SPECIAL_STACK_GLOBAL_POS3(type) GET_SPECIAL_STACK_REC_POS3(type)
#define GET_SPECIAL_STACK_GLOBAL_POS4(type) GET_SPECIAL_STACK_REC_POS4(type) 

#define NUM_STACK_STUB 200

void show_stack(mem_t sp, mem_t cur_retadd, mem_t top);

void stack_init(void);
void do_global_work(Proc_t *proc, int workToDo);  /* Do some global work */
void minor_global_scan(Proc_t *);    /* Return all initialized global locs not in the tenured list */
void minor_global_promote();         /* Move initialized global locs into the tenured list */
void major_global_scan(Proc_t *);
void local_root_scan(Proc_t *, Thread_t *);
void stub_error(void);

extern int useGenStack;
extern long GlobalTableSize;
extern long MutableTableSize;
extern long CodeSize;
extern long GCTableSize;
extern long SMLGlobalSize;
extern long TotalStackDepth, MaxStackDepth, TotalStackSize, TotalNewStackDepth;

#endif
