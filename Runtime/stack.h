#ifndef _stack_h
#define _stack_h

#include "tag.h"
#include "queue.h"

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


/* the size and layout of this structure affects the code in stack_asm.s */
struct StackSnapshot
{
  value_t saved_ra;            /* Real return address for this stub */
  value_t saved_sp;            /* Stack pointer position where stub was inserted */
  unsigned int saved_regstate; /* Register state (mask) at this point */
  Queue_t *roots;              /* Roots between this stub and the one above it */
};

typedef struct StackSnapshot StackSnapshot_t;

#define NUM_STACK_STUB 200
#define _inside_stack_h
#include "thread.h"

void show_stack(value_t sp, value_t cur_retadd, value_t top);

void stack_init(void);
void global_root_scan(Queue_t *global_roots, Queue_t *promoted_global_roots);
void local_root_scan(Thread_t *);
void stub_error(void);

extern long GlobalTableSize;
extern long MutableTableSize;
extern long CodeSize;
extern long GCTableSize;
extern long SMLGlobalSize;
extern long TotalStackDepth, MaxStackDepth, TotalStackSize, TotalNewStackDepth;
#endif

