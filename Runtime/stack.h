#ifndef _stack_h
#define _stack_h

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
#define GET_SPECIAL_STACK_REC_POS2(type) ((type >> 8) & 63)
#define GET_SPECIAL_STACK_REC_POS3(type) ((type >> 14) & 63)
#define GET_SPECIAL_STACK_REC_POS3(type) ((type >> 20) & 63)

#define GET_SPECIAL_STACK_GLOBAL_POS(type) ((type >> 2) & 63)
#define GET_SPECIAL_STACK_GLOBAL_POS2(type) ((type >> 8) & 63)
#define GET_SPECIAL_STACK_GLOBAL_POS3(type) ((type >> 14) & 63)
#define GET_SPECIAL_STACK_GLOBAL_POS3(type) ((type >> 20) & 63)


void show_stack(value_t sp, value_t cur_retadd, value_t top);
unsigned int trace_stack(unsigned long *saveregs,
			 value_t sp, value_t cur_retadd, value_t top, Queue_t *roots);

void stack_init();
long StackError(long badadd, long sp);

#endif

