/* exn_init should be called once; raise_exception is thread-safe */

#ifndef _exn_h
#define _exn_h

void exn_init(void);
extern ptr_t divide_exn;
extern ptr_t overflow_exn;
void raise_exception(struct ucontext *, ptr_t);

#endif
