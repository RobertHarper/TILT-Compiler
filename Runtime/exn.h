/* exn_init should be called once; raise_exception is thread-safe */

#ifndef _exn_h
#define _exn_h

void exn_init(void);
void raise_exception(struct ucontext *, ptr_t);

#endif
