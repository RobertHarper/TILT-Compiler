/* exn_init should be called once; raise_exception is thread-safe */

#ifndef _exn_h
#define _exn_h

void exn_init();
typedef value_t exn;
extern exn divide_exn;
extern exn overflow_exn;
void raise_exception(struct sigcontext *, exn);

#endif
