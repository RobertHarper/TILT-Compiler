/* Thread-safe */

#ifndef _til_signal_h
#define _til_signal_h

#include <ucontext.h>

void signal_init(void);
long GetSp(struct ucontext *);
long GetPc(struct ucontext *);
long GetIReg(struct ucontext *, int);
void GetIRegs(struct ucontext *, long *);
void SetIReg(struct ucontext *, int, long);

#endif
