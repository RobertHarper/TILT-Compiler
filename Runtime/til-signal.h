/* Thread-safe */

#ifndef _til_signal_h
#define _til_signal_h

#include <ucontext.h>

void signal_init(void);
void install_signal_handlers(int isMain);
mem_t GetSp(struct ucontext *);
mem_t GetPc(struct ucontext *);
unsigned long GetIReg(struct ucontext *, int);
void GetIRegs(struct ucontext *, unsigned long *);
void SetIReg(struct ucontext *, int, unsigned long);

#endif
