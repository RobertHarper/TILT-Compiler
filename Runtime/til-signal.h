/* Thread-safe */

#ifndef _til_signal_h
#define _til_signal_h

#include <ucontext.h>

void signal_init(void);
void install_signal_handlers(int isMain);
mem_t GetSp(ucontext_t *);
mem_t GetPc(ucontext_t *);
unsigned long GetIReg(ucontext_t *, int);
void GetIRegs(ucontext_t *, unsigned long *);
void SetIReg(ucontext_t *, int, unsigned long);

#endif
