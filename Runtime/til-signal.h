/* Thread-safe */

#ifndef _til_signal_h
#define _til_signal_h

#include <signal.h>

void signal_init(void);
long *GetSp(struct sigcontext *scp);
long *GetPc(struct sigcontext *scp);
long *GetIRegs(struct sigcontext *scp);

#endif
