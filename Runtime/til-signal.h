#include <signal.h>

long *GetSp(struct sigcontext *scp);
long *GetPc(struct sigcontext *scp);
long *GetIRegs(struct sigcontext *scp);
