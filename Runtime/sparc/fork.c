/* ../../Basis/Posix/posix-process.sml */

#include "s.h"
#include "r.h"
#include "sparc.h"

/*int*/cresult
posix_process_fork(unit unused)
{
	Proc_t* self = getProc();
	pid_t pid = fork();
	if(pid == (pid_t)-1)
		return Error(SysErr(errno));
	else {
		if(pid == (pid_t)0) {
			/* Child needs to reset pthread value. */
			self->pthread = pthread_self();
		}
		return Normal(pid);
	}
}
