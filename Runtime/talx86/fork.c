/* ../../Basis/Posix/posix-process.sml */

#include "s.h"
#include "r.h"

/*int*/cresult
posix_process_fork(unit unused)
{
	pid_t pid = fork();
	if(pid == (pid_t)-1)
		return Error(SysErr(errno));
	else
		return Normal(pid);
}
