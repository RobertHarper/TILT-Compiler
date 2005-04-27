/* ../../Basis/Posix/posix-process.sml */

#include "s.h"
#include "r.h"
#include "sparc.h"

int
posix_process_fork(cerr er)
{
	Proc_t* self = getProc();
	pid_t pid = fork();
	if(pid == (pid_t)-1)
		send_errno(er,errno);
	else if(pid == (pid_t)0) {
		/* Child needs to reset pthread value. */
		self->pthread = pthread_self();
	}
	return pid;
}
