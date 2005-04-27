/* ../../Basis/Posix/posix-process.sml */

#include "s.h"
#include "r.h"

int
posix_process_fork(cerr er)
{
	pid_t pid = fork();
	if(pid == (pid_t)-1)
		send_errno(er,errno);
	return pid;
}
