/* ../../Basis/Posix/posix-process.sml */

#include "s.h"
#include "r.h"
#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>

static struct sysval process_vec[] = {
	{"WNOHANG",	WNOHANG},
#ifdef WUNTRACED
	{"WUNTRACED",	WUNTRACED},
#endif
};

static struct sysvals process_values = {
	arraysize(process_vec),
	process_vec
};

/*int*/cresult
posix_process_num(string key)
{
	return sysval_num("posix_process_num", &process_values, key);
}

ptr_t	/* exn */
posix_process_exec(string mlpath, string_list args)
{
	char* cpath = mlstring2cstring_static(mlpath);
	char** argv = string_list_to_array_malloc(args);
	ptr_t exn;
	(void)execv(cpath, argv);
	exn = SysErr(errno);
	efree(argv);
	return exn;
}

ptr_t /* exn */
posix_process_exece(string mlpath, string_list args, string_list envs)
{
	char* cpath = mlstring2cstring_static(mlpath);
	char** argv = string_list_to_array_malloc(args);
	char** envp = string_list_to_array_malloc(envs);
	ptr_t exn;
	(void)execve(cpath, argv, envp);
	exn = SysErr(errno);
	efree(argv);
	efree(envp);
	return exn;
}

ptr_t /* exn */
posix_process_execp(string mlpath, string_list args)
{
	char* cpath = mlstring2cstring_static(mlpath);
	char** argv = string_list_to_array_malloc(args);
	ptr_t exn;
	(void)execvp(cpath,argv);
	exn = SysErr(errno);
	efree(argv);
	return exn;
}

/*waitpidrep*/cresult
posix_process_waitpid(int argpid, word options)
{
	int status;
	pid_t pid = waitpid(argpid, &status, options);
	int how, val;
	val_t fields[3];

	if(pid == (pid_t)-1)
		return Error(SysErr(errno));
	else if(WIFEXITED(status)) {
		how = 0;
		val = WEXITSTATUS(status);
	} else if(WIFSIGNALED(status)) {
		how = 1;
		val = WTERMSIG(status);
	} else if(WIFSTOPPED(status)) {
		how = 2;
		val = WSTOPSIG(status);
	} else
		return Error(SysErr_fmt("posix_process_waitpid: bad status: %d", status));

	fields[0] = (val_t) pid;
	fields[1] = (val_t) how;
	fields[2] = (val_t) val;
	return NormalPtr(alloc_record(fields, 0, arraysize(fields)));
}

ptr_t /* exn */
posix_process_exit(word8 status)
{
	exit(status);
	return SysErr(errno);
}

/*unit*/cresult
posix_process_kill(int pid, int sig)
{
	return unit_cresult(kill((pid_t)pid, sig));
}

int
posix_process_alarm(int sec)
{
	return alarm(sec);	/* can't fail */
}

unit
posix_process_pause(unit unused)
{
	(void) pause();	/* can't fail */
	return empty_record;
}

int
posix_process_sleep(int sec)
{
	return sleep(sec);	/* can't fail */
}
