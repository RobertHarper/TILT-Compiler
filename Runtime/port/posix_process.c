/* ../../Basis/Posix/posix-process.sml */

#include "s.h"
#include "r.h"
#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>

static char Ewaitpid[] = "posix_process_waitpid: bad status";
static char Eprocnum[] = "posix_process_num: bad name";

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

int
posix_process_num(cerr er, string key)
{
	return sysval_num(er, Eprocnum, &process_values, key);
}

int
posix_process_exec(string mlpath, string_list args)
{
	char* cpath = mlstring2cstring_static(mlpath);
	char** argv = string_list_to_array_malloc(args);
	int e;
	(void)execv(cpath, argv);
	e = errno;
	efree(argv);
	return e;
}

int
posix_process_exece(string mlpath, string_list args, string_list envs)
{
	char* cpath = mlstring2cstring_static(mlpath);
	char** argv = string_list_to_array_malloc(args);
	char** envp = string_list_to_array_malloc(envs);
	int e;
	(void)execve(cpath, argv, envp);
	e = errno;
	efree(argv);
	efree(envp);
	return e;
}

int
posix_process_execp(string mlpath, string_list args)
{
	char* cpath = mlstring2cstring_static(mlpath);
	char** argv = string_list_to_array_malloc(args);
	int e;
	(void)execvp(cpath,argv);
	e = errno;
	efree(argv);
	return e;
}

ptr_t	/*waitpidrep*/
posix_process_waitpid(cerr er, int argpid, word options)
{
	int status;
	pid_t pid = waitpid(argpid, &status, options);
	int how=0, val=0;
	val_t fields[3];

	if(pid == (pid_t)-1)
		send_errno(er,errno);
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
		send_errmsg(er,Ewaitpid);

	fields[0] = (val_t) pid;
	fields[1] = (val_t) how;
	fields[2] = (val_t) val;
	return alloc_record(fields, arraysize(fields));
}

int
posix_process_exit(word8 status)
{
	exit(status);
	return errno;
}

unit
posix_process_kill(cerr er, int pid, int sig)
{
	if(kill((pid_t)pid, sig) == -1)
		send_errno(er,errno);
	return empty_record;
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
