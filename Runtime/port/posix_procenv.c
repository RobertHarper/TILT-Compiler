/* ../../Basis/Posix/posix-procenv.sml */

#include "s.h"
#include "r.h"
#include <sys/utsname.h>
#include <time.h>
#include <sys/times.h>

int
posix_procenv_getpid(unit unused)
{
	return (int)getpid();	/* never fails */
}

int
posix_procenv_getppid(unit unused)
{
	return (int)getppid();	/* never fails */
}

word
posix_procenv_getuid(unit unused)
{
	return (word)getuid();	/* never fails */
}

word
posix_procenv_geteuid(unit unused)
{
	return (word)geteuid();	/* never fails */
}

word
posix_procenv_getgid(unit unused)
{
	return (word)getgid();	/* never fails */
}

word
posix_procenv_getegid(unit unused)
{
	return (word)getegid();	/* never fails */
}

/*unit*/cresult
posix_procenv_setuid(word uid)
{
	return unit_cresult(setuid((uid_t)uid));
}

/*unit*/cresult
posix_procenv_setgid(word gid)
{
	return unit_cresult(setgid((gid_t)gid));
}

/*word_list*/cresult
posix_procenv_getgroups(unit unused)
{
	gid_t grouplist[NGROUPS_MAX];
	int ngroups = getgroups(NGROUPS_MAX, grouplist);
	if(ngroups == -1)
		return Error(SysErr(errno));
	else {
		int i;
		ptr_t groups = 0;	/* nil */
		for (i=ngroups-1; i>=0; i--) {
			val_t car = (val_t)(word)grouplist[i];
			groups = cons_int_alloc(car, groups);
		}
		return NormalPtr(groups);
	}
}

/*string*/cresult
posix_procenv_getlogin(unit unused)
{
	char* cname = getlogin();
	if(cname == NULL)
		return Error(SysErr(errno));
	else {
		string name = cstring2mlstring_alloc(cname);
		return NormalPtr(name);
	}
}

int
posix_procenv_getpgrp(unit unused)
{
	return (int)getpgrp();	/* never fails */
}

/*int*/cresult
posix_procenv_setsid(unit unused)
{
	pid_t pid = setsid();
	if(pid == (pid_t)-1)
		return Error(SysErr(errno));
	else
		return Normal(pid);
}

/*unit*/cresult
posix_procenv_setpgid(int pid, int pgid)
{
	return unit_cresult(setpgid((pid_t)pid, (pid_t)pgid));
}

/*string_stringlist*/cresult
posix_procenv_uname(unit unused)
{
	struct utsname name;
	if(uname(&name) == -1)
		return Error(SysErr(errno));
	else {
		ptr_t acc = NULL;

		#define ACC(n,v) acc = cons_ptr_alloc(stringpair_ctoml_alloc(n, v), acc);

		ACC("sysname", name.sysname);
		ACC("nodename", name.nodename);
		ACC("release", name.release);
		ACC("version", name.version);
		ACC("machine", name.machine);

		#undef ACC
		return NormalPtr(acc);
	}
}

/*int*/cresult
posix_procenv_time(unit unused)
{
	time_t timeval = time(NULL);
	if(timeval == (time_t)-1)
		return Error(SysErr(errno));
	else
		return Normal(timeval);
}

/*
	Timesrep must agree with type timesrep
	in ../../Basis/externtys.sml
*/

static ptr_t
Timesrep(clock_t realtime, struct tms* tms)
{
	val_t fields[5];
	fields[0] = (val_t) realtime;
	fields[1] = (val_t) tms->tms_utime;
	fields[2] = (val_t) tms->tms_stime;
	fields[3] = (val_t) tms->tms_cutime;
	fields[4] = (val_t) tms->tms_cstime;
	return alloc_record(fields, 0, arraysize(fields));
}

/*timesrep*/cresult
posix_procenv_times(unit unused)
{
	struct tms tms;
	clock_t realtime = times(&tms);
	if(realtime == (clock_t)-1)
		return Error(SysErr(errno));
	else
		return NormalPtr(Timesrep(realtime, &tms));
}

string_option
posix_procenv_getenv(string mlname)
{
	char* cname = mlstring2cstring_static(mlname);
	char* cvalue = getenv(cname);
	return alloc_string_option(cvalue);
}

string_list
posix_procenv_environ(unit unused)
{
	extern char** environ;
	return array_to_string_list(environ);
}

/*string*/cresult
posix_procenv_ctermid(unit unused)
{
	char name[L_ctermid];
	if(ctermid(name) == NULL)
		return Error(SysErr(errno));
	else
		return NormalPtr(cstring2mlstring_alloc(name));
}

/*string*/cresult
posix_procenv_ttyname(int fd)
{
	char* name = ttyname(fd);
	if(name == NULL)
		return Error(SysErr(errno));
	else
		return NormalPtr(cstring2mlstring_alloc(name));
}

bool
posix_procenv_isatty(int fd)
{
	return isatty(fd) ? true : false;
}
