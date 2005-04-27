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

unit
posix_procenv_setuid(cerr er, word uid)
{
	if(setuid((uid_t)uid) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_procenv_setgid(cerr er, word gid)
{
	if(setgid((gid_t)gid) == -1)
		send_errno(er,errno);
	return empty_record;
}

typedef struct groupsrep* groupsrep;
struct groupsrep{
	int ngroups;
	gid_t* groups;
};

uct
posix_procenv_getgroups(cerr er)
{
	int maxgroups = NGROUPS_MAX + 1;	/* +1 for egid */
	gid_t* groups = (gid_t*)emalloc(maxgroups * sizeof(gid_t));
	int ngroups = getgroups(maxgroups,groups);
	if(ngroups == -1){
		send_errno(er,errno);
		efree(groups);
		return 0;
	}
	else{
		groupsrep rep = enew(struct groupsrep);
		rep->ngroups = ngroups;
		rep->groups = (gid_t*)erealloc(groups,ngroups * sizeof(gid_t));
		return (uct)rep;
	}
}

int
posix_procenv_getgroups_size(uct p)
{
	groupsrep rep = (groupsrep)p;
	return rep->ngroups;
}

word
posix_procenv_getgroups_nth(uct p, int n)
{
	groupsrep rep = (groupsrep)p;
	return rep->groups[n];
}

unit
posix_procenv_getgroups_free(uct p)
{
	groupsrep rep = (groupsrep)p;
	efree(rep->groups);
	efree(rep);
	return empty_record;
}

string
posix_procenv_getlogin(cerr er)
{
	char* cname = getlogin();
	string name;
	if(cname == NULL){
		send_errno(er,errno);
		*cname = 0;
	}
	name = cstring2mlstring_alloc(cname);
	return name;
}

int
posix_procenv_getpgrp(unit unused)
{
	return (int)getpgrp();	/* never fails */
}

int
posix_procenv_setsid(cerr er)
{
	pid_t pid = setsid();
	if(pid == (pid_t)-1)
		send_errno(er,errno);
	return pid;
}

unit
posix_procenv_setpgid(cerr er, int pid, int pgid)
{
	if(setpgid((pid_t)pid, (pid_t)pgid) == -1)
		send_errno(er,errno);
	return empty_record;
}

uct
posix_procenv_uname(cerr er)
{
	struct utsname* name = enew_atomic(struct utsname);
	if(uname(name) == -1){
		send_errno(er,errno);
		efree(name);
		name = 0;
	}
	return (uct)name;
}

string
posix_procenv_uname_sysname(uct p)
{
	struct utsname* name = (struct utsname*)p;
	return cstring2mlstring_alloc(name->sysname);
}

string
posix_procenv_uname_nodename(uct p)
{
	struct utsname* name = (struct utsname*)p;
	return cstring2mlstring_alloc(name->nodename);
}

string
posix_procenv_uname_release(uct p)
{
	struct utsname* name = (struct utsname*)p;
	return cstring2mlstring_alloc(name->release);
}

string
posix_procenv_uname_version(uct p)
{
	struct utsname* name = (struct utsname*)p;
	return cstring2mlstring_alloc(name->version);
}

string
posix_procenv_uname_machine(uct p)
{
	struct utsname* name = (struct utsname*)p;
	return cstring2mlstring_alloc(name->machine);
}

unit
posix_procenv_uname_free(uct p)
{
	struct utsname* name = (struct utsname*)p;
	efree(name);
	return empty_record;
}

int
posix_procenv_time(cerr er)
{
	time_t timeval = time(NULL);
	if(timeval == (time_t)-1)
		send_errno(er,errno);
	return timeval;
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
	return alloc_record(fields, arraysize(fields));
}

ptr_t	/*timesrep*/
posix_procenv_times(cerr er)
{
	struct tms tms;
	clock_t realtime = times(&tms);
	if(realtime == (clock_t)-1)
		send_errno(er,errno);
	return Timesrep(realtime, &tms);
}

string_option
posix_procenv_getenv(string mlname)
{
	char* cname = mlstring2cstring_static(mlname);
	char* cvalue = getenv(cname);
	return alloc_string_option(cvalue);
}

int
posix_procenv_environ_size(unit unused)
{
	extern char** environ;
	int n;
	for(n=0; environ[n]; n++)
		;
	return n;
}

string
posix_procenv_environ_nth(int i)
{
	extern char** environ;
	return cstring2mlstring_alloc(environ[i]);
}

string
posix_procenv_ctermid(cerr er)
{
	char name[L_ctermid];
	if(ctermid(name) == NULL){
		send_errno(er,errno);
		*name = 0;
	}
	return cstring2mlstring_alloc(name);
}

string
posix_procenv_ttyname(cerr er, int fd)
{
	char* name = ttyname(fd);
	if(name == NULL){
		send_errno(er,errno);
		*name = 0;
	}
	return cstring2mlstring_alloc(name);
}

bool
posix_procenv_isatty(int fd)
{
	return isatty(fd) ? true : false;
}
