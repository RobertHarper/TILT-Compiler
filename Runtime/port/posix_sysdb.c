/* ../../Basis/Posix/posix-sysdb.sml */

#include "s.h"
#include "r.h"
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>

typedef struct grouprep* grouprep;
struct grouprep{
	char*	name;
	char*	passwd;
	gid_t	gid;
	int	n;
	char**	mem;
};

static uct
Grouprep(struct group* group)
{
	int n, i;
	grouprep rep = enew(struct grouprep);
	rep->name = estrdup(group->gr_name);
	rep->passwd = estrdup(group->gr_passwd);
	rep->gid = group->gr_gid;
	for(n=0; group->gr_mem[n]; n++)
		;
	rep->n = n;
	rep->mem = (char**)emalloc(sizeof(char*) * n);
	for(i=0; i<n; i++)
		rep->mem[i] = estrdup(group->gr_mem[i]);
	return (uct)rep;
}

uct
posix_sysdb_getgrgid(cerr er, word gid)
{
	struct group* group;
	errno = 0;
	group = getgrgid((gid_t)gid);
	if(group == NULL){
		send_errno(er,errno);
		return 0;
	}
	return Grouprep(group);
}

uct
posix_sysdb_getgrnam(cerr er, string mlname)
{
	char* cname = mlstring2cstring_static(mlname);
	struct group* group;
	errno = 0;
	group = getgrnam(cname);
	if(group == NULL){
		send_errno(er,errno);
		return 0;
	}
	return Grouprep(group);
}

string
posix_sysdb_gr_name(uct p)
{
	grouprep rep = (grouprep)p;
	return cstring2mlstring_alloc(rep->name);
}

word
posix_sysdb_gr_gid(uct p)
{
	grouprep rep = (grouprep)p;
	return rep->gid;
}

int
posix_sysdb_gr_mem_size(uct p)
{
	grouprep rep = (grouprep)p;
	return rep->n;
}

string
posix_sysdb_gr_mem_nth(uct p, int n)
{
	grouprep rep = (grouprep)p;
	return cstring2mlstring_alloc(rep->mem[n]);
}

unit
posix_sysdb_gr_free(uct p)
{
	int i;
	grouprep rep = (grouprep)p;
	efree(rep->name);
	efree(rep->passwd);
	for(i=0; i<rep->n; i++)
		efree(rep->mem[i]);
	efree(rep->mem);
	efree(rep);
	return empty_record;
}

typedef struct passwdrep* passwdrep;
struct passwdrep{
	char*	name;
	uid_t	uid;
	gid_t	gid;
	char*	dir;
	char*	shell;
};

static uct
Passwdrep(struct passwd* passwd)
{
	passwdrep rep = enew(struct passwdrep);
	rep->name = estrdup(passwd->pw_name);
	rep->uid = passwd->pw_uid;
	rep->gid = passwd->pw_gid;
	rep->dir = estrdup(passwd->pw_dir);
	rep->shell = estrdup(passwd->pw_shell);
	return (uct)rep;
}

uct
posix_sysdb_getpwuid(cerr er, word uid)
{
	struct passwd* passwd;
	errno = 0;
	passwd = getpwuid((uid_t) uid);
	if(passwd == NULL){
		send_errno(er,errno);
		return 0;
	}
	return Passwdrep(passwd);
}

uct
posix_sysdb_getpwnam(cerr er, string mlname)
{
	char* cname = mlstring2cstring_static(mlname);
	struct passwd* passwd;
	errno = 0;
	passwd = getpwnam(cname);
	if(passwd == NULL){
		send_errno(er,errno);
		return 0;
	}
	return Passwdrep(passwd);
}

string
posix_sysdb_pw_name(uct p)
{
	passwdrep rep = (passwdrep)p;
	return cstring2mlstring_alloc(rep->name);
}

wordpair
posix_sysdb_pw_uid_gid(uct p)
{
	passwdrep rep = (passwdrep)p;
	val_t fields[2];
	fields[0] = (val_t) rep->uid;
	fields[1] = (val_t) rep->gid;
	return alloc_record(fields, arraysize(fields));
}

string
posix_sysdb_pw_dir(uct p)
{
	passwdrep rep = (passwdrep)p;
	return cstring2mlstring_alloc(rep->dir);
}

string
posix_sysdb_pw_shell(uct p)
{
	passwdrep rep = (passwdrep)p;
	return cstring2mlstring_alloc(rep->shell);
}

unit
posix_sysdb_pw_free(uct p)
{
	passwdrep rep = (passwdrep)p;
	efree(rep->name);
	efree(rep->dir);
	efree(rep->shell);
	efree(rep);
	return empty_record;
}
