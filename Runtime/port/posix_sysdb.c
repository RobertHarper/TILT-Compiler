/* ../../Basis/Posix/posix-sysdb.sml */

#include "s.h"
#include "r.h"
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>

/*
	Grouprep must agree with type
	grouprep in ../../Basis/externtys.sml
*/

static ptr_t
Grouprep(struct group* group)
{
	val_t fields[3];
	fields[0] = (val_t) cstring2mlstring_alloc(group->gr_name);
	fields[1] = (val_t) group->gr_gid;
	fields[2] = (val_t) array_to_string_list(group->gr_mem);
	return alloc_record(fields, (1<<0)|(1<<2), arraysize(fields));
}

/*grouprep*/cresult
posix_sysdb_getgrgid(word gid)
{
	struct group* group = getgrgid((gid_t)gid);
	if(group == NULL)
		return Error(SysErr(errno));
	else
		return NormalPtr(Grouprep(group));
}

/*grouprep*/cresult
posix_sysdb_getgrnam(string mlname)
{
	char* cname = mlstring2cstring_static(mlname);
	struct group* group = getgrnam(cname);
	if(group == NULL)
		return Error(SysErr(errno));
	else
		return NormalPtr(Grouprep(group));
}

/*
	Passwdrep must agree with type
	passwdrep in ../../Basis/externtys.sml
*/

static ptr_t
Passwdrep(struct passwd* passwd)
{
	val_t fields[5];
	fields[0] = (val_t) cstring2mlstring_alloc(passwd->pw_name);
	fields[1] = (val_t) passwd->pw_uid;
	fields[2] = (val_t) passwd->pw_gid;
	fields[3] = (val_t) cstring2mlstring_alloc(passwd->pw_dir);
	fields[4] = (val_t) cstring2mlstring_alloc(passwd->pw_shell);
	return alloc_record(fields, (1<<0)|(1<<3)|(1<<4), arraysize(fields));
}

/*passwdrep*/cresult
posix_sysdb_getpwuid(word uid)
{
	struct passwd* passwd = getpwuid((uid_t) uid);
	if(passwd == NULL)
		return Error(SysErr(errno));
	else
		return NormalPtr(Passwdrep(passwd));
}

/*passwdrep*/cresult
posix_sysdb_getpwnam(string mlname)
{
	char* cname = mlstring2cstring_static(mlname);
	struct passwd* passwd = getpwnam(cname);
	if(passwd == NULL)
		return Error(SysErr(errno));
	else
		return NormalPtr(Passwdrep(passwd));
}
