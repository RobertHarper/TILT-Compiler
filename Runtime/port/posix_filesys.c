/* ../../Basis/Posix/posix-filesys.sml */

#include "s.h"
#include "r.h"
#include <sys/types.h>
#include <dirent.h>
#include <utime.h>
#include <sys/stat.h>

/*int*/cresult
posix_filesys_opendir(string dirname)
{
	char* cdir = mlstring2cstring_static(dirname);
	DIR *dir = opendir(cdir);
	if(dir == NULL)
		return Error(SysErr(errno));
	else
		return Normal((val_t)dir);
}

static bool
filterArc(char* name)
{
	return name[0] == '.'
		&& (name[1] == '\0'
			|| (name[1] == '.' && name[2] == '\0'));
}

/*string*/cresult
posix_filesys_readdir(int arg)
{
	DIR* dir = (DIR *)arg;
	struct dirent* entry;
	do {
		errno = 0;
		entry = readdir(dir);
	} while (entry != NULL && filterArc(entry->d_name));
	if(entry != NULL)
		return NormalPtr(cstring2mlstring_alloc(entry->d_name));
	else if(errno != 0)
		return Error(SysErr(errno));
	else
		return NormalPtr(cstring2mlstring_alloc(""));
}

unit
posix_filesys_rewinddir(int arg)
{
	DIR* dir = (DIR*)arg;
	rewinddir(dir);	/* can't fail */
	return empty_record;
}

/*unit*/cresult
posix_filesys_closedir(int arg)
{
	DIR *dir = (DIR *)arg;
	return unit_cresult(closedir(dir));
}

/*unit*/cresult
posix_filesys_chdir(string dirname)
{
	char *dir = mlstring2cstring_static(dirname);
	return unit_cresult(chdir(dir));
}

static /*string*/cresult
do_getcwd(char* buf, int size)
{
	if(getcwd(buf,size) == NULL) {
		int e = errno;
		if(e==ERANGE) {
			int n = size * 2;
			char* newbuf = (char*)erealloc(buf,n);
			return do_getcwd(newbuf,n);
		} else {
			efree(buf);
			return Error(SysErr(e));
		}
	} else {
		string cwd = cstring2mlstring_alloc(buf);
		efree(buf);
		return NormalPtr(cwd);
	}
}

/*string*/cresult
posix_filesys_getcwd(unit unused)
{
	int n = 1024;
	char* buf = (char*)emalloc_atomic(n);
	return do_getcwd(buf,n);
}

/*int*/cresult
posix_filesys_openf(string filename, word oflag, word mode)
{
	const char *cfilename = mlstring2cstring_static(filename);
	int fd = open(cfilename,oflag,mode);
	if(fd == -1)
		return Error(SysErr(errno));
	else
		return Normal(fd);
}

word
posix_filesys_umask(word mask)
{
	return (word)umask((mode_t)mask);	/* can't fail */
}

/*unit*/cresult
posix_filesys_link(string from, string to)
{
	char* cfrom = mlstring2cstring_static(from);
	char* cto = mlstring2cstring_malloc(to);
	cresult r = unit_cresult(link(cfrom,cto));
	efree(cto);
	return r;
}

/*unit*/cresult
posix_filesys_rename(string from, string to)
{
	char* cfrom = mlstring2cstring_static(from);
	char* cto = mlstring2cstring_malloc(to);
	cresult r = unit_cresult(rename(cfrom,cto));
	efree(cto);
	return r;
}

/*unit*/cresult
posix_filesys_symlink(string from, string to)
{
	char* cfrom = mlstring2cstring_static(from);
	char* cto = mlstring2cstring_malloc(to);
	cresult r = unit_cresult(symlink(cfrom, cto));
	efree(cto);
	return r;
}

/*unit*/cresult
posix_filesys_mkdir(string mlDir, word mode)
{
	char* cdir = mlstring2cstring_static(mlDir);
	return unit_cresult(mkdir(cdir,mode));
}

/*unit*/cresult
posix_filesys_mkfifo(string path, word mode)
{
	char* cpath = mlstring2cstring_static(path);
	return unit_cresult(mkfifo(cpath, (mode_t)mode));
}

/*unit*/cresult
posix_filesys_unlink(string arg)
{
	char* path = mlstring2cstring_static(arg);
	return unit_cresult(unlink(path));
}

/*unit*/cresult
posix_filesys_rmdir(string path)
{
	char* cpath = mlstring2cstring_static(path);
	return unit_cresult(rmdir(cpath));
}

static /*string*/cresult
do_readlink(char* link, char* buf, int bufsize)
{
	int count = readlink(link, buf, bufsize);
	if(count==-1) {
		int e = errno;
		efree(buf);
		return Error(SysErr(e));
	} else if(count == bufsize) {
		/* link contents may be too large */
		int newbufsize = bufsize*2;
		char* newbuf = (char*)erealloc(buf,newbufsize);
		return do_readlink(link,newbuf,newbufsize);
	} else {
		/* readlink does not add a terminating NUL */
		string contents = alloc_string(count,buf);
		efree(buf);
		return NormalPtr(contents);
	}
}

/*string*/cresult
posix_filesys_readlink(string link)
{
	char* clink = mlstring2cstring_static(link);
	int bufsize = PATH_MAX;
	char* buf = (char*)emalloc_atomic(bufsize);
	return do_readlink(clink, buf, bufsize);
}

/*unit*/cresult
posix_filesys_ftruncate(int fd, int length)
{
	return unit_cresult(ftruncate(fd, (off_t)length));
}

/*
	Statrep must agree with type statrep in
	../../Basis/externtys.sml
*/

static ptr_t
Statrep(struct stat* stat)
{
	val_t fields[11];
	fields[0] = (val_t) stat->st_mode & S_IFMT;
	fields[1] = (val_t) stat->st_mode & (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID);
	fields[2] = (val_t) stat->st_ino;
	fields[3] = (val_t) stat->st_dev;
	fields[4] = (val_t) stat->st_nlink;
	fields[5] = (val_t) stat->st_uid;
	fields[6] = (val_t) stat->st_gid;
	fields[7] = (val_t) stat->st_size;
	fields[8] = (val_t) stat->st_atime;
	fields[9] = (val_t) stat->st_mtime;
	fields[10] = (val_t) stat->st_ctime;
	return alloc_record(fields, 0, arraysize(fields));
}

/*statrep*/cresult
posix_filesys_stat(string name)
{
	char* cname = mlstring2cstring_static(name);
	struct stat sb;
	if(stat(cname,&sb) == -1)
		return Error(SysErr(errno));
	else
		return NormalPtr(Statrep(&sb));
}

/*statrep*/cresult
posix_filesys_lstat(string name)
{
	char* cname = mlstring2cstring_static(name);
	struct stat sb;
	if(lstat(cname,&sb) == -1)
		return Error(SysErr(errno));
	else
		return NormalPtr(Statrep(&sb));
}

/*statrep*/cresult
posix_filesys_fstat(int fd)
{
	struct stat sb;
	if(fstat(fd,&sb) == -1)
		return Error(SysErr(errno));
	else
		return NormalPtr(Statrep(&sb));
}

/*bool*/cresult
posix_filesys_access(string name, word mode)
{
	/*
		Only allowed to raise an exception for errors
		unrelated to resolving the pathname and permissions.
	*/
	char *cname = mlstring2cstring_static(name);
	if(access(cname,mode) == -1) {
		int e = errno;
		if(e==EINTR)
			return Error(SysErr(e));
		else
			return Normal(0);
	} else
		return Normal(1);
}

/*unit*/cresult
posix_filesys_chmod(string path, word mode)
{
	char* cpath = mlstring2cstring_static(path);
	return unit_cresult(chmod(cpath, (mode_t)mode));
}

/*unit*/cresult
posix_filesys_fchmod(int fd, word mode)
{
	return unit_cresult(fchmod(fd, (mode_t)mode));
}

/*unit*/cresult
posix_filesys_chown(string path, word owner, word group)
{
	char* cpath = mlstring2cstring_static(path);
	return unit_cresult(chown(cpath, (uid_t)owner, (gid_t)group));
}

/*unit*/cresult
posix_filesys_fchown(int fd, word owner, word group)
{
	return unit_cresult(fchown(fd, (uid_t)owner, (gid_t)group));
}

/*unit*/cresult
posix_filesys_utime(string filename, int atime, int modtime)
{
	char* cfilename = mlstring2cstring_static(filename);
	int r;
	if(atime == -1)
		r = utime(cfilename,NULL);
	else {
		struct utimbuf arg;
		arg.actime = atime;
		arg.modtime = modtime;
		r = utime(cfilename,&arg);
	}
	return unit_cresult(r);
}

static cresult
unimplemented(char* f)
{
	return Error(SysErr_fmt("%s: unimplemented", f));
}

/*intpair*/cresult
posix_filesys_pathconf(string unused1, string unused)
{
	return unimplemented("posix_filesys_pathconf");
}

/*intpair*/cresult
posix_filesys_fpathconf (int unused1, string unused)
{ 
	return unimplemented("posix_filesys_fpathconf");
}
