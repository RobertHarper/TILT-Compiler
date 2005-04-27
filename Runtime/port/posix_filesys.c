/* ../../Basis/Posix/posix-filesys.sml */

#include "s.h"
#include "r.h"
#include <sys/types.h>
#include <dirent.h>
#include <utime.h>
#include <sys/stat.h>

static char Efsnum[] = "posix_filesys_num: bad name";
static char Epathconf[] = "posix_filesys_pathconf: bad name";

static struct sysval filesys_vec[] = {
	{"A_EXEC",	X_OK},
	{"A_FILE",	F_OK},
	{"A_READ",	R_OK},
	{"A_WRITE",	W_OK},
	{"O_APPEND",	O_APPEND},
	{"O_CREAT",	O_CREAT},
#ifdef O_DSYNC
	{"O_DSYNC",	O_DSYNC},
#else
	{"O_DSYNC",	0},
#endif
	{"O_EXCL",	O_EXCL},
	{"O_NOCTTY",	O_NOCTTY},
	{"O_NONBLOCK",	O_NONBLOCK},
#ifdef O_RSYNC
	{"O_RSYNC",	O_RSYNC},
#else
	{"O_RSYNC",	0},
#endif
#ifdef O_SYNC
	{"O_SYNC",	O_SYNC},
#else
	{"O_SYNC",	0},
#endif
	{"O_TRUNC",	O_TRUNC},
	{"irgrp",	S_IRGRP},
	{"iroth",	S_IROTH},
	{"irusr",	S_IRUSR},
	{"irwxg",	S_IRWXG},
	{"irwxo",	S_IRWXO},
	{"irwxu",	S_IRWXU},
	{"isgid",	S_ISGID},
	{"isuid",	S_ISUID},
	{"iwgrp",	S_IWGRP},
	{"iwoth",	S_IWOTH},
	{"iwusr",	S_IWUSR},
	{"ixgrp",	S_IXGRP},
	{"ixoth",	S_IXOTH},
	{"ixusr",	S_IXUSR},
};

static struct sysvals filesys_values = {
	arraysize(filesys_vec),
	filesys_vec
};

word
posix_filesys_num(cerr er, string key)
{
	return sysval_num(er, Efsnum, &filesys_values, key);
}

uct
posix_filesys_opendir(cerr er, string dirname)
{
	char* cdir = mlstring2cstring_static(dirname);
	DIR *dir = opendir(cdir);
	if(dir == NULL)
		send_errno(er,errno);
	return (uct)dir;
}

static bool
filterArc(char* name)
{
	return name[0] == '.'
		&& (name[1] == '\0'
			|| (name[1] == '.' && name[2] == '\0'));
}

string
posix_filesys_readdir(cerr er, uct p)
{
	DIR* dir = (DIR *)p;
	struct dirent* entry;
	do {
		errno = 0;
		entry = readdir(dir);
	} while (entry != NULL && filterArc(entry->d_name));
	if(entry != NULL)
		return cstring2mlstring_alloc(entry->d_name);
	else if(errno != 0)
		send_errno(er,errno);
	return cstring2mlstring_alloc("");
}

unit
posix_filesys_rewinddir(uct p)
{
	DIR* dir = (DIR*)p;
	rewinddir(dir);	/* can't fail */
	return empty_record;
}

unit
posix_filesys_closedir(cerr er, uct p)
{
	DIR* dir = (DIR*)p;
	if(closedir(dir) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_chdir(cerr er, string dirname)
{
	char *dir = mlstring2cstring_static(dirname);
	if(chdir(dir) == -1)
		send_errno(er,errno);
	return empty_record;
}

static string
do_getcwd(cerr er, char* buf, int size)
{
	if(getcwd(buf,size) == NULL) {
		int e = errno;
		if(e==ERANGE) {
			int n = size * 2;
			char* newbuf = (char*)erealloc(buf,n);
			return do_getcwd(er,newbuf,n);
		} else {
			efree(buf);
			send_errno(er,e);
			return cstring2mlstring_alloc("");
		}
	} else {
		string cwd = cstring2mlstring_alloc(buf);
		efree(buf);
		return cwd;
	}
}

string
posix_filesys_getcwd(cerr er)
{
	int n = 1024;
	char* buf = (char*)emalloc_atomic(n);
	return do_getcwd(er,buf,n);
}

int
posix_filesys_openf(cerr er, string filename, word oflag, word mode)
{
	const char *cfilename = mlstring2cstring_static(filename);
	int fd = open(cfilename,oflag,mode);
	if(fd == -1)
		send_errno(er,errno);
	return fd;
}

word
posix_filesys_umask(word mask)
{
	return (word)umask((mode_t)mask);	/* can't fail */
}

unit
posix_filesys_link(cerr er, string from, string to)
{
	char* cfrom = mlstring2cstring_static(from);
	char* cto = mlstring2cstring_malloc(to);
	if(link(cfrom,cto) == -1)
		send_errno(er,errno);
	efree(cto);
	return empty_record;
}

unit
posix_filesys_rename(cerr er, string from, string to)
{
	char* cfrom = mlstring2cstring_static(from);
	char* cto = mlstring2cstring_malloc(to);
	if(rename(cfrom,cto) == -1)
		send_errno(er,errno);
	efree(cto);
	return empty_record;
}

unit
posix_filesys_symlink(cerr er, string from, string to)
{
	char* cfrom = mlstring2cstring_static(from);
	char* cto = mlstring2cstring_malloc(to);
	if(symlink(cfrom, cto) == -1)
		send_errno(er,errno);
	efree(cto);
	return empty_record;
}

unit
posix_filesys_mkdir(cerr er, string mlDir, word mode)
{
	char* cdir = mlstring2cstring_static(mlDir);
	if(mkdir(cdir,mode) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_mkfifo(cerr er, string path, word mode)
{
	char* cpath = mlstring2cstring_static(path);
	if(mkfifo(cpath, (mode_t)mode) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_unlink(cerr er, string arg)
{
	char* path = mlstring2cstring_static(arg);
	if(unlink(path) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_rmdir(cerr er, string path)
{
	char* cpath = mlstring2cstring_static(path);
	if(rmdir(cpath) == -1)
		send_errno(er,errno);
	return empty_record;
}

static string
do_readlink(cerr er, char* link, char* buf, int bufsize)
{
	int count = readlink(link, buf, bufsize);
	if(count==-1) {
		int e = errno;
		efree(buf);
		send_errno(er,e);
		return cstring2mlstring_alloc("");
	} else if(count == bufsize) {
		/* link contents may be too large */
		int newbufsize = bufsize*2;
		char* newbuf = (char*)erealloc(buf,newbufsize);
		return do_readlink(er,link,newbuf,newbufsize);
	} else {
		/* readlink does not add a terminating NUL */
		string contents = alloc_string(count,buf);
		efree(buf);
		return contents;
	}
}

string
posix_filesys_readlink(cerr er, string link)
{
	char* clink = mlstring2cstring_static(link);
	int bufsize = PATH_MAX;
	char* buf = (char*)emalloc_atomic(bufsize);
	return do_readlink(er,clink, buf, bufsize);
}

unit
posix_filesys_ftruncate(cerr er, int fd, int length)
{
	if(ftruncate(fd, (off_t)length) == -1)
		send_errno(er,errno);
	return empty_record;
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
	return alloc_record(fields, arraysize(fields));
}

ptr_t	/*statrep*/
posix_filesys_stat(cerr er, string name)
{
	char* cname = mlstring2cstring_static(name);
	struct stat sb;
	if(stat(cname,&sb) == -1)
		send_errno(er,errno);
	return Statrep(&sb);
}

ptr_t	/*statrep*/
posix_filesys_lstat(cerr er, string name)
{
	char* cname = mlstring2cstring_static(name);
	struct stat sb;
	if(lstat(cname,&sb) == -1)
		send_errno(er,errno);
	return Statrep(&sb);
}

ptr_t	/*statrep*/
posix_filesys_fstat(cerr er, int fd)
{
	struct stat sb;
	if(fstat(fd,&sb) == -1)
		send_errno(er,errno);
	return Statrep(&sb);
}

bool
posix_filesys_access(cerr er, string name, word mode)
{
	/*
		Only allowed to raise an exception for errors
		unrelated to resolving the pathname and permissions.
	*/
	char *cname = mlstring2cstring_static(name);
	if(access(cname,mode) == -1) {
		int e = errno;
		if(e==EINTR)
			send_errno(er,e);
		return 0;
	}
	else
		return 1;
}

unit
posix_filesys_chmod(cerr er, string path, word mode)
{
	char* cpath = mlstring2cstring_static(path);
	if(chmod(cpath, (mode_t)mode) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_fchmod(cerr er, int fd, word mode)
{
	if(fchmod(fd, (mode_t)mode) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_chown(cerr er, string path, word owner, word group)
{
	char* cpath = mlstring2cstring_static(path);
	if(chown(cpath, (uid_t)owner, (gid_t)group) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_fchown(cerr er, int fd, word owner, word group)
{
	if(fchown(fd, (uid_t)owner, (gid_t)group) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_filesys_utime(cerr er, string filename, int atime, int modtime)
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
	if(r == -1)
		send_errno(er,errno);
	return empty_record;
}

static struct sysval pathconf_vec[] = {
	{"ASYNC_IO",	_PC_ASYNC_IO},
	{"CHOWN_RESTRICTED",	_PC_CHOWN_RESTRICTED},
#ifdef _PC_FILESIZEBITS
	{"FILESIZEBITS",	_PC_FILESIZEBITS},
#endif
	{"LINK_MAX",	_PC_LINK_MAX},
	{"MAX_CANON",	_PC_MAX_CANON},
	{"MAX_INPUT",	_PC_MAX_INPUT},
	{"NAME_MAX",	_PC_NAME_MAX},
	{"NO_TRUNC",	_PC_NO_TRUNC},
	{"PATH_MAX",	_PC_PATH_MAX},
	{"PIPE_BUF",	_PC_PIPE_BUF},
	{"PRIO_IO",	_PC_PRIO_IO},
	{"SYNC_IO",	_PC_SYNC_IO},
	{"VDISABLE",	_PC_VDISABLE},
};

static struct sysvals pathconf_values = {
	arraysize(pathconf_vec),
	pathconf_vec
};

int
posix_filesys_pathconf_name(cerr er, string key)
{
	return sysval_num(er, Epathconf, &pathconf_values, key);
}

word
posix_filesys_pathconf(cerr er, string path, int name)
{
	char* cpath = mlstring2cstring_static(path);
	long r;
	errno = 0;
	r = pathconf(cpath,name);
	if(r == -1 && errno != 0)
		send_errno(er,errno);
	return (word)r;
}

word
posix_filesys_fpathconf(cerr er, int fd, int name)
{
	long r;
	errno = 0;
	r = fpathconf(fd,name);
	if(r == -1 && errno != 0)
		send_errno(er,errno);
	return (word)r;
}
