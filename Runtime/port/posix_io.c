/* ../../Basis/Posix/posix-io.sml */

#include "s.h"
#include "r.h"

static struct sysval io_vec[] = {
	{"F_GETLK",	F_GETLK},
	{"F_RDLCK",	F_RDLCK},
	{"F_SETLK",	F_SETLK},
	{"F_SETLKW",	F_SETLKW},
	{"F_UNLCK",	F_UNLCK},
	{"F_WRLCK",	F_WRLCK},
	{"SEEK_CUR",	SEEK_CUR},
	{"SEEK_END",	SEEK_END},
	{"SEEK_SET",	SEEK_SET},
	{"cloexec",	FD_CLOEXEC},
};

static struct sysvals io_values = {
	arraysize(io_vec),
	io_vec
};

/*int*/cresult
posix_io_num(string key)
{
	return sysval_num("posix_io_num", &io_values, key);
}

/*int * int*/cresult
posix_io_pipe(unit unused)
{
	int fds[2];
	if(pipe(fds) == -1)
		return Error(SysErr(errno));
	else {
		ptr_t pair = alloc_intint(fds[0], fds[1]);
		return NormalPtr(pair);
	}
}

/*int*/cresult
posix_io_dup(int fd)
{
	int newfd = dup(fd);
	if(newfd == -1)
		return Error(SysErr(errno));
	else
		return Normal(newfd);
}

/*unit*/cresult
posix_io_dup2(int oldfd, int newfd)
{
	return unit_cresult(dup2(oldfd, newfd));
}

/*unit*/cresult
posix_io_close(int fd)
{
	return unit_cresult(close(fd));
}

/*word8vector*/cresult
posix_io_read(int fd, int size)
{
	if(size<0)
		return Error(SysErr_msg("read: negative size"));
	else {
		char* buf = NULL;
		string vec = alloc_uninit_string(size,&buf);
		int bytes_read = read(fd,buf,size);
		if(bytes_read==-1)
			return Error(SysErr(errno));
		adjust_stringlen(vec,bytes_read);
		return NormalPtr(vec);
	}
}

/*int*/cresult
posix_io_readbuf(int fd, word8array buf, int len, int start)
{ 
	int bytes_read = read(fd, (stringbuf(buf)) + start, len);
	if(bytes_read == -1)
		return Error(SysErr(errno));
	else
		return Normal(bytes_read);
}

/*int*/cresult
posix_io_writebuf(int fd, word8array buf, int len, int start)
{
	int written = write(fd, (stringbuf(buf)) + start, len);
	if(written == -1)
		return Error(SysErr(errno));
	else
		return Normal(written);
}

/*int*/cresult
posix_io_fcntl_d(int fd, int basefd)
{
	int newfd = fcntl(fd, F_DUPFD, basefd);
	if(newfd == -1)
		return Error(SysErr(errno));
	else
		return Normal(newfd);
}

/*word*/cresult
posix_io_fcntl_gfd(int fd)
{
	int r = fcntl(fd, F_GETFD);
	if(r == -1)
		return Error(SysErr(errno));
	else
		return Normal(r);	/* mask with FD_CLOEXEC? */
}

/*unit*/cresult
posix_io_fcntl_sfd(int fd, word flag)
{
	/* mask with FD_CLOEXEC? */
	return unit_cresult(fcntl(fd, F_SETFD, flag));
}

/*wordpair*/cresult
posix_io_fcntl_gfl(int fd)
{
	int r = fcntl(fd, F_GETFL);
	if(r == -1)
		return Error(SysErr(errno));
	else {
		word flags = r & (~O_ACCMODE);
		word mode = r & O_ACCMODE;
		ptr_t pair = alloc_intint(flags, mode);
		return NormalPtr(pair);
	}
}

/*unit*/cresult
posix_io_fcntl_sfl(int fd, word flags)
{
	return unit_cresult(fcntl(fd, F_SETFL, flags));
}

/*
	Flockrep and flockset must agree with type
	flockref in ../../Basis/externtys.sml
*/

static ptr_t
Flockrep(struct flock* flock)
{
	val_t fields[5];
	fields[0] = (val_t) flock->l_type;
	fields[1] = (val_t) flock->l_whence;
	fields[2] = (val_t) flock->l_start;
	fields[3] = (val_t) flock->l_len;
	fields[4] = (val_t) flock->l_pid;
	return alloc_record(fields, 0, arraysize(fields));
}

static struct flock*
flockset(struct flock* flock, ptr_t flockrep)
{
	flock->l_type = flockrep[0];
	flock->l_whence = flockrep[1];
	flock->l_start = flockrep[2];
	flock->l_len = flockrep[3];
	flock->l_pid = flockrep[4];
	return flock;
}
  
/*flockrep*/cresult
posix_io_fcntl_l(int fd, int cmd, ptr_t flockrep)
{
	struct flock flock;
	if(fcntl(fd, cmd, flockset(&flock, flockrep)) == -1)
		return Error(SysErr(errno));
	else
		return NormalPtr(Flockrep(&flock));
}

/*int*/cresult
posix_io_lseek(int filedes, int argoffset, int whence)
{
	off_t offset = lseek(filedes, argoffset, whence);
	if(offset == (off_t)-1)
		return Error(SysErr(errno));
	else
		return Normal(offset);
}

/*unit*/cresult
posix_io_fsync(int fd)
{
	return unit_cresult(fsync(fd));
}

