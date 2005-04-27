/* ../../Basis/Posix/posix-io.sml */

#include "s.h"
#include "r.h"

static char Eionum[] = "posix_io_num: bad name";

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

int
posix_io_num(cerr er, string key)
{
	return sysval_num(er, Eionum, &io_values, key);
}

intpair
posix_io_pipe(cerr er, unit unused)
{
	int fds[2];
	ptr_t pair;
	if(pipe(fds) == -1)
		send_errno(er, errno);
	pair = alloc_intint(fds[0], fds[1]);
	return pair;
}

int
posix_io_dup(cerr er, int fd)
{
	int newfd = dup(fd);
	if(newfd == -1)
		send_errno(er,errno);
	return newfd;
}

unit
posix_io_dup2(cerr er, int oldfd, int newfd)
{
	if(dup2(oldfd,newfd)==-1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_io_close(cerr er, int fd)
{
	if(close(fd)==-1)
		send_errno(er,errno);
	return empty_record;
}

string
posix_io_read(cerr er, int fd, int size)
{
	char* buf = NULL;
	string vec = alloc_uninit_string(size,&buf);
	int bytes_read = read(fd,buf,size);
	if(bytes_read==-1)
		send_errno(er,errno);
	else
		adjust_stringlen(vec,bytes_read);
	return vec;
}

int
posix_io_readbuf(cerr er, int fd, word8array buf, int len, int start)
{ 
	int bytes_read = read(fd, (stringbuf(buf)) + start, len);
	if(bytes_read == -1)
		send_errno(er,errno);
	return bytes_read;
}

int
posix_io_writebuf(cerr er, int fd, word8array buf, int len, int start)
{
	int written = write(fd, (stringbuf(buf)) + start, len);
	if(written == -1)
		send_errno(er,errno);
	return written;
}

int
posix_io_fcntl_d(cerr er, int fd, int basefd)
{
	int newfd = fcntl(fd, F_DUPFD, basefd);
	if(newfd == -1)
		send_errno(er,errno);
	return newfd;
}

word
posix_io_fcntl_gfd(cerr er, int fd)
{
	int r = fcntl(fd, F_GETFD);
	if(r == -1)
		send_errno(er,errno);
	return r;	/* mask with FD_CLOEXEC? */
}

unit
posix_io_fcntl_sfd(cerr er, int fd, word flag)
{
	if(fcntl(fd, F_SETFD, flag) == -1)	/* mask with FD_CLOEXEC? */
		send_errno(er,errno);
	return empty_record;
}

wordpair
posix_io_fcntl_gfl(cerr er, int fd)
{
	int r = fcntl(fd, F_GETFL);
	word flags;
	word mode;
	ptr_t pair;

	if(r == -1)
		send_errno(er,errno);
	flags = r & (~O_ACCMODE);
	mode = r & O_ACCMODE;
	pair = alloc_intint(flags, mode);
	return pair;
}

unit
posix_io_fcntl_sfl(cerr er, int fd, word flags)
{
	if(fcntl(fd, F_SETFL, flags) == -1)
		send_errno(er,errno);
	return empty_record;
}

/*
	Flockrep and flockset must agree with type
	flockrep in ../../Basis/externtys.sml
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
	return alloc_record(fields, arraysize(fields));
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
  
ptr_t
posix_io_fcntl_l(cerr er, int fd, int cmd, ptr_t flockrep)
{
	struct flock flock;
	if(fcntl(fd, cmd, flockset(&flock, flockrep)) == -1)
		send_errno(er,errno);
	return Flockrep(&flock);
}

int
posix_io_lseek(cerr er, int filedes, int argoffset, int whence)
{
	off_t offset = lseek(filedes, argoffset, whence);
	if(offset == (off_t)-1)
		send_errno(er,errno);
	return offset;
}

unit
posix_io_fsync(cerr er, int fd)
{
	if(fsync(fd) == -1)
		send_errno(er,errno);
	return empty_record;
}

