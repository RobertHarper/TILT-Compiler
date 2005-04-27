/* ../../Basis/Posix/posix-error.sml */

#include "s.h"
#include "r.h"

static char Ename[] = "posix_error_name: bad number";
static char Enum[] = "posix_error_num: bad name";

static struct sysval error_vec[] = {
	{"acces",	EACCES},
	{"again",	EAGAIN},
#if (defined(EWOULDBLOCK) && (EWOULDBLOCK != EAGAIN))
	{"wouldblock",	EWOULDBLOCK},
#endif
	{"badf",	EBADF},
#ifdef EBADMSG
	{"badmsg",	EBADMSG},
#else
	{"badmsg",	0},
#endif
	{"busy",	EBUSY},
#ifdef ECANCELED
	{"canceled",	ECANCELED},
#else
	{"canceled",	0},
#endif
	{"child",	ECHILD},
	{"deadlk",	EDEADLK},
	{"dom",	EDOM},
	{"exist",	EEXIST},
	{"fault",	EFAULT},
	{"fbig",	EFBIG},
	{"inprogress",	EINPROGRESS},
	{"intr",	EINTR},
	{"inval",	EINVAL},
	{"io",	EIO},
	{"isdir",	EISDIR},
	{"loop",	ELOOP},
	{"mfile",	EMFILE},
	{"mlink",	EMLINK},
	{"msgsize",	EMSGSIZE},
	{"nametoolong",	ENAMETOOLONG},
	{"nfile",	ENFILE},
	{"nodev",	ENODEV},
	{"noent",	ENOENT},
	{"noexec",	ENOEXEC},
	{"nolck",	ENOLCK},
	{"nomem",	ENOMEM},
	{"nospc",	ENOSPC},
	{"nosys",	ENOSYS},
	{"notdir",	ENOTDIR},
	{"notempty",	ENOTEMPTY},
#ifdef ENOTSUP
	{"notsup",	ENOTSUP},
#else
	{"notsup",	0},
#endif
	{"notty",	ENOTTY},
	{"nxio",	ENXIO},
	{"perm",	EPERM},
	{"pipe",	EPIPE},
	{"range",	ERANGE},
	{"rofs",	EROFS},
	{"spipe",	ESPIPE},
	{"srch",	ESRCH},
	{"toobig",	E2BIG},
	{"xdev",	EXDEV},
};

static struct sysvals error_values = {
	arraysize(error_vec),
	error_vec
};

string
posix_error_name(cerr er, int e)
{
	return sysval_name(er, Ename, &error_values, e);
}

int
posix_error_num(cerr er, string key)
{
	return sysval_num(er, Enum, &error_values, key);
}
