/* ../../Basis/Posix/pre-posix.sml */

#include "s.h"
#include "r.h"
#include <sys/stat.h>

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
	{"O_RDONLY",	O_RDONLY},
	{"O_RDWR",	O_RDWR},
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
	{"O_WRONLY",	O_WRONLY},
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

static struct sysval sysconf_vec[] = {
	{"JOB_CONTROL",	_SC_JOB_CONTROL},
	{"CLK_TCK",	_SC_CLK_TCK},
};

static struct sysvals sysconf_values = {
	arraysize(sysconf_vec),
	sysconf_vec
};

/*word*/cresult
posix_filesys_num(string key)
{
	return sysval_num("posix_filesys_num", &filesys_values, key);
}

/*word*/cresult
posix_process_sysconf(string key)
{
	return sysval_num("posix_process_sysconf", &sysconf_values, key);
}
