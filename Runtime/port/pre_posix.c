/* ../../Basis/Posix/pre-posix.sml */

#include "s.h"
#include "r.h"

static char Eomode[] = "pre_posix_omode: bad name";
static char Esysconf[] = "pre_posix_sysconf: bad name";

static struct sysval omode_vec[] = {
	{"O_RDONLY",	O_RDONLY},
	{"O_RDWR",	O_RDWR},
	{"O_WRONLY",	O_WRONLY},
};

static struct sysvals omode_values = {
	arraysize(omode_vec),
	omode_vec
};

static struct sysval sysconf_vec[] = {
	{"JOB_CONTROL",	_SC_JOB_CONTROL},
	{"CLK_TCK",	_SC_CLK_TCK},
};

static struct sysvals sysconf_values = {
	arraysize(sysconf_vec),
	sysconf_vec
};

word
pre_posix_omode(cerr er, string key)
{
	return sysval_num(er, Eomode, &omode_values, key);
}

word
pre_posix_sysconf(cerr er, string key)
{
	return sysval_num(er, Esysconf, &sysconf_values, key);
}
