/* ../../Basis/Posix/posix-signal.sml */

#include "s.h"
#include "r.h"
#include <signal.h>

static struct sysval signal_vec[] = {
	{"abrt",	SIGABRT},
	{"alrm",	SIGALRM},
	{"bus",	SIGBUS},
	{"chld",	SIGCHLD},
	{"cont",	SIGCONT},
	{"fpe",	SIGFPE},
	{"hup",	SIGHUP},
	{"ill",	SIGILL},
	{"int",	SIGINT},
	{"kill",	SIGKILL},
	{"pipe",	SIGPIPE},
	{"quit",	SIGQUIT},
	{"segv",	SIGSEGV},
	{"stop",	SIGSTOP},
	{"term",	SIGTERM},
	{"tstp",	SIGTSTP},
	{"ttin",	SIGTTIN},
	{"ttou",	SIGTTOU},
	{"usr1",	SIGUSR1},
	{"usr2",	SIGUSR2},
};

static struct sysvals signal_values = {
	arraysize(signal_vec),
	signal_vec
};

/*int*/cresult
posix_signal_num(string key)
{
	return sysval_num("posix_signal_num", &signal_values, key);
}
