#include <signal.h>

int
my_sigaction(int signal, const struct sigaction* action,
	struct sigaction* o_action)
{
	return sigaction(signal, action, o_action);
}
