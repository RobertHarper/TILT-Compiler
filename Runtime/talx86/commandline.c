/* ../../Basis/OS/commandline.sml */

#include "s.h"
#include "r.h"

extern char **tal_argv;

string
commandline_name(unit ignored)
{
	assert(tal_argv != NULL);
	return cstring2mlstring_alloc(tal_argv[0]);
}

int
commandline_arguments_size(unit ignored)
{
	int n;

	assert(tal_argv != NULL);
	for(n=1; tal_argv[n]; n++)
		;
	return n - 1;
}

string
commandline_arguments_nth(int n)
{
	assert(tal_argv != NULL);
	return cstring2mlstring_alloc(tal_argv[n + 1]);
}
