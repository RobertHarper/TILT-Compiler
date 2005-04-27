/* ../../Basis/OS/commandline.sml */

#include "s.h"
#include "r.h"
#include "sparc.h"

static char* commandline_cmd = NULL;
static char** commandline_argv = NULL;

void
setCommandLine(char* cmd, char** argv)
{
	commandline_cmd = cmd;
	commandline_argv = argv;
}

string
commandline_name(unit ignored)
{
	assert(commandline_name != NULL);
	return cstring2mlstring_alloc(commandline_cmd);
}

int
commandline_arguments_size(unit ignored)
{
	int n;
	assert(commandline_argv != NULL);
	for(n=0; commandline_argv[n]; n++)
		;
	return n;
}

string
commandline_arguments_nth(int n)
{
	assert(commandline_argv != NULL);
	return cstring2mlstring_alloc(commandline_argv[n]);
}
