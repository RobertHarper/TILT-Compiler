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

string_list
commandline_arguments(unit ignored)
{
	assert(commandline_argv != NULL);
	return array_to_string_list(commandline_argv);
}
