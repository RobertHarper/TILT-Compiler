#include "s.h"
#include "r.h"

extern char **tal_argv;

string
commandline_name(unit ignored)
{
	assert(tal_argv != NULL);
	return cstring2mlstring_alloc(tal_argv[0]);
}

string_list
commandline_arguments(unit ignored)
{
	assert(tal_argv != NULL);
	return array_to_string_list(tal_argv+1);
}
