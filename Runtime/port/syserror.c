/*
	../../Basis/ccall.sml
	../../Basis/Posix/posix-error.sml
*/

#include "s.h"
#include "r.h"

string
syserror_msg(int e)
{
	char* cmsg = strerror(e);
	return cstring2mlstring_alloc(cmsg);
}
