#include "s.h"
#include "r.h"

void
DIE(char* msg)
{
	fprintf(stderr,"tilt runtime error: %s\n",msg);
	fflush(stderr);
	abort();
}

void
DIEwith(int e)
{
	char* msg = strerror(e);
	DIE(msg);
}
