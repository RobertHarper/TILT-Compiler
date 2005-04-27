#include "s.h"
#include "r.h"

char*
estrdup(char* s)
{
	int n = strlen(s) + 1;
	char* r = (char*)emalloc_atomic(n);
	memcpy(r,s,n);
	return r;
}
