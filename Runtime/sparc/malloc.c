#include "s.h"
#include "r.h"

void*
emalloc(size_t size)
{
	void* buf = malloc(size);
	if(buf == NULL)
		DIEwith(errno);
	return buf;
}

void*
ecalloc(size_t a, size_t b)
{
	void* buf = calloc(a,b);
	if(buf == NULL)
		DIEwith(errno);
	return buf;
}

void*
emalloc_atomic(size_t size)
{
	void* buf = malloc(size);
	if(buf == NULL)
		DIEwith(errno);
	return buf;
}

void*
erealloc(void* buf, size_t size)
{
	buf = realloc(buf, size);
	if(buf == NULL)
		DIEwith(errno);
	return buf;
}

void
efree(void* dead)
{
	free(dead);
}
