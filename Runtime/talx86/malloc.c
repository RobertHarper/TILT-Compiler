#include "s.h"
#include "r.h"

extern void*	GC_malloc(size_t);
extern void*	GC_malloc_atomic(size_t);
extern void*	GC_realloc(void*, size_t);
extern void	GC_free(void*);

void*
emalloc(size_t size)
{
	void* buf = GC_malloc(size);
	if(buf == NULL)
		DIEwith(errno);
	return buf;
}

void*
emalloc_atomic(size_t size)
{
	void* buf = GC_malloc_atomic(size);
	if(buf == NULL)
		DIEwith(errno);
	return buf;
}

void*
erealloc(void* buf, size_t size)
{
	buf = GC_realloc(buf, size);
	if(buf == NULL)
		DIEwith(errno);
	return buf;
}

void
efree(void* dead)
{
	GC_free(dead);
}
