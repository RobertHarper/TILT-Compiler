#ifndef _gc_large_h
#define _gc_large_h

#include "memobj.h"
#include "queue.h"

Heap_t *large;

void gc_large_init(int threadSafe);
mem_t gc_large_alloc (Proc_t *, int byteLen);
void gc_large_addRoots (Queue_t *roots);
void gc_large_flush(void);

#endif
