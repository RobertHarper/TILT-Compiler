#ifndef _gc_large_h
#define _gc_large_h

#include "memobj.h"
#include "queue.h"
#include "gc.h"

extern int LargeHeapByte;
Heap_t *largeSpace;

void gc_large_init(void);
mem_t gc_large_alloc (Proc_t *, int byteLen, Align_t);   
void gc_large_startCollect(void);    /* one caller only */
void gc_large_addRoot (ptr_t root);  /* multiple callers */
void gc_large_endCollect(void);      /* one caller only */

#endif
