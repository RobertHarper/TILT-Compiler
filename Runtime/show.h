/* Not thread-safe */
#ifndef _show_h
#define _show_h

#include "bitmap.h"
#include "memobj.h"

extern long SemanticGarbageSize;
extern int traceError;

mem_t show_obj(mem_t start, ptr_t *objRef, int show, int doReplica, Heap_t **legalHeaps, Bitmap_t **legalStarts);
void scan_heap(char *label, mem_t start, mem_t finish, mem_t top, Heap_t **legalHeaps, Bitmap_t **legalStarts,
	       int show, int doReplica, Bitmap_t *makeStart);

void show_heap_raw(char *label, int numwords,
		   mem_t from_low, mem_t from_high,
		   mem_t to_low,   mem_t to_high);
void memdump(char *title, unsigned int *start, int len, unsigned int *target);

int inHeaps(ptr_t v, Heap_t **legalHeaps, Bitmap_t **legalStarts);

#endif
