/* Not thread-safe */
#ifndef _show_h
#define _show_h

#include "bitmap.h"

extern long SemanticGarbageSize;
Bitmap_t *scan_heap(char *label, mem_t start, mem_t finish, mem_t top, Heap_t **legalHeaps, Bitmap_t **legalStarts,
		    int show, int makeBitmap);

void show_heap_raw(char *label, int numwords,
		   mem_t from_low, mem_t from_high,
		   mem_t to_low,   mem_t to_high);
void memdump(char *title, int *start, int len, int *target);

#endif
