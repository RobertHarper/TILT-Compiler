/* Not thread-safe */
#ifndef _show_h
#define _show_h

extern long SemanticGarbageSize;
void scan_heap(char *label, value_t start, value_t finish, value_t top, Heap_t **legalHeaps, int show);

void show_heap_raw(char *label, int numwords,
		   value_t from_low, value_t from_high,
		   value_t to_low,   value_t to_high);
void memdump(char *title, int *start, int len, int *target);

#endif
