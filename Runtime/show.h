/* Not thread-safe */
#ifndef _show_h
#define _show_h

#include "bitmap.h"
#include "memobj.h"

extern long SemanticGarbageSize;
extern int numErrors;
extern int errorsToShow;

/* SelfReplica is relevant for mirrored ptr arrays in tenured space during a minor collection */
typedef enum ShowType__t {NoReplica, OtherReplica, SelfReplica} ShowType_t; 

mem_t show_obj(mem_t start, ptr_t *objRef, int show, ShowType_t replicaType, Heap_t **legalHeaps, Bitmap_t **legalStarts);
void scan_heap(char *label, mem_t start, mem_t finish, mem_t top, Heap_t **legalHeaps, Bitmap_t **legalStarts,
	       int show, ShowType_t replicaType, Bitmap_t *makeStart);

void show_heap_raw(char *label, int numwords,
		   mem_t from_low, mem_t from_high,
		   mem_t to_low,   mem_t to_high);
void memdump(char *title, unsigned int *start, int len, unsigned int *target);

int inHeaps(ptr_t v, Heap_t **legalHeaps, Bitmap_t **legalStarts);


#endif
