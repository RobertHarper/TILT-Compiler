#ifndef _gcstat_h
#define _gcstat_h

#include "thread.h"

struct Object_Profile__t
{
  int RecordWord;
  int Record;
  int Pair;
  int IArray;
  int RArray;
  int PArray;
  int IArrayWord;
  int RArrayWord;
  int PArrayWord;
};

typedef struct Object_Profile__t Object_Profile_t;

void object_profile_init(Object_Profile_t *);
void show_gcstats(Object_Profile_t *alloc, Object_Profile_t *collect);
void gc_heapstat(mem_t bottom, mem_t alloc_ptr);
void gc_sanity_stackreg_check(unsigned long *saveregs, Heap_t *from,
			      int *stackbot, int *stacktop);

void gcstat_heapprofile_beforecollect(mem_t bot, mem_t top);
void gcstat_heapprofile_aftercollect(mem_t bot, mem_t top);
void gcstat_show_heapprofile(void);


#endif
