#ifndef _gcstat_h
#define _gcstat_h

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
void gc_heapstat(value_t *bottom, value_t *alloc_ptr);
void gc_sanity_stackreg_check(unsigned long *saveregs, int *stackbot, int *stacktop);
void gcstat_normal(unsigned allocsize,
	 	   unsigned oldsize, double oldratio, 
		   unsigned newsize, unsigned copied);
void gcstat_finish(unsigned long allocptr);

void gcstat_heapprofile_beforecollect(value_t *bot, value_t *top);
void gcstat_heapprofile_aftercollect(value_t *bot, value_t *top);
void gcstat_show_heapprofile();


#endif
