#include "queue.h"
#include "hash.h"
#include "tag.h"
#include "general.h"
#include "gc.h"
#include "memobj.h"
#include "global.h"
#include "stack.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "stats.h"
#include "gcstat.h"
#include "show.h"

#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif


extern int NumGC;
int KBytesAllocated = 0;
int KBytesCollected = 0;


void object_profile_init(Object_Profile_t *p)
{
  p->RecordWord = 0;
  p->Record = 0;
  p->Pair = 0;
  p->IArray = 0;
  p->RArray = 0;
  p->PArray = 0;
  p->IArrayWord = 0;
  p->RArrayWord = 0;
  p->PArrayWord = 0;
}

void show_gcstats_help(char *msg, double copy, double alloc)
{
  double eps = 1e-5;
  if ((copy == (double)(long)copy) && (alloc == (double)(long)alloc))
    printf("  %15s   Copy/Alloc: %10d/%10d(%.2lf)\n",
	   msg,(long)copy, (long)alloc, copy/(alloc+eps));
  else
    printf("  %15s   Copy/Alloc: %10.2f/%10.2f(%.2lf)\n",
	   msg,copy, alloc, copy/(alloc+eps));
}

void show_gcstats(Object_Profile_t *alloc, Object_Profile_t *collect)
{
  double eps = 1e-5;
  printf("GC %d completed\n",NumGC);
  printf("Statistics:\n");
  show_gcstats_help("ObjCount",
		    collect->Record + collect->IArray + collect->RArray + collect->PArray,
		    alloc->Record + alloc->IArray + alloc->RArray + alloc->PArray);
  printf("\n");
  show_gcstats_help("Records", collect->Record, alloc->Record);
  show_gcstats_help("RecordsWord", collect->RecordWord, alloc->RecordWord);
  show_gcstats_help("AvgRecordWord", collect->RecordWord / (collect->Record + eps), 
		    alloc->RecordWord / (alloc->Record + eps));
  show_gcstats_help("Arrays", 
		    collect->IArray + collect->RArray + collect->PArray,
		    alloc->IArray + alloc->RArray + alloc->PArray);
  show_gcstats_help("ArraysWord", 		    
		    collect->IArrayWord + collect->RArrayWord + collect->PArrayWord,
		    alloc->IArrayWord + alloc->RArrayWord + alloc->PArrayWord);
  show_gcstats_help("AvgArrayWord", 
		    (collect->IArrayWord + collect->RArrayWord + collect->PArrayWord) / 
		    (collect->IArray + collect->RArray + collect->PArray + eps),
		    (alloc->IArrayWord + alloc->RArrayWord + alloc->PArrayWord) / 
		    (alloc->IArray + alloc->RArray + alloc->PArray + eps));
  printf("\n\n");
  show_gcstats_help("Pairs", collect->Pair, alloc->Pair);
  show_gcstats_help("IArray_count", collect->IArray, alloc->IArray);
  show_gcstats_help("IArray_size", collect->IArrayWord, alloc->IArrayWord);
  show_gcstats_help("RArray_count", collect->RArray, alloc->RArray);
  show_gcstats_help("RArray_sizes", collect->RArrayWord, alloc->RArrayWord);
  show_gcstats_help("PArray", collect->PArray, alloc->PArray);
  printf("\n\n");

}





void gc_sanity_stackreg_check(unsigned long *saveregs, Heap_t *fromspace,
			      int *stackbot, int *stacktop)
{
  int mi, i;
  int *wordptr;

#ifdef PARANOID
  for (i=0; i<32; i++)
    {
      static value_t debug_reg_val = 64;
      value_t *data_add =  (value_t *)&(saveregs[i]);
      value_t data = *data_add;
      if (i == ALLOCPTR ||
	  i == ALLOCLIMIT)
	continue;
      if (data >= fromspace->bottom && data < fromspace->top)
	{
	    printf("WARNING: reg %d has fromspace value %d"
		   " -- now resetting to %d\n",i,data,debug_reg_val);
	  saveregs[i] = debug_reg_val; 
	  debug_reg_val += 16;
	}
    }
  
  for (wordptr=stackbot; wordptr<stacktop; wordptr ++)
    {
      static value_t debug_stack_val = 10000;
      value_t *data_add =  (value_t *)wordptr;
      value_t data = *data_add;
      if (data >= fromspace->bottom && data < fromspace->top)
	{
	    printf("WARNING: stack loc %d has fromspace value %d;"
		   "resetting to %d\n",
		   data_add,data,debug_stack_val);
	  *data_add = debug_stack_val; 
	  debug_stack_val += 16;
	}
    }
#endif

}



#ifdef HEAPPROFILE
struct ProfileTagEntry
{
  unsigned long allocated_count;
  unsigned long allocated_size;
  unsigned long copied_size;
  unsigned long total_age;
  unsigned long total_oldcount;
};
typedef struct ProfileTagEntry ProfileTagEntry_t;
static HashTable_t *table = NULL;

ProfileTagEntry_t *GetProfileTagEntry(unsigned int proftag)
{
  unsigned int key = (proftag << (32-HP_AGESHIFT)) >>  (32-HP_AGESHIFT);
  HashEntry_t *entry = NULL;
  if (table == NULL)
	table = CreateHashTable(2000);
  
  entry = HashTableLookup(table,key);
  if (entry != NULL)
	return (ProfileTagEntry_t *)(entry->data);
  {
	ProfileTagEntry_t *tagentry = malloc(sizeof(ProfileTagEntry_t));
	HashEntry_t entry;
	entry.key = key;
	entry.data = (void *)tagentry;
	tagentry->allocated_count = 0;
	tagentry->allocated_size = 0;
	tagentry->copied_size = 0;
	tagentry->total_age = 0;
	tagentry->total_oldcount = 0;
        HashTableInsert(table,&entry,0);
	return (ProfileTagEntry_t *)(entry.data);
  }
}




#ifdef HEAPPROFILE
/* parameters denote region of newly allocated objects */
/* note that bot should be start of NEW allocation not copied stuff */
void gcstat_heapprofile_beforecollect(value_t *bot, value_t *top)
{
  value_t *cur = bot;
  value_t *limit = top;
  value_t temp;

  while (cur < limit)
  {
    int proftag = cur[0];
    int tag = cur[1];
    int size = 0;
    if (GET_TYPE(proftag) == SKIP_TAG) {
      cur += proftag >> SKIPLEN_OFFSET;
      continue; 
    }
    if (GET_TYPE(tag) == FORWARD_TAG) {
	fprintf(stderr,"\n\nproftag,tag is %d/%d  at  %d\n", proftag,tag,cur);
	fprintf(stderr,"heapprofile: no forward tags should be in from space\n");
	DIE("heap profile");
      }
    size = objlength(cur,&temp);
    update_object_profile(&allocated_object_profile,(value_t *)temp);
    cur += size;
    GetProfileTagEntry(proftag)->allocated_count++;
    GetProfileTagEntry(proftag)->allocated_size += size;
  }
}

/* parameters denote region of newly allocated objects after GC */
void gcstat_heapprofile_aftercollect(value_t *bot, value_t *top)
{
  value_t *cur = bot;
  while (cur < top)
  {
    int proftag = cur[0];
    int age = proftag >> HP_AGESHIFT;
    int tag = cur[1];
    if (GET_TYPE(proftag) == SKIP_TAG) {
      cur += proftag >> SKIPLEN_OFFSET;
      continue; 
    }
    else {
	ProfileTagEntry_t *x = GetProfileTagEntry(proftag);
	int size = objlength(cur,0);
	cur += size; 
	if (tag == FORWARD_TAG)
	  x->copied_size += size;
	else if (age)   /* should be called for old objects that just died */
	  {
	    if (NumGC < age)
	      {
		fprintf(stderr,"cur = %d\n",cur);
		fprintf(stderr,"proftag = %d\n",proftag);
		fprintf(stderr,"tag = %d\n",tag);
		fprintf(stderr,"NumGC = %d\n",NumGC);
		fprintf(stderr,"age = %d\n",age);
		DIE("bad object age");
	      }
	    x->total_age += NumGC - age;
	    x->total_oldcount++;
	  }
      }
  }
  {
    int i, temp = 0;
    for (i=0; i<table->size; i++)
      {
	HashEntry_t f = table->table[i];
	ProfileTagEntry_t *e = (ProfileTagEntry_t *)(f.data);
	if (f.key < 0 || e == NULL)
	  continue;
	temp += e->copied_size;
      }
  }
}
#endif

void gcstat_show_heapprofile(char *label, double alloc_tol, double copy_tol)
{
  int i;
  double eps = 1e-5;
  long total_allocated_size = KBytesAllocated * 256;  /* in words */
  long total_copied_size    = KBytesCollected * 256;
  long printed_entry = 0;
  double old_cutoff = 0.8;
  double cutoff_copy = 0.0;
  double cutoff_alloc = 0.0;
  long tally_copy = 0;
  long tally_alloc = 0;

  printf("------ heap profile start : %s -----------------------------------------\n",label);
  printf("\nNumGC = %d\n\n",NumGC);
  printf("         alloc      alloc     alloc              avg  copied  copied       \n");
  printf("site       %%         size      count    %% old     age    size    %%     cs/as  \n");
  printf("--------------------------------------------------------------------------------\n");
  for (i=0; i<table->size; i++)
  {
    HashEntry_t f = table->table[i];
    ProfileTagEntry_t *e = (ProfileTagEntry_t *)(f.data);
    if (f.key < 0 || e == NULL)
	continue;
    tally_alloc += e->allocated_size;
    tally_copy += e->copied_size;
    if (((100.0 * e->allocated_size)/total_allocated_size < alloc_tol) &&
	((100.0 * e->copied_size)/(eps + total_copied_size) < copy_tol))
	continue;
    printed_entry++;
    printf("%5d   %5.2f%%  %9d  %9d   %6.2f  %6.1f %7d  %6.2f  %4.2lf",
	   f.key,
	   (100.0 * e->allocated_size)/total_allocated_size,
	   e->allocated_size,
	   e->allocated_count,
	   (100.0 * e->total_oldcount)/(eps + e->allocated_count),
	   ((double)(e->total_age))/(eps+e->total_oldcount),
	   e->copied_size,
	   (100.0 * e->copied_size/(eps+total_copied_size)),
	   (e->copied_size) / (eps + e->allocated_size)
	   );
    if (e->total_oldcount/(eps + e->allocated_count) >= old_cutoff)
      {
	cutoff_copy += 100.0 * e->copied_size/(eps+total_copied_size);
	cutoff_alloc += 100.0 * e->allocated_size/(eps+total_allocated_size);
	printf(" <--");
      }
    printf("\n");
  }
  printf("-------------  heap profile end : %s ------------------------------------\n",label);
  printf("Showing only entries with alloc %% > %4.2lf\n",alloc_tol);
  printf("                  or with copy  %% > %4.2lf\n",copy_tol);
  printf("%d of %d entries displayed.\n",printed_entry,table->size,label);
  printf("Using a (%% old) cutoff of %5.2lf,\n"
	 "  targeted sites comprise %5.2lf%% copied and %5.2lf%% allocated.\n",
	 old_cutoff, cutoff_copy, cutoff_alloc);
  printf("----------------------------------------------------------------------------\n");
  printf("tally_alloc = %d\n",tally_alloc);
  printf("tally_copy = %d\n",tally_copy);
  printf("total_allocated_size = %d\n",total_allocated_size);
  printf("total_copied_size = %d\n",total_copied_size);
}
#endif

