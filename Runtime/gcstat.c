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


#ifdef HEAPPROFILE
#define HP_AGESHIFT 16
extern Object_Profile_t allocated_object_profile;
extern Object_Profile_t collected_object_profile;
#endif


extern int NumGC;



int TotalBytesAllocated = 0;
int TotalBytesCollected = 0;
int TotalGenBytesCollected = 0;

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



void gcstat_normal(unsigned allocsize,
		   unsigned oldsize, double oldratio, 
		   unsigned newsize, unsigned copied)
{
  /*
    printf("Cheap diag: size %d, ratio=%.2f  --->  newsize=%d   copied=%d\n",
	   oldsize,oldratio,newsize,copied);
	   */
  TotalBytesAllocated += allocsize;
  TotalBytesCollected += copied;
}



void gcstat_finish(unsigned long allocsize)
{
/*  printf("TotalBytesAllocated 4: %ld\n",TotalBytesAllocated); */
  TotalBytesAllocated += allocsize;
/*  printf("TotalBytesAllocated 4: %ld\n",TotalBytesAllocated); */
  TotalBytesCollected += TotalGenBytesCollected;
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

/* tagstart points to where the raw object starts.
   If no object is present, then a skip tag is there.
   Otherwise, the profile tag is first followed
   by the object tag(s) and the object data.
   result if non-zero is set to start of obj */
unsigned long objlength(value_t *tagstart, value_t **result)
{
  value_t tag, *i;
  if ((value_t)tagstart < 256)
    {
      printf("tagstart is %d\n",tagstart);
      exit(-1);
    }
  if (*tagstart == SKIP_TAG)
    return 1;
  tagstart++;
  tag = *tagstart;
  if (result)
    *result = tagstart + 1;
  switch (GET_TYPE(tag))
  {
    case IARRAY_TAG:
    case RARRAY_TAG:
    case ARRAY_TAG:
    {
      int bytelen = GET_ARRLEN(tag);
      if (bytelen == 0)
	return 3;
      else
	return 2 + ((bytelen + 3) / 4);
    case FORWARD_TAG:
      {
	value_t *newdata = ((value_t *)tagstart[1]);
	value_t *newstart = newdata - 1;
	while (GET_TYPE(*newstart) == RECORD_SUB_TAG)
	  newstart--;
	newstart--;
	return objlength(newstart,0);
      }
  case RECORD_TAG:
  case RECORD_SUB_TAG:
    {
      int taglen = 0, fieldlen = 0;
      int tag, curlen;
      value_t *tagpos;
      for (tagpos=tagstart; 1; tagpos++)
	{
	  tag = *tagpos;
	  if ((GET_TYPE(tag) == FORWARD_TAG))
	    {
	      value_t *newadd = tagpos[1];
	      value_t *newstart = newadd - taglen - 2; 
	      /* two for prof and forward tag */
	      return objlength(newstart,0);
	    }
	  if ((GET_TYPE(tag) != RECORD_TAG) &&
	      (GET_TYPE(tag) != RECORD_SUB_TAG))
	    {
	      printf("tagstart = %d\n",tagstart);
	      memdump("objlength expects record; encountered strange tags",
		      (int *)tagstart-10,15,(int *)tagpos);
	      assert(0);
	    }
	  curlen = GET_RECLEN(tag);
	  taglen++;
	  if (curlen < MAX_RECORDLEN)
	    {
	      fieldlen += curlen;
	      break;
	    }
	  fieldlen += MAX_RECORDLEN;
	}
      if (fieldlen == 0) /* empty records still have a space reserved */
	fieldlen = 1;
      if (result)
	*result += taglen - 1;
      return 1 + taglen + fieldlen;
    }
  case SKIP_TAG:
  default:
    {
      value_t *i;
      printf("bad tag %d at %d\n",tag,tagstart);
      memdump("",tagstart-10,30,tagstart);
      printf("\n\n\n");
      printf("NumGC is %d\n",NumGC);
    }
    assert(0);
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
    if (proftag == SKIP_TAG)
      { cur++; continue; }
    if (GET_TYPE(tag) == FORWARD_TAG)
      {
	printf("\n\nproftag,tag is %d/%d  at  %d\n", proftag,tag,cur);
	printf("heapprofile: no forward tags should be in from space\n");
	assert(0);
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
    if (proftag == SKIP_TAG)
      { cur++; continue; }
    else 
      {
	ProfileTagEntry_t *x = GetProfileTagEntry(proftag);
	int size = objlength(cur,0);
	cur += size; 
	if (tag == FORWARD_TAG)
	  x->copied_size += size;
	else if (age)   /* should be called for old objects that just died */
	  {
	    if (NumGC < age)
	      {
		printf("cur = %d\n",cur);
		printf("proftag = %d\n",proftag);
		printf("tag = %d\n",tag);
		printf("NumGC = %d\n",NumGC);
		printf("age = %d\n",age);
		assert(0);
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
  long total_allocated_size = TotalBytesAllocated / 4;
  long total_copied_size    = TotalBytesCollected / 4;
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

