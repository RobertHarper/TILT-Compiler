#include "tag.h"
#include "queue.h"
#include "show.h"
#include "gc.h"
#include "memobj.h"

#define LEAST_GC_TO_CHECK -1


#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif

#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "stats.h"
#include "gcstat.h"
#include "general.h"


enum GCType { Minor, Major, ForcedMajor, Complete };



#define WRITE
#define OLD_ALLOC

#ifdef OLD_ALLOC
value_t old_alloc_ptr = 0;
value_t old_alloc_limit = 0;
#endif



extern Queue_t *ScanQueue;

/* use generational by default */
long generational_flag = 1;

#ifdef HEAPPROFILE
int HeapProfileFlag = 0;
#define HP_AGESHIFT 16
#endif


struct range_st
{
  value_t low;
  value_t high;
  value_t diff;
};

typedef struct range_st range_t;
static range_t null_range;

void SetRange(range_t *range, value_t low, value_t high)
{
  range->low = low;
  range->high = high;
  range->diff = high - low;
}

#ifdef WRITE
/* enough room to process 64K writes before forcing a GC */
/* int writelist[64 * 1024]; */
HeapObj_t *writelist_obj = NULL;
int writelist_cursor = 0;
int writelist_start = 0;
int writelist_end = 0;
int write_count = 0;
#endif


static int floatheapsize = 16384 * 1024;
static int floatbitmapsize = 128;
long SHOW_GCDEBUG_FORWARD = 0;
long SHOW_GCERROR   = 1;
long SHOW_GCSTATS   = 0;
long SHOW_GCDEBUG   = 0;
long SHOW_HEAPS     = 0;
long SHOW_GCFORWARD = 0;
extern long TotalGenBytesCollected;
extern long TotalBytesAllocated;
int NumGC = 0;
int NumMajorGC = 0;
long NumLocatives = 0;

#ifdef HEAPPROFILE
Object_Profile_t allocated_object_profile;
Object_Profile_t collected_object_profile;
#endif

extern timer_mt gctime, stacktime, majorgctime;
HeapObj_t *fromheap = NULL;
HeapObj_t *old_fromheap = NULL, *old_toheap = NULL;
HeapObj_t *floatheap = NULL;
Bitmap_t  *floatbitmap = NULL;
Queue_t   *float_roots = 0;
HeapObj_t *toheap = NULL;

extern int InScheduler;
extern int module_count;
long CurHeapLimit = 0;
extern int GLOBAL_TABLE_BEGIN_VAL;
extern int GLOBAL_TABLE_END_VAL;
extern int MUTABLE_TABLE_BEGIN_VAL;
extern int MUTABLE_TABLE_END_VAL;
extern int SML_GLOBALS_BEGIN_VAL;
extern int SML_GLOBALS_END_VAL;


typedef void (forwarder_t)(value_t *, value_t *);
value_t* scan_major(value_t start_scan, value_t *alloc_ptr, value_t *stop, 
		    range_t *from_range, range_t *from2_range, range_t *to_range);
value_t* scan_nostop_minor(value_t start_scan, value_t *alloc_ptr,
		   range_t *from_range, range_t *to_range);
value_t* scan_stop_minor(value_t start_scan, value_t *alloc_ptr, value_t *stop,
		       range_t *from_range, range_t *to_range);


long YoungHeapByte = 0, MaxHeap = 0, MinHeap = 0;
double TargetRatio = 0.0, MaxRatio = 0.0;
double UpperRatioReward = 0.0, LowerRatioReward = 0.0;
double TargetSize = 0.0, SizePenalty = 0.0;

long ComputeHeapSize(long oldsize, double oldratio)
{
  double res = 0.0;
  double diff_ratio = (oldratio - TargetRatio) / TargetRatio;
  double diff_size  = (oldsize - 1024 * TargetSize) / (double) (1024 * TargetSize);
  double where = oldsize / (1024.0 * MaxHeap);
  double RatioReward = UpperRatioReward * (1 - where) + LowerRatioReward * where;
  double k_ratio = (diff_ratio < 0.0) ? 1.0 : (1.0 + diff_ratio * RatioReward);
  double k_size  = (diff_size < 0.0) ? 1.0 : (1.0 + diff_size * SizePenalty);
  if (k_size > 2.0)
    k_size = 2.0;
  res = oldsize * k_ratio / k_size;
  if (res < ((oldratio / MaxRatio) * oldsize))
      res = ((oldratio / MaxRatio) * oldsize);
  if (res >= 2.5 * oldsize)
    res = 2.5 * oldsize;
  if (res < 1024 * MinHeap)
    res = 1024 * MinHeap;
  if (res > (1024 * MaxHeap))
    res = 1024 * MaxHeap;
  
  if (!(res >= (long)(oldsize * oldratio)))
    {
      assert(0);
    }
  return ((long)res) / 4 * 4;
}

void debug_after_rootscan(unsigned long *saveregs, int regmask, Queue_t *root_lists)
{
#ifdef DEBUG
  int allocptr = saveregs[ALLOCPTR_REG];
    if (SHOW_GCDEBUG && NumGC > LEAST_GC_TO_CHECK)
      {
	int i,j;
	printf("\n--------------- ROOT INFORMATION ---------------\n");
	for (i=0; i<32; i++)
	  if (regmask & (1 << i))
	    printf("LIVE REGISTER VALUE %d: %d\n",i,saveregs[i]);
	
	for (j=0; j<QueueLength(root_lists); j++)
	  {
	    Queue_t *roots = QueueAccess(root_lists,j);
	    for (i=0; i<QueueLength(roots); i++)
	      printf("ROOT #%d: %d(%d)\n",
		     i,QueueAccess(roots,i),*((int *)QueueAccess(roots,i)));
	  }
      }
    if (SHOW_HEAPS)
      {
	if (SHOW_HEAPS)
	  memdump("From Heap Before collection:", (int *)fromheap->bottom,100,0);
	show_heap("ORIG FROM",fromheap->bottom,allocptr,fromheap->top); 
	memdump("OLD_FROMHEAP",(int *)old_fromheap->bottom,
		((value_t)old_alloc_ptr - (value_t)old_fromheap->bottom)/4,0);
	show_heap("OLD_FROMHEAP",old_fromheap->bottom,old_alloc_ptr,
		  old_fromheap->top); 
      }
    else
      {
	check_heap("ORIG FROM",fromheap->bottom,allocptr,fromheap->top); 
	check_heap("OLD_FROMHEAP",old_fromheap->bottom,old_alloc_ptr,
		   old_fromheap->top); 
      }
#endif
}

int root_scan(unsigned long *saveregs, long sp, long ret_add, Queue_t *root_lists, 	      
	      Queue_t *uninit_global_roots, Queue_t *global_roots, Queue_t *promoted_global_roots)
{
  int regmask = 0;
  unsigned long i,mi, stack_top;
  StackObj_t *stack = GetStack(sp);
  static int first_time = 1;
  static Queue_t *reg_roots = 0;


  stack_top = stack->top;
  
  start_timer(&stacktime);
  QueueClear(root_lists);
  
  regmask = trace_stack(saveregs, sp, ret_add, stack->top, root_lists);
  
  stop_timer(&stacktime);
  regmask |= 1 << EXNPTR_REG;
  if (first_time)
    reg_roots = QueueCreate(32);
  else
    QueueClear(reg_roots);
  for (i=0; i<32; i++)
    if (regmask & (1 << i))
      Enqueue(reg_roots,(int *)(&(saveregs[i])));
  Enqueue(root_lists,reg_roots);

  if (first_time)
  {
    for (mi=0; mi<module_count; mi++)
      for (i=(value_t)*((&GLOBAL_TABLE_BEGIN_VAL)+mi);
	   i<(value_t)*((&GLOBAL_TABLE_END_VAL)+mi); i+=16)
	{
	  value_t table_entry = ((value_t *)i)[0];
	  int     trace       = ((value_t *)i)[1];
	  value_t data = *((value_t *)table_entry);
	  int should_trace = 0;
	  if (IS_TRACE_YES(trace))
	    Enqueue(uninit_global_roots,(value_t *)table_entry);
	  else if (IS_TRACE_NO(trace))
	      ;
	  else if (IS_TRACE_CALLEE(trace))
	    { printf("cannot have trace_callee for globals\n"); exit(-1); }
	  else if (IS_TRACE_SPECIAL(trace))
	    { printf("cannot have trace_special for globals\n"); exit(-1); }
	}
  }
  QueueClear(promoted_global_roots);
  if (QueueLength(uninit_global_roots))
    {
      int i = 0, len = QueueLength(uninit_global_roots);
      for (i=0; i<len; i++)
	{
	  value_t *data_addr = Dequeue(uninit_global_roots);
	  if (*data_addr > 255)
	    {
	      Enqueue(global_roots,data_addr);
	      Enqueue(promoted_global_roots,data_addr);
	    }
	  else
	    Enqueue(uninit_global_roots,data_addr);
	}
    }

  debug_after_rootscan(saveregs,regmask,root_lists);
  first_time = 0;
  return stack_top;
}

void debug_and_stat_before(unsigned long *saveregs, long sp, 
			    long ret_add, long req_size)
{
  /* -------- print some debugging info and gather write statistics ---------- */
  int i;
  int allocptr = saveregs[ALLOCPTR_REG];
  write_count += (writelist_cursor - writelist_start) / sizeof(int);

  if (req_size == 0)
    {
      fprintf(stderr,"req_size = 0    means writelist_full.\n");
    }
#ifdef DEBUG
  if (SHOW_GCDEBUG && NumGC > LEAST_GC_TO_CHECK)
    {
      printf("\n\n");
      printf("--------gc_handler entered---------\n");
      printf("ret_add = %d   *ret_add = %d\n",ret_add,*((int *)ret_add));
      printf("alloc/curheaplimit: %d %d\n",allocptr,CurHeapLimit);
      printf("sp of first ML frame is %ld\n",sp);
      printf("REGISTERS SAVED AT: %ld\n",saveregs);
      printf("\n\n");
      for (i=0; i<32; i++)
	{
	  printf("%2d - *%d: %10ld",i,&(saveregs[i]),saveregs[i]);
	  if (i == ALLOCPTR_REG)
	    printf("    allocptr");
	  if (i == ALLOCLIMIT_REG)
	    printf("    alloclimit");
	  printf("\n");
	}
      printf("CurHeapLimit  = %d\n",CurHeapLimit);
      printf("LowHeapLimit  = %d\n",LowHeapLimit);
      printf("-----------------------------------\n");
    }
#endif
}

/* ------------------ Forwarding Routines ------------------ */


/* This should not be called directly. */
value_t *forward_raw(value_t *vpp, value_t *alloc_ptr)
{
  value_t *v = (value_t *)(*vpp);
  int tag;
unsigned int a,b;

  int foobar = *(alloc_ptr + 4); 
  /* read some garbage to simulate write-allocate */


#ifdef DIAG
  update_object_profile(&collected_object_profile,v);
#endif
  tag = v[-1];

  switch (tag)
    {
    case TAG_REC_EMPTY: /* empty records have one word of storage */
    case TAG_REC_INT:
    case TAG_REC_TRACE:
#ifdef HEAPPROFILE
	*alloc_ptr = v[-2];
	SetBirth(alloc_ptr);
	alloc_ptr++;
#endif
      {
	value_t *temp = alloc_ptr;
	temp[0] = v[-1];
	temp[1] = v[0];
	v[-1] = FORWARD_TAG;
	v[0] = (value_t)(temp+1);
	*vpp = (value_t)(temp+1);
	alloc_ptr = temp+2;
#ifdef SEMANTIC_GARBAGE
      update_lifetime(&v[-1],temp,1);
#endif
      }
      return alloc_ptr;
    case TAG_REC_INTINT:
    case TAG_REC_TRACEINT:
    case TAG_REC_INTTRACE:
    case TAG_REC_TRACETRACE:
#ifdef HEAPPROFILE
      *alloc_ptr = v[-2];
      SetBirth(alloc_ptr);
      alloc_ptr++;
#endif
      {
	value_t *temp = alloc_ptr;
	temp[0] = v[-1];
	temp[1] = v[0];
	temp[2] = v[1];
	v[-1] = FORWARD_TAG;
	v[0] = (value_t)(temp+1);
	*vpp =  (value_t)(temp+1);
	alloc_ptr = temp+3;
#ifdef SEMANTIC_GARBAGE
      update_lifetime(&v[-1],temp,2);
#endif
      }
      return alloc_ptr;
    default:
      break;
    }

  switch (GET_TYPE(tag))
    {
    case IARRAY_TAG:
    case ARRAY_TAG:
    case RARRAY_TAG:
      {
	value_t *rawstart = v-1;
	int i,upper = GET_POSSLEN(tag), newadd = 0;
	upper = (upper + 3) / 4;

	if (floatheap &&
	    ((value_t) v) >= floatheap->bottom &&
	    ((value_t) v) < floatheap->top)
	  {
	    Enqueue(float_roots,(void *)v);
	    return alloc_ptr;
	  }
	
	if (GET_TYPE(tag) == RARRAY_TAG)
	  {
	    int temp = (int) (alloc_ptr);
	    int oldage = GET_ARRAY_AGE(tag);
	    int newage = (oldage>=3) ? oldage : (oldage+1);
/* odd/even-word align the pointer so data is even/odd-aligned */
#ifdef HEAPPROFILE
	    if ((temp & 7) == 4)  
#else
	    if ((temp & 7) == 0)
#endif
	      {
		*((int *)temp) = SKIP_TAG;
		alloc_ptr = (value_t *)(temp+4);
	      }
	    v[-1] &= ~(0x3 << 3);
	    v[-1] |= (newage << 3);
	  }
#ifdef HEAPPROFILE
	    *alloc_ptr = v[-2];
	    SetBirth(alloc_ptr);
	    alloc_ptr++;
#endif
	/* empty arrays have 1 word of storage */
	if (upper == 0)
	  upper = 1;

	bcopy((char *)rawstart,(char *)(alloc_ptr),4*(1+upper));
	newadd = ((int)(alloc_ptr)) + 4;
	alloc_ptr += (1 + upper);
	v[-1] = FORWARD_TAG;
	v[0] = newadd;
	*vpp = newadd;
#ifdef SEMANTIC_GARBAGE
	update_lifetime(&v[-1],(value_t *)(((int)newadd)-4),upper);
#endif
	return alloc_ptr;
      }
    case RECORD_TAG:
    case RECORD_SUB_TAG:
      {
	value_t *rawstart = v-1;
	int upper = -1; 
        if (IS_RECORD(rawstart[0]))
	  {
	    int curlen = GET_RECLEN(rawstart[0]);
	    if (curlen > MAX_RECORDLEN)
	      curlen = MAX_RECORDLEN;
	    upper += curlen + 1;
	  }
	else
	  {
	    while (IS_RECORD_SUB(rawstart[0]))
	      {
		int curlen = GET_RECLEN(rawstart[0]);
		if (curlen > MAX_RECORDLEN)
		  curlen = MAX_RECORDLEN;
		rawstart--;
		upper += curlen + 1;
	      }
	    if (IS_RECORD(rawstart[0]))
	      {
		int curlen = GET_RECLEN(rawstart[0]);
		if (curlen > MAX_RECORDLEN)
		  curlen = MAX_RECORDLEN;
		upper += curlen + 1;
	      }
	    else
	      BUG("gc_forward_gen: bad record");
	  }
#ifdef HEAPPROFILE
	*alloc_ptr = rawstart[-1];
	SetBirth(alloc_ptr);
	alloc_ptr++;
#endif
	switch (upper)
	  {
	    /* empty records have one word of storage */
	  case 0: 
	  case 1:
	    {
	      int *temp = (int *)alloc_ptr;
	      temp[0] = rawstart[0];
	      temp[1] = rawstart[1];
	      v[-1] = FORWARD_TAG;
	      v[0] = (int)(((value_t)alloc_ptr) + ((value_t)v-(value_t)rawstart));
	      alloc_ptr += 2;
	      *vpp = v[0];
#ifdef SEMANTIC_GARBAGE
	    update_lifetime(&v[-1],(value_t *)(((int)(alloc_ptr))-8),1);
#endif
	      return alloc_ptr;
	    }
	  case 2:
	    {
	      int *temp = (int *)alloc_ptr;
	      value_t newadd = (value_t)temp + ((value_t)v-(value_t)rawstart);
	      temp[0] = rawstart[0];
	      temp[1] = rawstart[1];
	      temp[2] = rawstart[2];
	      alloc_ptr += 3;
	      v[-1] = FORWARD_TAG;
	      v[0] = (int)newadd;
	      *vpp = newadd;
	    }
#ifdef SEMANTIC_GARBAGE
	    update_lifetime(&v[-1],(value_t *)(((int)(*alloc_ptr))-12),2);
#endif
	    return alloc_ptr;
	  case 3:
	    {
	      int *temp = (int *)alloc_ptr;
	      temp[0] = rawstart[0];
	      temp[1] = rawstart[1];
	      temp[2] = rawstart[2];
	      temp[3] = rawstart[3];
	      v[-1] = FORWARD_TAG;
	      v[0] = (int)((value_t)temp + ((value_t)v-(value_t)rawstart));
	      alloc_ptr += 4;
	      *vpp = v[0];
#ifdef SEMANTIC_GARBAGE
	    update_lifetime(&v[-1],(value_t *)(((int)(alloc_ptr))-16),3);
#endif
	    }
	    return alloc_ptr;
	  default:
	    bcopy((char *)rawstart,(char *)(alloc_ptr),4*(1+upper));
	    v[-1] = FORWARD_TAG;
	    v[0] = (int)(((value_t)alloc_ptr) + ((value_t)v-(value_t)rawstart));
	    alloc_ptr += (1 + upper);
	    *vpp = v[0];
#ifdef SEMANTIC_GARBAGE
	    update_lifetime(&v[-1],(value_t *)(((int)(alloc_ptr))-4*(1+upper)),upper);
#endif
	    return alloc_ptr;
	  }
	break;
      }
    case FORWARD_TAG:
      *vpp = v[0];
      return alloc_ptr;
    case SKIP_TAG:

      BUG("gc_forward_gen: IMPOSSIBLE to get SKIP_TAG");
    default:
      printf("\n\ntag = %d\n",tag);
      BUG("gc_forward_gen: IMPOSSIBLE TAG");
    }
  foobar++;
  assert(FALSE);
}

/* ------------ These forwarding routines/macros are the ones to use --------- */

value_t *forward_major(value_t *vpp, value_t *alloc_ptr, 
			range_t *from_range, range_t *from_range2,
			range_t *to_range)
{
  value_t *v = (value_t *)(*vpp);

  if (!(((value_t)v - (from_range->low) < from_range->diff) ||
	((value_t)v - (from_range2->low) < from_range2->diff)))
    {
      return alloc_ptr;
    }
  return forward_raw(vpp,alloc_ptr);
}

/* At a major collection, objects in the nursery and old_from_space both need
   to be forwarded.  */
#define forward_mac_major(a,b,from,from2,to)  \
{ if ((((*(a)) - local_to_low) >= local_to_diff) &&(*(a) > 255)) \
b = forward_major(a,b,from,from2,to); }

/* At a minor collection, only objects in the nursery need to be forwarded */
#define forward_mac_minor(a,b,from)  \
{ if (((*(a)) - (from)->low) < (from)->diff) b = forward_raw(a,b); }

#define forward_mac_local_minor(a,b)  \
{ if (((*(a)) - local_from_low) < local_from_diff) b = forward_raw(a,b); }


/* -------------------------------------------------------------- */



#ifdef SEMANTIC_GARBAGE
void update_lifetime(value_t *from, value_t *to, value_t sz)
{
  extern value_t semantic_garbage_offset;
  int curstamp = *((value_t *)(((value_t)from) + semantic_garbage_offset));
  int used = 0, i;
  assert((*from) == FORWARD_TAG);
  for (i=1; i<=sz; i++)
    {
      int temp = *(int *)(((value_t)(from + i)) + semantic_garbage_offset);
      if (temp == 0)
	{ 
	  used = 1; 
	  break; 
	}
    }
  /*
  if (used || ((value_t)from >= fromheap->bottom && (value_t)from < fromheap->top))
    printf("copied obj: rawsize = %d   loc = %d   accessed (TO %d)  (%d,%d)\n",
	   4*(1+sz),from,to,NumMajorGC,curstamp);
  else
    printf("copied obj: rawsize = %d   loc = %d   NOT accessed (TO %d)   (%d,%d)\n",
	   4*(1+sz),from,to,NumMajorGC,curstamp);
	   */
  for (i=1; i<=sz; i++)
     *(int *)(((value_t)(to + i)) + semantic_garbage_offset) = 98765;
  if ((value_t)from >= fromheap->bottom && (value_t)from < fromheap->top)
    if ((value_t)to >= old_toheap->bottom && (value_t)to < old_toheap->top)
      curstamp = NumMajorGC;
    else
      curstamp = NumMajorGC-1;
  else if (used)
    curstamp = NumMajorGC;
  *((value_t *)(((value_t)to) + semantic_garbage_offset)) = curstamp;
}
#endif


void update_object_profile(Object_Profile_t *prof, value_t *objstart)
{
  value_t *tagstart = objstart - 1;
  int tag = *tagstart;
  switch(GET_TYPE(tag))
    {
    case IARRAY_TAG:
      prof->IArray++;
      prof->IArrayWord += ((GET_POSSLEN(tag) + 3) / 4) + 1;
      break;
    case RARRAY_TAG:
      prof->RArray++;
      prof->RArrayWord += GET_POSSLEN(tag) / 4 + 1;
      break;
    case ARRAY_TAG:
      prof->PArray++;
      prof->PArrayWord += GET_POSSLEN(tag) / 4 + 1;
      break;
    case RECORD_TAG:
    case RECORD_SUB_TAG:
      {
	int len = 0;
	for(;;tagstart--)
	  {
	    int tag = *tagstart;
	    int temp = GET_RECLEN(tag);
	    if (temp > MAX_RECORDLEN)
	      len += MAX_RECORDLEN;
	    else 
	      len += temp;
	    if (GET_TYPE(tag) == RECORD_TAG)
	      break;
	    if (GET_TYPE(tag) != RECORD_SUB_TAG)
	      assert(0);
	  }
	prof->Record++;
	if (len == 2)
	  prof->Pair++;
	prof->RecordWord += len + 1;
      }
      return;
    case SKIP_TAG:
    case FORWARD_TAG:
      return;
    default:
      printf("objstart = %d\n",objstart);
      printf("Got a tag of %d\n",tag);
      assert(0);
      return;
    }
}

#ifdef HEAPPROFILE
void SetBirth(value_t *proftag_addr)
{
  unsigned int proftag = *proftag_addr;
  unsigned int oldage = proftag >> HP_AGESHIFT;
  /*
  printf("NumGC = %d   progtag_addr = %d, proftag = %d, oldage = %d\n",
	 NumGC,proftag_addr, proftag,oldage);
	 */
  if (oldage == 0)
    proftag += NumGC << HP_AGESHIFT;
  *proftag_addr = proftag;
}
#endif




/*  ------- old debugging code -------
#ifdef DEBUG
  if (IsGlobalData(*vpp))
    {
      if (SHOW_GCDEBUG_FORWARD)	printf("NOT forwarding global data at loc %d value %d\n",vpp,v);
      return alloc_ptr;
    }
  if (IsConstructorData(*vpp))
    {
      if (SHOW_GCDEBUG_FORWARD)	printf("NOT forwarding construtor data at loc %d value %d\n",vpp,v);
      return alloc_ptr;
    }
      if ((value_t)v - from_range->low >= from_range->diff)
    {
      if (SHOW_GCDEBUG_FORWARD)
	if (!((*vpp)>=to_range->low || (*vpp < alloc_ptr))) 
	  printf("ERROR: NOT forwarding weird pointer *%d = %d\n",vpp,v);

      if (floatheap && ((value_t) v) >= floatheap->bottom &&
	  ((value_t) v) < floatheap->top)
	{
	  Enqueue(float_roots,(void *)v);
	}
      return alloc_ptr;
    }
  if (SHOW_GCFORWARD) printf("forwarding loc %d value %d\n",vpp,v);
#endif
  */





value_t *forward_root_lists_minor(Queue_t *root_lists, value_t *to_ptr, 
				 range_t *from_range, range_t *to_range)
{
  int i, j, rlen = QueueLength(root_lists);
  for (i=0; i<rlen; i++)
    {
      Queue_t *roots = QueueAccess(root_lists,i);
      int qlen = QueueLength(roots);
      for (j=0; j<qlen; j++)
	{
	  value_t *temp = (value_t *)QueueAccess(roots,j);
	  forward_mac_minor(temp,to_ptr,from_range);
	}
    }
  return to_ptr;
}


value_t *forward_root_lists_major(Queue_t *root_lists, value_t *to_ptr, 
				  range_t *from_range, range_t *from2_range, range_t *to_range)
{
  int i, j, rlen = QueueLength(root_lists);
  for (i=0; i<rlen; i++)
    {
      Queue_t *roots = QueueAccess(root_lists,i);
      int qlen = QueueLength(roots);
      for (j=0; j<qlen; j++)
	{
	  value_t *temp = (value_t *)QueueAccess(roots,j);
	  to_ptr = forward_major(temp,to_ptr,from_range,from2_range,to_range);
	}
    }
  return to_ptr;
}



value_t *forward_mutables_semi(value_t *to_ptr, 
			       range_t *from_range, range_t *to_range)
{
  int mi, i;
  for (mi=0; mi<module_count; mi++)
    for (i=(value_t)*((&MUTABLE_TABLE_BEGIN_VAL)+mi);
	 i<(value_t)*((&MUTABLE_TABLE_END_VAL)+mi); i+=4)
      {
	value_t *table_entry = (value_t *)(((value_t *)i)[0]);
	table_entry --;
	while (GET_TYPE(*table_entry) == RECORD_SUB_TAG)
	  table_entry--;
	to_ptr = scan_stop_minor(table_entry,to_ptr,table_entry+1,
				 from_range,to_range);
      }
  return to_ptr;
}

value_t *forward_mutables_gen(value_t *to_ptr, 
			      range_t *from_range, range_t *from2_range, range_t *to_range)
{
  int mi,i;
  for (mi=0; mi<module_count; mi++)
    {
      for (i=(value_t)*((&MUTABLE_TABLE_BEGIN_VAL)+mi);
	   i<(value_t)*((&MUTABLE_TABLE_END_VAL)+mi); i+=4)
	{
	  value_t *table_entry = (value_t *)(((value_t *)i)[0]);
	  table_entry --;
	  while (GET_TYPE(*table_entry) == RECORD_SUB_TAG)
	    table_entry--;
	  to_ptr = scan_major(table_entry,to_ptr,table_entry+1,
			      from_range,from2_range,to_range);
	}
    }
  return to_ptr;
}

value_t *forward_writelist_major(value_t *more_roots, value_t *to_ptr, 
				 range_t *from_range, range_t *from2_range, range_t *to_range)
{
  value_t *temp = 0;
  while (temp = (value_t *)(*(more_roots++)))
    if ((((value_t)temp) - from_range->diff >= from_range->diff) &&
	(((*temp) - from_range->low < from_range->diff) ||
	 ((*temp) - from2_range->low < from2_range->diff)))
      to_ptr = forward_major(temp,to_ptr,from_range,from2_range,to_range);
  return to_ptr;
}

value_t *forward_writelist_minor(value_t *more_roots, value_t *to_ptr, 
				 range_t *from_range, range_t *to_range)
{
  value_t *temp = 0;
  while (temp = (value_t *)(*(more_roots++)))
    if ((((value_t)temp) - from_range->diff >= from_range->diff) &&
	((*temp) - from_range->low < from_range->diff))
      forward_mac_minor(temp,to_ptr,from_range);
  return to_ptr;
}


value_t *forward_gen_locatives(value_t *to_ptr, 
			       range_t *from_range, range_t *from2_range, range_t *to_range)
{
  int i;
    static Queue_t *LastScanQueue = 0;
    if (LastScanQueue == 0)
      LastScanQueue = QueueCreate(100);

  /* --------- forward objects with unset and update unset from scanqueue --------- */
  NumLocatives += QueueLength(ScanQueue);
  for (i=0; i<QueueLength(ScanQueue); i++)
    {
      /* be very careful with type, in particular meta_addr is NOT a value_t **  */
      value_t *meta_addr = (value_t *)QueueAccess(ScanQueue,i);
      value_t *data_addr = (value_t *)(*meta_addr);
      value_t data = *data_addr;
      int offset = 0;
      value_t *obj_start = 0; 
      value_t *new_data_addr = 0;
      if (data <= 255)
	{
	  offset = 255 - data;
	  obj_start = data_addr - offset;
	}
      else /* the object containing the locative has been forwarded 
	      and the locative was in the forward-address slot, i.e. slot 0 */
	{
	  offset = 0;
	  obj_start = (value_t *)data;
	}
      to_ptr = forward_major((value_t *)(&obj_start),to_ptr,from_range,from2_range,to_range);
      if (data <= 255)
	new_data_addr = obj_start + offset;
      else
	new_data_addr = obj_start;
      *meta_addr = (value_t)new_data_addr;
    }
  

  /* ------ forward_int the unset locations from the last time and update ------ */

    for (i=0; i<QueueLength(LastScanQueue); i++)
      {
	value_t *data_addr = (value_t *)QueueAccess(LastScanQueue,i);
	to_ptr = forward_major(data_addr,to_ptr,from_range,from2_range,to_range); 
      }
    QueueClear(LastScanQueue);
    for (i=0; i<QueueLength(ScanQueue); i++)
      {
	value_t *meta_addr = (value_t *)QueueAccess(ScanQueue,i);
	Enqueue(LastScanQueue,(void *)(*meta_addr));
      }
    return to_ptr;
}




#define INT_INIT(x,y) { if (x == 0) x = y; }
#define DOUBLE_INIT(x,y) { if (x == 0.0) x = y; }

void gc_init()
{
  /* ??? 64 does not work for MaxHeap for some reason */
  int writelist_size = 0;
#ifdef HEAPPROFILE
  object_profile_init(&allocated_object_profile);
  object_profile_init(&collected_object_profile);
#endif
  SetRange(&null_range,0,0);
  if (generational_flag)
    { 
      /* secondary cache size */
      int cache_size = GetBcacheSize();
      INT_INIT(YoungHeapByte, (int)(0.85 * cache_size));
      writelist_size = (YoungHeapByte/2);

#ifdef SEMANTIC_GARBAGE
      INT_INIT(MaxHeap, 12 * 1024);
#else
      INT_INIT(MaxHeap, 32 * 1024);
#endif
      INT_INIT(MinHeap, 1024);
      if (MinHeap > MaxHeap)
	MinHeap = MaxHeap;
#ifdef OLD_ALLOC
      DOUBLE_INIT(TargetRatio, 0.3);
#else
      DOUBLE_INIT(TargetRatio, 0.4);
#endif
      DOUBLE_INIT(MaxRatio, 0.85);
      DOUBLE_INIT(UpperRatioReward, 1.5);
      DOUBLE_INIT(LowerRatioReward, 0.75);
      DOUBLE_INIT(TargetSize, 4096.0);
      DOUBLE_INIT(SizePenalty, 0.2);
      assert(MinHeap >= 1.2*(YoungHeapByte / 1024));
      fromheap = HeapObj_Alloc(YoungHeapByte, YoungHeapByte);
      old_fromheap = HeapObj_Alloc(MinHeap * 1024, MaxHeap * 1024);
      old_toheap = HeapObj_Alloc(MinHeap * 1024, MaxHeap * 1024);  
      floatheap = HeapObj_Alloc(floatheapsize,floatheapsize);
      floatbitmap = CreateBitmap(floatheapsize / floatbitmapsize);
      float_roots = QueueCreate(100);
#ifdef OLD_ALLOC
      old_alloc_ptr = old_fromheap->alloc_start;
      old_alloc_limit = old_fromheap->top;
#endif
    }
  else /* semispace GC */
    {
      INT_INIT(MaxHeap, 32 * 1024);
      INT_INIT(MinHeap, 256);
      if (MinHeap > MaxHeap)
	MinHeap = MaxHeap;
      DOUBLE_INIT(TargetRatio, 0.08);
      DOUBLE_INIT(MaxRatio, 0.8);
      DOUBLE_INIT(UpperRatioReward, 1.5);
      DOUBLE_INIT(LowerRatioReward, 0.75);
      DOUBLE_INIT(TargetSize, 8192.0);
      DOUBLE_INIT(SizePenalty, 0.1);
      fromheap = HeapObj_Alloc(MinHeap * 1024, MaxHeap * 1024);
      toheap = HeapObj_Alloc(MinHeap * 1024, MaxHeap * 1024);  
      writelist_size = (MinHeap*1024/2);
    }
#ifdef WRITE
  {
    writelist_obj = HeapObj_Alloc(writelist_size,writelist_size);
#ifdef DEBUG
    printf("YoungHeapByte = %d\n",YoungHeapByte);
    printf("writelist_obj->bottom = %d\n",writelist_obj->bottom);
    printf("writelist_obj->top = %d\n",writelist_obj->top);
#endif
    writelist_start = (int)(writelist_obj -> bottom);
    /* leave room to write the terminating zero and also permit
       that the invariant writelist_cursor <= writelist_end is what is needed*/ 
    writelist_end = ((int)(writelist_obj -> top)) - sizeof(int);
    writelist_cursor = writelist_start;
  }
#endif
}


#ifdef HEAPPROFILE
value_t int_alloc(unsigned long *saveregs, long sp, long ret_add, 
		  long req_wordsize, int init_val, int profiletag)
{
  printf("no profiletag for int_alloc yet\n");
  exit(-1);
}
#else
value_t int_alloc(unsigned long *saveregs, long sp, long ret_add, 
		    long word_len, int init_val)
{
  value_t *res = 0;
  int i, tag = IARRAY_TAG | word_len << (2 + POSSLEN_SHIFT);

#ifdef DEBUG
  printf("\nint_alloc called with word_len = %d   init_val = %d\n",
	 word_len, init_val);
#endif

  if (!generational_flag)
    {
      value_t alloc_ptr = saveregs[ALLOCPTR_REG];
      value_t alloc_limit = saveregs[ALLOCLIMIT_REG];
      
      if (alloc_ptr + (word_len + 1) >= alloc_limit)
	{
#ifdef DEBUG
	  printf("DOING GC inside int_alloc with a semispace collector\n");
#endif
	  gc_handler(saveregs,sp,ret_add, word_len + 1, 0);
	}
      alloc_ptr = saveregs[ALLOCPTR_REG];
      alloc_limit = saveregs[ALLOCLIMIT_REG];
      assert(alloc_ptr + (word_len + 1) < alloc_limit);
#ifdef HEAPPROFILE
      *(value_t *)alloc_ptr = 30006;
      alloc_ptr += 4;
#endif
      res = (value_t *)(alloc_ptr + 4);
      alloc_ptr += 4 * (word_len + 1);
      saveregs[ALLOCPTR_REG] = alloc_ptr;
    }
  else
    {
      if (old_fromheap->alloc_start + (word_len + 1) >= old_fromheap->top)
	gc_handler(saveregs,sp,ret_add,4,0);
      assert(old_fromheap->alloc_start + (word_len + 1) < old_fromheap->top);
#ifdef HEAPPROFILE
      *(value_t *)(old_fromheap->alloc_start) = 30007;
      old_fromheap->alloc_start += 4;
#endif
      res = (value_t *)(old_fromheap->alloc_start + 4);
      old_fromheap->alloc_start = (value_t)(res + word_len);
      old_alloc_ptr = old_fromheap->alloc_start;
      assert(old_fromheap->alloc_start < old_fromheap->top);
    }
  res[-1] = tag;
  for (i=0; i<word_len; i++)
    res[i] = init_val;
printf("TotalBytesAllocated 1: %ld\n",TotalBytesAllocated);
  TotalBytesAllocated += 4*(word_len+1);
printf("TotalBytesAllocated 1: %ld\n",TotalBytesAllocated);
  return (value_t)res;
}
#endif



#ifdef HEAPPROFILE
value_t ptr_alloc(unsigned long *saveregs, long sp, long ret_add, 
		  long req_size, int init_val, int profiletag)
{
  printf("no profiletag for ptr_alloc yet\n");
  exit(-1);
}
#else
value_t ptr_alloc(unsigned long *saveregs, long sp, long ret_add, 
		    long log_len, int init_val)
{
  value_t *res = 0;
  int i, tag = ARRAY_TAG | log_len << (2+POSSLEN_SHIFT);

#ifdef DEBUG
  printf("\nptr_alloc called with log_len = %d   init_val = %d\n",
	 log_len, init_val);
#endif

  if (!generational_flag)
    {
      value_t alloc_ptr = saveregs[ALLOCPTR_REG];
      value_t alloc_limit = saveregs[ALLOCLIMIT_REG];
      
      if (alloc_ptr + 4 * (log_len + 2) >= alloc_limit)
	{
#ifdef DEBUG
	  printf("DOING GC inside ptr_alloc with a semispace collector\n");
#endif
	  gc_handler(saveregs,sp,ret_add,4 * (log_len + 2),0);
	}
      alloc_ptr = saveregs[ALLOCPTR_REG];
      alloc_limit = saveregs[ALLOCLIMIT_REG];
      assert(alloc_ptr + 4 * (log_len + 2) < alloc_limit);
#ifdef HEAPPROFILE
      *(value_t *)alloc_ptr = 30008;
      alloc_ptr += 4;
#endif
      res = (value_t *)(alloc_ptr + 4);
      alloc_ptr += 4 * (log_len + 1);
      saveregs[ALLOCPTR_REG] = alloc_ptr;
    }
  else
    {
      /* we collect if init_val is from young area */
      if ((old_fromheap->alloc_start + 4 * (log_len + 2) >= old_fromheap->top) ||
	  (init_val >= fromheap->bottom && init_val < fromheap->top))
	gc_handler(saveregs,sp,ret_add,4,0);
      assert(old_fromheap->alloc_start + 4 * (log_len + 2) < old_fromheap->top);
#ifdef HEAPPROFILE
      *(value_t *)(old_fromheap->alloc_start) = 30009;
      old_fromheap->alloc_start += 4;
#endif
      res = (value_t *)(old_fromheap->alloc_start + 4);
      old_fromheap->alloc_start = (value_t)(res + log_len);
      assert(old_fromheap->alloc_start < old_fromheap->top);
      old_alloc_ptr = old_fromheap->alloc_start;
    }
  res[-1] = tag;
  for (i=0; i<log_len; i++)
    res[i] = init_val;
  return (value_t)res;
}
#endif



#ifdef HEAPPROFILE
value_t float_alloc(unsigned long *saveregs, long sp, long ret_add, 
		    long log_len, double init_val, int profiletag)
#else
value_t float_alloc(unsigned long *saveregs, long sp, long ret_add, 
		    long log_len, double init_val)
#endif
{
  double *rawstart = NULL;
  value_t *res = NULL;
  int pos, i, tag = RARRAY_TAG + (2 * log_len) << (2 + POSSLEN_SHIFT);

#ifdef DEBUG
  printf("(log_len,init_val)  is  (%d, %lf)\n",log_len,init_val);
#endif

  if (!generational_flag)
    {
      value_t alloc_ptr = saveregs[ALLOCPTR_REG];
      value_t alloc_limit = saveregs[ALLOCLIMIT_REG];
      
      if (alloc_ptr + 8 * (log_len + 3) >= alloc_limit)
	{
#ifdef DEBUG
	  printf("DOING GC inside float_alloc with a semispace collector\n");
#endif
	  gc_handler(saveregs,sp,ret_add,8 * (log_len + 3),1);
	  alloc_ptr = saveregs[ALLOCPTR_REG];
	  alloc_limit = saveregs[ALLOCLIMIT_REG];
	}
      assert(alloc_ptr + 8 * (log_len + 3) <= alloc_limit);
#ifdef HEAPPROFILE
      if (alloc_ptr % 8 != 0)
 	{ 
	  *(value_t *)alloc_ptr = SKIP_TAG;
	  alloc_ptr += 4;       
	}
      *(value_t *)alloc_ptr = 30010;
      alloc_ptr += 4;
#else
      if (alloc_ptr % 8 == 0)
 	{ 
	  *(value_t *)alloc_ptr = SKIP_TAG;
	  alloc_ptr += 4;
	}
#endif
      res = (value_t *)(alloc_ptr + 4);
      assert ((value_t)res % 8 == 0);
      alloc_ptr = (((value_t) res) + (8 * log_len));
      saveregs[ALLOCPTR_REG] = alloc_ptr;
    }
  else
    {
      assert(log_len >= 4096);
      assert(4*(2*log_len+2) < floatheapsize);
      pos = AllocRange(floatbitmap,DivideUp(4*(2*log_len+2),floatbitmapsize));

      if (pos < 0)
	{
	  int qlen;
#ifdef DEBUG
	  printf("Have to do a GC while doing a float_alloc.  Hope this works.\n");
#endif
	  QueueClear(float_roots);    
	  gc_handler(saveregs,sp,ret_add,4,1);
#ifdef DEBUG
	  printf("float_roots has %d items\n",QueueLength(float_roots));
#endif
	  /* --- stuff already marked; sweep the bitmap, then mark the live stuff --- */
	  ClearBitmap(floatbitmap);
	  qlen = QueueLength(float_roots);
	  for (i=0; i<qlen; i++)
	    {
	      value_t far_val = (value_t) QueueAccess(float_roots,i);
	      value_t tag = ((int *)far_val)[-1];
	      int word_len = GET_POSSLEN(tag)/4;
	      value_t start = RoundDown(far_val, floatbitmapsize);
	      value_t end   = RoundUp(far_val+4*word_len, floatbitmapsize);
	      int bitmap_start = (start - floatheap->bottom) / floatbitmapsize;
	      int bitmap_end   = (end - floatheap->bottom) / floatbitmapsize;
	      int bitmap_size = bitmap_end - bitmap_start;
#ifdef DEBUG
	      printf("float root #%d; *%d = %d\n",i,far_val,tag);
#endif
	      if (!(IS_RARRAY(tag)))
		{
		  printf("expected array tag at %d: got %d\n",far_val,tag);
		  assert(0);
		}
	      SetBitmapRange(floatbitmap,bitmap_start,bitmap_size);
#ifdef DEBUG
	      printf("Restoring array with (start,end,size) word_len = (%d, %d, %d) %d\n",
		     bitmap_start,bitmap_end,bitmap_size,word_len);
#endif
	    }
	  pos = AllocRange(floatbitmap,DivideUp(4*(2*log_len+2),floatbitmapsize));
#ifdef DEBUG
	  printf("pos = %d, size = %d start = %d, safeend = %d\n",
		 pos,DivideUp(4*(2*log_len+2),floatbitmapsize),
		 (double *)(floatheap->bottom + floatbitmapsize * pos),
		 (double *)(floatheap->bottom + floatbitmapsize * pos) + (log_len + 1));
#endif
	}
      assert(pos >= 0);
      
      rawstart = (double *)(floatheap->bottom + floatbitmapsize * pos);
      res = (value_t *)(rawstart + 1);
      TotalBytesAllocated += RoundUp(4*(2*log_len+2),floatbitmapsize);
    }
#ifdef HEAPPROFILE
  ((int *)res)[-2] = 30011;
#endif
  ((int *)res)[-1] = RARRAY_TAG | ((2 * log_len) << (2+POSSLEN_SHIFT));
  for (i=0; i<log_len; i++)
    ((double *)res)[i] = init_val;

#ifdef DEBUG
{
  static int count = 0;
  printf("\n--- float_alloc %d returning [%d(-8) to %d)\n",++count,res,res+2*log_len);
}
#endif
  return (value_t)(res);
}


void paranoid_check(long sp, int stack_top)
{
#ifdef PARANOID
    int count = 0, mi, i;
    for (count = sp; count < stack_top; count += 4)
      {
	int *data_add = (int *)count;
	int data = *data_add;
	if (data >= fromheap->bottom && data < fromheap->top)
	  {
	    static int newval = 42000;
	    printf("TRACE WARNING: stack loc %d has fromheap value %d",
		   data_add,data);
	    printf("      changing to %d\n", newval);
	    *data_add = newval;
	    newval++;
	  }
      }
    for (count = old_fromheap->bottom; count < old_fromheap->alloc_start; count +=4)
      {
	int *data_add = (int *)count;
	int data = *data_add;
	if (data >= fromheap->bottom && data < fromheap->top)
	  {
	    printf("TRACE WARNING: old_fromheap has a fromheap value after collection");
	    printf("   data_add = %d   data = %d\n",data_add,data);
	  }
      }
    for (mi=0; mi<module_count; mi++)
      for (i=(value_t)*((&GLOBAL_TABLE_BEGIN_VAL)+mi);
	   i<(value_t)*((&GLOBAL_TABLE_END_VAL)+mi); i+=16)
	{
	  value_t table_entry = ((value_t *)i)[0];
	  int *data_add = (int *)table_entry;
	  int data = *data_add;
	  if (data >= fromheap->bottom && data < fromheap->top)
	    {
	      printf("TRACE WARNING: global has a fromheap value after collection");
	      printf("   data_add = %d   data = %d\n",data_add,data);
	    }
	}
#endif
}


void measure_semantic_garbage_after()
{
#ifdef SEMANTIC_GARBAGE
    /* from/to heap has been swapped at this point */
    extern long SemanticGarbageSize;
    extern long TotalBytesCollected;
    if (GCtype == Major)
      scandead_heap("OLD_FROMHEAP",old_toheap->bottom,old_toheap->alloc_start,
		  old_toheap->top); 
    printf("\nSEMANTIC GARBAGE: GC,MajorGC = %d,%d    garb/copied = %d %d\n",
	 NumGC,NumMajorGC,SemanticGarbageSize,TotalBytesCollected);
#endif
}

void measure_semantic_garbage_before()
{  
#ifdef SEMANTIC_GARBAGE
  /* ---------- Old code to measure "semantic garbage" --------------- */
    extern value_t semantic_garbage_offset;
    int count = 0;
    value_t *i, *start, *end;
    printf("SEMANTIC GARBAGE: about to scan\n");

    start = (value_t *)(fromheap->bottom + semantic_garbage_offset);
    end = (value_t *)(fromheap->top + semantic_garbage_offset);
    for (i=start; i<end; i++)
      if (*i == 0) count++;
    printf("SEMANTIC GARBAGE: %d words were accessed in the fromheap area\n",count);

    count = 0;
    start = (value_t *)(old_fromheap->bottom + semantic_garbage_offset);
    end = (value_t *)(old_fromheap->top + semantic_garbage_offset);
    for (i=start; i<end; i++)
      if (*i == 0) count++;
    printf("SEMANTIC GARBAGE: %d words were accessed in the old_fromheap area\n",count);
#endif
}




void debug_after_collect()
{
#ifdef DEBUG
      value_t a = fromheap->bottom, b = fromheap->top;
      gc_sanity_stackreg_check(saveregs,(int *) sp, (int *)stack->top);
      if (SHOW_HEAPS)
	show_heap("FINAL FROM",fromheap->bottom,allocptr,fromheap->top);
      fromheap->bottom = 0;
      fromheap->top = 0;
      if (SHOW_HEAPS)
	show_heap("OLD_FROMHEAP",old_fromheap->bottom,old_fromheap->alloc_start,
		 old_fromheap->top); 
      else
	check_heap("OLD_FROMHEAP",old_fromheap->bottom,old_fromheap->alloc_start,
		   old_fromheap->top); 
      fromheap->bottom = a;
      fromheap->top = b;
#endif
}







void gc_handler_semi(unsigned long *saveregs, long sp, long ret_add, long req_size)
{
  int i, mi;  
  int regmask = 0;
  int stack_top;
  int allocsize, allocptr;
  struct rusage start,finish;
  static Queue_t *root_lists = 0;
  static Queue_t *uninit_global_roots = 0;
  static Queue_t *global_roots = 0;
  static Queue_t *promoted_global_roots = 0;
  value_t *to_ptr = (value_t *)(toheap->bottom);
  range_t from_range, to_range;

#ifdef SEMANTIC_GARBAGE
  assert(0); /* unimplemented */
#endif

  InScheduler = 1;
  start_timer(&gctime);

  if (root_lists == 0)
    {
      root_lists = QueueCreate(200);
      uninit_global_roots = QueueCreate(100);
      global_roots = QueueCreate(100);
      promoted_global_roots = QueueCreate(100);
    }

  if (CurHeapLimit == LowHeapLimit)
    {
      fprintf(stderr,"Cur/Low %d %d\n",CurHeapLimit,LowHeapLimit);
      thread_scheduler_clean(saveregs,sp,ret_add);
      return;
    }

  if (req_size == 0)
    {
      fprintf(stderr,"alloc_size = 0    means writelist_full.\n");
    }

  allocsize = saveregs[CSECONDARG_REG];
  allocptr = saveregs[ALLOCPTR_REG];

  debug_and_stat_before(saveregs, sp, ret_add, req_size);

  if (CurHeapLimit != LowHeapLimit)
    {
      if (!(allocptr <= CurHeapLimit))
	{
	  printf("allocptr=%d   CurHeapLimit=%d\n",allocptr,CurHeapLimit);
	  assert(0);
	}
    }

  HeapObj_Resize(toheap,allocptr - fromheap->bottom);
  HeapObj_Unprotect(toheap);

  stack_top = root_scan(saveregs,sp,ret_add,root_lists,
			uninit_global_roots,global_roots,promoted_global_roots);
  Enqueue(root_lists,global_roots);
  Enqueue(root_lists,promoted_global_roots);

  SetRange(&from_range, fromheap->bottom, fromheap->top);
  SetRange(&to_range, toheap->bottom, toheap->top);

    {
      static Queue_t *loc_roots = 0;
      if (loc_roots == 0)
	loc_roots = QueueCreate(200);
      else 
	QueueClear(loc_roots);
 
    for (i=0; i<QueueLength(ScanQueue); i++)
      {
	value_t first = (value_t) (QueueAccess(ScanQueue,i));
	value_t data_addr = *((value_t *)first);
	value_t data = *((value_t *)data_addr);
	if (data > 255)
	  Enqueue(loc_roots,(int *)first);
	else
	  {
	    int offset = ((255 - data) * sizeof(value_t));
	    int obj_start = data_addr - offset;
	    forward_mac_minor((value_t *)(&obj_start),to_ptr,&from_range);
	    *((int *)first) = obj_start + offset;
	  }
      }
      Enqueue(root_lists, loc_roots); 
    }
    

#ifdef HEAPPROFILE
  gcstat_heapprofile_beforecollect((value_t *)fromheap->alloc_start,
				(value_t *)allocptr);

#endif


  /* --------------- forward the roots ------------------ */
  to_ptr = forward_root_lists_minor(root_lists, to_ptr, 
				    &from_range, &to_range);



  /* ---------------- forward mutables  --------------------- */
  to_ptr = forward_mutables_semi(to_ptr, &from_range,&to_range);
  to_ptr = scan_nostop_minor(to_range.low,to_ptr,&from_range,&to_range);

#ifdef HEAPPROFILE
  gcstat_heapprofile_aftercollect((value_t *)from_low,
				(value_t *)allocptr);

#endif
  
    toheap->alloc_start = to_ptr;

    
#ifdef DEBUG
    if (SHOW_HEAPS)
      {
	memdump("From Heap After collection:", (int *)fromheap->bottom,40);
        memdump("To Heap After collection:", (int *)toheap->bottom,40);
	show_heap("FINAL FROM",fromheap->bottom,allocptr,fromheap->top);
	show_heap("FINAL TO",toheap->bottom,to_ptr,toheap->top);
      }
#endif

    {
      long oldsize = fromheap->top - fromheap->bottom;
      long copied = to_ptr - toheap->bottom;
      double oldratio = ((double)copied + req_size) / (oldsize + req_size);
      long eff_oldsize = (copied + req_size > oldsize) ? 
	(copied + req_size) : oldsize;
      long newsize = ComputeHeapSize(eff_oldsize, oldratio);
      long allocsize = allocptr - fromheap->alloc_start;
      if (newsize < copied + req_size)
	{
	  fprintf(stderr,"FATAL ERROR: failure reqesting %d bytes\n",req_size);
	  exit(-1);
	}

      gcstat_normal(allocsize,oldsize,oldratio,newsize,copied);

      HeapObj_Resize(toheap,newsize);
      HeapObj_Unprotect(toheap); 

    if (toheap->top - (value_t)to_ptr <= req_size)
      {
	printf("copied = %d\n",copied);
	printf("req_size = %d\n",req_size);
	printf("eff_oldsize = %d\n",eff_oldsize);
	printf("newsize = %d\n",newsize);
	printf("toheap->top %d\n",toheap->top);
	printf("to_ptr = %d\n",to_ptr);
	exit(-1);
      }
    }
 
    HeapObj_Protect(fromheap);
    saveregs[ALLOCPTR_REG] = to_ptr;
    saveregs[ALLOCLIMIT_REG] = toheap->top;


#ifdef DEBUG
{
  int count;
    if (SHOW_GCDEBUG)
      gc_sanity_stackreg_check(saveregs,(int *) sp, (int *)stack->top);
    for (count = toheap->bottom; count < toheap->alloc_start; count +=4)
      {
	int *data_add = (int *)count;
	int data = *data_add;
	if (data >= fromheap->bottom && data < fromheap->top)
	  {
	    printf("TRACE WARNING: toheap has a fromheap value after collection");
	    printf("   data_add = %d   data = %d\n",data_add,data);
	    printf("toheap->bottom = %d toheap->alloc_start = %d\n",
		   toheap->bottom, toheap->alloc_start);
	    printf("fromheap->bottom = %d fromheap->top = %d\n",
		   fromheap->bottom, fromheap->top);
	  }
      }
}
#endif


    typed_swap(HeapObj_t *, fromheap, toheap);

    CurHeapLimit = fromheap->top;



  NumGC++;

  stop_timer(&gctime);

  InScheduler = 0;

}

void gc_handler_gen(unsigned long *saveregs, long sp, 
		    long ret_add, long req_size, int isMajor)
{
  int regmask = 0;
  int stack_top = 0;
  int allocsize = saveregs[CSECONDARG_REG];
  int allocptr = saveregs[ALLOCPTR_REG];
  struct rusage start,finish;
  static Queue_t *root_lists = 0;
  static Queue_t *uninit_global_roots = 0;
  static Queue_t *global_roots = 0;
  static Queue_t *promoted_global_roots = 0;
  enum GCType GCtype = isMajor ? Major : Minor;
  value_t to_allocptr;

  /* start timer */
  InScheduler = 1;
  start_timer(&gctime);

  /* Initialize the global lists for holdings roots */
  if (root_lists == 0)
    {
      root_lists = QueueCreate(200);
      uninit_global_roots = QueueCreate(100);
      global_roots = QueueCreate(100);
      promoted_global_roots = QueueCreate(100);
    }

  /* Thread stuff and sanity check */
  if (CurHeapLimit == LowHeapLimit)
    {
      fprintf(stderr,"Cur/Low %d %d\n",CurHeapLimit,LowHeapLimit);
      thread_scheduler_clean(saveregs,sp,ret_add,req_size);
      return;
    }
  else
    {
      if (!(allocptr <= CurHeapLimit))
	printf("CurHeapLimit = %d,   allocptr = %d\n",CurHeapLimit,allocptr);
      assert(allocptr <= CurHeapLimit);
    }

  /* these are debugging and stat-gatherting procedure that are ifdef-ed */
  measure_semantic_garbage_before();
  debug_and_stat_before(saveregs, sp, ret_add, req_size);


  /* root processing */
  stack_top = root_scan(saveregs,sp,ret_add,root_lists,
			uninit_global_roots,global_roots,promoted_global_roots);
  Enqueue(root_lists,promoted_global_roots);

  /* -------------- the actual heap collection ---------------------- */
    {
      int old_unused_amount = (old_fromheap->top - old_alloc_ptr);
      double old_unused_ratio = ((double)old_unused_amount) /
	(old_fromheap->top - old_fromheap->bottom);
      /* A minor collection is promoted to a major collection if
	 (1) there is less than 10% free in the tenured area
	 (2) the amount of space left in the tenured area is less
	     than the size of the nursery.
     */
      if (GCtype == Minor)
	{
	  if ((old_unused_ratio < 0.1) || 
	    (old_unused_amount < (fromheap->top - fromheap->bottom)))
	      {
		GCtype = Major;
#ifdef DEBUG
		printf("old_unused_ratio = %lf\n",old_unused_ratio);
		printf("old_unused_amount = %d\n",old_unused_amount);
#endif
	      }
        }
      assert(fromheap->alloc_start     < fromheap->top);
      assert(old_fromheap->alloc_start < old_fromheap->top);
      assert(old_toheap->alloc_start   < old_toheap->top);

      assert(writelist_cursor <= writelist_end);
      *((int *)writelist_cursor) = 0;

   /* Perform just a minor GC if it is very likely we don't need a major GC */
      if (GCtype == Minor)
	{
	  range_t from_range, from2_range, to_range;
	  value_t *to_ptr = old_alloc_ptr;

	  assert(old_alloc_ptr >= old_fromheap->alloc_start);
	  assert(old_fromheap->top - old_alloc_ptr > (saveregs[ALLOCPTR_REG] - fromheap->bottom));

	  SetRange(&from_range,fromheap->bottom, fromheap->top);
	  SetRange(&from2_range,0,0);
	  SetRange(&to_range,old_fromheap->bottom, old_fromheap->top);

#ifdef HEAPPROFILE
         /* necessary since before collect assumes no forward tags */
	    gcstat_heapprofile_beforecollect((value_t *)fromheap->alloc_start,
				     (value_t *)allocptr);
	  
#endif
	  
	  /* do the normal roots and the writelist */
	  to_ptr = forward_root_lists_minor(root_lists, to_ptr, 
					    &from_range, &to_range);
	  
	  to_ptr = forward_writelist_minor((value_t *)writelist_start, to_ptr, 
					   &from_range, &to_range);
	  
	  to_ptr = forward_gen_locatives(to_ptr,&from_range,&from2_range,&to_range); 
	  to_ptr = forward_mutables_semi(to_ptr,&from_range,&to_range); 
	  to_ptr = scan_stop_minor(old_fromheap->alloc_start,to_ptr,old_alloc_ptr,
				   &from_range,&to_range);
	  to_ptr = scan_nostop_minor(old_alloc_ptr,to_ptr,&from_range,&to_range);
	  
#ifdef HEAPPROFILE
	  gcstat_heapprofile_aftercollect((value_t *)from_range->low,
					  (value_t *)allocptr);
	  
#endif
	  to_allocptr = to_ptr;

	  old_fromheap->alloc_start = old_alloc_ptr;
	  assert(old_fromheap->alloc_start < old_fromheap->top);
	  gcstat_normal(allocptr - fromheap->alloc_start,
			0, 0.0, 0,
			to_allocptr - old_alloc_ptr);

	  old_alloc_ptr = to_allocptr;
	  old_alloc_limit = old_fromheap->top;



	  old_fromheap->alloc_start = to_allocptr;
	  assert(old_fromheap->alloc_start < old_fromheap->top);

	  /* If the minor GC failed to produce less free space in the tenured area
	     than the size of the nursery, then trigger a major GC */
	  if ((old_fromheap->top - old_fromheap->alloc_start) < 
	      (fromheap->top - fromheap->bottom))
	    {
	      printf("ForcedMajorGC\n");
	      GCtype = ForcedMajor;
	    }

	}

      /* Perform a major GC if 
	  (1) a minor GC was not taken in the first place
	  (2) a minor GC was performed but this triggered a major GC due to low space
      */
      if (GCtype != Minor)
	{
	  range_t from_range, from2_range, to_range;
	  value_t *to_ptr = old_toheap->bottom;

	  int i, newsize;
	  start_timer(&majorgctime);
	  newsize = ((old_fromheap->alloc_start - old_fromheap->bottom) +
		     (fromheap->top - fromheap->bottom));
	  if ((newsize >= (HeapObj_Getsize(old_toheap))) && (MinHeap != MaxHeap))
	    {
	      printf("WARNING: GC failure possible due to newsize\n");
	      printf("old_fromheap->top = %d, old_fromheap->bottom = %d\n",
		     old_fromheap->top, old_fromheap->bottom);
	      printf("fromheap->top = %d, fromheap->bottom = %d\n",
		     fromheap->top, fromheap->bottom);
	      printf("old_toheap->rawtop = %d, old_toheap->top = %d, old_toheap->bottom = %d\n",
		     old_toheap->rawtop, old_toheap->top, old_toheap->bottom);
	    }    
	  newsize = HeapObj_Getsize(old_toheap);
	  HeapObj_Resize(old_toheap,newsize);
	  HeapObj_Unprotect(old_toheap); 	  
	  fprintf(stderr,"--------DOING MAJOR GC %d at GC %d---------\n",NumMajorGC, NumGC);
#ifdef DEBUG
	  printf("fromheap->bottom, fromheap->top: %d %d\n",
		 fromheap->bottom, fromheap->top);
	  printf("old_fromheap->{bottom, alloc_start, top}: %d %d %d\n",
		 old_fromheap->bottom, old_fromheap->alloc_start, old_fromheap->top);
#endif

	  assert(old_alloc_ptr >= old_fromheap->alloc_start);
	  Enqueue(root_lists,global_roots);
	  SetRange(&from_range,fromheap->bottom, fromheap->top);
	  SetRange(&from2_range,old_fromheap->bottom, old_alloc_ptr);
	  SetRange(&to_range,old_toheap->bottom, old_toheap->top);



#ifdef HEAPPROFILE
 /* necessary since before collect assumes no forward tags */
	  if (GCtype != ForcedMajor)
	    gcstat_heapprofile_beforecollect((value_t *)fromheap->alloc_start,
					     (value_t *)allocptr);
	  
#endif

	  /* do the normal roots and the writelist */
	  to_ptr = forward_root_lists_major(root_lists, to_ptr, 
					    &from_range, &from2_range, &to_range);
	  
	  to_ptr = forward_writelist_major((value_t *)writelist_start, to_ptr, 
					   &from_range, &from2_range, &to_range);
	  
	  to_ptr = forward_gen_locatives(to_ptr,&from_range,&from2_range,&to_range); 
	  to_ptr = forward_mutables_gen(to_ptr,&from_range,&from2_range,&to_range); 
	  to_ptr = scan_major(old_toheap->bottom,to_ptr,to_range.high,&from_range,&from2_range,&to_range);


#ifdef HEAPPROFILE
	  gcstat_heapprofile_aftercollect((value_t *)from2_range->low,
					    (value_t *)from2_range->high);
	  gcstat_heapprofile_aftercollect((value_t *)from_range->low,
					  (value_t *)allocptr);
	  
#endif
	  to_allocptr = to_ptr;

	  assert(to_allocptr < old_toheap->top);
	  old_toheap->alloc_start = to_allocptr;
	  old_alloc_ptr = to_allocptr;
	  old_alloc_limit = old_toheap->top;
	  gcstat_normal(allocptr - fromheap->alloc_start,
			0, 0.0, 0,
			to_allocptr - old_toheap->bottom);


	  typed_swap(HeapObj_t *, old_fromheap, old_toheap);


	  stop_timer(&majorgctime);
	}


      assert(fromheap->alloc_start     < fromheap->top);
      assert(old_fromheap->alloc_start < old_fromheap->top);
      assert(old_toheap->alloc_start   < old_toheap->top);
    }


    writelist_cursor = writelist_start;

    if (GCtype != Minor)
    {
      long oldsize = (GCtype != Minor)?(old_toheap->top - old_toheap->bottom):
	(old_fromheap->top - old_fromheap->bottom);
      long copied = old_fromheap->alloc_start - old_fromheap->bottom;
      long eff_oldsize = (copied + req_size > oldsize) ? 
	(copied + req_size) : oldsize;
      double oldratio = oldsize ? (double)(copied + req_size)/ oldsize : 1.0;
      long newsize = ComputeHeapSize(eff_oldsize, oldratio);
      if ((newsize < copied + req_size) || 
	  req_size > (fromheap->top - fromheap->bottom))
	{
	  fprintf(stderr,"\ncopied = %d,  req_size = %d\n",copied,req_size);
	  fprintf(stderr,"oldsize = %d,  newsize = %d\n",oldsize,newsize);
	  fprintf(stderr,"FATAL ERROR: gen failure reqesting %d bytes\n",req_size);
	  exit(-1);
	}
      HeapObj_Resize(old_fromheap,newsize);
    }


    HeapObj_Unprotect(old_fromheap);
    debug_after_collect();

    saveregs[ALLOCPTR_REG] = fromheap->bottom;
    saveregs[ALLOCLIMIT_REG] = fromheap->top;
    CurHeapLimit = fromheap->top;

  if (GCtype != Minor)
    NumMajorGC++;
  NumGC++;

  /* More debugging and stat-gathering procedure */
  measure_semantic_garbage_after();    
  paranoid_check(sp,stack_top);

  /* stop timer */
  stop_timer(&gctime); 
  InScheduler = 0;
}

void gc_handler(unsigned long *saveregs, long sp, long ret_add, long req_size, int isMajor)
{
#ifdef DEBUG
  printf("NumGC = %d\n",NumGC);
#endif
  if (generational_flag)
     gc_handler_gen(saveregs, sp, ret_add, req_size, isMajor);
  else
     gc_handler_semi(saveregs, sp, ret_add, req_size);
#ifdef DEBUG
  printf("NumGC = %d over\n",NumGC-1);
#endif
}







/* --------------- Scanning routines ----------------------- */

value_t * scan_oneobject_major(value_t **where,  value_t *alloc_ptr, 
			range_t *from_range, range_t *from2_range, range_t *to_range)
{
  value_t *cur = (value_t *)(*where);
  value_t tag = cur[0];
  value_t type= GET_TYPE(tag);
  value_t local_to_low = to_range->low;
  value_t local_to_diff = to_range->diff;

  if (tag == SKIP_TAG)
    {
      *where = cur + 1;
      return alloc_ptr;
    }

  switch (type)
    {
    case RECORD_TAG:
    case RECORD_SUB_TAG:
      if (GET_RECLEN(tag) <= MAX_RECORDLEN)
	{
	  int i, fieldlen = GET_RECLEN(tag);
	  value_t *end = cur + 1 + fieldlen;
	  unsigned mask = GET_RECMASK(tag);
	  cur++;
	  for (; cur<end; cur++, mask >>= 1)
	    if (mask & 1)
	      forward_mac_major(cur,alloc_ptr,from_range,from2_range,to_range);
	}
      else
	{
	  value_t *rawstart = cur;
	  int i, fieldlen = 0;
	  
	  value_t *objstart;
	  {
	    
	    value_t *curtag = rawstart;
	    while (1)
	      {
		int curlen = GET_RECLEN(*curtag);
		if (curlen <= MAX_RECORDLEN)
		  {
		    fieldlen += curlen;
		    break;
		  }
		else
		  fieldlen += MAX_RECORDLEN;
		curtag++;
	      }
	    objstart = curtag + 1;
	  }
	  for (i=0; i<fieldlen; i++)
	    {
	      unsigned int mask = GET_RECMASK(rawstart[i/MAX_RECORDLEN]);
	      if (mask & 1 << (i % MAX_RECORDLEN))
		forward_mac_major(objstart+i,alloc_ptr,from_range,from2_range,to_range);
	    }
	  cur = objstart + fieldlen;
	}
      break;
    case SKIP_TAG:
      cur++;
      break;
    case IARRAY_TAG:
      {
	unsigned int upper = GET_POSSLEN(tag);
	upper = (upper + 3) / 4; /* round up to multiple of 4 first */
	if (upper == 0)
	  upper = 1;
	cur += 1 + upper;
	break;
      }
    case RARRAY_TAG:
      {
	unsigned int upper = GET_POSSLEN(tag);
	upper /= 4;   /* already a multiple of 8 */
	if (upper == 0)
	  upper = 1;
	cur += 1 + upper;
	break;
      }
    case ARRAY_TAG:
      {
	unsigned int upper = GET_POSSLEN(tag);
	upper /= 4;   /* already a multiple of 4 */
	if (upper == 0)
	  cur += 2;
	else
	  {
	    unsigned int *end = cur + 1 + upper;
	    cur++;
	    while (cur < end)
	      {
		forward_mac_major((cur),alloc_ptr,from_range,from2_range,to_range);
		cur++;
	      }
	  }
	break;
      }
    case FORWARD_TAG:
    default:
      printf("\n\ntag = %d at cur=%d\n",tag,cur);
      printf("scan_gen: IMPOSSIBLE TAG\n");
      assert(0);
    }
  (*where) = cur;
  return alloc_ptr;
}


value_t * scan_oneobject_minor(value_t **where,  value_t *alloc_ptr, 
			       range_t *from_range,  range_t *to_range)
{
  value_t *cur = (value_t *)(*where);
  value_t tag = cur[0];
  value_t type= GET_TYPE(tag);
  value_t local_from_low = from_range->low;
  value_t local_from_diff = from_range->diff;

  if (tag == SKIP_TAG)
    {
      *where = cur + 1;
      return alloc_ptr;
    }

  switch (type)
    {
    case RECORD_TAG:
    case RECORD_SUB_TAG:
      if (GET_RECLEN(tag) <= MAX_RECORDLEN)
	{
	  int i, fieldlen = GET_RECLEN(tag);
	  value_t *end = cur + 1 + fieldlen;
	  unsigned mask = GET_RECMASK(tag);
	  cur++;
	  for (; cur<end; cur++, mask >>= 1)
	    if (mask & 1)
	      forward_mac_local_minor(cur,alloc_ptr);
	}
      else
	{
	  value_t *rawstart = cur;
	  int i, fieldlen = 0;
	  
	  value_t *objstart;
	  {
	    
	    value_t *curtag = rawstart;
	    while (1)
	      {
		int curlen = GET_RECLEN(*curtag);
		if (curlen <= MAX_RECORDLEN)
		  {
		    fieldlen += curlen;
		    break;
		  }
		else
		  fieldlen += MAX_RECORDLEN;
		curtag++;
	      }
	    objstart = curtag + 1;
	  }
	  for (i=0; i<fieldlen; i++)
	    {
	      unsigned int mask = GET_RECMASK(rawstart[i/MAX_RECORDLEN]);
	      if (mask & 1 << (i % MAX_RECORDLEN))
		forward_mac_local_minor(objstart+i,alloc_ptr);
	    }
	  cur = objstart + fieldlen;
	}
      break;
    case SKIP_TAG:
      cur++;
      break;
    case IARRAY_TAG:
      {
	unsigned int upper = (GET_POSSLEN(tag) + 3) / 4;
	if (upper == 0)
	  upper = 1;
	cur += 1 + upper;
	break;
      }
    case RARRAY_TAG:
      {
	unsigned int upper = GET_POSSLEN(tag) / 4;
	if (upper == 0)
	  upper = 1;
	cur += 1 + upper;
	break;
      }
    case ARRAY_TAG:
      {
	unsigned int upper = GET_POSSLEN(tag) / 4;
	if (upper == 0)
	  cur += 2;
	else
	  {
	    unsigned int *end = cur + 1 + upper;
	    cur++;
	    while (cur < end)
	      {
		forward_mac_local_minor(cur,alloc_ptr);
		cur++;
	      }
	  }
	break;
      }
    case FORWARD_TAG:
    default:
      printf("\n\ntag = %d at cur=%d\n",tag,cur);
      printf("scan_gen: IMPOSSIBLE TAG\n");
      assert(0);
    }
  (*where) = cur;
  return alloc_ptr;
}


value_t* scan_major(value_t start_scan, value_t *alloc_ptr, value_t *stop, 
		    range_t *from_range, range_t *from2_range, range_t *to_range)
{
  value_t *cur = (value_t *)start_scan;
  unsigned int a,b, c=0;
  value_t local_to_low = to_range->low;
  value_t local_to_diff = to_range->diff;


  while (cur < alloc_ptr && cur < stop)
    {
#ifdef HEAPPROFILE
      unsigned int proftag = *(cur++);
#endif
      value_t tag = cur[0];
      value_t type= GET_TYPE(tag);
      

#ifdef HEAPPROFILE
      if (proftag == SKIP_TAG)
	  continue; /* break; */
#endif

      if ((GET_TYPE(tag) == RECORD_TAG) && (GET_RECLEN(tag) <= 2))
	{
	  if (tag == TAG_REC_TRACETRACE)
	    {
	      forward_mac_major(cur+1,alloc_ptr,from_range,from2_range,to_range);
	      forward_mac_major(cur+2,alloc_ptr,from_range,from2_range,to_range);
	      cur += 3;
	      continue;
	    }
	  if (tag == TAG_REC_INTTRACE)
	    {
	      forward_mac_major(cur+2,alloc_ptr,from_range,from2_range,to_range);
	      cur += 3;
	      continue;
	    }
	  if (tag == TAG_REC_EMPTY ||
	      tag == TAG_REC_INT)
	    {
	      cur += 2;
	      continue;
	    }
	  if (tag == TAG_REC_TRACE)
	    {
	      forward_mac_major(cur+1,alloc_ptr,from_range,from2_range,to_range);
	      cur += 2;
	      continue;
	    }
	  if (tag == TAG_REC_INTINT)
	    {
	      cur += 3;
	      continue;
	    }
	  if (tag == TAG_REC_TRACEINT)
	    {
		forward_mac_major(cur+1,alloc_ptr,from_range,from2_range,to_range);
		cur += 3;
		continue;
	    }
      }
      else if (GET_TYPE(tag) == SKIP_TAG)
	{
	  cur++;
	  continue;
	}

      {
	value_t *temp = cur;
	alloc_ptr = scan_oneobject_major(&temp, alloc_ptr, from_range, from2_range, to_range);
	cur = temp;
	continue;
      }
    }
  return alloc_ptr;
}



value_t* scan_nostop_minor(value_t start_scan, value_t *alloc_ptr, 
			   range_t *from_range, range_t *to_range)
{
  value_t *cur = (value_t *)start_scan;
  unsigned int a,b, c=0;
  value_t local_from_low = from_range->low;
  value_t local_from_diff = from_range->diff;

  while (cur < alloc_ptr)
    {
#ifdef HEAPPROFILE
      unsigned int proftag = *(cur++);
#endif
      value_t tag = cur[0];
      value_t type= GET_TYPE(tag);
      

#ifdef HEAPPROFILE
      if (proftag == SKIP_TAG)
	  continue; /* break; */
#endif

      if ((GET_TYPE(tag) == RECORD_TAG) && (GET_RECLEN(tag) <= 2))
	{
	  if (tag == TAG_REC_TRACETRACE)
	    {
	      forward_mac_local_minor(cur+1,alloc_ptr);
	      forward_mac_local_minor(cur+2,alloc_ptr);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_INTTRACE)
	    {
	      forward_mac_local_minor(cur+2,alloc_ptr);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_EMPTY ||
		   tag == TAG_REC_INT)
	    {
	      cur += 2;
	    }
	  else if (tag == TAG_REC_TRACE)
	    {
	      forward_mac_local_minor(cur+1,alloc_ptr);
	      cur += 2;
	    }
	  else if (tag == TAG_REC_INTINT)
	    {
	      cur += 3;
	    }
	  else if (tag == TAG_REC_TRACEINT)
	    {
		forward_mac_local_minor(cur+1,alloc_ptr);
		cur += 3;
	    }
	  else 
	    alloc_ptr = scan_oneobject_minor(&cur, alloc_ptr, from_range, to_range);
      }
      else if (GET_TYPE(tag) == SKIP_TAG)
	cur++;
      else
	alloc_ptr = scan_oneobject_minor(&cur, alloc_ptr, from_range, to_range);
    }
  return alloc_ptr;
}


value_t* scan_stop_minor(value_t start_scan, value_t *alloc_ptr, value_t *stop,
			range_t *from_range, range_t *to_range)
{
  register value_t *cur = (value_t *)start_scan;
  register value_t local_from_low = from_range->low;
  register value_t local_from_diff = from_range->diff;


  while (cur < stop)
    {

#ifdef HEAPPROFILE
      unsigned int proftag = *(cur++);
#endif
      
      value_t tag = cur[0]; /* note that declaration of variable causes a stack slot
				 allocated for it by the C compiler */


      
#ifdef HEAPPROFILE
      if (proftag == SKIP_TAG)
	continue; /* break; */
#endif
      if (tag == SKIP_TAG)
	continue;
      if ((GET_TYPE(tag) == RECORD_TAG) && (GET_RECLEN(tag) <= 2))
	{
	  if (tag == TAG_REC_TRACETRACE)
	    {
	      forward_mac_local_minor(cur+1,alloc_ptr);
	      forward_mac_local_minor(cur+2,alloc_ptr);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_INTTRACE)
	    {
	      forward_mac_local_minor(cur+2,alloc_ptr);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_EMPTY ||
		   tag == TAG_REC_INT)
	    {
	      cur += 2;
	    }
	  else if (tag == TAG_REC_TRACE)
	    {
	      forward_mac_local_minor(cur+1,alloc_ptr);
	      cur += 2;
	    }
	  else if (tag == TAG_REC_INTINT)
	    {
	      cur += 3;
	    }
	  else if (tag == TAG_REC_TRACEINT)
	    {
		forward_mac_local_minor(cur+1,alloc_ptr);
		cur += 3;
	    }
	  else 
	    {
	      value_t *temp = cur;
	      alloc_ptr = scan_oneobject_minor(&temp, alloc_ptr, from_range, to_range);
	      cur = temp;
	    }
	}
      else if (GET_TYPE(tag) == SKIP_TAG)
	  cur++;
      else 
	{
	  value_t *temp = cur;
	  alloc_ptr = scan_oneobject_minor(&temp, alloc_ptr, from_range, to_range);
	  cur = temp;
	}
    }

  return alloc_ptr;
}
  
  

