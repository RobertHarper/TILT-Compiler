/* Not thread-safe */
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "platform.h"
#include "client.h"
#include "show.h"


extern Queue_t *ScanQueue;


static int floatheapsize = 16384 * 1024;
static int floatbitmapsize = 128;
Queue_t   *float_roots = 0;
static Queue_t *global_roots;


enum GCType { Minor, Major, ForcedMajor, Complete };



Heap_t *nursery = NULL;  /* nursery used by old_alloc assembly routine */
static Heap_t *old_fromheap = NULL, *old_toheap = NULL;
Heap_t *floatheap = NULL; /* floatheap used by forward.c */
static Bitmap_t  *floatbitmap = NULL;


#ifdef OLD_ALLOC
value_t old_alloc_ptr = 0;
value_t old_alloc_limit = 0;
#endif

/* ------------------  Generational array allocation routines ------------------- */

value_t alloc_bigintarray_gen(int word_len, value_t init_val, int ptag)
{
  Thread_t *curThread = getThread();
  long *saveregs = curThread->saveregs;
  value_t *res = 0;
  int i, tag = IARRAY_TAG | word_len << (2 + ARRLEN_OFFSET);
  int real_byte_len = 4 * (word_len + 1);

#ifdef DEBUG
  printf("\nint_alloc called with word_len = %d   init_val = %d\n",
	 word_len, init_val);
#endif


    {
      if (old_fromheap->alloc_start + real_byte_len >= old_fromheap->top) {
	curThread->request = 4;
	gc_gen(curThread,0);
      }
     if (old_fromheap->alloc_start + real_byte_len >= old_fromheap->top)
       {
	 printf("old_fromheap->alloc_start + 4*(word_len + 1) < old_fromheap->top\n%d + %d < %d\n",
		old_fromheap->alloc_start, real_byte_len, old_fromheap->top);
	 assert(0);
       }
#ifdef HEAPPROFILE
      real_byte_len needs to be 4 bigger if we profile objects
      *(value_t *)(old_fromheap->alloc_start) = 30007;
      old_fromheap->alloc_start += 4;
#endif
      res = (value_t *)(old_fromheap->alloc_start + 4);
      old_fromheap->alloc_start += real_byte_len;
      old_alloc_ptr = old_fromheap->alloc_start;
      assert(old_fromheap->alloc_start < old_fromheap->top);
    }
  res[-1] = tag;
  for (i=0; i<word_len; i++)
    res[i] = init_val;
printf("TotalBytesAllocated 1: %ld\n",TotalBytesAllocated);
  TotalBytesAllocated += 4*(word_len+1);
printf("TotalBytesAllocated 1: %ld\n",TotalBytesAllocated);
  return (value_t) res;
}



value_t alloc_bigptrarray_gen(int log_len, value_t init_val, int ptag)
{
  Thread_t *curThread = getThread();
  long *saveregs = curThread->saveregs;
  value_t *res = 0;
  int i, tag = PARRAY_TAG | log_len << (2 + ARRLEN_OFFSET);
  int real_byte_len = 4 * (log_len + 1);

    {
      /* we collect if init_val is from young area */
      if ((old_fromheap->alloc_start + real_byte_len >= old_fromheap->top) ||
	  (init_val >= nursery->bottom && init_val < nursery->top)) {
	curThread->request = 4;
	gc_gen(curThread,0);
      }
      assert(old_fromheap->alloc_start + real_byte_len < old_fromheap->top);
#ifdef HEAPPROFILE
      real_byte_len needs to be 4 bigger if we profile objects
      *(value_t *)(old_fromheap->alloc_start) = 30009;
      old_fromheap->alloc_start += 4;
#endif
      res = (value_t *)(old_fromheap->alloc_start + 4);
      old_fromheap->alloc_start += real_byte_len;
      assert(old_fromheap->alloc_start < old_fromheap->top);
      old_alloc_ptr = old_fromheap->alloc_start;
    }
  res[-1] = tag;
  for (i=0; i<log_len; i++)
    res[i] = init_val;
  return (value_t) res;
}



value_t alloc_bigfloatarray_gen(int log_len, double init_val, int ptag)
{
  double *rawstart = NULL;
  value_t *res = NULL;
  long i;
  Thread_t *curThread = getThread();
  long *saveregs = curThread->saveregs;
  int pos, tag = RARRAY_TAG | (log_len << (3 + ARRLEN_OFFSET));
  int real_byte_len = 8 * (log_len + 1);

#ifdef DEBUG
  printf("(log_len,init_val)  is  (%d, %lf)\n",log_len,init_val);
#endif


    {
      assert(log_len >= 4096);
      assert(real_byte_len < floatheapsize);
      pos = AllocBitmapRange(floatbitmap,DivideUp(real_byte_len,floatbitmapsize));

      if (pos < 0)
	{
	  int qlen;
#ifdef DEBUG
	  printf("Have to do a GC while doing a float_alloc.  Hope this works.\n");
#endif
	  QueueClear(float_roots);    
	  curThread->request = 4;
	  gc_gen(curThread,1);
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
	      int word_len = GET_ARRLEN(tag)/4;
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
	  pos = AllocBitmapRange(floatbitmap,DivideUp(real_byte_len,floatbitmapsize));
#ifdef DEBUG
	  printf("pos = %d, size = %d start = %d, safeend = %d\n",
		 pos,DivideUp(real_byte_len,floatbitmapsize),
		 (double *)(floatheap->bottom + floatbitmapsize * pos),
		 (double *)(floatheap->bottom + floatbitmapsize * pos) + (log_len + 1));
#endif
	}
      assert(pos >= 0);
      
      rawstart = (double *)(floatheap->bottom + floatbitmapsize * pos);
      res = (value_t *)(rawstart + 1);
      TotalBytesAllocated += RoundUp(real_byte_len,floatbitmapsize);
    }
#ifdef HEAPPROFILE
  ((int *)res)[-2] = 30011;
#endif
  ((int *)res)[-1] = RARRAY_TAG | (log_len << (3 + ARRLEN_OFFSET));
  for (i=0; i<log_len; i++)
    ((double *)res)[i] = init_val;

#ifdef DEBUG
{
  static int count = 0;
  printf("\n--- float_alloc %d returning [%d(-8) to %d)\n",++count,res,res+2*log_len);
}
#endif
  return (value_t) res;
}

/* --------------------- Generational collector --------------------- */


value_t *forward_writelist_major(value_t *more_roots, value_t *to_ptr, 
				 range_t *from_range, range_t *from2_range, range_t *to_range)
{
  value_t *slot = 0;
  while (slot = (value_t *)(*(more_roots++))) {
    value_t data = *slot;
    if (data - to_range->low >= to_range->diff)
      forward_major(slot,to_ptr,from_range,from2_range,to_range);
  }
  return to_ptr;
}

value_t *forward_writelist_minor(value_t *more_roots, value_t *to_ptr, 
				 range_t *from_range, range_t *to_range)
{
  value_t *slot = 0;
  while (slot = (value_t *)(*(more_roots++))) {
    value_t data = *slot;
    if (data - to_range->low >= to_range->diff)
	forward_minor(slot,to_ptr,from_range);
  }
  return to_ptr;
}


value_t *forward_gen_locatives(value_t *to_ptr, 
			       range_t *from_range, range_t *from2_range, range_t *to_range)
{
  long i;
    static Queue_t *LastScanQueue = 0;
    if (LastScanQueue == 0)
      LastScanQueue = QueueCreate(0,100);

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
      forward_major((value_t *)(&obj_start),to_ptr,from_range,from2_range,to_range);
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
	forward_major(data_addr,to_ptr,from_range,from2_range,to_range); 
      }
    QueueClear(LastScanQueue);
    for (i=0; i<QueueLength(ScanQueue); i++)
      {
	value_t *meta_addr = (value_t *)QueueAccess(ScanQueue,i);
	Enqueue(LastScanQueue,(void *)(*meta_addr));
      }
    return to_ptr;
}


void gc_gen(Thread_t *curThread, int isMajor)
{
  int regmask = 0;
  SysThread_t *sysThread = getSysThread();
  long *saveregs = curThread->saveregs;
  int allocptr = sysThread->alloc;
  int alloclimit = sysThread->limit;
  int req_size = curThread->request;

  struct rusage start,finish;
  Queue_t *root_lists, *loc_roots;
  enum GCType GCtype = isMajor ? Major : Minor;
  value_t to_allocptr;

  /* Check for first time heap value needs to be initialized */
  assert(sysThread->userThread == NULL);
  if (alloclimit == StartHeapLimit)
    {
      sysThread->alloc = nursery->bottom;
      sysThread->limit = nursery->top;
      return;
    }

  assert(saveregs[ALLOCPTR] <= saveregs[ALLOCLIMIT]);
  assert(allocptr <= alloclimit);
  assert(req_size >= 0);
  assert(writelist_cursor <= writelist_end);

  /* start timer */
  root_lists = curThread->root_lists;
  loc_roots = curThread->loc_roots;
  start_timer(&sysThread->gctime);

  if (paranoid) {
    Heap_t *legalHeaps[3];
    legalHeaps[0] = nursery;
    legalHeaps[1] = old_fromheap;
    legalHeaps[2] = NULL;
    paranoid_check_heap_global(nursery, legalHeaps);  
    paranoid_check_heap_global(old_fromheap, legalHeaps);  
  }

  /* these are debugging and stat-gatherting procedure */
#ifdef DEBUG
  measure_semantic_garbage_before();
  debug_and_stat_before(saveregs, req_size);
#endif

  /* Compute the roots from the stack and register set */
  local_root_scan(sysThread,curThread,nursery);

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
	    (old_unused_amount < (nursery->top - nursery->bottom)))
	      {
		GCtype = Major;
#ifdef DEBUG
		printf("old_unused_ratio = %lf\n",old_unused_ratio);
		printf("old_unused_amount = %d\n",old_unused_amount);
#endif
	      }
        }
      assert(nursery->alloc_start     < nursery->top);
      assert(old_fromheap->alloc_start < old_fromheap->top);
      assert(old_toheap->alloc_start   < old_toheap->top);

      assert(writelist_cursor <= writelist_end);
      *((int *)writelist_cursor) = 0;

   /* Perform just a minor GC if it is very likely we don't need a major GC */
      if (GCtype == Minor)
	{
	  range_t from_range, from2_range, to_range;
	  value_t *to_ptr = (value_t *)old_alloc_ptr;

	  assert(old_alloc_ptr >= old_fromheap->alloc_start);
	  assert(old_fromheap->top - old_alloc_ptr > (nursery->top - nursery->bottom));

	  SetRange(&from_range,nursery->bottom, nursery->top);
	  SetRange(&from2_range,0,0);
	  SetRange(&to_range,old_fromheap->bottom, old_fromheap->top);

#ifdef HEAPPROFILE
         /* necessary since before collect assumes no forward tags */
	    gcstat_heapprofile_beforecollect((value_t *)nursery->alloc_start,
				     (value_t *)allocptr);
	  
#endif
	  
	  /* do the normal roots and the writelist */
	  to_ptr = forward_root_lists_minor(root_lists, to_ptr, 
					    &from_range, &to_range);
	  
	  to_ptr = forward_writelist_minor((value_t *)writelist_start, to_ptr, 
					   &from_range, &to_range);
	  
	  to_ptr = forward_gen_locatives(to_ptr,&from_range,&from2_range,&to_range); 
	  to_ptr = scan_stop_minor(old_fromheap->alloc_start,to_ptr,(value_t *)old_alloc_ptr,
				   &from_range,&to_range);
	  to_ptr = scan_nostop_minor(old_alloc_ptr,to_ptr,&from_range,&to_range);
	  
#ifdef HEAPPROFILE
	  gcstat_heapprofile_aftercollect((value_t *)from_range->low,
					  (value_t *)allocptr);
	  
#endif
	  to_allocptr = (value_t)to_ptr;

	  old_fromheap->alloc_start = old_alloc_ptr;
	  assert(old_fromheap->alloc_start < old_fromheap->top);
	  gcstat_normal(allocptr - nursery->alloc_start,
			0, 0.0, 0,
			to_allocptr - old_alloc_ptr);

	  old_alloc_ptr = to_allocptr;
	  old_alloc_limit = old_fromheap->top;



	  old_fromheap->alloc_start = to_allocptr;
	  assert(old_fromheap->alloc_start < old_fromheap->top);

	  /* If the minor GC failed to produce less free space in the tenured area
	     than the size of the nursery, then trigger a major GC */
	  if ((old_fromheap->top - old_fromheap->alloc_start) < 
	      (nursery->top - nursery->bottom))
	    GCtype = ForcedMajor;
	}

      /* Perform a major GC if 
	  (1) a minor GC was not taken in the first place
	  (2) a minor GC was performed but this triggered a major GC due to low space
      */
      if (GCtype != Minor)
	{
	  range_t from_range, from2_range, to_range;
	  value_t *to_ptr = (value_t *)old_toheap->bottom;

	  int i, newsize;
	  start_timer(&(sysThread->majorgctime));
	  newsize = ((old_fromheap->alloc_start - old_fromheap->bottom) +
		     (nursery->top - nursery->bottom));
	  if ((newsize >= (Heap_Getsize(old_toheap))) && (MinHeap != MaxHeap))
	    {
	      printf("WARNING: GC failure possible due to newsize\n");
	      printf("old_fromheap->top = %d, old_fromheap->bottom = %d\n",
		     old_fromheap->top, old_fromheap->bottom);
	      printf("nursery->top = %d, nursery->bottom = %d\n",
		     nursery->top, nursery->bottom);
	      printf("old_toheap->rawtop = %d, old_toheap->top = %d, old_toheap->bottom = %d\n",
		     old_toheap->rawtop, old_toheap->top, old_toheap->bottom);
	    }    
	  newsize = Heap_Getsize(old_toheap);
	  Heap_Resize(old_toheap,newsize);
	  Heap_Unprotect(old_toheap); 	  
	  if (GCtype == ForcedMajor)
	      fprintf(stderr,"--------MAJOR GC %d at GC %d---------\n",NumMajorGC, NumGC);
	  else 
	      fprintf(stderr,"--------FORCED MAJOR GC %d at GC %d---------\n",NumMajorGC, NumGC);
#ifdef DEBUG
	  printf("nursery->bottom, nursery->top: %d %d\n",
		 nursery->bottom, nursery->top);
	  printf("old_fromheap->{bottom, alloc_start, top}: %d %d %d\n",
		 old_fromheap->bottom, old_fromheap->alloc_start, old_fromheap->top);
#endif

	  assert(old_alloc_ptr >= old_fromheap->alloc_start);
	  global_root_scan(sysThread,global_roots,nursery);
	  Enqueue(root_lists,global_roots);
	  SetRange(&from_range,nursery->bottom, nursery->top);
	  SetRange(&from2_range,old_fromheap->bottom, old_alloc_ptr);
	  SetRange(&to_range,old_toheap->bottom, old_toheap->top);



#ifdef HEAPPROFILE
 /* necessary since before collect assumes no forward tags */
	  if (GCtype != ForcedMajor)
	    gcstat_heapprofile_beforecollect((value_t *)nursery->alloc_start,
					     (value_t *)allocptr);
	  
#endif

	  /* do the normal roots and the writelist */
	  to_ptr = forward_root_lists_major(root_lists, to_ptr, 
					    &from_range, &from2_range, &to_range);
	  
	  to_ptr = forward_writelist_major((value_t *)writelist_start, to_ptr, 
					   &from_range, &from2_range, &to_range);
	  
	  to_ptr = forward_gen_locatives(to_ptr,&from_range,&from2_range,&to_range); 
	  to_ptr = scan_major(old_toheap->bottom,to_ptr,(value_t *)to_range.high,
			      &from_range,&from2_range,&to_range);


#ifdef HEAPPROFILE
	  gcstat_heapprofile_aftercollect((value_t *)from2_range->low,
					    (value_t *)from2_range->high);
	  gcstat_heapprofile_aftercollect((value_t *)from_range->low,
					  (value_t *)allocptr);
	  
#endif
	  to_allocptr = (value_t) to_ptr;

	  assert(to_allocptr < old_toheap->top);
	  old_toheap->alloc_start = to_allocptr;
	  old_alloc_ptr = to_allocptr;
	  old_alloc_limit = old_toheap->top;
	  gcstat_normal(allocptr - nursery->alloc_start,
			0, 0.0, 0,
			to_allocptr - old_toheap->bottom);


	  typed_swap(Heap_t *, old_fromheap, old_toheap);


	  stop_timer(&sysThread->majorgctime);
	}


      assert(nursery->alloc_start     < nursery->top);
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
	  req_size > (nursery->top - nursery->bottom))
	{
	  fprintf(stderr,"\ncopied = %d,  req_size = %d\n",copied,req_size);
	  fprintf(stderr,"oldsize = %d,  newsize = %d\n",oldsize,newsize);
	  fprintf(stderr,"FATAL ERROR: gen failure reqesting %d bytes\n",req_size);
	  exit(-1);
	}
      Heap_Resize(old_fromheap,newsize);
    }


    Heap_Unprotect(old_fromheap);
    debug_after_collect(nursery, old_fromheap);


    sysThread->alloc = nursery->bottom;
    sysThread->limit = nursery->top;

  /* More debugging and stat-gathering procedure */
  measure_semantic_garbage_after();    
  if (paranoid) {
    Heap_t *legalHeaps[2];
    legalHeaps[0] = old_fromheap;
    legalHeaps[1] = NULL;
    paranoid_check_stack(curThread,nursery);
    paranoid_check_heap_global(old_fromheap, legalHeaps);  
  }

  if (GCtype != Minor)
    NumMajorGC++;
  NumGC++;

  /* stop timer */
  stop_timer(&sysThread->gctime); 

}

#define INT_INIT(x,y) { if (x == 0) x = y; }
#define DOUBLE_INIT(x,y) { if (x == 0.0) x = y; }

void gc_init_gen() 
{
  /* secondary cache size */
  int cache_size = GetBcacheSize();
  INT_INIT(YoungHeapByte, (int)(0.85 * cache_size));
  
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
  nursery = Heap_Alloc(YoungHeapByte, YoungHeapByte);
  old_fromheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  old_toheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  floatheap = Heap_Alloc(floatheapsize,floatheapsize);
  floatbitmap = CreateBitmap(floatheapsize / floatbitmapsize);
  float_roots = QueueCreate(0,100);
  global_roots = QueueCreate(0,100);
#ifdef OLD_ALLOC
  old_alloc_ptr = old_fromheap->alloc_start;
  old_alloc_limit = old_fromheap->top;
#endif
}


void gc_finish_gen()
{
  Thread_t *th = getThread();
  int allocsize = th->saveregs[ALLOCPTR] - nursery->alloc_start;
  gcstat_finish(allocsize);
#ifdef HEAPPROFILE
  gcstat_heapprofile_beforecollect((value_t *)nursery->alloc_start,
				   (value_t *)allocptr);
  gcstat_heapprofile_aftercollect((value_t *)nursery->bottom,
				   (value_t *)allocptr);
  printf("gcstat_heapprofile_aftercollect: old_fromheap->bottom = %d\n",
	 old_fromheap->bottom);
  printf("gcstat_heapprofile_aftercollect: old_fromheap->alloc_start = %d\n",
	 old_fromheap->alloc_start);
  gcstat_heapprofile_aftercollect((value_t *)old_fromheap->bottom,
				  (value_t *)old_fromheap->alloc_start);
  gcstat_show_heapprofile("full",0.0,0.0);
  printf("\n\n\n\n");
  gcstat_show_heapprofile("short",1.0,1.0);
#endif
}
