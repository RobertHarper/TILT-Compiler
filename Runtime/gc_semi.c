/* Not thread-safe */

#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "stack.h"
#include "thread.h"
#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "global.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "show.h"


extern int NumGC;
extern Queue_t *ScanQueue;
extern value_t MUTABLE_TABLE_BEGIN_VAL;
extern value_t MUTABLE_TABLE_END_VAL;
extern int module_count;

static Heap_t *fromheap = NULL, *toheap = NULL;
static Queue_t *global_roots = 0;


/* ------------------  Semispace array allocation routines ------------------- */

static value_t* alloc_big(int wordLen, int oddAlign) 
{
  value_t *res = 0;
  Thread_t *curThread = getThread();
  long *saveregs = curThread->saveregs;
  value_t alloc_ptr = saveregs[ALLOCPTR];
  value_t alloc_limit = saveregs[ALLOCLIMIT];
  int byteLen = 4 * wordLen;
  int byteLenPad = byteLen + (oddAlign ? 4 : 0);

  /* Make sure there is enough space */
  if (alloc_ptr + byteLenPad >= alloc_limit)
    {
      GCFromC(curThread, byteLenPad, 0);
      alloc_ptr = saveregs[ALLOCPTR];
      alloc_limit = saveregs[ALLOCLIMIT];
    }
  assert(alloc_ptr + byteLenPad < alloc_limit);

  /* Perform alignment and actual allocation */
  if ((alloc_ptr & 7) == 0) {
    *((value_t *)alloc_ptr) = SKIP_TAG;
    alloc_ptr += 4;
  }
  res = (value_t *)(alloc_ptr);
  alloc_ptr += byteLen;
  saveregs[ALLOCPTR] = alloc_ptr;

  /* Update statistics */
  gcstat_normal(byteLen, 0);

  return res;
}

value_t alloc_bigintarray_Semi(int byteLen, value_t initVal, int ptag)
{
  int wordLen = 4 + (byteLen + 3) / 4;
  value_t *space = alloc_big(wordLen,0);
  value_t * res = space + 1;
  init_iarray(res, byteLen, initVal);
  return (value_t) res;
}

value_t alloc_bigptrarray_Semi(int wordLen, value_t initVal, int ptag)
{
  value_t *space = alloc_big(4 + wordLen,0);
  value_t *res = space + 1;
  init_parray(res, wordLen, initVal);
  return (value_t) res;
}

value_t alloc_bigfloatarray_Semi(int doubleLen, double initVal, int ptag)
{
  int wordLen = 2 * doubleLen;
  value_t *space = alloc_big(4 + wordLen,1);
  value_t *res = space + 1;
  init_farray(res, doubleLen, initVal);
  return (value_t) res;
}


/* --------------------- Semispace collector --------------------- */
int GCAllocate_Semi(SysThread_t *sysThread, int req_size)
{
  /* Check for first time heap value needs to be initialized */
  assert(sysThread->userThread == NULL);
  if (sysThread->limit == StartHeapLimit)
    {
      sysThread->alloc = fromheap->bottom;
      sysThread->limit = fromheap->top;
      return (req_size < (sysThread->alloc  - sysThread->limit));
    }
  return 0;
}

void GC_Semi(SysThread_t *sysThread, int req_size)
{
  int i;  
  Thread_t *curThread;
  int allocptr = sysThread->alloc;
  int alloclimit = sysThread->limit;
  value_t *to_ptr = (value_t *)toheap->bottom;
  range_t from_range, to_range;
  Queue_t *root_lists, *loc_roots;
  long alloc = 0;

  /* Start timing this collection */
  start_timer(&sysThread->gctime);

  /* Check that processor is unmapped, write list is not overflowed, allocation region intact */
  assert(sysThread->userThread == NULL);
  assert(writelist_cursor >= writelist_start);
  assert(writelist_cursor <= writelist_end);
  writelist_cursor = writelist_start;          /* Write list irrelevant in a semispace collector */
  assert(allocptr <= alloclimit);
  assert(req_size >= 0);

  alloc = allocptr - fromheap->alloc_start;
  fromheap->alloc_start = allocptr;
  curThread = &(Threads[0]);             /* In a sequential collector, 
					    there is only one user thread */
  root_lists = curThread->root_lists;
  loc_roots = curThread->loc_roots;


  if (debug)
    debug_and_stat_before(curThread->saveregs, req_size);
  if (paranoid) {
    Heap_t *legalHeaps[2];
    legalHeaps[0] = fromheap;
    legalHeaps[1] = NULL;
    paranoid_check_heap_global(fromheap,legalHeaps);
  }

  /* Compute the roots from the stack and register set */
  local_root_scan(sysThread,curThread,fromheap);
  global_root_scan(sysThread,global_roots,fromheap);
  Enqueue(root_lists,global_roots);

  /* Also add in the locative roots */
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
	  forward_minor((value_t *)(&obj_start),to_ptr,&from_range);
	  *((int *)first) = obj_start + offset;
	}
    }
  Enqueue(root_lists, loc_roots); 
  
    
  /* Get tospace and ranges ready for the collection */
  Heap_Resize(toheap, fromheap->top - fromheap->bottom);
  Heap_Unprotect(toheap);
  SetRange(&from_range, fromheap->bottom, fromheap->top);
  SetRange(&to_range, toheap->bottom, toheap->top);

#ifdef HEAPPROFILE
  gcstat_heapprofile_beforecollect((value_t *)fromheap->alloc_start,
				   (value_t *)allocptr);

#endif


  /* forward the roots */
  to_ptr = forward_root_lists_minor(root_lists, to_ptr, 
				    &from_range, &to_range);

  /* perform a Cheney scan */
  to_ptr = scan_nostop_minor(to_range.low,to_ptr,&from_range,&to_range);
  toheap->alloc_start = (value_t) to_ptr;

#ifdef HEAPPROFILE
  gcstat_heapprofile_aftercollect((value_t *)from_low,
				(value_t *)allocptr);

#endif

    if (debug && SHOW_HEAPS)
      {
	memdump("From Heap After collection:", (int *)fromheap->bottom,40,0);
        memdump("To Heap After collection:", (int *)toheap->bottom,40,0);
	show_heap_raw("FINAL FROM",(allocptr - fromheap->bottom) / 4,
		      fromheap->bottom,fromheap->top,
		      toheap->bottom,toheap->top);
      }

    if (paranoid) {
      Heap_t *legalHeaps[2];
      legalHeaps[0] = toheap;
      legalHeaps[1] = NULL;
      paranoid_check_stack(curThread,fromheap);
      paranoid_check_heap_global(toheap,legalHeaps);
    }

  /* Resize the tospace by using the oldspace size and liveness ratio */
    {
      long alloc = fromheap->top - fromheap->bottom;
      long copied = ((value_t)to_ptr) - toheap->bottom;
      long used = fromheap->top - fromheap->bottom + req_size;
      long live = copied + req_size;
      double ratio = (double)(live) / used;
      long new = ComputeHeapSize(live, ratio);
      if (new < live) {
	  fprintf(stderr,"FATAL ERROR: failure reqesting %d bytes\n",req_size);
	  assert(0);
	}

      gcstat_normal(alloc,copied);

      Heap_Resize(toheap,new);
      Heap_Unprotect(toheap); 

    }
 
  /* Make sure that we actually fulfilled the GC request */
  if (toheap->top - (value_t)to_ptr <= req_size)
    {
      printf("Error condition: toheap->top - (value_t)to_ptr <= req_size\n");
      printf("                 %d - %d <= %d\n\n",
	     toheap->top, (value_t)to_ptr, req_size);
      assert(0);
      exit(-1);
    }

  /* Switch roles of from-space and tospace-space */
  Heap_Protect(fromheap);
  typed_swap(Heap_t *, fromheap, toheap);

  /* Update systhread's allocation variables */
  fromheap->alloc_start = (value_t) to_ptr;
  sysThread->alloc = fromheap->alloc_start;
  sysThread->limit = fromheap->top;

  NumGC++;

  /* Stop timer for collection */
  stop_timer(&sysThread->gctime);
}

#define INT_INIT(x,y) { if (x == 0) x = y; }
#define DOUBLE_INIT(x,y) { if (x == 0.0) x = y; }

void gc_init_Semi()
{
  INT_INIT(MaxHeap, 80 * 1024);
  INT_INIT(MinHeap, 256);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  DOUBLE_INIT(MinRatio, 0.1);
  DOUBLE_INIT(MaxRatio, 0.7);
  DOUBLE_INIT(MinRatioSize, 512);         
  DOUBLE_INIT(MaxRatioSize, 50 * 1024);
  fromheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toheap = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  global_roots = QueueCreate(0,100);
}


void gc_finish_Semi()
{
  Thread_t *th = getThread();
  int allocsize = th->saveregs[ALLOCPTR] - fromheap->alloc_start;
  gcstat_finish(allocsize);
#ifdef HEAPPROFILE
  gcstat_heapprofile_beforecollect((value_t *)fromheap->alloc_start,
				   (value_t *)allocptr);
  gcstat_heapprofile_aftercollect((value_t *)fromheap->bottom,
				   (value_t *)allocptr);
  gcstat_show_heapprofile("full",0.0,0.0);
  printf("\n\n\n\n");
  gcstat_show_heapprofile("short",1.0,1.0);
#endif
}
