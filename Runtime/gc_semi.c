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

value_t alloc_bigwordarray_semi(int tagType, int byte_len, value_t init_val, int ptag)
{
  Thread_t *curThread = getThread();
  long *saveregs = curThread->saveregs;
  value_t alloc_ptr = saveregs[ALLOCPTR];
  value_t alloc_limit = saveregs[ALLOCLIMIT];
  int word_len = (byte_len + 3) / 4;
  int tag = tagType | (byte_len << ARRLEN_OFFSET);

  int i;
  value_t *res = 0;

  /* Make sure there is enough space */
  if (alloc_ptr + (word_len + 1) >= alloc_limit)
    {
#ifdef DEBUG
      printf("DOING GC inside int_alloc with a semispace collector\n");
#endif
      curThread->request = 4 * (word_len + 1);
      gc_semi(curThread);
      alloc_ptr = saveregs[ALLOCPTR];
      alloc_limit = saveregs[ALLOCLIMIT];
    }
  assert(alloc_ptr + (word_len + 1) < alloc_limit);

  /* Perform actual allocation and initialization */
#ifdef HEAPPROFILE
  *(value_t *)alloc_ptr = 30006;
  alloc_ptr += 4;
#endif
  res = (value_t *)(alloc_ptr + 4);
  alloc_ptr += 4 * (word_len + 1);
  saveregs[ALLOCPTR] = alloc_ptr;
  res[-1] = tag;
  for (i=0; i<word_len; i++)
    res[i] = init_val;

  /* Update statistics */
  gcstat_normal(4*(word_len+1), 0);

  return (value_t) res;
}


value_t alloc_bigintarray_semi(int bytelen, value_t value, int ptag)
{
  return alloc_bigwordarray_semi(IARRAY_TAG, bytelen, value, ptag);
}

value_t alloc_bigptrarray_semi(int wordlen, value_t value, int ptag)
{
  return alloc_bigwordarray_semi(PARRAY_TAG, wordlen * 4, value, ptag);
}

value_t alloc_bigfloatarray_semi(int log_len, double init_val, int ptag)
{
  double *rawstart = NULL;
  value_t *res = NULL;
  long i;
  Thread_t *curThread = getThread();
  long *saveregs = curThread->saveregs;
  int byte_len = log_len << 3;
  int pos, tag = RARRAY_TAG | (byte_len << ARRLEN_OFFSET);
  value_t alloc_ptr = saveregs[ALLOCPTR];
  value_t alloc_limit = saveregs[ALLOCLIMIT];

  /* Make sure there is enough space */
  if (alloc_ptr + 8 * (log_len + 3) >= alloc_limit)
    {
      long req_size = 8 * (log_len + 3);
#ifdef DEBUG
      printf("DOING GC inside float_alloc with a semispace collector\n");
#endif
      saveregs[ALLOCLIMIT] = req_size;
      gc_semi(curThread);
      alloc_ptr = saveregs[ALLOCPTR];
      alloc_limit = saveregs[ALLOCLIMIT];
    }
  assert(alloc_ptr + 8 * (log_len + 3) <= alloc_limit);

  /* Perform alignment and write heap profile tag */
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

  /* Allocate and initialize */
  res = (value_t *)(alloc_ptr + 4);
  assert ((value_t)res % 8 == 0);
  alloc_ptr = (((value_t) res) + (8 * log_len));
  saveregs[ALLOCPTR] = alloc_ptr;
  res[-1] = tag;
  for (i=0; i<log_len; i++)
    ((double *)res)[i] = init_val;

  return (value_t) res;
}




/* --------------------- Semispace collector --------------------- */


void gc_semi(Thread_t *curThread) /* Not mapped */
{
  int i;  
  SysThread_t *sysThread = getSysThread();
  long *saveregs = curThread->saveregs;
  int allocptr = sysThread->alloc;
  int alloclimit = sysThread->limit;
  int req_size = curThread->request;
  value_t *to_ptr = (value_t *)toheap->bottom;
  range_t from_range, to_range;
  Queue_t *root_lists, *loc_roots;
  long alloc = 0;

#ifdef SEMANTIC_GARBAGE
  assert(0); /* unimplemented */
#endif

  assert(sysThread->userThread == NULL);
  assert(curThread->sysThread == NULL);
  assert(writelist_cursor >= writelist_start);
  assert(writelist_cursor <= writelist_end);
  writelist_cursor = writelist_start;          /* Write list irrelevnat in a semispace collector */

  /* Check for first time heap value needs to be initialized */
  if (sysThread->limit == StartHeapLimit)
    {
      sysThread->alloc = fromheap->bottom;
      sysThread->limit = fromheap->top;
      return;
    }

  assert(allocptr <= alloclimit);
  assert(req_size >= 0);
  alloc = allocptr - fromheap->alloc_start;
  fromheap->alloc_start = allocptr;

  /* Start timing this collection */
  root_lists = curThread->root_lists;
  loc_roots = curThread->loc_roots;
  start_timer(&sysThread->gctime);

#ifdef DEBUG
  debug_and_stat_before(saveregs, req_size);
#endif

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

#ifdef DEBUG
    if (SHOW_HEAPS)
      {
	memdump("From Heap After collection:", (int *)fromheap->bottom,40,0);
        memdump("To Heap After collection:", (int *)toheap->bottom,40,0);
	show_heap("FINAL FROM",fromheap->bottom,allocptr,fromheap->top);
	show_heap("FINAL TO",toheap->bottom,to_ptr,toheap->top);
      }
#endif

    if (paranoid) {
      Heap_t *legalHeaps[2];
      legalHeaps[0] = toheap;
      legalHeaps[1] = NULL;
      paranoid_check_stack(curThread,fromheap);
      paranoid_check_heap_global(toheap,legalHeaps);
    }

  /* Resize the tospace by using the oldspace size and liveness ratio */
    {
      long old = fromheap->top - fromheap->bottom;
      long copied = ((value_t)to_ptr) - toheap->bottom;
      double oldratio = (double)(copied) / old;
      long cur = copied + req_size;
      long live = (cur > old) ? cur : old;
      long new = ComputeHeapSize(live, oldratio);
      if (new < cur)
	{
	  fprintf(stderr,"FATAL ERROR: failure reqesting %d bytes\n",req_size);
	  exit(-1);
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

void gc_init_semi()
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


void gc_finish_semi()
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
