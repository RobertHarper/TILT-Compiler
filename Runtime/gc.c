/* Not thread-safe */
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "memobj.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "show.h"



/* use generational by default */
int paranoid = 0;
int verbose = 0;
int diag = 0;
int debug = 0;
int collector_type = Generational;
int SHOW_GCDEBUG_FORWARD = 0;
int SHOW_GCERROR   = 1;
int SHOW_GCSTATS   = 0;
int SHOW_GCDEBUG   = 0;
int SHOW_HEAPS     = 0;
int SHOW_GCFORWARD = 0;



unsigned int write_count = 0;

int NumGC = 0;
int NumMajorGC = 0;
int NumLocatives = 0;
int NumRoots = 0;


extern int module_count;
extern val_t GLOBALS_BEGIN_VAL;
extern val_t GLOBALS_END_VAL;

static range_t null_range;


int YoungHeapByte = 0, MaxHeap = 0, MinHeap = 0;
double MinRatio = 0.0, MaxRatio = 0.0;
int MinRatioSize = 0,  MaxRatioSize = 0;

long ComputeHeapSize(long oldsize, double oldratio)
{
  /* Internally, we will round to the nearest K */
  long oldlive = ((long)(oldsize * oldratio) + 1023) / 1024;  /* must round up here */
  double rawWhere = (oldlive - MinRatioSize) / (double)(MaxRatioSize - MinRatioSize);
  double where = (rawWhere > 1.0) ? 1.0 : ((rawWhere < 0.0) ? 0.0 : rawWhere);
  double newratio = MinRatio + where * (MaxRatio - MinRatio);
  long newsize = (oldlive / newratio) / 32 * 32 + 32;
  if (oldlive > MaxHeap) {
    fprintf(stderr,"GC error: livedata = %d but maxheap constrained to %d\n", oldlive, MaxHeap);
    assert(0);
  }
  if (newsize > MaxHeap) {
    double constrainedRatio = ((double)oldlive) / MaxHeap;
    fprintf(stderr,"GC warning: Would like to make newheap %d but constrained to <= %d\n",newsize, MaxHeap);
    if (constrainedRatio > 0.95)
      fprintf(stderr,"GC warning: Ratio is dangerously high %lf.\n", constrainedRatio);
    newsize = MaxHeap;
  }
  if (newsize < MinHeap) {
    if (diag)
      fprintf(stderr,"GC warning: Would like to make newheap %d but constrained to >= %d\n",newsize, MinHeap);
    newsize = MinHeap;
  }
  assert(newsize > oldlive);
  return 1024 * newsize;
}

void HeapAdjust(int show, unsigned int bytesRequested, Heap_t **froms, Heap_t *to)
{
  long copied = 0, occupied = 0, live = 0, newSize = 0;
  double liveRatio = 0.0;

  while (*froms != NULL) {
    Heap_t *cur = *(froms++);
    if (cur != NULL)
      occupied += (sizeof (val_t)) * (cur->top - cur->bottom);
  }
  copied = (sizeof (val_t)) * (to->alloc_start - to->bottom);
  live = copied + bytesRequested;
  liveRatio = (double) live / (double) occupied;
  newSize = ComputeHeapSize(occupied, liveRatio);
  Heap_Resize(to, newSize);
  if (newSize - copied < bytesRequested) {
    printf("Error: newSize - copied < bytesRequested\n");
    printf("       %d - %d <= %d\n\n",
	   newSize, copied, bytesRequested);
    assert(0);
  }
  if (show) {
    fprintf(stderr,"---- MAJOR GC %d (%d): ", NumMajorGC, NumGC);
    fprintf(stderr,"live = %d     oldHeap = %d(%.3lf)  -> newHeap = %d(%.3lf)\n", 
	    live, occupied, liveRatio, newSize, ((double)live)/newSize);
  }
}

void gc_init(void)
{
  SetRange(&null_range,0,0);
  switch (collector_type) 
    {
      case Semispace :
	{
	  gc_init_Semi();
	  break;
	}
      case Generational :
	{ 
	  gc_init_Gen();
	  break;
	}
      case SemispaceParallel:
	{
	  gc_init_SemiPara();
	  break;
	}
      case GenerationalParallel:
	{
	  gc_init_GenPara();
	  break;
	}
      }
}

void paranoid_check_global(char *label, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int count = 0, mi, i;
  /* check globals */
  for (mi=0; mi<module_count; mi++) {
    mem_t start = (mem_t) (&GLOBALS_BEGIN_VAL)[mi];
    mem_t stop = (mem_t) (&GLOBALS_END_VAL)[mi];
    scan_heap(label,start,stop,stop, legalHeaps, legalStarts, SHOW_HEAPS, 0);
  }
}

Bitmap_t *paranoid_check_heap_without_start(char *label, Heap_t *curSpace, Heap_t **legalHeaps)
{
  int count = 0, mi, i;
  return scan_heap(label,curSpace->bottom, curSpace->alloc_start, 
		   curSpace->top, legalHeaps, NULL,
		   0, 1);
}

void paranoid_check_heap_with_start(char *label, Heap_t *curSpace, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int count = 0, mi, i;
  scan_heap(label,curSpace->bottom, curSpace->alloc_start, 
	    curSpace->top, legalHeaps, legalStarts,
	    SHOW_HEAPS && (NumGC >= LEAST_GC_TO_CHECK), 0);
}

void paranoid_check_stack(char *label, Thread_t *thread, Heap_t *fromspace)
{
    int count = 0, mi, i;
    mem_t cursor;
    long *saveregs = thread->saveregs;
    mem_t sp = (mem_t) saveregs[SP];
    mem_t stack_top = thread->stackchain->stacks[0]->top;
    /* should check start_addr */
    if ((mem_t)saveregs[ALLOCLIMIT] == StopHeapLimit)
      return;
    for (i=thread->nextThunk; i<thread->numThunk; i++) { /* check thunks */
      ptr_t thunk = thread->thunks[i];
      if (thunk >= fromspace->bottom && thunk < fromspace->top) {
	printf("TRACE ERROR*: thunk %d has from-space value after collection: %d", i, thunk);
	assert(0);
      }
    }
      
    if (thread->nextThunk == 0) /* thunk not started */
      return;
    for (count = 0; count < 32; count++)
      {
	int data = saveregs[count];
	if (count == ALLOCPTR) {
	  if (verbose)
	    printf("Allocation Register %d has value %d\n", count, data);
	}
	else if (count == ALLOCLIMIT) {
	  if (verbose)
	    printf("Allocation Limit Register %d has value %d\n", count, data);
	}
	else if ((data & 3) == 0 && (mem_t)data >= fromspace->bottom && (mem_t)data < fromspace->top)
	  {
	    static int newval = 62000;
	    if (verbose) {
	      printf("TRACE WARNING GC %d: register %d has from space value %d",
		     NumGC,count,data);
	      printf("      changing to %d\n", newval);
	    }
	    saveregs[count] = newval; 
	    newval++;
	  }
	else if (verbose)
	  printf("Register %d has okay value %d\n", count, data);
      }

    for (cursor = sp; cursor < stack_top - 16; cursor++)
      {
	val_t data = *cursor;
	if ((data & 3) == 0 && (mem_t) data >= fromspace->bottom && (mem_t) data < fromspace->top)
	  {
	    static int newval = 42000;
	    if (verbose) {
	      printf("TRACE WARNING: stack location %d has from-space address %d changing to %d\n",
		     cursor,data,newval);
	    }
	    *cursor = newval; 
	    newval++;
	  }
      }

}


void debug_after_collect(Heap_t *fromheap, Heap_t* old_fromheap)
{
#ifdef DEBUG
      mem_t a = fromheap->bottom, b = fromheap->top;
      /*      gc_sanity_stackreg_check(saveregs,fromheap,(int *) sp, (int *)stack->top); */
      if (SHOW_HEAPS)
	show_heap("FINAL FROM",fromheap->bottom,fromheap->alloc_start,fromheap->top);
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



/* ------------------------------ Interface Routines -------------------- */
ptr_t alloc_bigintarray(int byteLen, int value, int ptag)
{  
  switch (collector_type) 
    {
    case Semispace :    { return alloc_bigintarray_Semi(byteLen,value,ptag); }
    case Generational : { return alloc_bigintarray_Gen (byteLen,value,ptag); }
    case SemispaceParallel :     { return alloc_bigintarray_SemiPara(byteLen,value,ptag); }
    case GenerationalParallel :  { return alloc_bigintarray_GenPara(byteLen,value,ptag); }
    }
}

ptr_t alloc_bigptrarray(int wordlen, ptr_t value, int ptag)
{  
  switch (collector_type) 
    {
    case Semispace :    { return alloc_bigptrarray_Semi(wordlen,value,ptag); }
    case Generational : { return alloc_bigptrarray_Gen (wordlen,value,ptag); }
    case SemispaceParallel :    { return alloc_bigptrarray_SemiPara(wordlen,value,ptag); }
    case GenerationalParallel : { return alloc_bigptrarray_GenPara(wordlen,value,ptag); }
    }
}

ptr_t alloc_bigfloatarray(int loglen, double value, int ptag)
{  
  switch (collector_type) {
    case Semispace :    { return alloc_bigfloatarray_Semi(loglen,value,ptag); }
    case Generational : { return alloc_bigfloatarray_Gen (loglen,value,ptag); }
    case SemispaceParallel :     { return alloc_bigfloatarray_SemiPara(loglen,value,ptag); }
    case GenerationalParallel :     { return alloc_bigfloatarray_GenPara(loglen,value,ptag); }
  }
}

void gc_poll(SysThread_t *sth)
{
  if (collector_type == SemispaceParallel ||
      collector_type == GenerationalParallel)
    gc_poll_SemiPara(sth);
  return;
}


int GCAllocate(SysThread_t *sth, int req)
{  
  /* req = 0 means write buffer was full */
  if (req == 0) {
    if (sth->writelistCursor < sth->writelistEnd)
      return 1;
  } 
  else if (req + sth->alloc <= sth->limit) 
    return 1; 
  switch (collector_type) {
    case Semispace :            { return GCAllocate_Semi(sth,req); }
    case Generational :         { return GCAllocate_Gen(sth,req); }
    case SemispaceParallel :    { return GCAllocate_SemiPara(sth,req); }
    case GenerationalParallel : { return GCAllocate_GenPara(sth,req); }
  }
}

void GC(Thread_t *curThread)
{
  SysThread_t *self = getSysThread();
  mem_t alloc = (mem_t) curThread->saveregs[ALLOCPTR];
  mem_t limit = (mem_t) curThread->saveregs[ALLOCLIMIT];
  mem_t syslimit = self->limit;

  /* Check that we are running on own stack */
  assert((self->stack - (int) (&self)) < 1024); 

  /* Called from GCFromML - need to unmap first */ 
  assert(self == curThread->sysThread);
  assert(self->userThread == curThread);
  assert((limit == syslimit) || (limit == StopHeapLimit));
  assert(alloc <= syslimit);
  ReleaseJob(self);

  /* Write skip tag to end of region */
  if (alloc < syslimit) {
    unsigned int wordsLeft = syslimit - alloc;
    tag_t skiptag = SKIP_TAG | (wordsLeft << SKIPLEN_OFFSET);
    *alloc = skiptag;
  }

  /* If limit pointer set to StopHeapLimit, then not a real GC */
  if (limit == StopHeapLimit) {
    scheduler(self);
    assert(0);
  }

  /* If requestInfo is zero, then the write buffer is full and we must GC.
     Otherwise, try allocating from the current heap */
  if (curThread->requestInfo != 0) {
    if (GCAllocate(self, curThread->requestInfo))
      scheduler(self);
  }

  /* Dispatch to the underlying collector */
  switch (collector_type) 
    {
    case Semispace : 
      GC_Semi(self);
      scheduler(self);
      break; 
    case Generational : 
      GC_Gen(self);
      scheduler(self); 
      break; 
    case SemispaceParallel :     
      GC_SemiPara(self);
      scheduler(self); 
      break; 
    case GenerationalParallel :     
      GC_GenPara(self);
      scheduler(self); 
      break; 
    }
  assert(0);
}

  
void gc_finish()
{
  switch (collector_type) 
    {
    case Semispace : { gc_finish_Semi(); break; }
    case Generational : { gc_finish_Gen(); break; }
    case SemispaceParallel : { gc_finish_SemiPara(); break; }
    case GenerationalParallel : { gc_finish_GenPara(); break; }
    }
}


  
