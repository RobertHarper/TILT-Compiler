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
int SHOW_GLOBALS   = 0;
int SHOW_GCFORWARD = 0;


enum GCStatus GCStatus = GCOff;
enum GCType GCType = Minor;

Heap_t *fromSpace = NULL, *toSpace = NULL;
Heap_t *nursery = NULL, *tenuredFrom = NULL, *tenuredTo = NULL;
SharedStack_t *workStack = NULL, *majorWorkStack = NULL, *majorRegionWorkStack = NULL;

int NumGC = 0;
int NumMajorGC = 0;

extern int module_count;
extern val_t GLOBALS_BEGIN_VAL;
extern val_t GLOBALS_END_VAL;

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
  copied = (sizeof (val_t)) * (to->cursor - to->bottom);
  live = copied + bytesRequested;
  liveRatio = (double) live / (double) occupied;
  newSize = ComputeHeapSize(occupied, liveRatio);
  Heap_Resize(to, newSize, 0);
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
  switch (collector_type) {
  case Semispace:
    gc_init_Semi();
    break;
  case Generational:
    gc_init_Gen();
    break;
  case SemispaceParallel:
    gc_init_SemiPara();
    break;
  case GenerationalParallel:
    gc_init_GenPara();
    break;
  case SemispaceConcurrent:
    gc_init_SemiConc();
    break;
  case GenerationalConcurrent:
    gc_init_GenConc();
    break;
  default: 
    assert(0);
  }
}

void paranoid_check_global(char *label, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int count = 0, mi, i;
  char buffer[100];
  /* check globals */
  for (mi=0; mi<module_count; mi++) {
    mem_t start = (mem_t) (&GLOBALS_BEGIN_VAL)[mi];
    mem_t stop = (mem_t) (&GLOBALS_END_VAL)[mi];
    sprintf(buffer, "globals of module %d: %s", mi, label);
    scan_heap(buffer,start,stop,stop, legalHeaps, legalStarts, 
	      SHOW_GLOBALS && (NumGC >= LEAST_GC_TO_CHECK), NULL);
  }
}

void paranoid_check_heap_without_start(char *label, Heap_t *curSpace, Heap_t **legalHeaps, Bitmap_t *start)
{
  int count = 0, mi, i;
  scan_heap(label,curSpace->bottom, curSpace->cursor, 
	    curSpace->top, legalHeaps, NULL,
	    0, start);
}

void paranoid_check_heap_with_start(char *label, Heap_t *curSpace, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int count = 0, mi, i;
  scan_heap(label,curSpace->bottom, curSpace->cursor, 
	    curSpace->top, legalHeaps, legalStarts,
	    SHOW_HEAPS && (NumGC >= LEAST_GC_TO_CHECK), NULL);
}

void paranoid_check_stack(char *label, Thread_t *thread, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
    int count = 0, mi, i;
    mem_t cursor;
    unsigned long *saveregs = thread->saveregs;
    mem_t sp = (mem_t) saveregs[SP];
    mem_t stack_top = thread->stackchain->stacks[0]->top;
    /* should check start_addr */
    if ((mem_t)saveregs[ALLOCLIMIT] == StopHeapLimit)
      return;
    for (i=thread->nextThunk; i<thread->numThunk; i++) { /* check thunks */
      ptr_t thunk = thread->thunks[i];
      if (!inHeaps(thunk,legalHeaps,legalStarts) && inSomeHeap(thunk)) {
	printf("TRACE ERROR: thunk %d has from-space value after collection: %d", i, thunk);
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
	else if (!inHeaps((ptr_t)data,legalHeaps,legalStarts) && inSomeHeap((ptr_t)data)) {
	  static int newval = 62000;
	  if (diag)
	    printf("TRACE WARNING GC %d: register %d has from space value %d --> changing to %d\n",
		   NumGC,count,data,newval);
	  saveregs[count] = newval; 
	  newval++;
	}
	else if (verbose)
	  printf("Register %d has okay value %d\n", count, data);
      }

    for (cursor = sp; cursor < stack_top - 16; cursor++) {
      val_t data = *cursor;
      if (!inHeaps((ptr_t)data,legalHeaps,legalStarts) && inSomeHeap((ptr_t)data)) {
	static int newval = 42000;
	if (diag)
	  printf("TRACE WARNING: stack location %d has illegal heap address %d changing to %d\n",
		 cursor,data,newval);
	*cursor = newval; 
	newval++;
      }
    }

}

void paranoid_check_all(Heap_t *firstPrimary, Heap_t *secondPrimary,
			Heap_t *firstReplica, Heap_t *secondReplica)
{
  char *when = (firstReplica == NULL) ? "Before GC" : "After GC";
  char msg[100];
  Heap_t *legalPrimaryHeaps[3] = {NULL, NULL,NULL};
  Heap_t *legalReplicaHeaps[3] = {NULL, NULL,NULL};
  Heap_t **legalCurrentHeaps;
  Bitmap_t *legalPrimaryStarts[3] = {NULL, NULL, NULL};
  Bitmap_t *legalReplicaStarts[3] = {NULL, NULL, NULL};
  Bitmap_t **legalCurrentStarts;
  Thread_t *curThread;

  assert(firstPrimary != NULL);
  legalPrimaryHeaps[0] = firstPrimary;
  legalPrimaryHeaps[1] = secondPrimary;
  legalReplicaHeaps[0] = firstReplica;
  legalReplicaHeaps[1] = secondReplica;
  legalPrimaryStarts[0] = firstPrimary->bitmap;
  legalPrimaryStarts[1] = secondPrimary ? secondPrimary->bitmap : NULL;
  legalReplicaStarts[0] = firstReplica ? firstReplica->bitmap : NULL;
  legalReplicaStarts[1] = secondReplica ? secondReplica->bitmap : NULL;

  sprintf(msg, "%s: first primary heap", when);
  paranoid_check_heap_without_start(msg,firstPrimary,legalPrimaryHeaps, legalPrimaryStarts[0]);
  if (secondPrimary != NULL) {
    sprintf(msg, "%s: second primary heap", when);
    paranoid_check_heap_without_start(msg,secondPrimary,legalPrimaryHeaps, legalPrimaryStarts[1]);
  }
  if (firstReplica != NULL) {
    sprintf(msg, "%s: first replica heap", when);
    paranoid_check_heap_without_start(msg,firstReplica,legalReplicaHeaps, legalReplicaStarts[0]);
  }
  if (secondReplica != NULL) {
    sprintf(msg, "%s: second replica heap", when);
    paranoid_check_heap_without_start(msg,secondReplica,legalReplicaHeaps, legalReplicaStarts[1]);
  }

  if (firstReplica == NULL) {
    legalCurrentHeaps = legalPrimaryHeaps;
    legalCurrentStarts = legalPrimaryStarts;
  }
  else {
    legalCurrentHeaps = legalReplicaHeaps;
    legalCurrentStarts = legalReplicaStarts;
  }

  ResetJob();
  while ((curThread = NextJob()) != NULL) {
    sprintf(msg, "%s: stack %d", when, curThread->tid);
    paranoid_check_stack(msg,curThread,legalCurrentHeaps,legalCurrentStarts);
  }
  ResetJob();
  sprintf(msg, "%s: globals", when);
  paranoid_check_global(msg, legalCurrentHeaps,legalCurrentStarts);
  sprintf(msg, "%s: first primary heap", when);
  paranoid_check_heap_with_start(msg, firstPrimary, legalPrimaryHeaps, legalPrimaryStarts);
  if (secondPrimary != NULL) {
    sprintf(msg, "%s: second primary heap", when);
    paranoid_check_heap_with_start(msg, secondPrimary, legalPrimaryHeaps, legalPrimaryStarts);
  }
  if (firstReplica != NULL) {
    sprintf(msg, "%s: first replica heap", when);
    paranoid_check_heap_with_start(msg,firstReplica,legalReplicaHeaps, legalReplicaStarts);
  }
  if (secondReplica != NULL) {
    sprintf(msg, "%s: second replica heap", when);
    paranoid_check_heap_with_start(msg,secondReplica,legalReplicaHeaps, legalReplicaStarts);
  }

  if (traceError) {
    printf("\n\nProgram halted due to TRACE ERROR(s)\n\n");
    assert(0);
  }
}




/* ------------------------------ Interface Routines -------------------- */
ptr_t alloc_bigintarray(int byteLen, int value, int ptag)
{  
  switch (collector_type) 
    {
    case Semispace:              { return alloc_bigintarray_Semi(byteLen,value,ptag); }
    case Generational:           { return alloc_bigintarray_Gen (byteLen,value,ptag); }
    case SemispaceParallel:      { return alloc_bigintarray_SemiPara(byteLen,value,ptag); }
    case GenerationalParallel:   { return alloc_bigintarray_GenPara(byteLen,value,ptag); }
    case SemispaceConcurrent:    { return alloc_bigintarray_SemiConc(byteLen,value,ptag); }
    case GenerationalConcurrent: { return alloc_bigintarray_GenConc(byteLen,value,ptag); }
    default: assert(0);
    }
}

ptr_t alloc_bigptrarray(int wordlen, ptr_t value, int ptag)
{  
  switch (collector_type) 
    {
    case Semispace:              { return alloc_bigptrarray_Semi(wordlen,value,ptag); }
    case Generational:           { return alloc_bigptrarray_Gen (wordlen,value,ptag); }
    case SemispaceParallel:      { return alloc_bigptrarray_SemiPara(wordlen,value,ptag); }
    case GenerationalParallel:   { return alloc_bigptrarray_GenPara(wordlen,value,ptag); }
    case SemispaceConcurrent:    { return alloc_bigptrarray_SemiConc(wordlen,value,ptag); }
    case GenerationalConcurrent: { return alloc_bigptrarray_GenConc(wordlen,value,ptag); }
    default: assert(0);
    }
}

ptr_t alloc_bigfloatarray(int loglen, double value, int ptag)
{  
  switch (collector_type) {
    case Semispace:               { return alloc_bigfloatarray_Semi(loglen,value,ptag); }
    case Generational:            { return alloc_bigfloatarray_Gen (loglen,value,ptag); }
    case SemispaceParallel:       { return alloc_bigfloatarray_SemiPara(loglen,value,ptag); }
    case GenerationalParallel:    { return alloc_bigfloatarray_GenPara(loglen,value,ptag); }
    case SemispaceConcurrent:     { return alloc_bigfloatarray_SemiConc(loglen,value,ptag); }
    case GenerationalConcurrent:  { return alloc_bigfloatarray_GenConc(loglen,value,ptag); }
    default: assert(0);
  }
}

void gc_poll(Proc_t *sth)
{
  switch (collector_type) {
    case Semispace:               return;
    case Generational:            return;
    case SemispaceParallel:       gc_poll_SemiPara(sth); return;
    case GenerationalParallel:    gc_poll_GenPara(sth); return;
    case SemispaceConcurrent:     gc_poll_SemiConc(sth); return;
    case GenerationalConcurrent:  gc_poll_GenConc(sth); return;
    default: assert(0);
  }
  return;
}


/* Is there enough room in sth to satisfy mapping th onto it */
int GCSatisfied(Proc_t *sth, Thread_t *th)
{
  /* requestInfo < 0 means that many butes in write buffer is requested 
     requestInfo > 0 means that many bytes of allocation is requested */
  if (th->requestInfo < 0) {
    if ((val_t)sth->writelistCursor - th->requestInfo <= (val_t)sth->writelistEnd)
      return 1;
  } 
  else if (th->requestInfo > 0) {
    if (th->requestInfo + (val_t) sth->allocCursor <= (val_t) sth->allocLimit) 
      return 1; 
  }
  else 
    assert(0);
  return 0;
}

/* If there is enough allocation/writelist space in sth to satisy th, return 1.
   If not, try to allocate more and return 2 if succeeded.
   If there is still not room, perform a GC and return 3 if the th is satisfied by sth. 
   Otherwise, return 0. */
int GCFromScheduler(Proc_t *sth, Thread_t *th)
{  
  assert(sth->userThread == NULL);
  assert(th->proc == NULL);
  procChangeState(sth, GC);
  if (GCSatisfied(sth,th))
    return 1;
  switch (collector_type) {
    case Semispace:            
      if (GCTry_Semi(sth,th))
	return 2;
      break;
    case Generational:
      if (GCTry_Gen(sth,th))
	return 2;
      break;
    case SemispaceParallel:
      if (GCTry_SemiPara(sth,th))
	return 2;
      break;
    case GenerationalParallel:
      if (GCTry_GenPara(sth,th))
	return 2;
      break;
    case SemispaceConcurrent:
      if (GCTry_SemiConc(sth,th)) {
	return 2;
      }
      break;
    case GenerationalConcurrent:
      if (GCTry_GenConc(sth,th))
	return 2;
      break;
    default: 
      assert(0);
  }
  if (diag) {
    if (th->requestInfo > 0)
      printf("Proc %d: cannot resume user thread %d; need %d, only have %d; proceeding to stop-copy GC\n",
	     sth->stid, th->tid, th->requestInfo, sth->allocLimit - sth->allocCursor);
    else if (th->requestInfo < 0)
      printf("Proc %d: cannot resume user thread %d; need %d bytes from write list; %d available; proceeding to stop-copy GC\n",
	     sth->stid, th->tid, -(th->requestInfo), (sth->writelistEnd - sth->writelistCursor) * sizeof(val_t));
    else 
      assert(0);
  }
  switch (collector_type) {
    case Semispace:            
      GCStop_Semi(sth);
      break;
    case Generational:
      GCStop_Gen(sth);
      break;
    case SemispaceParallel:
      GCStop_SemiPara(sth);
      break;
    case GenerationalParallel:
      GCStop_GenPara(sth);
      break;
    case SemispaceConcurrent:
      assert(0);
    case GenerationalConcurrent:
      assert(0);
    default: 
      assert(0);
  }
  if (GCSatisfied(sth,th))
    return 3;
  return 0;
}

void GCRelease(Proc_t *proc)
{  
  procChangeState(proc, GC);
  switch (collector_type) {
    case Semispace:              GCRelease_Semi(proc); return;
    case Generational:           GCRelease_Gen(proc); return;
    case SemispaceParallel:      GCRelease_SemiPara(proc); return;
    case GenerationalParallel:   GCRelease_GenPara(proc); return;
    case SemispaceConcurrent:    GCRelease_SemiConc(proc); return;
    case GenerationalConcurrent: GCRelease_GenConc(proc); return;
    default: assert(0);
  }
}

/* Does not return - goes to scheduler */
void GCFromMutator(Thread_t *curThread)
{
  Proc_t *self = getProc();
  mem_t alloc = (mem_t) curThread->saveregs[ALLOCPTR];
  mem_t limit = (mem_t) curThread->saveregs[ALLOCLIMIT];
  mem_t sysAllocCursor = self->allocCursor;
  mem_t sysAllocLimit = self->allocLimit;

  /* Check that we are running on own stack and allocation pointers consistent */
  assert(self == curThread->proc);
  assert(self->userThread == curThread);
  assert((self->stack - (int) (&self)) < 1024) ;
  assert((limit == sysAllocLimit) || (limit == StopHeapLimit));
  assert(alloc <= sysAllocLimit);

  /* Write skip tag to indicate the end of region and then release job */
  PadHeapArea(alloc, sysAllocLimit);

  /* For now, always unmap the job and run the scheduler which will invoke GC if necessary */
  ReleaseJob(self);
  scheduler(self);
  assert(0);
}

  



  
