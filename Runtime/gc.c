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
int collectDiag = 0;
int timeDiag = 0;
int debug = 0;
int collector_type = Generational;
int SHOW_GCDEBUG_FORWARD = 0;
int SHOW_GCERROR   = 1;
int SHOW_GCSTATS   = 0;
int SHOW_GCDEBUG   = 0;
int SHOW_HEAPS     = 0;
int SHOW_GLOBALS   = 0;
int SHOW_GCFORWARD = 0;
double pauseWarningThreshold = -1.0;
double warnUtil = 0.0;
int warnThreshold = 0;
int doCopyCopySync = 1;
int noSharing = 0, noWorkTrack = 0;
int relaxed = 0;
int addOldStackletOnUnderflow = 0;

int cacheSize = 4, fetchSize = 4, cacheSize2 = 0; /* in pages */

volatile GCType_t GCType = Minor;
volatile GCStatus_t GCStatus = GCOff;

Statistic_t        heapSizeStatistic;    
Statistic_t        minorSurvivalStatistic;
Statistic_t        majorSurvivalStatistic;

Heap_t *fromSpace = NULL, *toSpace = NULL;
Heap_t *nursery = NULL, *tenuredFrom = NULL, *tenuredTo = NULL;
SharedStack_t *workStack = NULL;
Barriers_t *barriers = NULL;

int NumGC = 0;
int NumMajorGC = 0;
int forceMirrorArray = 0, mirrorGlobal = 0, mirrorArray = 0;
int primaryGlobalOffset = 0, replicaGlobalOffset = sizeof(val_t);
int primaryArrayOffset = 0, replicaArrayOffset = sizeof(val_t);

extern int module_count;
extern val_t GLOBALS_BEGIN_VAL;
extern val_t GLOBALS_END_VAL;
extern void PopStackletFromML(void);

int minAllocRegion = 256;         /* Minimum allocation region size in bytes.  Mutator will be resumed with at least this 
				     many bytes even if allocation request is less. */
int NurseryByte = 0, MaxHeapByte = 0, MinHeapByte = 0;
double MinRatio = 0.0, MaxRatio = 0.0;
int MinRatioSize = 0,  MaxRatioSize = 0;
int minOffRequest, maxOffRequest, minOnRequest;
int copyPageSize;   /* Collector allocates this much memory from a shared heap and then satisfies the normal requests from this local pool. */
int copyCheckSize;  
int copyChunkSize;  /* If a large amount (>= copyChunkSize) is needed by the collector and is not satisfiable from the local pool,
		       then this amount is directly allocated from the shared heap, leaving the local pool alone. A small
		       request that is not satisfiable from the local pool will cause the a new local pool to be allocated.
		       Typically, copyChunkSize = 1K and copyPageSize = 8K which ensures that we waste no more than 1/8
		       of the memory.
		    */

int threadFetchSize = 1;
int globalLocFetchSize = 1000;
int rootLocFetchSize = 1000;
int grayRegionFetchSize = 2;
int objFetchSize = 100;
int segFetchSize = 2;             
int backObjFetchSize = 2;
int backLocFetchSize = 100;
/* Work is per-byte and so each field can be up to 4.0 */
double objCopyWeight = 2.5;      /* Corresponds to tag */
double objScanWeight = 1.5;
double fieldCopyWeight = 1.0;    /* Corresponds to fields - Each field is copied or scanend but not both. */
double fieldScanWeight = 3.0;
double ptrFieldScanWeight = 3.0;
double rootWeight = 6.0;
double globalWeight = 6.0;
double stackSlotWeight = 10.0;
double pageWeight = 100.0;
int arraySegmentSize = 0;         /* If zero, not splitting arrays into segments.
				     If non-zero, must be greater than the compiler's maxByteRequest */
int localWorkSize = 4096;
int usageCount = 50;
int ordering = DefaultOrder;
int forceSpaceCheck = 0;
int grayAsReplica = 0;

/* Agressive: Use the Off -> On -> Commit protocol.
   Conservative: Use Off -> Commit protocol. */
int doAgressive = 1; 
int doMinorAgressive = 0; 
double targetUtil = 1.0;

double CollectionRate = 2.0;   /* Ratio of coll rate to alloc rate */

static void (*GCFun)(Proc_t *, Thread_t *) = NULL;
static void (*GCReleaseFun)(Proc_t *) = NULL;
static void (*GCPollFun)(Proc_t *) = NULL;

/* Compute reduced heap size by reserving area and rounding down to nearest page */
double computeReserve(double rate, int phases)
{
  assert(rate > 1.0);
  if (phases == 0)  /* not concurrent */
    return 0.0;
  else if (phases == 1) /* concurrent */
    return rate / (rate + 1.0);
  else if (phases == 2) /* concurrent - non-committing, committing */
    return (rate + 1.0) / (rate * rate + rate + 1.0);
}

int expandedToReduced(int size, double rate, int phases)
{
  if (phases == 0) 
    return size;
  size = RoundUp(size, minOnRequest);
  size = (int) size * (1.0 - computeReserve(rate, phases));
  size = size - (2 * NumProc) * minOnRequest;
  size = RoundDown(size, minOnRequest);
  return size;
}

int reducedToExpanded(int size, double rate, int phases)
{
  if (phases == 0) 
    return size;
  size = RoundUp(size, minOnRequest);
  size = size + (2 * NumProc) * minOnRequest;
  size = (int) size / (1.0 - computeReserve(rate, phases));
  size = RoundDown(size, minOnRequest);
  return size;
}

/* Compute the new (reduced) heap size given the liveness ratio and amount of live data */
long ComputeHeapSize(long live, double curRatio, double rate, int phases)
{
  double rawWhere = (live - (1024 * MinRatioSize)) / (1024.0 * (MaxRatioSize - MinRatioSize));
  double where = (rawWhere > 1.0) ? 1.0 : ((rawWhere < 0.0) ? 0.0 : rawWhere);
  double newratio = MinRatio + where * (MaxRatio - MinRatio);
  long newReducedSize = RoundUp(live / newratio, 1024);
  long newExpandedSize = reducedToExpanded(newExpandedSize, rate, phases);
  /* maxReducedSize and minReducedSize are not reduced if relaxed is true or if phases is already zero */
  long maxReducedSize = expandedToReduced(MaxHeapByte, rate, relaxed ? 0 : phases);
  long minReducedSize = expandedToReduced(MinHeapByte, rate, relaxed ? 0 : phases);

  if (live > maxReducedSize) {
    fprintf(stderr,"GC error: Amount of live data (%d) exceeds maxiumum heap size (%d)\n", live, maxReducedSize);
    assert(0);
  }
  if (newExpandedSize > MaxHeapByte) {
    double constrainedRatio = ((double)live) / maxReducedSize;
    if (collectDiag >= 1 || constrainedRatio > 0.95)
      printf("GC warning: There is %d kb of live data.  The desired new heap size is %d kb but is downwardly constrained to %d kb.\n",
	     live / 1024, newReducedSize / 1024, maxReducedSize / 1024);
    if (constrainedRatio >= 1.00)
      printf("GC warning: New liveness ratio is too high %lf.\n", constrainedRatio);
    else if (constrainedRatio > 0.90)
      printf("GC warning: New liveness ratio is dangerously high %lf.\n", constrainedRatio);
    newReducedSize = maxReducedSize;
    newExpandedSize = MaxHeapByte;
  }
  if (newExpandedSize < MinHeapByte) {
    if (collectDiag >= 1)
      printf("GC warning: There is %d kb of live data.  The desired new heap size is %d kb but is upwardly constrained to %d kb.\n",
	     live / 1024, newReducedSize / 1024, minReducedSize / 1024);
    newReducedSize = minReducedSize;
    newExpandedSize = MinHeapByte;
  }
  assert(newExpandedSize >= MinHeapByte);
  assert(newExpandedSize <= MaxHeapByte);
  assert(newReducedSize >= live);
  return newReducedSize;
}

void HeapAdjust(int request, int unused, int withhold, double rate, int phases, Heap_t **froms, Heap_t *to)
{
  long copied = 0, occupied = -unused, live = 0, newSize = 0;
  double liveRatio = 0.0;

  add_statistic(&heapSizeStatistic, Heap_GetUsed(fromSpace) / 1024);

  assert(request >= 0);
  while (*froms != NULL) {
    Heap_t *cur = *(froms++);
    if (cur != NULL)
      occupied += (sizeof (val_t)) * (cur->cursor - cur->bottom);
  }
  copied = (sizeof (val_t)) * (to->cursor - to->bottom);
  assert(occupied >= copied);
  assert(copied >= withhold);
  live = copied + request;
  assert(live >= withhold);
  liveRatio = (double) (live - withhold) / (double) (occupied - withhold);
  add_statistic(&majorSurvivalStatistic, liveRatio);
  newSize = ComputeHeapSize(live, liveRatio, rate, phases);
  Heap_Resize(to, newSize, 0);
  if (newSize - copied < request) {
    printf("Error: newSize - copied < request\n");
    printf("       %d - %d <= %d\n\n",
	   newSize, copied, request);
    assert(0);
  }
  if (collectDiag >= 1) {
    printf("---- GC %d (%d): ", NumGC, NumMajorGC);
    printf("req = %3d    live = %7d    withhold = %7d    oldHeap = %8d(%.3lf)   ->   newHeap = %8d(%.3lf)\n", 
	   request, live, withhold, occupied, liveRatio, newSize, ((double)live)/newSize);
  }
}

void HeapAdjust1(int request, int unused, int withhold, double rate, int phases, Heap_t *from1, Heap_t *to)
{
  Heap_t *froms[2];
  froms[0] = from1;
  froms[1] = NULL;
  HeapAdjust(request, unused, withhold, rate, phases, froms, to);
}

void HeapAdjust2(int request, int unused, int withhold,  double reserve, int phases, Heap_t *from1, Heap_t *from2, Heap_t *to)
{
  Heap_t *froms[3];
  froms[0] = from1;
  froms[1] = from2;
  froms[2] = NULL;
  HeapAdjust(request, unused, withhold, reserve, phases, froms, to);
}

void GCInit_Help(int defaultMinHeap, int defaultMaxHeap, 
		 double defaultMinRatio, double defaultMaxRatio, 
		 int defaultMinRatioSize, int defaultMaxRatioSize)
{
  init_int(&MinHeapByte, 1024 * defaultMinHeap);
  init_int(&MaxHeapByte, 1024 * defaultMaxHeap);
  MinHeapByte = Min(MinHeapByte, MaxHeapByte);
  init_double(&MinRatio, defaultMinRatio);
  init_double(&MaxRatio, defaultMaxRatio);
  init_int(&MinRatioSize, defaultMinRatioSize);
  init_int(&MaxRatioSize, defaultMaxRatioSize);
  /*
  fromSpace = Heap_Alloc(MinHeapByte, MaxHeapByte);
  toSpace = Heap_Alloc(MinHeapByte, MaxHeapByte);  
  */
  fromSpace = Heap_Alloc(MinHeapByte, 1024 * defaultMaxHeap);
  toSpace = Heap_Alloc(MinHeapByte, 1024 * defaultMaxHeap);
  Heap_Resize(fromSpace, (MinHeapByte + MaxHeapByte) / 2, 1);
  Heap_Resize(toSpace, (MinHeapByte + MaxHeapByte) / 2, 1);
}

void GCInit(void)
{
  init_int(&minOffRequest, 1 * pagesize);
  init_int(&maxOffRequest, 8 * pagesize);
  init_int(&minOnRequest, 1 * pagesize);
  init_int(&copyPageSize, pagesize / 2);
  init_int(&copyCheckSize, pagesize / 2);
  init_int(&copyChunkSize, 256);
  minOffRequest = RoundUp(minOffRequest, pagesize);
  minOnRequest = RoundUp(minOnRequest, pagesize);

  reset_statistic(&minorSurvivalStatistic);
  reset_statistic(&heapSizeStatistic);
  reset_statistic(&majorSurvivalStatistic);

  switch (collector_type) {
  case Semispace:
    GCFun = GC_Semi;
    GCReleaseFun = GCRelease_Semi;
    GCPollFun = NULL;
    GCInit_Semi();
    break;
  case Generational:
    GCFun = GC_Gen;
    GCReleaseFun = GCRelease_Gen;
    GCPollFun = NULL;
    GCInit_Gen();
    break;
  case SemispaceParallel:
    GCFun = GC_SemiPara;
    GCReleaseFun = GCRelease_SemiPara;
    GCPollFun = GCPoll_SemiPara;
    GCInit_SemiPara();
    break;
  case GenerationalParallel:
    GCFun = GC_GenPara;
    GCReleaseFun = GCRelease_GenPara;
    GCPollFun = GCPoll_GenPara;
    GCInit_GenPara();
    break;
  case SemispaceConcurrent:
    GCFun = GC_SemiConc;
    GCReleaseFun = GCRelease_SemiConc;
    GCPollFun = GCPoll_SemiConc;
    GCInit_SemiConc();
    break;
  case GenerationalConcurrent:
    GCFun = GC_GenConc;
    GCReleaseFun = GCRelease_GenConc;
    GCPollFun = GCPoll_GenConc;
    GCInit_GenConc();
    break;
  default: 
    assert(0);
  }
  if (forceMirrorArray)
    mirrorArray = 1;
}

void AssertMirrorPtrArray(int moduleMirrorArray)
{
  assert(moduleMirrorArray == mirrorArray);
}

void paranoid_check_global(char *label, Heap_t **legalHeaps, Bitmap_t **legalStarts, ShowType_t replicaType)
{
  int count = 0, mi, i;
  char buffer[100];
  /* check globals */
  for (mi=0; mi<module_count; mi++) {
    mem_t start = (mem_t) (&GLOBALS_BEGIN_VAL)[mi];
    mem_t stop = (mem_t) (&GLOBALS_END_VAL)[mi];
    sprintf(buffer, "globals of module %d: %s", mi, label);
    scan_heap(buffer,start,stop,stop, legalHeaps, legalStarts, 
	      SHOW_GLOBALS && (NumGC >= LEAST_GC_TO_CHECK), replicaType, NULL);
  }
}

void paranoid_check_heap_without_start(char *label, Heap_t *curSpace, Heap_t **legalHeaps, Bitmap_t *start, ShowType_t replicaType)
{
  int count = 0, mi, i;
  scan_heap(label,curSpace->bottom, curSpace->cursor, 
	    curSpace->top, legalHeaps, NULL,
	    0, replicaType, start);
}

void paranoid_check_heap_with_start(char *label, Heap_t *curSpace, Heap_t **legalHeaps, Bitmap_t **legalStarts, ShowType_t replicaType)
{
  int count = 0, mi, i;
  scan_heap(label,curSpace->bottom, curSpace->cursor, 
	    curSpace->top, legalHeaps, legalStarts,
	    SHOW_HEAPS && (NumGC >= LEAST_GC_TO_CHECK), replicaType, NULL);
}

void paranoid_check_stack(char *label, Thread_t *thread, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
    int count = 0, mi, i;
    volatile unsigned long *saveregs = thread->saveregs;
    StackChain_t *stackChain = (StackChain_t *) thread->stack;
    ptr_t thunk = thread->thunk;

    /* should check start_addr */
    if ((mem_t)saveregs[ALLOCLIMIT] == StopHeapLimit)
      return;
    if (!inHeaps(thunk,legalHeaps,legalStarts) && inSomeHeap(thunk)) {
      printf("TRACE ERROR at GC %d: thread %d's thunk %d is in from-space\n", NumGC, thread->tid, thunk);
      assert(0);
    }
    if (thunk != NULL) /* thunk not started */
      return;

    for (count = 0; count < 32; count++) {
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
	static int newval = 42000;
	if (verbose)
	  printf("TRACE WARNING GC %d: register %d has from space value %d --> changing to %d\n",
		 NumGC,count,data,newval);
	saveregs[count] = newval++;  
      }
      else if (verbose)
	printf("Register %d has okay value %d\n", count, data);
    }

    for (i=0; i<stackChain->cursor; i++) {
      Stacklet_t *stacklet = stackChain->stacklets[i];
      mem_t cursor, top = StackletPrimaryTop(stacklet);
      for (cursor = StackletPrimaryCursor(stacklet); cursor < top; cursor++) {
	val_t data = *cursor;
	if (!inHeaps((ptr_t)data,legalHeaps,legalStarts) && inSomeHeap((ptr_t)data)) {
	  static int newval = 62000;
	  if (verbose)
	    printf("TRACE WARNING GC %d: stack location %d has fromspace value %d --> changing to %d\n",
		 NumGC, cursor,data,newval);
	  *cursor = newval++;  
	}
      }
    }
    
}

void paranoid_check_all(Heap_t *firstPrimary, Heap_t *secondPrimary,
			Heap_t *firstReplica, Heap_t *secondReplica,
			Heap_t *largeSpace)
{
  int beforeGC = firstReplica == NULL;
  char *when = beforeGC ? "Before GC" : "After GC";
  char msg[100];
  Heap_t *legalPrimaryHeaps[4] = {NULL, NULL,NULL, NULL};
  Heap_t *legalReplicaHeaps[4] = {NULL, NULL,NULL, NULL};
  Bitmap_t *legalPrimaryStarts[4] = {NULL, NULL, NULL, NULL};
  Bitmap_t *legalReplicaStarts[4] = {NULL, NULL, NULL, NULL};
  Heap_t **legalCurrentHeaps;
  Bitmap_t **legalCurrentStarts;
  Thread_t *curThread;
  int showReplica1 = (firstReplica != NULL) ? OtherReplica : NoReplica;
  int showReplica2 = (firstReplica != NULL) ? (firstReplica == secondPrimary ? SelfReplica : OtherReplica) : NoReplica;
  int showReplica3 = (firstReplica != NULL) ? SelfReplica : NoReplica;

  if (!paranoid)
    return;
  if (NumGC < checkAtGC)
    return;

  assert(firstPrimary != NULL);
  legalPrimaryHeaps[0] = firstPrimary;
  legalPrimaryHeaps[1] = secondPrimary;
  legalReplicaHeaps[0] = firstReplica;
  legalReplicaHeaps[1] = secondReplica;
  legalPrimaryStarts[0] = firstPrimary->bitmap;
  legalPrimaryStarts[1] = secondPrimary ? secondPrimary->bitmap : NULL;
  legalReplicaStarts[0] = firstReplica ? firstReplica->bitmap : NULL;
  legalReplicaStarts[1] = secondReplica ? secondReplica->bitmap : NULL;
  if (largeSpace) {
    if (legalPrimaryHeaps[1])
      legalPrimaryHeaps[2] = largeSpace;
    else
      legalPrimaryHeaps[1] = largeSpace;
    if (legalReplicaHeaps[1])
      legalReplicaHeaps[2] = largeSpace;
    else
      legalReplicaHeaps[1] = largeSpace;
  }
  sprintf(msg, "%s: first primary heap", when);
  paranoid_check_heap_without_start(msg,firstPrimary,NULL, legalPrimaryStarts[0], showReplica1);
  if (secondPrimary != NULL) {
    sprintf(msg, "%s: second primary heap", when);
    paranoid_check_heap_without_start(msg,secondPrimary,NULL, legalPrimaryStarts[1], showReplica1);
  }
  if (firstReplica != NULL) {
    sprintf(msg, "%s: first replica heap", when);
    paranoid_check_heap_without_start(msg,firstReplica,NULL, legalReplicaStarts[0], showReplica2);
  }
  if (secondReplica != NULL) {
    sprintf(msg, "%s: second replica heap", when);
    paranoid_check_heap_without_start(msg,secondReplica,NULL, legalReplicaStarts[1], showReplica1);
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
  paranoid_check_global(msg, legalCurrentHeaps,legalCurrentStarts, showReplica3);

  sprintf(msg, "%s: first primary heap", when);
  paranoid_check_heap_with_start(msg, firstPrimary, legalPrimaryHeaps, legalPrimaryStarts, showReplica1);
  if (secondPrimary != NULL) {
    if (firstReplica != NULL && secondPrimary != firstReplica) {
      printf("Skipping paranoid check on secondPrimary on major GC\n");
    }
    else {
      sprintf(msg, "%s: second primary heap", when);
      paranoid_check_heap_with_start(msg, secondPrimary, legalPrimaryHeaps, legalPrimaryStarts, showReplica2);
    }
  }
  if (firstReplica != NULL) {
    sprintf(msg, "%s: first replica heap", when);
    paranoid_check_heap_with_start(msg,firstReplica,legalReplicaHeaps, legalReplicaStarts, showReplica2);
  }
  if (secondReplica != NULL) {
    sprintf(msg, "%s: second replica heap", when);
    paranoid_check_heap_with_start(msg,secondReplica,legalReplicaHeaps, legalReplicaStarts, showReplica1);
  }

  if (numErrors) {
    printf("\n\nProgram halted due to %d TRACE ERROR(s).  At most %d shown.\n\n", numErrors, errorsToShow);
    assert(0);
  }
}




/* ------------------------------ Helper Routines -------------------- */

mem_t AllocFromThread(Thread_t *thread, int bytesToAlloc, Align_t align) /* bytesToAlloc does not include alignment */
{
  mem_t alloc = (mem_t) thread->saveregs[ALLOCPTR];
  mem_t limit = (mem_t) thread->saveregs[ALLOCLIMIT]; /* limit might be StopHeapLimit */
  int wordsToAlloc = bytesToAlloc >> 2;
  mem_t region = NULL;
  if (alloc + wordsToAlloc + (align == NoWordAlign ? 0 : 1) <= limit) {
    alloc = AlignMemoryPointer(alloc, align);
    region = alloc;
    alloc += wordsToAlloc;
    thread->saveregs[ALLOCPTR] = (val_t) alloc;
  }
  assert((thread->saveregs[ALLOCLIMIT] == (unsigned long) limit) ||
	 (thread->saveregs[ALLOCPTR] < thread->saveregs[ALLOCLIMIT]));
  return region;
}

mem_t AllocFromHeap(Heap_t *heap, Thread_t *thread, int bytesToAlloc, Align_t align) /* bytesToAlloc does not include alignment */
{
  mem_t start, cursor, limit;
  int padBytes = bytesToAlloc + ((align == NoWordAlign) ? 0 : 4);
  int pagePadBytes = RoundUp(padBytes, pagesize);
  GetHeapArea(fromSpace, pagePadBytes, &start, &cursor, &limit);
  if (start == NULL) 
    return NULL;
  cursor = AlignMemoryPointer(cursor, align);
  PadHeapArea(cursor + bytesToAlloc / sizeof(val_t), limit);
  return cursor;
}

static ptr_t alloc_bigdispatcharray(ArraySpec_t *spec)
{
  ptr_t result = NULL;
  Proc_t *proc = getProc();
  Thread_t *thread = proc->userThread;
  assert(spec->byteLen >= 512);
  if (spec->type == PointerField || spec->type == MirrorPointerField)
    installThreadRoot(thread,&spec->pointerVal);
  switch (collector_type) {
    case Semispace:              result = AllocBigArray_Semi(proc,thread,spec); break;
    case Generational:           result = AllocBigArray_Gen(proc,thread,spec); break;
    case SemispaceParallel:      result = AllocBigArray_SemiPara(proc,thread,spec); break;
    case GenerationalParallel:   result = AllocBigArray_GenPara(proc,thread,spec); break;
    case SemispaceConcurrent:    result = AllocBigArray_SemiConc(proc,thread,spec); break;
    case GenerationalConcurrent: result = AllocBigArray_GenConc(proc,thread,spec); break;
    default: assert(0);
  }
  if (spec->type == PointerField || spec->type == MirrorPointerField)
    uninstallThreadRoot(thread,&spec->pointerVal);
  return result;
}


/* ------------------------------ Interface Routines -------------------- */
ptr_t alloc_bigintarray(int elemLen, int initVal, int ptag)
{  
  ArraySpec_t spec;
  spec.type = IntField;
  spec.elemLen = elemLen;
  spec.byteLen = RoundUp(elemLen, 4);  /* excluding tag */
  spec.intVal = initVal;
  return alloc_bigdispatcharray(&spec);
}

ptr_t alloc_bigptrarray(int elemLen, ptr_t initVal, int ptag)
{  
  ArraySpec_t spec;
  spec.elemLen = elemLen;
  spec.pointerVal = initVal;
  if (mirrorArray) {
    spec.type = MirrorPointerField;
    spec.byteLen = 8 * elemLen;      /* excluding tag */
  }
  else {
    spec.type = PointerField;
    spec.byteLen = 4 * elemLen;      /* excluding tag */
  }
  return alloc_bigdispatcharray(&spec);
}

ptr_t alloc_bigfloatarray(int elemLen, double initVal, int ptag)
{  
  ArraySpec_t spec;
  spec.type = DoubleField;
  spec.elemLen = elemLen;
  spec.byteLen = 8 * elemLen;      /* excluding tag */
  spec.doubleVal = initVal;
  return alloc_bigdispatcharray(&spec);
}

void GCPoll(Proc_t *proc)
{
  if (GCPollFun != NULL)
    (*GCPollFun)(proc);
}


/* Is there enough room in proc to satisfy mapping th onto it?  
   Does not consider overriding factors such as intentionally signalled 
   major GC or flipping collector on and off.
   Area is discarded if too small by considering the request unsatisfied.
 */
int GCSatisfiable(Proc_t *proc, Thread_t *th)
{
  /* requestInfo < 0 means that many bytes in write buffer is requested 
     requestInfo > 0 means that many bytes of allocation is requested 
     requestInfo == 0 is illegal
  */
  int allocSpaceLeft = (val_t) proc->allocLimit - (val_t) proc->allocCursor;
  if (th->requestInfo < 0) {
    return ((val_t)proc->writelistCursor - th->requestInfo <= (val_t)proc->writelistEnd);
  } 
  else if (th->requestInfo > 0) {
    return (allocSpaceLeft > minAllocRegion) && (th->requestInfo < allocSpaceLeft);
  }
  else
    assert(0);
}

/* Assign heap to allocation area of a processor - if heap is null, fields set to StartHeapLimit - must be null for multiple processors*/
void ResetAllocation(Proc_t *proc, Heap_t *heap)
{
  if (heap == NULL) {
    proc->allocStart = StartHeapLimit;
    proc->allocCursor = StartHeapLimit;
    proc->allocLimit = StartHeapLimit;
    return;
  }
  proc->allocStart = heap->cursor;
  proc->allocCursor = heap->cursor;
  proc->allocLimit = heap->top;
  heap->cursor = heap->top;
}

void GCFromScheduler(Proc_t *proc, Thread_t *th)
{  
  procChangeState(proc, GC, 1000);
  ((*GCFun)(proc,th));
  assert(GCSatisfiable(proc,th));
}

void GCReleaseThread(Proc_t *proc)
{  
  procChangeState(proc, Scheduler, 1002);
  (*GCReleaseFun)(proc);
}

/* Does not return - goes to scheduler */
void GCFromMutator(Thread_t *curThread)
{
  Proc_t *proc = (Proc_t *) curThread->proc;
  mem_t alloc = (mem_t) curThread->saveregs[ALLOCPTR];
  mem_t limit = (mem_t) curThread->saveregs[ALLOCLIMIT];
  mem_t sysAllocStart = proc->allocStart;
  mem_t sysAllocLimit = proc->allocLimit;

  /* Put registers in stacklet */
  int i;
  Stacklet_t *stacklet = CurrentStacklet(curThread->stack);
  mem_t primaryRegs = (mem_t) &stacklet->bottomBaseRegs[primaryStackletOffset == 0 ? 0 : 32];
  for (i=0; i<32; i++) 
    primaryRegs[i] = curThread->saveregs[i];

  /* Check that we are running on own stack and allocation pointers consistent */
  if (paranoid)
    assert(proc == (getProc())); /* getProc is slow */
  assert(proc->userThread == curThread);
  assert((proc->stack - (int) (&proc)) < 1024) ;
  assert((limit == sysAllocLimit) || (limit == StopHeapLimit));
  assert(alloc <= sysAllocLimit);

  /* ReleaseJob(proc) */
  UpdateJob(proc); /* Update processor's info, GCRelease thread, but don't unmap */
  procChangeState(proc, Scheduler, 1003);
  scheduler(proc);
  assert(0);
}

#if defined(solaris)
int calleeSaveMask = 0;
#elif defined(alpha_osf)
int calleeSaveMask = 0;
#endif

/* maxOffset is non-zero if the caller passed arguments on the stack */
void NewStackletFromMutator(Thread_t *curThread, int maxOffset)
{
  int i;
  mem_t sp = (mem_t) curThread->saveregs[SP];
  mem_t returnToCaller = (mem_t) curThread->saveregs[ASMTMP2];
  mem_t primaryRegs;
#ifdef solaris
  mem_t returnToCallee = (mem_t) curThread->saveregs[LINK];
#else
  mem_t returnToCallee = (mem_t) curThread->saveregs[RA];
#endif
  Stacklet_t *oldStacklet, *newStacklet;
  StackChain_t *stackChain = (StackChain_t *) curThread->stack;

  oldStacklet = EstablishStacklet(stackChain, sp); /* saves sp already */
  oldStacklet->retadd = returnToCaller;
  primaryRegs = (mem_t) &oldStacklet->bottomBaseRegs[primaryStackletOffset == 0 ? 0 : 32];
  for (i=0; i<32; i++) {
    primaryRegs[i] = curThread->saveregs[i];
    if ((1<<i) & calleeSaveMask)
      curThread->saveregs[i] = NULL;
  }
  if (addOldStackletOnUnderflow && GCStatus != GCOff && oldStacklet->state == Inconsistent) {
    extern Stacklet_t *Stacklets;
    /*    printf("GC %d: Adding stacklet %d\n", NumGC, oldStacklet - Stacklets); */
    oldStacklet->state = Pending;
    SetPush(&curThread->proc->work.stacklets, (ptr_t) oldStacklet);
  }
  assert(maxOffset == 0);  /* Not handling overflow arguments yet */
  newStacklet = NewStacklet(stackChain);
  curThread->saveregs[SP] = (val_t) StackletPrimaryCursor(newStacklet);
  curThread->stackLimit = StackletPrimaryBottom(newStacklet);
#ifdef solaris
  curThread->saveregs[LINK] = (val_t) (&PopStackletFromML) - 8;
#else
  curThread->saveregs[RA] = (reg_t) (&PopStackletFromML);
#endif
  Stacklet_KillReplica(newStacklet);
  returnToML(curThread, returnToCallee);
  assert(0);
}

void PopStackletFromMutator(Thread_t *curThread)
{
  int i;
  mem_t sp = (mem_t) curThread->saveregs[SP];
  Stacklet_t *newStacklet = NULL;
  mem_t primaryRegs;

  EstablishStacklet(curThread->stack, sp);
  PopStacklet(curThread->stack);
  newStacklet = CurrentStacklet(curThread->stack);
  /* Even though we saved all registers, we only restore the callee-save ones */
  primaryRegs = (mem_t) &newStacklet->bottomBaseRegs[primaryStackletOffset == 0 ? 0 : 32];
  for (i=0; i<32; i++)
    if ((1<<i) & calleeSaveMask)
      curThread->saveregs[i] = primaryRegs[i];

  curThread->saveregs[SP] = (val_t) StackletPrimaryCursor(newStacklet);
#ifdef solaris
  curThread->saveregs[LINK] = (val_t) newStacklet->retadd; /* Not really necessary */
#else
  curThread->saveregs[RA] = (val_t) newStacklet->retadd; /* Not really necessary */
#endif
  curThread->stackLimit = StackletPrimaryBottom(newStacklet);
  Stacklet_KillReplica(newStacklet);
  returnToML(curThread, newStacklet->retadd);
  assert(0);
}
