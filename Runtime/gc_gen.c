/* Not thread-safe */
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "gc_large.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "platform.h"
#include "client.h"
#include "show.h"



mem_t AllocBigArray_Gen(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj;
  int tagByteLen = spec->byteLen + 4;
  Align_t align = (spec->type == DoubleField) ? OddWordAlign : NoWordAlign; /* Since there is one tag word */

  /* Allocate region - use tenured heap if object has pointers */
  if (spec->type == PointerField) {
    region = AllocFromHeap(fromSpace, thread, tagByteLen, align);
    if (region == NULL) {
      GCFromC(thread, tagByteLen + 4, 1);
      region = AllocFromHeap(fromSpace, thread, tagByteLen, align);
    }
  }
  else {
    region = gc_large_alloc(proc, tagByteLen, align);
    if (region == NULL) {
      GCFromC(thread, 4, 1);       /* not bytelen since object is not from tenured space */
      region = gc_large_alloc(proc, tagByteLen, align);
    }
  }
  assert(region != NULL);

  /* Allocate object; update stats; initialize */
  obj = region + 1;
  proc->majorUsage.bytesAllocated += tagByteLen;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
  case PointerField : init_parray(obj, spec->elemLen, spec->pointerVal); 
                      pushStack(&proc->rootVals, spec->pointerVal);
                      pushStack(proc->primaryReplicaObjFlips, obj);
		      break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Generational collector --------------------- */
void GCRelease_Gen(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->minorUsage.bytesAllocated += alloc;
}

int GCTry_Gen(Proc_t *proc, Thread_t *th)
{
  assert(proc->userThread == NULL);
  assert(th->proc == NULL);
  if (proc->allocLimit == StartHeapLimit) {
    proc->allocStart = nursery->bottom;
    proc->allocCursor = nursery->bottom;
    proc->allocLimit = nursery->top;
    nursery->cursor = nursery->top;
  }
  if (th->requestInfo > 0) {
    unsigned int bytesAvailable = (val_t) proc->allocLimit - 
				  (val_t) proc->allocCursor;
    return (th->requestInfo <= bytesAvailable);
  }
  else if (th->requestInfo < 0) {
    unsigned int bytesAvailable = sizeof(val_t) * (proc->writelistEnd - proc->writelistCursor);
    return ((-th->requestInfo) <= bytesAvailable);
  }
  else 
    assert(0);
  return 0;
}

void GCStop_Gen(Proc_t *proc)
{
  int req_size = 0;
  Thread_t *curThread = NULL;
  double liveRatio = 0.0;

  /* A Major GC is forced if the tenured space is potentially too small.  */
  if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery)) 
    GCType = Major;                    
  else
    GCType = Minor;

  /* Sanity checks */
  assert(NumProc == 1);
  assert((0 <= req_size) && (req_size < pagesize));
  assert(nursery->cursor     <= nursery->top);
  assert(fromSpace->cursor <= fromSpace->top);
  assert(toSpace->cursor   <= toSpace->top);
  /* At this point, the nursery and tenured from-space may have cross-pointers */
  paranoid_check_all(nursery, fromSpace, NULL, NULL, largeSpace);

  /* Get all roots */
  resetStack(proc->roots);
  procChangeState(proc, GCStack);
  ResetJob();
  while ((curThread = NextJob()) != NULL) {
    /* If negative, requestnfo signifies full writelist */
    if (curThread->requestInfo >= 0)
      req_size += curThread->requestInfo;
    local_root_scan(proc,curThread);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC)  /* Upgrade to major GC */
      GCType = Major;      
  }

  proc->gcSegment1 = (GCType == Minor) ? MinorWork : MajorWork;
  proc->gcSegment2 = FlipBoth;

  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  procChangeState(proc, GCGlobal);
  if (GCType == Minor) {            /* Roots from globals and back pointers */
    minor_global_scan(proc);
    add_writelist_to_rootlist(proc, &nursery->range, &fromSpace->range);
  }
  else {
    major_global_scan(proc);
    discard_writelist(proc);      /* Write list can be ignored on major GC */
  }

  procChangeState(proc, GC);
  if (GCType == Major)
    proc->gcSegment1 = MajorWork;

  /* Perform just a minor GC */
  if (GCType == Minor) {
    /* --- forward the roots and the writelist; then Cheney-scan until no gray objects */
    /* The non-standard use of CopyRange only works for uniprocessors */
    mem_t scanStart = fromSpace->cursor;   /* Record before it gets changed */
    SetCopyRange(&proc->minorRange, proc, fromSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
    proc->minorRange.start = proc->minorRange.cursor = fromSpace->cursor;
    proc->minorRange.stop = fromSpace->top;
    while (!isEmptyStack(proc->roots)) 
      locCopy1_noSpaceCheck(proc, (ploc_t) popStack(proc->roots), &proc->minorRange, &nursery->range);
    while (!isEmptyStack(&proc->rootVals)) 
      copy1_noSpaceCheck(proc, (ptr_t) popStack(&proc->rootVals), &proc->minorRange, &nursery->range);
    while (!isEmptyStack(proc->primaryReplicaObjFlips)) {
      /* Not transferScanObj_* since this object is a primaryReplica.
	 Since this is a stop-copy collector, we can use _locCopy_ immediately */
      ptr_t obj = popStack(proc->primaryReplicaObjFlips);
      scanObj_locCopy1_noSpaceCheck(proc, obj, &proc->minorRange, &nursery->range);
    }
    scanUntil_locCopy1_noSpaceCheck(proc, scanStart, &proc->minorRange, &nursery->range);
    fromSpace->cursor = proc->minorRange.cursor;
    proc->minorRange.stop = proc->minorRange.cursor;
    ClearCopyRange(&proc->minorRange);
    paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
    liveRatio = (double) (fromSpace->cursor - fromSpace->prevCursor) / (double) (nursery->cursor - nursery->bottom); 
    fromSpace->prevCursor = fromSpace->cursor;
    add_statistic(&proc->minorSurvivalStatistic, liveRatio);
  }
  else if (GCType == Major) {
    mem_t scanStart = toSpace->bottom;
    int toSpaceSize = Heap_GetMaximumSize(toSpace);
    int maxLive = (sizeof (val_t)) * (fromSpace->cursor - fromSpace->bottom) +
                  (sizeof (val_t)) * (nursery->top - nursery->bottom);

    if (maxLive >= toSpaceSize) {
      printf("WARNING: GC failure possible since maxPossibleLive = %d > toSpaceSize = %d\n",
	     maxLive, toSpaceSize);
      Heap_Resize(toSpace, toSpaceSize, 1);
    }    
    else {
      Heap_Resize(toSpace, maxLive, 1);
    }

    /* The non-standard use of CopyRange only works for uniprocessors */
    SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
    proc->majorRange.start = proc->majorRange.cursor = toSpace->cursor;
    proc->majorRange.stop = toSpace->top;

    /* do the normal roots; backpointers can be skipped on a major GC;
       then the usual Cheney scan followed by sweeping the large-object region */
    gc_large_startCollect();
    while (!isEmptyStack(proc->roots)) 
      locCopy2L_noSpaceCheck(proc,(ploc_t) popStack(proc->roots), &proc->majorRange,
			    &nursery->range, &fromSpace->range, &largeSpace->range);
    resetStack(proc->primaryReplicaObjFlips);
    scanUntil_locCopy2L_noSpaceCheck(proc,scanStart, &proc->majorRange,
				    &nursery->range, &fromSpace->range, &largeSpace->range);
    assert(proc->majorRange.cursor < toSpace->top);
    toSpace->cursor = proc->majorRange.cursor;
    proc->majorRange.stop = proc->majorRange.cursor;
    ClearCopyRange(&proc->majorRange);
    gc_large_endCollect();
    paranoid_check_all(nursery, fromSpace, toSpace, NULL, largeSpace);

    /* Resize the tenured toSpace. Discard fromSpace. Flip Spaces. */
    liveRatio = HeapAdjust2(req_size, nursery, fromSpace, toSpace);
    add_statistic(&proc->majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace,0,1);
    typed_swap(Heap_t *, fromSpace, toSpace);
  }
  Heap_Reset(nursery);

  /* Update sole thread's allocation pointers and root lists */
  if (GCType == Minor)
    minor_global_promote();
  proc->allocStart = nursery->bottom;
  proc->allocCursor = nursery->bottom;
  proc->allocLimit = nursery->top;
  nursery->cursor = nursery->top;
  assert(proc->writelistCursor == proc->writelistStart);

  /* stop timer and update counts */
  if (GCType == Major) 
    NumMajorGC++;
  NumGC++;
}


void GCInit_Gen() 
{
  int cache_size = GetBcacheSize();

  init_int(&YoungHeapByte, (int)(0.85 * cache_size));
  init_int(&MaxHeap, 128 * 1024);
  init_int(&MinHeap, 1024);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  init_double(&MinRatio, 0.2);
  init_double(&MaxRatio, 0.8);
  init_int(&MinRatioSize, 512);
  init_int(&MaxRatioSize, 50 * 1024);
  assert(MinHeap >= 1.2*(YoungHeapByte / 1024));

  nursery = Heap_Alloc(YoungHeapByte, YoungHeapByte);
  fromSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
  gc_large_init();
}


