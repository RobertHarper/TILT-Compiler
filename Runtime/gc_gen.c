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
      GCFromC(thread, tagByteLen + 4, 0);
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
  proc->segUsage.bytesAllocated += tagByteLen;

  /* Allocate object; update stats; initialize */
  obj = region + 1;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : init_parray(obj, spec->elemLen, spec->pointerVal); 
                        pushStack(proc->backObjs, obj);
		        break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Generational collector --------------------- */
void GCRelease_Gen(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->allocStart = proc->allocCursor;
  proc->segUsage.bytesAllocated += alloc;
}


void GCCollect_Gen(Proc_t *proc)
{
  int totalRequest = 0, totalUnused = 0;
  Thread_t *curThread = NULL;
  double liveRatio = 0.0;
  ploc_t rootLoc, globalLoc;
  ptr_t PRObj;

  /* A Major GC is forced if the tenured space is potentially too small.  */
  if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery)) 
    GCType = Major;                    
  else
    GCType = Minor;

  /* Sanity checks */
  assert(NumProc == 1);
  assert(nursery->cursor     <= nursery->top);
  assert(fromSpace->cursor <= fromSpace->top);
  assert(toSpace->cursor   <= toSpace->top);
  /* At this point, the nursery and tenured from-space may have cross-pointers */
  paranoid_check_all(nursery, fromSpace, NULL, NULL, largeSpace);

  /* Get all roots */
  assert(isEmptyStack(proc->rootLocs));
  procChangeState(proc, GCStack);
  ResetJob();
  totalUnused += sizeof(val_t) * (proc->allocLimit - proc->allocCursor);
  while ((curThread = NextJob()) != NULL) {
    /* If negative, requestnfo signifies full writelist */
    if (curThread->requestInfo >= 0)
      totalRequest += curThread->requestInfo;
    thread_root_scan(proc,curThread);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC)  /* Upgrade to major GC */
      GCType = Major;      
  }

  proc->segmentType |= (FlipOn | FlipOff | ((GCType == Minor) ? MinorWork : MajorWork));

  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  procChangeState(proc, GCGlobal);
  if (GCType == Minor) {            
    process_writelist(proc, nursery, fromSpace); /* Get globals and backpointers */
    minor_global_scan(proc);
  }
  else {
    process_writelist(proc, NULL, NULL);  /* Get globals; Backpointers can be ignored on major GC */
    major_global_scan(proc);
  }

  procChangeState(proc, GC);

  /* Perform just a minor GC */
  if (GCType == Minor) {
    /* --- forward the roots and the writelist; then Cheney-scan until no gray objects */
    /* The non-standard use of CopyRange only works for uniprocessors */
    mem_t scanStart = fromSpace->cursor;   /* Record before it gets changed */
    SetCopyRange(&proc->minorRange, proc, fromSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
    proc->minorRange.start = proc->minorRange.cursor = fromSpace->cursor;
    proc->minorRange.stop = fromSpace->top;
    while (rootLoc = (ploc_t) popStack(proc->rootLocs)) 
      locCopy1_noSpaceCheck(proc, rootLoc, &proc->minorRange, nursery);
    while (PRObj = popStack(proc->backObjs)) {
      /* Not transferScanObj_* since this object is a primaryReplica.
         Since this is a stop-copy collector, we can use _locCopy_ immediately */
      scanObj_locCopy1_noSpaceCheck(proc, PRObj, &proc->minorRange, nursery);
    }
    assert(primaryGlobalOffset == 0);
    while (globalLoc = (ploc_t) popStack(proc->globalLocs)) 
      locCopy1_noSpaceCheck(proc, globalLoc, &proc->minorRange, nursery);
    scanUntil_locCopy1_noSpaceCheck(proc, scanStart, &proc->minorRange, nursery);
    fromSpace->cursor = proc->minorRange.cursor;
    proc->minorRange.stop = proc->minorRange.cursor;
    ClearCopyRange(&proc->minorRange);
    paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
    liveRatio = (double) (bytesCopied(&proc->cycleUsage) + bytesCopied(&proc->segUsage)) / 
                (double) (nursery->cursor - nursery->bottom); 
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
    while (rootLoc = (ploc_t) popStack(proc->rootLocs)) 
      locCopy2L_noSpaceCheck(proc, rootLoc, &proc->majorRange, nursery, fromSpace, largeSpace);
    resetStack(proc->backObjs);
    assert(primaryGlobalOffset == 0);
    while (globalLoc = (ploc_t) popStack(proc->globalLocs)) 
      locCopy2L_noSpaceCheck(proc, globalLoc, &proc->majorRange, nursery, fromSpace, largeSpace);
    scanUntil_locCopy2L_noSpaceCheck(proc,scanStart, &proc->majorRange,
				    nursery, fromSpace, largeSpace);
    assert(proc->majorRange.cursor < toSpace->top);
    toSpace->cursor = proc->majorRange.cursor;
    proc->majorRange.stop = proc->majorRange.cursor;
    ClearCopyRange(&proc->majorRange);
    gc_large_endCollect();
    paranoid_check_all(nursery, fromSpace, toSpace, NULL, largeSpace);

    /* Resize the tenured toSpace. Discard fromSpace. Flip Spaces. */
    liveRatio = HeapAdjust2(totalRequest, totalUnused, 0, 0.0, nursery, fromSpace, toSpace);
    add_statistic(&proc->majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace,0,1);
    typed_swap(Heap_t *, fromSpace, toSpace);
  }
  Heap_Reset(nursery);

  /* Update sole thread's allocation pointers and root lists */
  if (GCType == Minor)
    minor_global_promote(proc);
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

void GC_Gen(Proc_t *proc, Thread_t *th)
{
  assert(proc->userThread == NULL);
  assert(th->proc == NULL);
  /* First time */
  if (proc->allocLimit == StartHeapLimit) {
    proc->allocStart = nursery->bottom;
    proc->allocCursor = nursery->bottom;
    proc->allocLimit = nursery->top;
    nursery->cursor = nursery->top;
  }
  /* Check for forced Major GC's */
  if (th->request != MajorGCRequestFromC &&
      GCSatisfiable(proc,th))
    return;
  GCCollect_Gen(proc);
  assert(GCSatisfiable(proc,th));
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


