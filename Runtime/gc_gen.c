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
                        SetPush(&proc->work.backObjs, obj);
		        break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}

/* --------------------- Generational collector --------------------- */
void GCRelease_Gen(Proc_t *proc)
{
  proc->allocStart = proc->allocCursor;
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
  if (GCType == Minor) {            
    minor_global_scan(proc);
    process_writelist(proc, nursery, fromSpace); /* Get globals and backpointers */
  }
  else {
    major_global_scan(proc);
    process_writelist(proc, NULL, NULL);  /* Get globals; Backpointers can be ignored on major GC */
  }

  procChangeState(proc, GCWork, 304);

  /* Perform just a minor GC */
  if (GCType == Minor) {
    /* --- forward the roots and the writelist; then Cheney-scan until no gray objects */
    mem_t scanStart = fromSpace->cursor;   /* Record before it gets changed */
    SetCopyRange(&proc->copyRange, proc, fromSpace, NULL);
    AllocEntireCopyRange(&proc->copyRange);  /* no-space chech hack - only for uniprocessor*/
    while (rootLoc = (ploc_t) SetPop(&proc->work.roots))
      locCopy1_noSpaceCheck(proc, rootLoc, nursery);
    while (PRObj = SetPop(&proc->work.backObjs)) {
      /* Not transferScanObj_* since this object is a primaryReplica.
         Since this is a stop-copy collector, we can use _locCopy_ immediately */
      scanObj_locCopy1_noSpaceCheck(proc, PRObj, nursery);
    }
    assert(primaryGlobalOffset == 0);
    while (globalLoc = (ploc_t) SetPop(&proc->work.globals)) 
      locCopy1_noSpaceCheck(proc, globalLoc, nursery);
    scanUntil_locCopy1_noSpaceCheck(proc, scanStart, nursery);
    ReturnCopyRange(&proc->copyRange);    /* no-space hack - uniprocessor only */
    ClearCopyRange(&proc->copyRange);
    paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
    liveRatio = (double) (bytesCopied(&proc->cycleUsage) + bytesCopied(&proc->segUsage)) / 
                (double) (Heap_GetUsed(nursery));
    add_statistic(&minorSurvivalStatistic, liveRatio);
  }
  else if (GCType == Major) {
    mem_t scanStart = toSpace->bottom;
    int toSpaceSize = Heap_GetMaximumSize(toSpace);
    int maxLive = Heap_GetUsed(fromSpace) + Heap_GetUsed(nursery);

    if (maxLive >= toSpaceSize) {
      printf("WARNING: GC failure possible since maxPossibleLive = %d > toSpaceSize = %d\n",
	     maxLive, toSpaceSize);
      Heap_Resize(toSpace, toSpaceSize, 1);
    }    
    else {
      Heap_Resize(toSpace, maxLive, 1);
    }

    SetCopyRange(&proc->copyRange, proc, toSpace, NULL);
    AllocEntireCopyRange(&proc->copyRange);  /* no-space chech hack - only for uniprocessor*/

    /* do the normal roots; backpointers can be skipped on a major GC;
       then the usual Cheney scan followed by sweeping the large-object region */
    gc_large_startCollect();
    while (rootLoc = (ploc_t) SetPop(&proc->work.roots)) 
      locCopy2L_noSpaceCheck(proc, rootLoc, nursery, fromSpace, largeSpace);
    SetReset(&proc->work.backObjs);
    assert(primaryGlobalOffset == 0);
    while (globalLoc = (ploc_t) SetPop(&proc->work.globals)) 
      locCopy2L_noSpaceCheck(proc, globalLoc, nursery, fromSpace, largeSpace);
    scanUntil_locCopy2L_noSpaceCheck(proc,scanStart, 
				    nursery, fromSpace, largeSpace);
    ReturnCopyRange(&proc->copyRange);    /* no-space hack - uniprocessor only */
    ClearCopyRange(&proc->copyRange);
    gc_large_endCollect();
    paranoid_check_all(nursery, fromSpace, toSpace, NULL, largeSpace);

    /* Resize the tenured toSpace. Discard fromSpace. Flip Spaces. */
    liveRatio = HeapAdjust2(totalRequest, totalUnused, 0, CollectionRate, 0, nursery, fromSpace, toSpace);
    add_statistic(&majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace,0,1);
    typed_swap(Heap_t *, fromSpace, toSpace);
  }
  Heap_Reset(nursery);

  /* Update sole thread's allocation pointers and root lists */
  if (GCType == Minor)
    minor_global_promote(proc);
  ResetAllocation(proc, nursery);
  assert(proc->writelistCursor == proc->writelistStart);

  /* stop timer and update counts */
  if (GCType == Major) 
    NumMajorGC++;
  NumGC++;

}

void GC_Gen(Proc_t *proc, Thread_t *th)
{
  /* First time */
  if (proc->allocLimit == StartHeapLimit) 
    ResetAllocation(proc, nursery);
  if (2 * SetLength(&proc->work.roots) < SetFullSize(&proc->work.roots))
    process_writelist(proc,nursery,fromSpace);
  /* Check for forced Major GC's */
  if (th->request != MajorGCRequestFromC &&
      GCSatisfiable(proc,th))
    return;
  GCCollect_Gen(proc);
  assert(GCSatisfiable(proc,th));
}

void GCInit_Gen(void)
{
  int cache_size = GetBcacheSize();

  GCInit_Help(1024, 128 * 1024, 0.2, 0.8, 512, 50 * 1024);   
  init_int(&NurseryByte, (int)(0.85 * cache_size));
  assert(MinHeapByte >= 1.2*NurseryByte);
  nursery = Heap_Alloc(NurseryByte, NurseryByte);
  gc_large_init();
}


