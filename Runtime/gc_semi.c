#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "stack.h"
#include "thread.h"
#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "global.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "show.h"
#include "forward.h"



/* ------------------------ Main collector functions ------------------ */

ptr_t AllocBigArray_Semi(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj;
  int tagByteLen = spec->byteLen + 4;

  /* Allocate the space */
  region = AllocFromThread(thread, tagByteLen, (spec->type == DoubleField) ? OddWordAlign : NoWordAlign);
  if (region == NULL) {
    GCFromC(thread, tagByteLen + 4, 0);
    region = AllocFromThread(thread, tagByteLen, (spec->type == DoubleField) ? OddWordAlign : NoWordAlign);
  }
  assert(region != NULL);
  proc->segUsage.bytesAllocated += tagByteLen;

  /* Allocate object; update stats; initialize */
  obj = region + 1;
  switch (spec->type) {
    case IntField : init_iarray(obj, spec->elemLen, spec->intVal); break;
    case PointerField : init_parray(obj, spec->elemLen, spec->pointerVal); break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}


void GCRelease_Semi(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->allocStart = proc->allocCursor;
  proc->segUsage.bytesAllocated += alloc;
}


static void GCCollect_Semi(Proc_t *proc)
{
  long totalRequested = 0, totalUnused = 0;
  mem_t allocCursor = proc->allocCursor;
  mem_t allocLimit = proc->allocLimit;
  Thread_t *curThread = NULL;
  double liveRatio = 0.0;
  ploc_t globalLoc, rootLoc;
  ptr_t gray;

  /* Check that processor is unmapped, write list is not overflowed, allocation region intact */
  assert(NumProc == 1);
  assert(allocCursor <= allocLimit);
  assert(totalRequested >= 0);
  assert(primaryGlobalOffset == 0);
  assert(SetIsEmpty(&proc->work.roots));
  paranoid_check_all(fromSpace, NULL, NULL, NULL, NULL);

  /* Compute the roots from the stack and register set and globals */
  proc->segmentType |= MajorWork | FlipOn | FlipOff;
  totalUnused += sizeof(val_t) * (proc->allocLimit - proc->allocCursor);
  ResetJob();
  while ((curThread = NextJob()) != NULL) {
    if (curThread->requestInfo >= 0)
      totalRequested += curThread->requestInfo;
    thread_root_scan(proc,curThread);
  }
  major_global_scan(proc);

  /* Get toSpace ready for collection. Forward roots. Process gray objects */
  procChangeState(proc, GCWork, 203);
  Heap_Resize(toSpace, Heap_GetSize(fromSpace), 1);
  SetCopyRange(&proc->copyRange, proc, toSpace, NULL);

  if (!forceSpaceCheck)
    AllocEntireCopyRange(&proc->copyRange);                      /* no spacecheck - uniprocessor only */

  if (ordering == ImplicitOrder) {
    if (!forceSpaceCheck) {
      while (rootLoc = (ploc_t) SetPop(&proc->work.roots))          
	locCopy1_noSpaceCheck(proc, rootLoc, fromSpace);
      while (globalLoc = (ploc_t) SetPop(&proc->work.globals)) 
	locCopy1_noSpaceCheck(proc, (ploc_t) globalLoc, fromSpace);
      scanUntil_locCopy1_noSpaceCheck(proc,toSpace->range.low,fromSpace);
    }
    else {
      while (rootLoc = (ploc_t) SetPop(&proc->work.roots))          
	locCopy1(proc, rootLoc, fromSpace);
      while (globalLoc = (ploc_t) SetPop(&proc->work.globals)) 
	locCopy1(proc, (ploc_t) globalLoc, fromSpace);
      scanUntil_locCopy1(proc,toSpace->range.low,fromSpace);
    }
  }
  else if (ordering == HybridOrder) {
    int workedSome = 1;
    assert(forceSpaceCheck == 1);
    while (rootLoc = (ploc_t) SetPop(&proc->work.roots))          
      locCopy1(proc, rootLoc, fromSpace);
    while (globalLoc = (ploc_t) SetPop(&proc->work.globals)) 
      locCopy1(proc, (ploc_t) globalLoc, fromSpace);
    while (1) {
      mem_t start = NULL, stop;
      if (start = (mem_t) SetPop2(&proc->work.grayRegion, &stop))
	scanRegion_locCopy1(proc,start,stop,fromSpace);
      AddGrayCopyRange(&proc->copyRange);
      if (SetIsEmpty(&proc->work.grayRegion))
	break;
    }
    assert(SetIsEmpty(&proc->work.objs));
    assert(SetIsEmpty(&proc->work.grayRegion));
  }
  else {
    ptr_t gray;

    while (rootLoc = (ploc_t) SetPop(&proc->work.roots))
      locCopy1_replicaSet(proc, rootLoc,fromSpace); 
    while (globalLoc = (ploc_t) SetPop(&proc->work.globals))
      locCopy1_replicaSet(proc, globalLoc,fromSpace); 
    if (ordering == StackOrder) {
      while (gray = (ptr_t) SetPop(&proc->work.objs)) 
	(void) scanObj_locCopy1_replicaSet(proc,gray,fromSpace);   
    }
    else if (ordering == QueueOrder) {
      while (gray = (ptr_t) SetDequeue(&proc->work.objs)) 
	(void) scanObj_locCopy1_replicaSet(proc,gray,fromSpace);   
    }
    else
      assert(0);
  }

  if (!forceSpaceCheck)
    ReturnCopyRange(&proc->copyRange);                           /* no spacecheck - uniprocessor only */

  ClearCopyRange(&proc->copyRange);
  assert(SetIsEmpty(&proc->work.roots));

  paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
  liveRatio = HeapAdjust1(totalRequested, totalUnused, 
			  0, 0.0, fromSpace, toSpace);
  add_statistic(&majorSurvivalStatistic, liveRatio);
  Heap_Resize(fromSpace,0,1);
  typed_swap(Heap_t *, fromSpace, toSpace);
  NumGC++;

  ResetAllocation(proc, fromSpace);                        /* One processor can grab all of fromSpace for further allocation */
  assert(proc->writelistCursor == proc->writelistStart);
}

void GC_Semi(Proc_t *proc, Thread_t *th)
{
  if (proc->allocLimit == StartHeapLimit)                  
    ResetAllocation(proc, fromSpace);                      /* One processor can grab all of fromSpace for further allocation */
  process_writelist(proc, NULL, NULL);                     /* Get globals; discard backpointers */
  if (GCSatisfiable(proc,th))
    return;
  GCCollect_Semi(proc);
  assert(GCSatisfiable(proc,th));
}


void GCInit_Semi(void)
{
  if (ordering == DefaultOrder)
    ordering = ImplicitOrder;
  GCInit_Help(256, 128 * 1024, 0.1, 0.7, 512, 50 * 1024);
}

