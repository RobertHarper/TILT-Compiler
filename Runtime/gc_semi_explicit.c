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


ptr_t AllocBigArray_SemiExplicit(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj;
  int tagByteLen = spec->byteLen + 4;
  Align_t align = (spec->type == DoubleField) ? OddWordAlign : NoWordAlign; /* Since there is one tag word */

  /* Allocate the space */
  region = AllocFromThread(thread, tagByteLen, align);
  if (region == NULL) 
    region = AllocFromHeap(fromSpace, thread, tagByteLen, align);
  if (region == NULL) {
    GCFromC(thread, tagByteLen + 4, 0);
    region = AllocFromThread(thread, tagByteLen, align);
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

void GCRelease_SemiExplicit(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->allocStart = proc->allocCursor;
  proc->segUsage.bytesAllocated += alloc;
}

static void stop_copy(Proc_t *proc)
{
  int i;
  mem_t to_alloc_start;         /* Designated thread records this initially */
  Thread_t *curThread = NULL;
  static long req_size;            /* These are shared across processors. */
  ploc_t rootLoc, globalLoc;
  ptr_t gray;
  
  assert(SetIsEmpty(&proc->work.objs)); 

  /* Using asynchronous version, we detect the first thread and permit
     it to do some preliminary work while other processors have not reached the barrier.
     Note that we prevent other threads from proceeding until the first thread has done its work
     by counting the first thread twice in the barrier size. */
  Heap_Resize(toSpace, Heap_GetMaximumSize(toSpace), 1);
  ResetJob();                        /* Reset counter so all user threads are scanned */
  req_size = 0;



  proc->segmentType |= (MajorWork | FlipOn | FlipOff);
  /* All threads get local structures ready */
  assert(isLocalWorkEmpty(&proc->work));
  SetCopyRange(&proc->copyRange, proc, toSpace, NULL);
  AllocEntireCopyRange(&proc->copyRange);  /* no-space chech hack - only for uniprocessor*/

  /* Write list can be ignored */
  process_writelist(proc,NULL,NULL);

  /* Roots include globals and stack slots */
  major_global_scan(proc);
  while ((curThread = NextJob()) != NULL) {
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&req_size, curThread->requestInfo);
    thread_root_scan(proc,curThread);
  }
  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);

  /* Forward all stack slots and globals. Process until no more gray objects. */
  procChangeState(proc, GCWork, 702);
  if (ordering == StackOrder) {
    while (rootLoc = (ploc_t) SetPop(&proc->work.roots))
      locCopy1_noSpaceCheck_replicaSet(proc, rootLoc,fromSpace); 
    while (globalLoc = (ploc_t) SetPop(&proc->work.globals))
      locCopy1_noSpaceCheck_replicaSet(proc, globalLoc,fromSpace); 
    /*    while (gray = (ptr_t) SetPop(&proc->work.objs)) 
      (void) scanObj_locCopy1_noSpaceCheck_replicaSet(proc,gray,fromSpace);   */
    scanUntil_locCopy1_noSpaceCheck(proc,toSpace->range.low,fromSpace); /* XXXX */
  }
  else if (ordering == QueueOrder) {
    while (rootLoc = (ploc_t) SetDequeue(&proc->work.roots))
      locCopy1_noSpaceCheck_replicaSet(proc, rootLoc,fromSpace); 
    while (globalLoc = (ploc_t) SetDequeue(&proc->work.globals))
      locCopy1_noSpaceCheck_replicaSet(proc, globalLoc,fromSpace); 
    /*    while (gray = (ptr_t) SetDequeue(&proc->work.objs)) 
      (void) scanObj_locCopy1_noSpaceCheck_replicaSet(proc,gray,fromSpace);  
      */
    scanUntil_locCopy1_noSpaceCheck(proc,toSpace->range.low,fromSpace); /* XXXX */
  }
  else
    assert(0);

  SetReset(&proc->work.objs); /* XXX */
  /*  assert(isLocalWorkEmpty(&proc->work)); XXX */
  SetReset(&proc->work.objs);

  ReturnCopyRange(&proc->copyRange);    /* no-space hack - uniprocessor only */
  ClearCopyRange(&proc->copyRange);


  /* Only the designated thread needs to perform the following */
  {
    long alloc = (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom);
    double liveRatio = 0.0;

    /* Check the tospace heap */
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
    /* Resize heaps and do stats */
    liveRatio = HeapAdjust1(req_size, 0, 0, 0.0, fromSpace, toSpace);
    add_statistic(&majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace, 0, 1);
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  ResetAllocation(proc, NULL);
  assert(proc->writelistCursor == proc->writelistStart);

}

void GC_SemiExplicit(Proc_t *proc, Thread_t *th)
{
  /*  int roundSize = Max(th->requestInfo, minOffRequest);    */
   int numRequest = DivideDown(Heap_GetAvail(fromSpace), minOffRequest);
   int roundSize = RoundUp(th->requestInfo, (numRequest ? numRequest : 1) * minOffRequest);

  process_writelist(proc,NULL,NULL);
  if (GCSatisfiable(proc,th))   
    return;
  if (th->requestInfo > 0) {
    GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
    if (proc->allocStart) 
      return;
  }
  stop_copy(proc);
  GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  assert(GCSatisfiable(proc,th));
}

void GCInit_SemiExplicit(void)
{
  if (ordering == DefaultOrder)
    ordering = ImplicitOrder;
  GCInit_Help(0.1, 0.7, 512, 50 * 1024);
}


