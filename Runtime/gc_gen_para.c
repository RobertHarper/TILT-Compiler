#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>
#include <strings.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "gc_para.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "platform.h"
#include "show.h"
#include "gc_large.h"


mem_t AllocBigArray_GenPara(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
{
  mem_t region;
  ptr_t obj;
  int tagByteLen = spec->byteLen + 4;
  Align_t align = (spec->type == DoubleField) ? OddWordAlign : NoWordAlign; /* Since there is one tag word */

  /* Allocate the space */
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
void GCRelease_GenPara(Proc_t *proc)
{
  proc->allocStart = proc->allocCursor;
}

static long totalRequest = 0;
static long totalUnused = 0;

static void GCCollect_GenPara(Proc_t *proc)
{
  int isFirst = 0;                    /* Am I the first processor? */
  Thread_t *curThread = NULL;
  ploc_t rootLoc, globalLoc;
  ptr_t PRObj;

  /* Collection cannot proceed until all processors have stopped running mutators.
     While waiting for the processors, the "first" processor begins to do some
     prelimiary work.  This work must be completed before any processor begins collection.
     As a result, the "first" processor is counted twice.
  */
  StopAllThreads();
  isFirst = (weakBarrier(barriers, proc) == 0);
  if (isFirst) {
    paranoid_check_all(nursery, fromSpace, NULL, NULL, largeSpace);
    /* A Major GC is forced if the tenured space is potentially too small */
    if (Heap_GetAvail(fromSpace) < Heap_GetSize(nursery)) 
      GCType = Major;
    else 
      GCType = Minor;
    totalRequest = totalUnused = 0;
    ResetJob();                        /* Reset counter so all user threads are scanned */
  }
  strongBarrier(barriers, proc);

  /* All processors compute thread-specific roots in parallel
     and determine whether a major GC has been requested. */
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  while ((curThread = NextJob()) != NULL) {
    /* If negative, requestnfo signifies full writelist */
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&totalRequest, curThread->requestInfo);
    thread_root_scan(proc,curThread);
    if (GCType == Minor && curThread->request == MajorGCRequestFromC)  /* Upgrade to major GC */
      GCType = Major;      
  }
  strongBarrier(barriers, proc);

  /* After barrier, we know if GC is major */
  proc->segmentType |= (FlipOff | FlipOn | ((GCType == Minor) ? MinorWork : MajorWork));

  if (isFirst) {
    if (GCType == Minor)
      ;
    else {
      int toSpaceSize = Heap_GetMaximumSize(toSpace);
      int maxLive = (sizeof (val_t)) * (fromSpace->cursor - fromSpace->bottom) +
	            (sizeof (val_t)) * (nursery->top - nursery->bottom);
      if (maxLive >= toSpaceSize) {
	printf("WARNING at GC %d: failure possible since maxPossibleLive = %d > toSpaceSize = %d\n",
	       NumGC, maxLive, toSpaceSize);
	Heap_Resize(toSpace, toSpaceSize, 1);
      }    
      else
	Heap_Resize(toSpace, maxLive, 1);
      gc_large_startCollect();
    }
  }
  strongBarrier(barriers, proc);

  if (GCType == Minor) {
    process_writelist(proc, nursery, fromSpace);
    if (isFirst)
      minor_global_scan(proc);
  }
  else {
    process_writelist(proc, NULL, NULL);
    if (isFirst)
      major_global_scan(proc);
  }

  procChangeState(proc, GCWork, 502);

  SetCopyRange(&proc->copyRange, proc, (GCType == Minor) ? fromSpace : toSpace, NULL);
  
  /* Now forward all the roots which initializes the local work stacks */
  proc->numRoot += SetLength(&proc->work.roots) + SetLength(&proc->work.globals);
  assert(primaryGlobalOffset == 0);
  if (GCType == Minor) {
    while (rootLoc = (ploc_t) SetPop(&proc->work.roots)) 
      locCopy1_copyCopySync_replicaSet(proc, rootLoc,nursery);
    while (globalLoc = (ploc_t) SetPop(&proc->work.globals))
      locCopy1_copyCopySync_replicaSet(proc, globalLoc,nursery);
    while (PRObj = SetPop(&proc->work.backObjs)) {
      /* Not transferScanObj_* since this object is a primaryReplica.
         Since this is a stop-copy collector, we can use _locCopy_ immediately */
      scanObj_locCopy1_copyCopySync_replicaSet(proc, PRObj, nursery);
    }
  }
  else { /* Major collection */
    while (rootLoc = (ploc_t) SetPop(&proc->work.roots)) 
      locCopy2L_copyCopySync_replicaSet(proc, rootLoc,nursery,fromSpace,largeSpace);
    while (globalLoc = (ploc_t) SetPop(&proc->work.globals)) 
      locCopy2L_copyCopySync_replicaSet(proc, globalLoc,nursery,fromSpace,largeSpace);
    SetReset(&proc->work.backObjs);
  }

  /* Move everything from local stack to global stack to balance work; note the omitted popSharedStack */
  if (!noSharing)
    resetSharedStack(workStack, &proc->work, 0);
  strongBarrier(barriers, proc);

  if (!noSharing)
    pushSharedStack(0,workStack, &proc->work);
  addMaxWork(proc, MAXINT);

  /* Get work from global stack; operate on local stack; put work back on global stack */
  while (1) {
    ptr_t gray;

    if (!noSharing)
      popSharedStack(workStack, &proc->work);

    if (SetIsEmpty(&proc->work.objs))
      procChangeState(proc, GCIdle, 405);
    else
      procChangeState(proc, GCWork, 406);
    
    if (GCType == Minor) {
      while (!reachCheckWork(proc) &&
	     ((gray = SetPop(&proc->work.objs)) != NULL)) 
	scanObj_locCopy1_copyCopySync_replicaSet(proc,gray,nursery);
    }
    else {
      while (!reachCheckWork(proc) &&
	     ((gray = SetPop(&proc->work.objs)) != NULL)) 
	scanObj_locCopy2L_copyCopySync_replicaSet(proc,gray,nursery,fromSpace,largeSpace);
    }

    if (noSharing && SetIsEmpty(&proc->work.objs))
      break;
    if (!noSharing && pushSharedStack(0,workStack,&proc->work))   /* We must call this even if local stack is empty */
      break;
  }

  assert(isLocalWorkEmpty(&proc->work));
  if (GCType == Minor) 
    PadCopyRange(&proc->copyRange);       /* Carry over minor ranges across minor GCs to avoid internal fragmentation */
  else if (GCType == Major)
    ClearCopyRange(&proc->copyRange);

  /* Wait for all active threads to reach this point so all forwarding is complete */
  strongBarrier(barriers, proc);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    if (GCType == Minor) 
      paranoid_check_all(nursery, fromSpace, fromSpace, NULL, largeSpace);
    else
      paranoid_check_all(nursery, fromSpace, toSpace, NULL, largeSpace);

    /* Globals, statistics, resize/reset/flip */
    if (GCType == Minor) {
      double liveRatio = 0.0;
      int i, copied = 0;
      minor_global_promote(proc);
      for (i=0; i<NumProc; i++) {
	Proc_t *p = getNthProc(i);;
	copied += bytesCopied(&p->cycleUsage) + bytesCopied(&p->segUsage);
      }
      liveRatio = (double) (copied) / (double) Heap_GetUsed(nursery);
      add_statistic(&minorSurvivalStatistic, liveRatio);
    }
    else {
      gc_large_endCollect();
      HeapAdjust2(totalRequest,totalUnused,0,CollectionRate,0,nursery,fromSpace,toSpace);
      Heap_Resize(fromSpace, 0, 1);
      typed_swap(Heap_t *, fromSpace, toSpace);
      NumMajorGC++;
    }
    Heap_Reset(nursery);
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  ResetAllocation(proc, NULL);
  assert(proc->writelistCursor == proc->writelistStart);

  /* Resume normal scheduler work and start mutators */
  strongBarrier(barriers, proc);

}

void GC_GenPara(Proc_t *proc, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo, minOffRequest);

  /* Check for forced Major GC's */
  if (th->request != MajorGCRequestFromC) {
    if (GCSatisfiable(proc,th))
      return;
    if (th->requestInfo > 0) {
      GetHeapArea(nursery,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
      if (proc->allocStart != NULL)
	return;
    }   
  }
  /* Process write list if we can; now can we resume mutator? */
  if (2 * SetLength(&proc->work.roots) < SetFullSize(&proc->work.roots)) 
    process_writelist(proc,nursery,fromSpace);
  if (th->request != MajorGCRequestFromC) {
    if (GCSatisfiable(proc,th))
      return;
  }
  retry:
  GCCollect_GenPara(proc);
  GetHeapArea(nursery,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  if (!GCSatisfiable(proc,th)) {
    printf("Warning: GC_GenPara failed to obtain enough memory. Processor %d unmapped for too long? Retrying...\n", proc->procid);
    goto retry;
  }
}

void GCPoll_GenPara(Proc_t *proc)
{
  if (checkBarrier(barriers,proc) > 0)
    GCCollect_GenPara(proc);
}

void GCInit_GenPara(void) 
{
  int cache_size = GetBcacheSize();
  
  GCInit_Help(1024, 128 * 1024, 0.2, 0.8, 512, 50 * 1024);   
  init_int(&NurseryByte, (int)(0.85 * cache_size));
  assert(MinHeapByte >= 1.2*NurseryByte);
  nursery = Heap_Alloc(NurseryByte, NurseryByte);
  gc_large_init();
  workStack = SharedStack_Alloc(1, 0, 0, 0, 64 * 1024, 1024, 2048, 4096);
  barriers = createBarriers(NumProc, 7);
}

