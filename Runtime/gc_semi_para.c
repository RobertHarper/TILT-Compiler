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
#include "platform.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "show.h"

mem_t AllocBigArray_SemiPara(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
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
    case PointerField : 
      assert(IsTagData(spec->pointerVal) ||
	     IsGlobalData(spec->pointerVal) ||
	     inHeap(spec->pointerVal,fromSpace));
      init_parray(obj, spec->elemLen, spec->pointerVal); break;
    case DoubleField : init_farray(obj, spec->elemLen, spec->doubleVal); break;
  }
  return obj;
}


/* --------------------- Parallel collector --------------------- */
void GCRelease_SemiPara(Proc_t *proc)
{
  proc->allocStart = proc->allocCursor;
}

static long totalRequest = 0;   /* Total space requested by all threads */
static long totalUnused = 0;    /* Total allocation space unused by and mapped to all processors */

static void stop_copy(Proc_t *proc)
{
  int count = 0 ; /* XXX */
  int isFirst = 0;
  Thread_t *curThread = NULL;
  ploc_t rootLoc, globalLoc;
  
  /* Using a weak barrier, we detect the first thread and permit it to do some preliminary work 
     while other processors have not reached the barrier and are still in the mutator.
     We use a strong barrier to prevent other threads from proceeding until the first thread has finished.
  */

  /*  procChangeState(proc, GCWork, 400);                        XXX */
  proc->segmentType |= (MajorWork | FlipOff | FlipOn);
  StopAllThreads();                                          /* signal all processors to stop mutators and perform collection */
  isFirst = (weakBarrier(barriers,proc) == 0);
  if (isFirst) {
    assert(isEmptySharedStack(workStack));
    Heap_Resize(toSpace, Heap_GetMaximumSize(toSpace), 1);
    ResetJob();                                              /* Prepare to scan all threads */
    totalRequest = totalUnused = 0;
  }
  assert(isLocalWorkAlmostEmpty(&proc->work));
  assert(empty_writelist(proc));
  if (!noSharing)                                                        
    resetSharedStack(workStack,&proc->work, 0);              /* Prepare shared stack for later use */
  strongBarrier(barriers,proc);

  SetCopyRange(&proc->copyRange, proc, toSpace, NULL);        /* Copy range initialized after toSpace prepared */

  if (isFirst) 
    major_global_scan(proc);                                  /* One processor gets globals */

  FetchAndAdd(&totalUnused, UnusedProcAlloc(proc));           /* Accumulate total allocation space unused by processors */
  while ((curThread = NextJob()) != NULL) {                   /* In parallel, process threads */
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&totalRequest, curThread->requestInfo);     /* Accumulate total amount requested by all threads */
    thread_root_scan(proc,curThread);                         /* Scan for root locations */
  }

  procChangeState(proc, GCWork, 403);                                    /* Entering main GC work */
  proc->numRoot += SetLength(&proc->work.roots);
  proc->numRoot += SetLength(&proc->work.globals);
  if (!noSharing)
    pushSharedStack(0,workStack,&proc->work);                 /* this is necessary even if local stack is empty;
								 this call is placed after the root/global to ensure 
								 all root/globals are shared */
  while (1) {
      ptr_t gray;
      count++;      
      if (!noSharing)
	popSharedStack(workStack, &proc->work);               /* Grab shared work */
      procChangeState(proc,                                   /* Switch from/to Idle to measure work imbalance */
		      (SetIsEmpty(&proc->work.objs) && SetIsEmpty(&proc->work.grayRegion)) ? GCIdle : GCWork, 405);

      if (ordering == StackOrder) {
	while (rootLoc = (ploc_t) SetPop(&proc->work.roots))                   
	  locCopy1_copyCopySync_replicaSet(proc, rootLoc, fromSpace);  
	while (globalLoc = (ploc_t) SetPop(&proc->work.globals))               
	  locCopy1_copyCopySync_replicaSet(proc, globalLoc, fromSpace);   
	if (noWorkTrack) {
	  while ((gray = SetPop(&proc->work.objs)) != NULL)    /*  or there are no more local gray objects */
	    scanObj_locCopy1_copyCopySync_replicaSet(proc,gray,fromSpace);   
	}
	else {
	  while (!recentWorkDone(proc) &&          /* Work on gray object unless time to re-distribute work */  
		 (gray = SetPop(&proc->work.objs)) != NULL)      /*      or there are no more local gray objects */
	    scanObj_locCopy1_copyCopySync_replicaSet(proc,gray,fromSpace);   
	}
      }
      else if (ordering == HybridOrder) {
	mem_t start = NULL, stop;
	while (1) {
	  while (rootLoc = (ploc_t) SetPop(&proc->work.roots))                 
	    locCopy1_copyCopySync(proc, rootLoc, fromSpace);  
	  while (globalLoc = (ploc_t) SetPop(&proc->work.globals))             
	    locCopy1_copyCopySync(proc, globalLoc, fromSpace);   
	  if (start = (mem_t) SetPop2(&proc->work.grayRegion, &stop))
	    scanRegion_locCopy1_copyCopySync(proc,start,stop,fromSpace);
	  AddGrayCopyRange(&proc->copyRange);
	  if (!noWorkTrack) {
	    updateWorkDone(proc);   /* recentWorkDone only counts down once */
	    if (recentWorkDone(proc))
	      break;
	  }
	  if (SetIsEmpty(&proc->work.grayRegion))
	    break;
	}
	assert(proc->copyRange.start == proc->copyRange.cursor);
      }
      else 
	assert(0);

      if ((noSharing && SetIsEmpty(&proc->work.objs) && SetIsEmpty(&proc->work.grayRegion)) || 
                                                                      /* If not sharing work, check if local stack is empty */
	  (!noSharing && pushSharedStack(0,workStack,&proc->work)))   /* If sharing, push to shared stack, check if it is empty */
	break;
  }

  assert(isLocalWorkEmpty(&proc->work));                    
  ClearCopyRange(&proc->copyRange);                        /* Clean up copy range */
  strongBarrier(barriers,proc);

  if (isFirst) {                                            /* Only one thread can perform the following */
    double liveRatio;
    assert(isEmptySharedStack(workStack));
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);  /* Paranoid check must precede heap adjustment */
    liveRatio = HeapAdjust1(totalRequest, totalUnused,
			    0, CollectionRate, 0,
			    fromSpace, toSpace);
    add_statistic(&majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace, 0, 1);
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
  }
  ResetAllocation(proc, NULL);                              /* All system threads need to reset their limit pointer */
  assert(proc->writelistCursor == proc->writelistStart);
  strongBarrier(barriers,proc);                             /* Resume normal scheduler work and start mutators */

}

void GCPoll_SemiPara(Proc_t *proc)
{
  if (checkBarrier(barriers,proc) > 0) {
    process_writelist(proc,NULL,NULL);
    stop_copy(proc);
  }
}


void GC_SemiPara(Proc_t *proc, Thread_t *th)
{
  int roundSize = RoundUp(th->requestInfo,maxOffRequest);
  if (Heap_GetAvail(fromSpace) <= NumProc * roundSize)
    roundSize = RoundUp(th->requestInfo,minOffRequest);

  process_writelist(proc,NULL,NULL);
  if (GCSatisfiable(proc,th))   
    return;
  if (th->requestInfo > 0) {
    GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
    if (proc->allocStart != NULL)
      return;
  }
 retry:
  stop_copy(proc);
  GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  if (!GCSatisfiable(proc,th)) {
    printf("Warning: GC_SemiPara failed to obtain enough memory. Processor %d unmapped for too long? Retrying...\n", proc->procid);
    goto retry;
  }
}

void GCInit_SemiPara(void)
{
  if (ordering == DefaultOrder)
    ordering = StackOrder;
  GCInit_Help(256, 128 * 1024, 0.1, 0.7, 512, 50 * 1024);
  workStack = SharedStack_Alloc(0, 100, 16 * 1024, 12 * 1024, 64 * 1024, 16 * 1024, 0, 0);
  barriers = createBarriers(NumProc, 5);
}
