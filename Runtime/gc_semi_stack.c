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


ptr_t AllocBigArray_SemiStack(Proc_t *proc, Thread_t *thread, ArraySpec_t *spec)
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

void GCRelease_SemiStack(Proc_t *proc)
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
  
  assert(isEmptyStack(&proc->majorObjStack)); 

  /* Using asynchronous version, we detect the first thread and permit
     it to do some preliminary work while other processors have not reached the barrier.
     Note that we prevent other threads from proceeding until the first thread has done its work
     by counting the first thread twice in the barrier size. */
  Heap_Resize(toSpace, Heap_GetSize(fromSpace) + NumProc * minOffRequest, 1);
  ResetJob();                        /* Reset counter so all user threads are scanned */
  req_size = 0;

  proc->gcSegment1 = MajorWork;
  proc->gcSegment2 = FlipBoth;

  /* All threads get local structures ready */
  assert(isEmptyStack(proc->rootLocs));
  SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
  /* no-space chech hack - only for uniprocessor*/
  proc->majorRange.start = proc->majorRange.cursor = toSpace->cursor;
  proc->majorRange.stop = toSpace->top;

  /* Write list can be ignored */
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  process_writelist(proc,NULL,NULL);

  /* The "first" processor is in charge of the globals. */
  procChangeState(proc, GCGlobal);
  major_global_scan(proc);

  /* All other processors compute thread-specific roots in parallel */
  procChangeState(proc, GCStack);
  while ((curThread = NextJob()) != NULL) {
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&req_size, curThread->requestInfo);
    thread_root_scan(proc,curThread);
  }

  procChangeState(proc, GC);
  /* Now forward all the roots which initializes the local work stacks */
  proc->numRoot += lengthStack(proc->rootLocs) + lengthStack(proc->globalLocs);
  while (rootLoc = (ploc_t) popStack(proc->rootLocs))
    locCopy1_noSpaceCheck_replicaStack(proc, rootLoc,
				       &proc->majorObjStack,&proc->majorRange,fromSpace); 
  while (globalLoc = (ploc_t) popStack(proc->globalLocs))
    locCopy1_noSpaceCheck_replicaStack(proc, globalLoc,
				       &proc->majorObjStack,&proc->majorRange,fromSpace); 

  while (1) {
    ptr_t gray = popStack(&proc->majorObjStack);
    if (gray == NULL) 
      break;
    (void) scanObj_locCopy1_noSpaceCheck_replicaStack(proc,gray,&proc->majorObjStack,&proc->majorRange,fromSpace);
  }

  assert(isEmptyStack(&proc->threads));
  assert(isEmptyStack(proc->globalLocs));
  assert(isEmptyStack(proc->rootLocs));
  assert(isEmptyStack(&proc->majorObjStack)); 
  /* no-space hack - uniprocessor only */
  toSpace->cursor = proc->majorRange.cursor;
  proc->majorRange.stop = proc->majorRange.cursor;
  ClearCopyRange(&proc->majorRange);



  /* Only the designated thread needs to perform the following */
  {
    long alloc = (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom);
    double liveRatio = 0.0;

    /* Check the tospace heap */
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
    /* Resize heaps and do stats */
    liveRatio = HeapAdjust1(req_size, 0, 0, 0.0, fromSpace, toSpace);
    add_statistic(&proc->majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace, 0, 1);
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  assert(proc->writelistCursor == proc->writelistStart);

}

void GC_SemiStack(Proc_t *proc, Thread_t *th)
{
  /*  int roundSize = Max(th->requestInfo, minOffRequest);    */
   int numRequest = DivideDown(Heap_GetAvail(fromSpace), minOffRequest);
   int roundSize = RoundUp(th->requestInfo, (numRequest ? numRequest : 1) * minOffRequest);

  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
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

void GCInit_SemiStack()
{
  minOffRequest = 4 * pagesize;
  init_int(&MaxHeap, 128 * 1024);
  init_int(&MinHeap, 256);
  if (MinHeap > MaxHeap)
    MinHeap = MaxHeap;
  init_double(&MinRatio, 0.1);
  init_double(&MaxRatio, 0.7);
  init_int(&MinRatioSize, 512);         
  init_int(&MaxRatioSize, 50 * 1024);
  fromSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);
  toSpace = Heap_Alloc(MinHeap * 1024, MaxHeap * 1024);  
}


