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
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  proc->allocStart = proc->allocCursor;
  proc->segUsage.bytesAllocated += alloc;
}

static long totalRequest = 0;
static long totalUnused = 0;

static void stop_copy(Proc_t *proc)
{
  int i;
  int isFirst = 0;
  mem_t to_alloc_start;         /* Designated thread records this initially */
  Thread_t *curThread = NULL;
  ploc_t rootLoc, globalLoc;
  int curObjFetchSize = objFetchSize;
  int sharedSize, popSize, pushSize, itemsDone = 0; /* Debugging */
  
  /* Using a weak barrier, we detect the first thread and permit it to do some preliminary work 
     while other processors have not reached the barrier and are still in the mutator.
     We use a strong barrier to prevent  other threads from proceeding until the first thread has finished.
  */

  procChangeState(proc, GCWork, 400);

  isFirst = (weakBarrier(barriers,proc) == 0);
  if (isFirst) {
    assert(isEmptyStack(&proc->work.objs));
    assert(isEmptySharedStack(workStack));
    Heap_Resize(toSpace, Heap_GetMaximumSize(toSpace), 1);
    ResetJob();                        /* Reset counter so all user threads are scanned */
    totalRequest = totalUnused = 0;
  }
  strongBarrier(barriers,proc);
  proc->segmentType |= (MajorWork | FlipOff | FlipOn);

  /* All threads get local structures ready */
  assert(isEmptyStack(&proc->work.roots));
  SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
  assert(empty_writelist(proc));

  /* The "first" processor is in charge of the globals. */
  if (isFirst) {
    procChangeState(proc, GCGlobal, 401);
    major_global_scan(proc);
  }

  /* All other processors compute thread-specific roots in parallel */
  procChangeState(proc, GCStack, 402);
  FetchAndAdd(&totalUnused, sizeof(val_t) * (proc->allocLimit - proc->allocCursor));
  while ((curThread = NextJob()) != NULL) {
    if (curThread->requestInfo >= 0)
      FetchAndAdd(&totalRequest, curThread->requestInfo);
    thread_root_scan(proc,curThread);
  }

  procChangeState(proc, GCWork, 403);

  /* Now forward all the roots which initializes the local work stacks */
  proc->numRoot += lengthStack(&proc->work.roots) + lengthStack(&proc->work.globals);
  while (rootLoc = (ploc_t) popStack(&proc->work.roots))
    locCopy1_copyCopySync_replicaStack(proc, rootLoc, &proc->work.objs, &proc->majorRange,fromSpace); 
  while (globalLoc = (ploc_t) popStack(&proc->work.globals))
    locCopy1_copyCopySync_replicaStack(proc, globalLoc, &proc->work.objs, &proc->majorRange,fromSpace); 
  resetSharedStack(workStack,&proc->work);
  strongBarrier(barriers,proc);
  pushSharedStack(0,workStack,&proc->work);       /* We must call this even if local stack is empty */

  procChangeState(proc, GCWork, 404); 

  while (1) {

    int globalEmpty;
    ptr_t gray;

    sharedSize = lengthStack(&workStack->work.objs); 
    if (curObjFetchSize * NumProc > sharedSize)
      curObjFetchSize = 1 + (int) (1.0 * sharedSize / NumProc);

    popSharedStack(workStack, &proc->work,
		   threadFetchSize, 
		   globalLocFetchSize, 
		   rootLocFetchSize,
		   curObjFetchSize, 
		   0);

    /*    popSize = lengthStack(&proc->majorObjStack); */

    while (!recentWorkDone(proc, localWorkSize) &&
	   (gray = popStack(&proc->work.objs)) != NULL) {
      scanObj_locCopy1_copyCopySync_replicaStack(proc,gray,&proc->work.objs,&proc->majorRange,fromSpace);
      /*      itemsDone++; */
    }
    
    /* pushSize = lengthStack(&proc->majorObjStack); */

    globalEmpty = pushSharedStack(0,workStack,&proc->work);  /* We must call this even if local stack is empty */
    if (globalEmpty)
      break;
  }

  procChangeState(proc, GCWork, 405); 

  assert(isEmptyStack(&proc->work.stacklets));
  assert(isEmptyStack(&proc->work.globals));
  assert(isEmptyStack(&proc->work.roots));

  ClearCopyRange(&proc->majorRange);
  strongBarrier(barriers,proc);

  /* Only the designated thread needs to perform the following */
  if (isFirst) {
    long alloc = (sizeof (val_t)) * (fromSpace->top - fromSpace->bottom);
    double liveRatio = 0.0;

    assert(isEmptyStack(&proc->work.objs));
    assert(isEmptySharedStack(workStack));
    /* Check the tospace heap */
    paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);
    /* Resize heaps and do stats */
    liveRatio = HeapAdjust1(totalRequest,totalUnused,0,0.0,fromSpace,toSpace);
    add_statistic(&majorSurvivalStatistic, liveRatio);
    Heap_Resize(fromSpace, 0, 1);
    typed_swap(Heap_t *, fromSpace, toSpace);
    NumGC++;
  }

  /* All system threads need to reset their limit pointer */
  proc->allocStart = StartHeapLimit;
  proc->allocCursor = StartHeapLimit;
  proc->allocLimit = StartHeapLimit;
  assert(proc->writelistCursor == proc->writelistStart);

  /* Resume normal scheduler work and start mutators */
  memBarrier();
  memOrder();
  strongBarrier(barriers,proc);
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
  int roundSize = RoundUp(th->requestInfo,minOffRequest);

  process_writelist(proc,NULL,NULL);
  if (GCSatisfiable(proc,th))   
    return;
  if (th->requestInfo > 0) {
    GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
    if (proc->allocStart != NULL)
      return;
  }
  stop_copy(proc);
  GetHeapArea(fromSpace,roundSize,&proc->allocStart,&proc->allocCursor,&proc->allocLimit);
  assert(GCSatisfiable(proc,th));
}

void GCInit_SemiPara()
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
  workStack = SharedStack_Alloc(0, 0, 0, 32 * 1024, 1024);
  barriers = createBarriers(NumProc, 5);
}
