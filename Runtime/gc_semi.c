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


int GCTry_Semi(Proc_t *proc, Thread_t *th)
{
  assert(proc->userThread == NULL);
  assert(th->proc == NULL);
  /* First time through */
  if (proc->allocLimit == StartHeapLimit) {
    proc->allocStart = fromSpace->bottom;
    proc->allocCursor = fromSpace->bottom;
    proc->allocLimit = fromSpace->top;
    fromSpace->cursor = fromSpace->top;
  }
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  process_writelist(proc,NULL,NULL);
  if (th->requestInfo > 0) {
    int bytesAvailable = sizeof(val_t) * (proc->allocLimit - proc->allocCursor);
    return (th->requestInfo <= bytesAvailable);
  }
  else if (th->requestInfo < 0) {
    int bytesAvailable = sizeof(val_t) * (proc->writelistEnd - proc->writelistCursor);
    return ((-th->requestInfo) <= bytesAvailable);
  }
  else 
    assert(0);
  return 0;
}

void GCStop_Semi(Proc_t *proc)
{
  int bytesRequested = 0;
  mem_t allocCursor = proc->allocCursor;
  mem_t allocLimit = proc->allocLimit;
  Thread_t *curThread = NULL;
  double liveRatio = 0.0;
  ploc_t globalLoc, rootLoc;

  /* Check that processor is unmapped, write list is not overflowed, allocation region intact */
  procChangeState(proc, GC);
  assert(NumProc == 1);
  assert(proc->userThread == NULL);
  assert(allocCursor <= allocLimit);
  assert(bytesRequested >= 0);

  paranoid_check_all(fromSpace, NULL, NULL, NULL, NULL);

  proc->gcSegment2 = FlipBoth;

  /* Write list can be ignored */
  proc->numWrite += (proc->writelistCursor - proc->writelistStart) / 3;
  process_writelist(proc, NULL, NULL); /* Get globals; discard backpointers */

  /* Compute the roots from the stack and register set */
  assert(isEmptyStack(proc->rootLocs));
  procChangeState(proc, GCStack);
  ResetJob();
  while ((curThread = NextJob()) != NULL) {
    if (curThread->requestInfo >= 0)
      bytesRequested += curThread->requestInfo;
    thread_root_scan(proc,curThread);
  }
  procChangeState(proc, GCGlobal);
  major_global_scan(proc);
  procChangeState(proc, GC);

  /* Get toSpace ready for collection. Forward roots. Do Cheney scan. */
  /* The non-standard use of CopyRange only works for uniprocessors */
  Heap_Resize(toSpace, Heap_GetSize(fromSpace), 1);
  SetCopyRange(&proc->majorRange, proc, toSpace, expandCopyRange, dischargeCopyRange, NULL, 0);
  proc->majorRange.start = proc->majorRange.cursor = toSpace->cursor;
  proc->majorRange.stop = toSpace->top;
  while (rootLoc = (ploc_t) popStack(proc->rootLocs))     /* NULL when empty */
    locCopy1_noSpaceCheck(proc, rootLoc, &proc->majorRange, fromSpace);
  assert(primaryGlobalOffset == 0);
  while (globalLoc = (ploc_t) popStack(proc->globalLocs)) /* NULL when empty */
    locCopy1_noSpaceCheck(proc, (ploc_t) globalLoc, &proc->majorRange, fromSpace);
  scanUntil_locCopy1_noSpaceCheck(proc,toSpace->range.low,&proc->majorRange, fromSpace);
  toSpace->cursor = proc->majorRange.cursor;
  proc->majorRange.stop = proc->majorRange.cursor;
  ClearCopyRange(&proc->majorRange);

  paranoid_check_all(fromSpace, NULL, toSpace, NULL, NULL);

  /* Resize the tospace, discard fromspace, flip space */
  liveRatio = HeapAdjust1(bytesRequested, 0, 0.0, fromSpace, toSpace);
  add_statistic(&proc->majorSurvivalStatistic, liveRatio);
  Heap_Resize(fromSpace,0,1);
  typed_swap(Heap_t *, fromSpace, toSpace);

  /* Update proc's allocation variables */
  proc->allocStart = fromSpace->cursor;
  proc->allocCursor = fromSpace->cursor;
  proc->allocLimit = fromSpace->top;
  fromSpace->cursor = fromSpace->top;
  assert(proc->writelistCursor == proc->writelistStart);

  NumGC++;
}


void GCInit_Semi()
{
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


