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


/* ------------------  Semispace array allocation routines ------------------- */

static mem_t alloc_big(int byteLen, int hasPointers)
{
  ptr_t res = 0;
  Thread_t *curThread = getThread();
  Proc_t *proc = curThread->proc;
  unsigned long *saveregs = curThread->saveregs;
  mem_t alloc = (mem_t) saveregs[ALLOCPTR];
  mem_t limit = (mem_t) saveregs[ALLOCLIMIT];
  int wordLen = byteLen / (sizeof (val_t));

  /* Make sure there is enough space */
  if (alloc + wordLen > limit) {
    GCFromC(curThread, byteLen, 0);
    alloc = (mem_t) saveregs[ALLOCPTR];
    limit = (mem_t) saveregs[ALLOCLIMIT];
    assert(alloc + wordLen <= limit);
  }

  assert(wordLen * (sizeof (val_t)) == byteLen);
  assert(alloc + wordLen < limit);

  /* Perform allocation */
  res = alloc;
  alloc += wordLen;
  saveregs[ALLOCPTR] = (unsigned long) alloc;

  /* Update statistics */
  gcstat_normal(proc,byteLen, 0, 0);

  return res;
}

ptr_t alloc_bigintarray_Semi(int elemLen, int initVal, int ptag)
{
  /* elemements are byte-sized */
  int wordLen = 1 + (elemLen + 3) / 4;
  mem_t space = alloc_big(4 * wordLen,0);
  ptr_t res = space + 1;
  init_iarray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigptrarray_Semi(int elemLen, ptr_t initVal, int ptag)
{
  int wordLen = 1 + elemLen;
  mem_t space = alloc_big(4 * wordLen,1);
  ptr_t res = space + 1;
  init_parray(res, elemLen, initVal);
  return res;
}

ptr_t alloc_bigfloatarray_Semi(int elemLen, double initVal, int ptag)
{
  ptr_t res = NULL;
  mem_t region = NULL;
  Thread_t *curThread = getThread();
  int wordLen = 2 + (elemLen << 1);  /* Includes one word for alignment */

  region = alloc_big(4 * wordLen,0);
  if ((((val_t)region) & 7) != 0) {
    region[0] = SKIP_TAG | (1 << SKIPLEN_OFFSET);
    res = region + 1;
  }
  else {
    region[wordLen-1] = SKIP_TAG | (1 << SKIPLEN_OFFSET);
    res = region;
  }
  init_farray(res, elemLen, initVal);
  return res;
}

/* --------------------- Semispace collector --------------------- */
void GCRelease_Semi(Proc_t *proc)
{
  int alloc = sizeof(val_t) * (proc->allocCursor - proc->allocStart);
  gcstat_normal(proc, alloc, 0, 0);
}


int GCTry_Semi(Proc_t *proc, Thread_t *th)
{
  int write = (proc->writelistCursor - proc->writelistStart) / 2;
  assert(proc->userThread == NULL);
  assert(th->proc == NULL);
  if (proc->allocLimit == StartHeapLimit) {
    proc->allocStart = fromSpace->bottom;
    proc->allocCursor = fromSpace->bottom;
    proc->allocLimit = fromSpace->top;
    fromSpace->cursor = fromSpace->top;
  }
  gcstat_normal(proc, 0, 0, write);
  discard_writelist(proc);
  if (th->requestInfo > 0) {
    unsigned int bytesAvailable = (val_t) proc->allocLimit - 
				  (val_t) proc->allocCursor;
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
  int i;  
  Thread_t *oneThread;
  int bytesRequested;
  mem_t allocCursor = proc->allocCursor;
  mem_t allocLimit = proc->allocLimit;
  mem_t to_ptr = toSpace->bottom;
  Queue_t *root_lists = proc->root_lists;
  int write = (proc->writelistCursor - proc->writelistStart) / 2;

  GCStatus = GCOn;
  oneThread = &(Threads[0]);             /* In a sequential collector, 
					    there is only one user thread */
  bytesRequested = oneThread->requestInfo;

  /* Check that processor is unmapped, write list is not overflowed, allocation region intact */
  assert(proc->userThread == NULL);
  assert(allocCursor <= allocLimit);
  assert(bytesRequested >= 0);

  fromSpace->cursor = allocCursor;

  if (paranoid) 
    paranoid_check_all(fromSpace, NULL, NULL, NULL);

  /* Write list can be ignored */
  discard_writelist(proc);

  /* Compute the roots from the stack and register set */
  QueueClear(proc->root_lists);
  local_root_scan(proc,oneThread);
  major_global_scan(proc);

  /* Get toSpace ready for the collection */
  Heap_Resize(toSpace, Heap_GetSize(fromSpace), 1);

  /* forward the roots */
  to_ptr = forward1_root_lists(proc,root_lists, to_ptr, &fromSpace->range, &toSpace->range);

  /* perform a Cheney scan */
  to_ptr = scan1_until(proc,toSpace->range.low, to_ptr, &fromSpace->range, &toSpace->range);
  toSpace->cursor = to_ptr;

  if (debug && SHOW_HEAPS) {
	memdump("From Heap After collection:", fromSpace->bottom,40,0);
        memdump("To Heap After collection:", toSpace->bottom,40,0);
	show_heap_raw("FINAL FROM",(allocCursor - fromSpace->bottom),
		      fromSpace->bottom,fromSpace->top,
		      toSpace->bottom,toSpace->top);
  }

  if (paranoid) 
    paranoid_check_all(fromSpace, NULL, toSpace, NULL);

  gcstat_normal(proc, 0, sizeof (val_t) * (toSpace->cursor - toSpace->bottom), write);

  /* Resize the tospace, discard old space, flip space */
  {
    Heap_t *froms[2] = {NULL, NULL};
    froms[0] = fromSpace;
    HeapAdjust(0, bytesRequested, froms, toSpace);
    Heap_Resize(fromSpace,0,1);
    typed_swap(Heap_t *, fromSpace, toSpace);
  }

  /* Update proc's allocation variables */
  fromSpace->cursor = to_ptr;
  proc->allocStart = fromSpace->cursor;
  proc->allocCursor = fromSpace->cursor;
  proc->allocLimit = fromSpace->top;
  assert(proc->writelistCursor == proc->writelistStart);

  NumGC++;
  GCStatus = GCOff;

}


void gc_init_Semi()
{
  init_int(&MaxHeap, 80 * 1024);
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


