#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "gc_para.h"


/* ------------ Two Room Functions ------------ */
typedef void *Thunk_t(void *);

static void resetTwoRoom(TwoRoom_t *tr)
{
  tr->Gate = tr->Turn1 = tr->Turn2 = 0;
}

static void SynchStart(TwoRoom_t *tr)
{
  while (tr->Gate) 
   flushStore();  /* Without this, updates of tr->Gate from other procetrors are not visible here */
  FetchAndAdd(&tr->Turn1, 1);
  while (tr->Gate) {
    FetchAndAdd(&tr->Turn1, -1);
    while (tr->Gate)
      flushStore();
    FetchAndAdd(&tr->Turn1, 1);
  }
  flushStore();   /* Without this, the change to tr->Turn1 is not visible */
}

static void SynchMid(TwoRoom_t *tr)
{
  tr->Gate = 1;
  flushStore(); /* for updating tr->Gate */
  FetchAndAdd(&tr->Turn2, 1);
  FetchAndAdd(&tr->Turn1, -1);
  while (tr->Turn1 > 0) 
    flushStore();
}

/* If thunk is not NULL, runs thunk on thunkData, returning the result for the last processor exitting.
   Return NULL otherwise */
static void *SynchEnd(TwoRoom_t *tr, Thunk_t *thunk, void *thunkData)
{
  void *result = NULL;
  int i = FetchAndAdd(&tr->Turn2, -1);
  if (i==1) {     /* Last one to decrement */
    if (thunk != NULL)
      result = (*thunk)(thunkData);
    tr->Gate = 0;
    flushStore(); /* for updating tr->Gate */
  }
  return result;
}

/* ------------ SharedObjStack  Functions ------------ */
static void resetAllocSharedObjStack(SharedObjStack_t *sos, int objStackSize)
{
  sos->size = objStackSize;
  sos->data = (ptr_t *) malloc(sizeof(ptr_t) * objStackSize);
  sos->cursor = 0;
  sos->numStack = 0;
  resetTwoRoom(&sos->twoRoom);
}

SharedObjStack_t *SharedObjStack_Alloc(int size)
{
  SharedObjStack_t *sos = (SharedObjStack_t *) malloc(sizeof(SharedObjStack_t));
  resetAllocSharedObjStack(sos, size);
  return sos;
}

static int internalIsEmptySharedObjStack(SharedObjStack_t *ss)
{
  return (ss->cursor == 0 && ss->numStack == 0);
}

static void moveToSharedObjStack(SharedObjStack_t *ss, Stack_t *oStack)
{
  int i, numToTransfer = oStack->cursor;
  int oldCursor = FetchAndAdd(&ss->cursor,numToTransfer);
  for (i=0; i<numToTransfer; i++) 
    ss->data[oldCursor+i] = oStack->data[i];
  oStack->cursor = 0;
  if (ss->cursor >= ss->size) {
    printf("Shared stack overflowed with %d items\n", ss->cursor);
    assert(0);
  }
}

static int getFromSharedObjStack(SharedObjStack_t *ss, Stack_t *oStack, int numToFetch)
{
  int i, oldCursor, newCursor;
  assert(oStack->cursor == 0);
  oldCursor = FetchAndAdd(&ss->cursor,-numToFetch);  /* FetchAndAdd returns the pre-added value */
  newCursor = oldCursor - numToFetch;  
  if (oldCursor < 0) {            /* Handle complete overreach */
    numToFetch = 0;
    ss->cursor = 0;               /* Multiple processors might execute this; this is fine since there are no increments */
  }
  else if (newCursor < 0) {       /* Handle partial overreach */
    numToFetch += newCursor;
    newCursor = oldCursor - numToFetch;
    ss->cursor = 0;               /* Multiple processors might execute this; this is fine since there are no increments */
  }
  for (i=0; i<numToFetch; i++)
    oStack->data[i] = ss->data[newCursor+i];
  oStack->cursor = numToFetch;
  return numToFetch;
}


void popSharedObjStack(SharedObjStack_t *sos, Stack_t *oStack, int numToFetch)
{
  int numFetched;
  SynchStart(&sos->twoRoom);
  numFetched = getFromSharedObjStack(sos, oStack, numToFetch);
  assert(sos->numStack >= 0);
  FetchAndAdd(&sos->numStack,1);  /* Local stack is possibly non-empty now; note that we must increment even when empty since 
				         we don't know how to conditionally decrement later */
  SynchMid(&sos->twoRoom);
  SynchEnd(&sos->twoRoom, NULL, NULL);
}

int isEmptySharedObjStack(SharedObjStack_t *sos)
{
  int empty;
  SynchStart(&sos->twoRoom);
  empty = internalIsEmptySharedObjStack(sos);
  SynchMid(&sos->twoRoom);
  SynchEnd(&sos->twoRoom, NULL, NULL);
  return empty;
}

void resetSharedObjStack(SharedObjStack_t *sos, int n)
{
  assert(isEmptySharedObjStack(sos));
  sos->numStack = n;
}

int pushSharedObjStack(SharedObjStack_t *sos, Stack_t *oStack)
{
  int empty;
  SynchStart(&sos->twoRoom);
  SynchMid(&sos->twoRoom);
  moveToSharedObjStack(sos, oStack);
  FetchAndAdd(&sos->numStack,-1);  /* Local stack is non-empty now */
  assert(sos->numStack >= 0);
  empty = (int) SynchEnd(&sos->twoRoom, (Thunk_t *) (&internalIsEmptySharedObjStack), sos);
  return empty;
}

int condPushSharedObjStack(SharedObjStack_t *sos, Stack_t *oStack)
{
  int isNonEmpty;
  SynchStart(&sos->twoRoom);
  SynchMid(&sos->twoRoom);
  isNonEmpty = !internalIsEmptySharedObjStack(sos);
  if (isNonEmpty)
    moveToSharedObjStack(sos, oStack);
  SynchEnd(&sos->twoRoom, NULL, NULL);
  return isNonEmpty;   /* we pushed only if it was not empty */
}

/* ------------ SharedObjRegionStack  Functions ------------ */
SharedObjRegionStack_t *SharedObjRegionStack_Alloc(int objStackSize)
{
  SharedObjRegionStack_t *sors = (SharedObjRegionStack_t *) malloc(sizeof(SharedObjRegionStack_t));
  resetAllocSharedObjStack(&sors->objStack, objStackSize);
  sors->regionCursor = 0;
  return sors;
}

static int internalIsEmptySharedObjRegionStack(SharedObjRegionStack_t *sors)
{
  return (sors->objStack.cursor == 0 && sors->objStack.numStack == 0 && sors->regionCursor == 0);
}

void popObjSharedObjRegionStack(SharedObjRegionStack_t *sors, Stack_t *oStack, int numToFetch)
{
  SynchStart(&sors->objStack.twoRoom);
  getFromSharedObjStack(&sors->objStack, oStack, numToFetch);
  assert(sors->objStack.numStack >= 0);
  FetchAndAdd(&sors->objStack.numStack,1);  /* Local stack is possibly non-empty now; note that we must increment even when empty since 
						    we don't know how to conditionally decrement later */
  SynchMid(&sors->objStack.twoRoom);
  SynchEnd(&sors->objStack.twoRoom, NULL, NULL);
}

int pushObjSharedObjRegionStack(SharedObjRegionStack_t *sors, Stack_t *oStack)
{
  int empty;
  SynchStart(&sors->objStack.twoRoom);
  SynchMid(&sors->objStack.twoRoom);
  moveToSharedObjStack(&sors->objStack, oStack);
  FetchAndAdd(&sors->objStack.numStack,-1);  /* Local stack is non-empty now */
  assert(sors->objStack.numStack >= 0);
  empty = (int) SynchEnd(&sors->objStack.twoRoom, (Thunk_t *) (&internalIsEmptySharedObjRegionStack), sors);
  return empty;
}

int condPushObjSharedObjRegionStack(SharedObjRegionStack_t *sors, Stack_t *oStack)
{
  int isNonEmpty;
  SynchStart(&sors->objStack.twoRoom);
  SynchMid(&sors->objStack.twoRoom);
  isNonEmpty = !internalIsEmptySharedObjRegionStack(sors);
  if (isNonEmpty)
    moveToSharedObjStack(&sors->objStack, oStack);
  SynchEnd(&sors->objStack.twoRoom, NULL, NULL);
  return isNonEmpty;
}

int isEmptySharedObjRegionStack(SharedObjRegionStack_t *sors)
{
  int empty;
  SynchStart(&sors->objStack.twoRoom);
  empty = internalIsEmptySharedObjRegionStack(sors);
  SynchMid(&sors->objStack.twoRoom);
  SynchEnd(&sors->objStack.twoRoom, NULL, NULL);
  return empty;
}

void resetSharedObjRegionStack(SharedObjRegionStack_t *sors, int n)
{
  assert(isEmptySharedObjRegionStack(sors));
  sors->objStack.numStack = n;
}

int condPushToSharedObjRegionStack(SharedObjRegionStack_t *sors, Stack_t *oStack)
{
  int isNonEmpty;
  SynchStart(&sors->objStack.twoRoom);
  SynchMid(&sors->objStack.twoRoom);
  isNonEmpty = !internalIsEmptySharedObjRegionStack(sors);
  if (isNonEmpty)
    moveToSharedObjStack(&sors->objStack, oStack);
  SynchEnd(&sors->objStack.twoRoom, NULL, NULL);
  return isNonEmpty; /* we pushed only if it was not empty */
}

int popRegionSharedObjRegionStack(SharedObjRegionStack_t *sors, Stack_t *rStack)
{
  int newCursor, success = 1;
  SynchStart(&sors->objStack.twoRoom);
  newCursor = FetchAndAdd(&sors->regionCursor,-2) - 2;
  if (newCursor < 0) {  /* overreach */
    success = 0;
    FetchAndAdd(&sors->regionCursor,2);
  }
  else 
    pushStack2(rStack, sors->regionStack[newCursor], sors->regionStack[newCursor+1]);
  SynchMid(&sors->objStack.twoRoom);
  SynchEnd(&sors->objStack.twoRoom, NULL, NULL);
  return success;  /* Returns 1 if region actually popped */
}

int condPushRegionSharedObjRegionStack(SharedObjRegionStack_t *sors, Stack_t *rStack)
{
  int isNonEmpty;
  SynchStart(&sors->objStack.twoRoom);
  SynchMid(&sors->objStack.twoRoom);
  isNonEmpty = !internalIsEmptySharedObjRegionStack(sors);
  if (isNonEmpty) {
    int i, numToTransfer = rStack->cursor;
    int oldCursor = FetchAndAdd(&sors->regionCursor,numToTransfer);
    assert(oldCursor + numToTransfer <= (sizeof(sors->regionStack) / sizeof(mem_t)));
    for (i=0; i<numToTransfer; i++)
      sors->regionStack[oldCursor+i] = rStack->data[i];
    rStack->cursor = 0;
  }
  SynchEnd(&sors->objStack.twoRoom, NULL, NULL);
  return isNonEmpty; /* we pushed only if a stack was non-empty or numStack is not zero */
}

/* ---------- Other Helper Functions ------------------- */

long synchBarrier(long *counter, long barrierSize, long*prevCounter)
{
  long preCounterValue = FetchAndAdd(counter, 1);
  while (*counter < barrierSize) 
    flushStore();
  *prevCounter = 0;
  return preCounterValue;
}

long asynchReachBarrier(long *counter)
{
  long preCounterValue = FetchAndAdd(counter, 1);
  flushStore();
  return preCounterValue;
}


long asynchCheckBarrier(long *counter, long barrierSize, long *prevCounter)
{
  flushStore();
  if (*counter < barrierSize)
    return 0;
  else {
    *prevCounter = 0;
    return 1;
  }
}
