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

/* ------------ SharedStack  Functions ------------ */
static void resetAllocSharedStack(SharedStack_t *ss, int objStackSize)
{
}

SharedStack_t *SharedStack_Alloc(int rootValSize, int objSize, int segmentSize)
{
  SharedStack_t *ss = (SharedStack_t *) malloc(sizeof(SharedStack_t));
  ss->rootValSize = rootValSize;
  ss->objSize = objSize;
  ss->segmentSize = segmentSize;
  ss->rootValData = (ptr_t *) malloc(sizeof(ptr_t) * rootValSize);
  ss->objData = (ptr_t *) malloc(sizeof(ptr_t) * objSize);
  ss->segmentData = (ptr_t *) malloc(sizeof(ptr_t) * segmentSize);
  ss->rootValCursor = 0;
  ss->objCursor = 0;
  ss->segmentCursor = 0;
  ss->numLocalStack = 0;
  resetTwoRoom(&ss->twoRoom);
  return ss;
}

static int internalIsEmptySharedStack(SharedStack_t *ss)
{
  return (ss->rootValCursor == 0 && ss->objCursor == 0 && ss->segmentCursor == 0 && ss->numLocalStack == 0);
}

/* Transfer all items from 'from' to 'to' */
static void moveToSharedStack(ptr_t *to, long *toCursor, long toSize, ptr_t *from, long *fromCursor)
{
  int i, numToTransfer = *fromCursor;
  int oldToCursor = FetchAndAdd(toCursor,numToTransfer);
  for (i=0; i<numToTransfer; i++) 
    to[oldToCursor+i] = from[i];
  *fromCursor = 0;
  if (*toCursor >= toSize && *toCursor > 0) {
    printf("Shared stack overflowed with %d items\n", *toCursor);
    assert(0);
  }
}

/* Transfer up to numToFetch items from 'from' to 'to', handling overreach */
static int getFromSharedStack(ptr_t *to, long *toCursor, long toSize, ptr_t *from, long *fromCursor, long numToFetch)
{
  int i, oldFromCursor, newFromCursor;
  oldFromCursor = FetchAndAdd(fromCursor,-numToFetch);  /* FetchAndAdd returns the pre-added value */
  newFromCursor = oldFromCursor - numToFetch;  
  if (oldFromCursor < 0) {        /* Handle complete overreach */
    numToFetch = 0;
    *fromCursor = 0;              /* Multiple processors might execute this; this is fine since there are no increments */
  }
  else if (newFromCursor < 0) {   /* Handle partial overreach */
    numToFetch += newFromCursor;  /* Fetching fewer items than requested */
    newFromCursor = oldFromCursor - numToFetch; /* Recompute newFromCursor */
    *fromCursor = 0;              /* Multiple processors might execute this; this is fine since there are no increments */
  }
  assert(*toCursor == 0);
  for (i=0; i<numToFetch; i++)
    to[i] = from[newFromCursor+i];
  *toCursor = numToFetch;
  return numToFetch;
}


int isEmptySharedStack(SharedStack_t *ss)
{
  int empty;
  SynchStart(&ss->twoRoom);
  empty = internalIsEmptySharedStack(ss);
  SynchMid(&ss->twoRoom);
  SynchEnd(&ss->twoRoom, NULL, NULL);
  return empty;
}

void resetSharedStack(SharedStack_t *ss, int n)
{
  assert(isEmptySharedStack(ss));
  ss->numLocalStack = n;
}

static int segSize = 3; /* Number of items that constitute a segment */

void popSharedStack(SharedStack_t *ss, Stack_t *rootVal, int rootValRequest, Stack_t *obj, int objRequest, Stack_t *segment, int segRequest)
{
  int rootValFetched, objFetched;
  SynchStart(&ss->twoRoom);
  rootValFetched = getFromSharedStack(rootVal->data, &rootVal->cursor, rootVal->size, ss->rootValData, &ss->rootValCursor, rootValRequest);
  objRequest = (int) (objRequest * ((double)(rootValRequest - rootValFetched) / rootValRequest));
  if (objRequest > 0) {
    objFetched = getFromSharedStack(obj->data, &obj->cursor, obj->size, ss->objData, &ss->objCursor, objRequest);
    segRequest = (int) (segRequest * ((double)(objRequest - objFetched) / objRequest));
    if (segRequest > 0)
      getFromSharedStack(segment->data, &segment->cursor, segment->size, ss->segmentData, &ss->segmentCursor, segSize * segRequest);
  }
  assert(ss->numLocalStack >= 0);
  FetchAndAdd(&ss->numLocalStack,1);  /* Local stack is possibly non-empty now; note that we must increment even when empty since 
					 we don't know how to conditionally decrement later */
  SynchMid(&ss->twoRoom);
  SynchEnd(&ss->twoRoom, NULL, NULL);
}

int pushSharedStack(SharedStack_t *ss, Stack_t *rootVal, Stack_t *obj, Stack_t *segment)
{
  int empty;
  SynchStart(&ss->twoRoom);
  SynchMid(&ss->twoRoom);
  moveToSharedStack(ss->rootValData, &ss->rootValCursor, ss->rootValSize, rootVal->data, &rootVal->cursor);
  moveToSharedStack(ss->objData, &ss->objCursor, ss->objSize, obj->data, &obj->cursor);
  moveToSharedStack(ss->segmentData, &ss->segmentCursor, ss->segmentSize, segment->data, &segment->cursor);
  FetchAndAdd(&ss->numLocalStack,-1);  /* Local stack is non-empty now */
  assert(ss->numLocalStack >= 0);
  empty = (int) SynchEnd(&ss->twoRoom, (Thunk_t *) (&internalIsEmptySharedStack), ss);
  return empty;
}

int condPushSharedStack(SharedStack_t *ss, Stack_t *rootVal, Stack_t *obj, Stack_t *segment)
{
  int isNonEmpty;
  SynchStart(&ss->twoRoom);
  SynchMid(&ss->twoRoom);
  isNonEmpty = !internalIsEmptySharedStack(ss);
  if (isNonEmpty) {
    moveToSharedStack(ss->rootValData, &ss->rootValCursor, ss->rootValSize, rootVal->data, &rootVal->cursor);
    moveToSharedStack(ss->objData, &ss->objCursor, ss->objSize, obj->data, &obj->cursor);
    moveToSharedStack(ss->segmentData, &ss->segmentCursor, ss->segmentSize, segment->data, &segment->cursor);
  }
  SynchEnd(&ss->twoRoom, NULL, NULL);
  return isNonEmpty;   /* we pushed only if it was not empty */
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
