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
SharedStack_t *SharedStack_Alloc(int stackletSize, int globalLocSize, int rootLocSize, int objSize, int segmentSize)
{
  SharedStack_t *ss = (SharedStack_t *) malloc(sizeof(SharedStack_t));
  allocStack(&ss->stacklet, stackletSize);
  allocStack(&ss->globalLoc, globalLocSize);
  allocStack(&ss->rootLoc, rootLocSize);
  allocStack(&ss->obj, objSize);
  allocStack(&ss->segment, segmentSize);
  ss->numLocalStack = 0;
  resetTwoRoom(&ss->twoRoom);
  return ss;
}

static int internalIsEmptySharedStack(SharedStack_t *ss)
{
  return (ss->stacklet.cursor == 0 && ss->globalLoc.cursor == 0 && ss->rootLoc.cursor == 0 && 
	  ss->obj.cursor == 0 && ss->segment.cursor == 0 && ss->numLocalStack == 0);
}

/* Transfer all items from 'from' to 'to' */
static void moveToSharedStack(Stack_t *to, Stack_t *from)
{
  int i; 
  int numToTransfer = from->cursor;
  int oldToCursor = FetchAndAdd(&to->cursor,numToTransfer);
  memcpy(&to->data[oldToCursor], &from->data[0], sizeof(val_t) * numToTransfer);
  from->cursor = 0;
  if (to->cursor >= to->size && to->cursor > 0) {
    printf("Shared stack of size %d overflowed with %d items\n", to->size, to->cursor);
    assert(0);
  }
}

/* Transfer up to numToFetch items from 'from' to 'to', handling overreach */
static int getFromSharedStack(Stack_t *to, Stack_t *from, long numToFetch)
{
  int i;
  int oldFromCursor = FetchAndAdd(&from->cursor,-numToFetch);  /* FetchAndAdd returns the pre-added value */
  int newFromCursor = oldFromCursor - numToFetch;  
  if (oldFromCursor < 0) {        /* Handle complete overreach */
    numToFetch = 0;
    from->cursor = 0;             /* Multiple processors might execute this; this is fine since there are no increments */
  }
  else if (newFromCursor < 0) {   /* Handle partial overreach */
    numToFetch += newFromCursor;  /* Fetching fewer items than requested */
    newFromCursor = oldFromCursor - numToFetch; /* Recompute newFromCursor */
    from->cursor = 0;             /* Multiple processors might execute this; this is fine since there are no increments */
  }
  assert(to->cursor == 0);
  memcpy(&to->data[0], &from->data[newFromCursor], sizeof(val_t) * numToFetch);
  to->cursor = numToFetch;
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

#define MakeFraction(a,b,c) ((int) ((a) * (((double)((b) - (c))) / (b))))
void popSharedStack(SharedStack_t *ss, 
		    Stack_t *stacklet,     int stackletRequest,
		    Stack_t *globalLoc,    int globalLocRequest,
		    Stack_t *rootLoc,      int rootLocRequest, 
		    Stack_t *obj,          int objRequest, 
		    Stack_t *segment,      int segRequest)
{
  int stackletFetched, globalLocFetched, rootLocFetched, objFetched;
  SynchStart(&ss->twoRoom);
  stackletFetched = getFromSharedStack(stacklet, &ss->stacklet, stackletRequest);
  globalLocRequest = MakeFraction(globalLocRequest, stackletRequest, stackletFetched);
  if (globalLocRequest > 0) {
    globalLocFetched = getFromSharedStack(globalLoc, &ss->globalLoc, globalLocRequest);
    rootLocRequest = MakeFraction(rootLocRequest, globalLocRequest, globalLocFetched);
    if (rootLocRequest > 0) {
      rootLocFetched = getFromSharedStack(rootLoc, &ss->rootLoc, rootLocRequest);
      objRequest = MakeFraction(objRequest, rootLocRequest, rootLocFetched);
      if (objRequest > 0) {
	objFetched = getFromSharedStack(obj, &ss->obj, objRequest);
	segRequest = MakeFraction(segRequest, objRequest, objFetched);
	if (segRequest > 0)
	  getFromSharedStack(segment, &ss->segment, segSize * segRequest);
      }
    }
  }
  assert(ss->numLocalStack >= 0);
  FetchAndAdd(&ss->numLocalStack,1);  /* Local stack is possibly non-empty now; note that we must increment even when empty since 
					 we don't know how to conditionally decrement later */
  SynchMid(&ss->twoRoom);
  SynchEnd(&ss->twoRoom, NULL, NULL);
}

static void helpPushSharedStack(SharedStack_t *ss, Stack_t *stacklet, Stack_t *globalLoc, Stack_t *rootLoc,
				Stack_t *obj, Stack_t *segment)
{
  moveToSharedStack(&ss->stacklet, stacklet);
  moveToSharedStack(&ss->globalLoc, globalLoc);
  moveToSharedStack(&ss->rootLoc, rootLoc);
  moveToSharedStack(&ss->obj, obj);
  moveToSharedStack(&ss->segment, segment);
}


int pushSharedStack(SharedStack_t *ss, Stack_t *stacklet, Stack_t *globalLoc, Stack_t *rootLoc, Stack_t *obj, Stack_t *segment)
{
  int empty;
  SynchStart(&ss->twoRoom);
  SynchMid(&ss->twoRoom);
  helpPushSharedStack(ss, stacklet, globalLoc, rootLoc, obj, segment);
  FetchAndAdd(&ss->numLocalStack,-1);  /* Local stack is non-empty now */
  assert(ss->numLocalStack >= 0);
  empty = (int) SynchEnd(&ss->twoRoom, (Thunk_t *) (&internalIsEmptySharedStack), ss);
  return empty;
}

int condPushSharedStack(SharedStack_t *ss, Stack_t *stacklet, Stack_t *globalLoc, Stack_t *rootLoc, Stack_t *obj, Stack_t *segment)
{
  int isNonEmpty;
  SynchStart(&ss->twoRoom);
  SynchMid(&ss->twoRoom);
  isNonEmpty = !internalIsEmptySharedStack(ss);
  if (isNonEmpty) 
    helpPushSharedStack(ss, stacklet, globalLoc, rootLoc, obj, segment);
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
