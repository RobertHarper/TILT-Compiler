#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>
#include <string.h>

#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "rooms.h"
#include "gc_para.h"




/* ------------ SharedStack  Functions ------------ */
static int internalIsEmptySharedStack(SharedStack_t *ss)
{
  return (isLocalWorkEmpty(&ss->work) && ss->numLocalStack == 0);
}

#define StackEmpty 2
#define StackNotEmpty 3
static int finalizer(void *d) 
{
  SharedStack_t *ss = (SharedStack_t *) d;
  if (internalIsEmptySharedStack(ss))
    return StackEmpty;
  else
    return StackNotEmpty;
}

SharedStack_t *SharedStack_Alloc(int stackletSize, int globalLocSize, int rootLocSize, int objSize, int segmentSize)
{
  SharedStack_t *ss = (SharedStack_t *) malloc(sizeof(SharedStack_t));
  init_localWork(&ss->work, objSize, segmentSize, globalLocSize, rootLocSize, stackletSize);
  ss->numLocalStack = 0;
  ss->threeRoom = createRooms(3);
  assignExitCode(ss->threeRoom, 1, &finalizer, (void *) ss);  /* check if stack empty in push room */
  ss->numPush = ss->numPop = 0;
  return ss;
}

/* Transfer all items from 'from' to 'to' */
static void moveToSharedStack(Stack_t *to, Stack_t *from)
{
  int i; 
  int numToTransfer = from->cursor;
  int oldToCursor = FetchAndAdd(&to->cursor,numToTransfer);
  if (to->cursor >= to->size && to->cursor > 0) {
    printf("Shared stack of size %d overflowed with %d items\n", to->size, to->cursor);
    assert(0);
  }
  memcpy((void *)&to->data[oldToCursor], (void *)&from->data[0], sizeof(val_t) * numToTransfer);
  from->cursor = 0;
}

/* Transfer up to numToFetch items from 'from' to 'to', handling overreach */
static int getFromSharedStack(Stack_t *to, Stack_t *from, long numToFetch)
{
  int i;
  int oldFromCursor = FetchAndAdd(&from->cursor,-numToFetch);  /* FetchAndAdd returns the pre-added value */
  int newFromCursor = oldFromCursor - numToFetch;  
  if (oldFromCursor < 0) {        /* Handle complete overreach */
    numToFetch = 0;
    from->cursor = 0;             /* Multiple processors might execute this; ok since there are no increments */
  }
  else if (newFromCursor < 0) {   /* Handle partial overreach */
    numToFetch += newFromCursor;  /* Fetching fewer items than requested */
    newFromCursor = oldFromCursor - numToFetch; /* Recompute newFromCursor */
    from->cursor = 0;             /* Multiple processors might execute this; ok since there are no increments */
  }
  memcpy((void *)&to->data[to->cursor], (void *)&from->data[newFromCursor], sizeof(val_t) * numToFetch);
  to->cursor += numToFetch;
  assert(to->cursor < to->size);
  return numToFetch;
}


int isEmptySharedStack(SharedStack_t *ss)
{
  int empty;
  enterRoom(ss->threeRoom,1);             /* Enter push room */
  empty = internalIsEmptySharedStack(ss); /* If empty in push room, stack is really empty at this point */
  exitRoom(ss->threeRoom);
  return empty;
}

void resetSharedStack(SharedStack_t *ss, LocalWork_t *lw)
{
  assert(lw->hasShared == 0);
  lw->hasShared = 1;
  FetchAndAdd(&ss->numLocalStack, 1);
}

static int segSize = 3; /* Number of items that constitute a segment */

extern double roomTime;

#define MakeFraction(a,b,c) ((int) ((a) * (((double)((b) - (c))) / (b))))
void popSharedStack(SharedStack_t *ss, 
		    LocalWork_t *lw,
		    int stackletRequest,
		    int globalLocRequest,
		    int rootLocRequest, 
		    int objRequest, 
		    int segRequest)
{
  int stackletFetched = 0, globalLocFetched = 0, rootLocFetched = 0, objFetched = 0, segFetched = 0;
  enterRoom(ss->threeRoom,0);
  stackletFetched = getFromSharedStack(&lw->stacklets, &ss->work.stacklets, stackletRequest);
  globalLocRequest = MakeFraction(globalLocRequest, stackletRequest, stackletFetched);
  if (globalLocRequest > 0) {
    globalLocFetched = getFromSharedStack(&lw->globals, &ss->work.globals, globalLocRequest);
    rootLocRequest = MakeFraction(rootLocRequest, globalLocRequest, globalLocFetched);
    if (rootLocRequest > 0) {
      rootLocFetched = getFromSharedStack(&lw->roots, &ss->work.roots, rootLocRequest);
      objRequest = MakeFraction(objRequest, rootLocRequest, rootLocFetched);
      if (objRequest > 0) {
	objFetched = getFromSharedStack(&lw->objs, &ss->work.objs, objRequest);
	segRequest = MakeFraction(segRequest, objRequest, objFetched);
	if (segRequest > 0)
	  segFetched = getFromSharedStack(&lw->segments, &ss->work.segments, segSize * segRequest);
      }
    }
  }
  assert(ss->numLocalStack >= 0);
  if (stackletFetched || globalLocFetched || rootLocFetched || objFetched || segFetched) {
    assert(lw->hasShared == 0);
    lw->hasShared = 1;
    FetchAndAdd(&ss->numLocalStack,1);  
  }
  FetchAndAdd(&ss->numPop, 1);
  exitRoom(ss->threeRoom);
}


static void helpPushSharedStack(SharedStack_t *ss, LocalWork_t *lw)
{
  moveToSharedStack(&ss->work.stacklets, &lw->stacklets);
  moveToSharedStack(&ss->work.globals, &lw->globals);
  moveToSharedStack(&ss->work.roots, &lw->roots);
  moveToSharedStack(&ss->work.objs, &lw->objs);
  moveToSharedStack(&ss->work.segments, &lw->segments);
}


int pushSharedStack(int conditional, SharedStack_t *ss, LocalWork_t *lw)
{
  int empty;
  enterRoom(ss->threeRoom,conditional ? 2 : 1);
  helpPushSharedStack(ss, lw);
  if (!conditional) {
    if (lw->hasShared == 1) {
      FetchAndAdd(&ss->numLocalStack,-1);  
      lw->hasShared = 0;
    }
  }
  else 
    assert(lw->hasShared == 0);
  assert(ss->numLocalStack >= 0);
  empty = (exitRoom(ss->threeRoom) == StackEmpty);
  FetchAndAdd(&ss->numPush, 1);
  return empty;
}
