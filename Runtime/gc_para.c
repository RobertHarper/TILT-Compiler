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
  return (ss->stacklet.cursor == 0 && ss->globalLoc.cursor == 0 && ss->rootLoc.cursor == 0 && 
	  ss->obj.cursor == 0 && ss->segment.cursor == 0 && ss->numLocalStack == 0);
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
  allocStack(&ss->stacklet, stackletSize);
  allocStack(&ss->globalLoc, globalLocSize);
  allocStack(&ss->rootLoc, rootLocSize);
  allocStack(&ss->obj, objSize);
  allocStack(&ss->segment, segmentSize);
  ss->numLocalStack = 0;
  ss->threeRoom = createRooms(3);
  assignExitCode(ss->threeRoom, 1, &finalizer, (void *) ss);  /* check if stack empty in push room */
  return ss;
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
    from->cursor = 0;             /* Multiple processors might execute this; ok since there are no increments */
  }
  else if (newFromCursor < 0) {   /* Handle partial overreach */
    numToFetch += newFromCursor;  /* Fetching fewer items than requested */
    newFromCursor = oldFromCursor - numToFetch; /* Recompute newFromCursor */
    from->cursor = 0;             /* Multiple processors might execute this; ok since there are no increments */
  }
  memcpy(&to->data[to->cursor], &from->data[newFromCursor], sizeof(val_t) * numToFetch);
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

void resetSharedStack(SharedStack_t *ss, int n)
{
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
  enterRoom(ss->threeRoom,0);
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
  exitRoom(ss->threeRoom);
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


int pushSharedStack(int conditional,
		    SharedStack_t *ss, Stack_t *stacklet, Stack_t *globalLoc, 
		    Stack_t *rootLoc, Stack_t *obj, Stack_t *segment)
{
  int empty;
  enterRoom(ss->threeRoom,conditional ? 2 : 1);
  helpPushSharedStack(ss, stacklet, globalLoc, rootLoc, obj, segment);
  if (!conditional)
    FetchAndAdd(&ss->numLocalStack,-1);  
  assert(ss->numLocalStack >= 0);
  empty = (exitRoom(ss->threeRoom) == StackEmpty);
  return empty;
}
