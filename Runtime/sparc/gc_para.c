#include "s.h"
#include "r.h"
#include "sparc.h"

/*
	SharedStack Functions
*/
static int
internalIsEmptySharedStack(SharedStack_t* ss)
{
	return (isLocalWorkEmpty(&ss->work) && ss->numLocalStack == 0);
}

enum {
	StackEmpty = 2,
	StackNotEmpty = 3,
};

static int
finalizer(void* d)
{
	SharedStack_t* ss = (SharedStack_t*) d;
	if (internalIsEmptySharedStack(ss))
		return StackEmpty;
	else
		return StackNotEmpty;
}

SharedStack_t*
SharedStack_Alloc(int doubleProcess, int stackletSize, int globalLocSize,
	int rootLocSize, int objSize, int segmentSize, int backObjSize,
	int backLocSize)
{
	SharedStack_t *ss = (SharedStack_t *) malloc(sizeof(SharedStack_t));
	ss->doubleProcess = doubleProcess;
	init_localWork(&ss->work, objSize, segmentSize, globalLocSize,
		rootLocSize, stackletSize, objSize, backLocSize);
	ss->numLocalStack = 0;
	ss->threeRoom = createRooms(3);
	/* check if stack empty in push room */
	assignExitCode(ss->threeRoom, 1, &finalizer, (void *) ss);
	ss->numPush = ss->numPop = 0;
	return ss;
}

/* Transfer all items from 'from' to 'to' */
static void
moveToSharedStack(Set_t* to, Set_t* from)
{
	int numToTransfer = SetLength(from);
	ptr_t * oldToCursor = (ptr_t *) FetchAndAdd(
		(long *)&to->last,
		4 * numToTransfer);
	if (to->last >= to->limit && to->limit > to->data) {
		fprintf(stderr,"Shared stack %lx of size %ld overflowed with %d items\n",
			(long)to, to->size, to->last - to->data);
		DIE("shared stack overflow");
	}
	memcpy(oldToCursor, from->first, sizeof(val_t) * numToTransfer);
	from->last = from->data;
}

/* Transfer up to numToFetch items from 'from' to 'to', handling overreach */
static int
getFromSharedStack(Set_t* to, Set_t* from, long numToFetch)
{
	/* FetchAndAdd returns the pre-added value */
	ptr_t *oldFromCursor = (ptr_t *) FetchAndAdd(
		(long *)&from->last,
		-(sizeof(val_t) * numToFetch));
	ptr_t *newFromCursor = oldFromCursor - numToFetch;
	if (oldFromCursor <= from->data) {	/* Handle complete overreach */
		numToFetch = 0;
		/*
			Multiple processors might execute this; ok since there are
			no increments.
		*/
		from->last = from->data;
	}
	else if (newFromCursor < from->data) {	/* Handle partial overreach */
		/* Fetching fewer items than requested */
		numToFetch -= (from->data - newFromCursor);
		/* Recompute newFromCursor */
		newFromCursor = oldFromCursor - numToFetch;
		assert(numToFetch > 0);
		/*
			Multiple processors might execute this; ok since there are
			no increments
		*/
		from->last = from->data;
	}
	memcpy(to->last, newFromCursor, sizeof(val_t) * numToFetch);
	to->last += numToFetch;
	assert(to->last < to->limit);
	return numToFetch;
}

int
isEmptySharedStack(SharedStack_t* ss)
{
	int empty;
	enterRoom(ss->threeRoom,1);	/* Enter push room */
	/* If empty in push room, stack is really empty at this point */
	empty = internalIsEmptySharedStack(ss);
	exitRoom(ss->threeRoom);
	return empty;
}

void
resetSharedStack(SharedStack_t* ss, LocalWork_t* lw, int getNext)
{
	int position;
	assert(lw->hasShared == 0);
	lw->hasShared = 1;
	position = FetchAndAdd(&ss->numLocalStack, 1);
	if (position == 0 && getNext) {
		assert(ss->doubleProcess);
		SetTransfer(&ss->work.nextBackObjs, &ss->work.backObjs);
		SetTransfer(&ss->work.nextBackLocs, &ss->work.backLocs);
	}
}

void
discardNextSharedStack(SharedStack_t* ss)
{
	SetReset(&ss->work.nextBackObjs);
	SetReset(&ss->work.nextBackLocs);
}

static int segSize = 3;	/* Number of items that constitute a segment */
static int regionSize = 2; /* Number of items that constitute a region */

#define MakeFraction(a,b,c) ((int) ((a) * (((double)((b) - (c))) / (b))))
void
popSharedStack(SharedStack_t* ss, LocalWork_t* lw)
{
	int stackletFetched = 0;
	int globalLocFetched = 0;
	int rootLocFetched = 0;
	int grayRegionFetched = 0;
	int objFetched = 0;
	int segFetched = 0;
	int backLocFetched = 0;
	int backObjFetched = 0;
	int stackletRequest = threadFetchSize;
	int globalLocRequest = globalLocFetchSize;
	int rootLocRequest = rootLocFetchSize;
	int grayRegionRequest = grayRegionFetchSize;
	int objRequest = objFetchSize;
	int segRequest = segFetchSize;
	int backLocRequest = backLocFetchSize;
	int backObjRequest = backObjFetchSize;

	int objSize = SetLength(&ss->work.objs);
	objRequest = Min(objFetchSize, 1 + (int) (objSize / (double) NumProc));

	enterRoom(ss->threeRoom,0);

	stackletFetched = getFromSharedStack(&lw->stacklets,
		&ss->work.stacklets, stackletRequest);
	globalLocRequest = MakeFraction(globalLocRequest, stackletRequest,
		stackletFetched);
	if (globalLocRequest == 0)
		goto done;

	globalLocFetched = getFromSharedStack(&lw->globals,
		&ss->work.globals, globalLocRequest);
	rootLocRequest = MakeFraction(rootLocRequest, globalLocRequest,
		globalLocFetched);
	if (rootLocRequest == 0)
		goto done;

	rootLocFetched = getFromSharedStack(&lw->roots,
		&ss->work.roots, rootLocRequest);
	grayRegionRequest = MakeFraction(grayRegionRequest, rootLocRequest,
		rootLocFetched);
	if (grayRegionRequest == 0)
		goto done;

	grayRegionFetched = getFromSharedStack(&lw->grayRegion,
		&ss->work.grayRegion, regionSize * grayRegionRequest) / regionSize;
	objRequest = MakeFraction(objRequest, grayRegionRequest,
		grayRegionFetched);
	if (objRequest == 0)
		goto done;

	objFetched = getFromSharedStack(&lw->objs, &ss->work.objs, objRequest);
	if ((segRequest = MakeFraction(segRequest, objRequest, objFetched)) == 0)
		goto done;

	segFetched = getFromSharedStack(&lw->segments,
		&ss->work.segments, segSize * segRequest) / segSize;
	backObjRequest = MakeFraction(backObjRequest, segRequest, segFetched);
	if (backObjRequest == 0)
		goto done;

	backObjFetched = getFromSharedStack(&lw->backObjs,
		&ss->work.backObjs, backObjRequest);
	backLocRequest = MakeFraction(backLocRequest, backObjRequest,
		backObjFetched);
	if (backLocRequest == 0)
		goto done;

	backLocFetched = getFromSharedStack(&lw->backLocs,
		&ss->work.backLocs, backLocRequest);

done:
	assert(ss->numLocalStack >= 0);
	if (stackletFetched || globalLocFetched || rootLocFetched
	|| grayRegionFetched || objFetched || segFetched || backObjFetched
	|| backLocFetched) {
		assert(lw->hasShared == 0);
		lw->hasShared = 1;
		FetchAndAdd(&ss->numLocalStack,1);
	}
	FetchAndAdd(&ss->numPop, 1);
	exitRoom(ss->threeRoom);
}

static void
helpPushSharedStack(SharedStack_t* ss, LocalWork_t* lw)
{
	moveToSharedStack(&ss->work.stacklets, &lw->stacklets);
	moveToSharedStack(&ss->work.globals, &lw->globals);
	moveToSharedStack(&ss->work.roots, &lw->roots);
	moveToSharedStack(&ss->work.objs, &lw->objs);
	moveToSharedStack(&ss->work.grayRegion, &lw->grayRegion);
	moveToSharedStack(&ss->work.segments, &lw->segments);
	moveToSharedStack(&ss->work.backObjs, &lw->backObjs);
	moveToSharedStack(&ss->work.backLocs, &lw->backLocs);
	if (ss->doubleProcess) {
		moveToSharedStack(&ss->work.nextBackObjs, &lw->nextBackObjs);
		moveToSharedStack(&ss->work.nextBackLocs, &lw->nextBackLocs);
	}
}

int
pushSharedStack(int conditional, SharedStack_t* ss, LocalWork_t* lw)
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
