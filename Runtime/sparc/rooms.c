#include "s.h"
#include "r.h"
#include "sparc.h"

static void
lockRooms(Rooms_t *r)
{
	while (CompareAndSwap(&r->lock, 0, 1) != 0)
		;
}

static void
unlockRooms(Rooms_t* r)
{
	r->lock = 0;
}

Rooms_t*
createRooms(int n)
{
	Rooms_t *r = (Rooms_t *)emalloc(sizeof(Rooms_t));
	assert(n > 0);
	assert(2 * n < INT_MAX);
	r->cur = (long *) ecalloc(n, sizeof(long));
	r->wait = (long *) ecalloc(n, sizeof(long));
	r->prev = (long *) ecalloc(n, sizeof(long));
	r->finalizer = (finalizer_t *) ecalloc(n, sizeof(finalizer_t));
	r->finalizerData = (void **) ecalloc(n, sizeof(void *));
	r->size = n;
	r->which = -1;
	r->lock = 0;
	return r;
}

void
destroyRooms(Rooms_t* r)
{
	efree(r->cur);
	efree(r->wait);
	efree(r->prev);
	efree(r->finalizer);
	efree(r->finalizerData);
	efree((void *)r);
}

void
assignExitCode(Rooms_t* r, int i, finalizer_t f, void* d)
{
	lockRooms(r);
	r->finalizer[i] = f;
	r->finalizerData[i] = d;
	unlockRooms(r);
}

void
assignLogCode(Rooms_t* r, int i, logger_t f, void* d)
{
	lockRooms(r);
	r->logger = f;
	r->loggerData = d;
	unlockRooms(r);
}

static void
transition(Rooms_t* r, int from, int to)
{
	if (r->logger != NULL) {
		lockRooms(r);
		if (r->logger != NULL)
			(*r->logger)(from,to,r->loggerData);
		unlockRooms(r);
	}
}

void
enterRoom(Rooms_t* r, int i)
{
	int oldWait = FetchAndAdd(&r->wait[i],1) + 1;
	while (oldWait > r->cur[i]) {
		memBarrier();
		if (CompareAndSwap(&r->which,-1,i) == -1) {
			r->cur[i] = r->wait[i];
			transition(r,-1,i);
			break;
		}
	}
	assert(r->which == i);
	memBarrier();
}

int
exitRoom(Rooms_t* r)
{
	int wh = r->which;
	int j = FetchAndAdd(&r->prev[wh],1) + 1, k;
	if (j == r->cur[wh]) {
		int finalizerResult = 1, newWh = wh;
		if (r->finalizer[wh] != NULL) {
			lockRooms(r);
			if (r->finalizer[wh] != NULL)
				finalizerResult = (*r->finalizer[wh])
					(r->finalizerData[wh]);
			unlockRooms(r);
		}
		for (k = 0; k < r->size; k++) {
			newWh = (newWh + 1) % r->size;
			if (r->cur[newWh] < r->wait[newWh]) {
				r->which = newWh;
				r->cur[newWh] = r->wait[newWh];
				transition(r,wh,newWh);
				memBarrier();
				return finalizerResult;
			}
		}
		r->which = -1;
		transition(r,wh,-1);
		memBarrier();
		return finalizerResult;
	}
	return 0;
}

int
changeRoom(Rooms_t* r, int i)
{
	int wh = r->which;
	int oldWait = FetchAndAdd(&r->wait[i],1) + 1;
	int j = FetchAndAdd(&r->prev[wh],1) + 1, k;
	if (j == r->cur[wh]) {
		int finalizerResult = 1, newWh = wh;
		if (r->finalizer[wh] != NULL) {
			lockRooms(r);
			if (r->finalizer[wh] != NULL)
				finalizerResult = (*r->finalizer[wh])
					(r->finalizerData[wh]);
			unlockRooms(r);
		}
		for (k = 0; k < r->size; k++) {
			newWh = (newWh + 1) % r->size;
			if (r->cur[newWh] < r->wait[newWh]) {
				r->which = newWh;
				r->cur[newWh] = r->wait[newWh];
				transition(r,wh,newWh);
				while (oldWait > r->cur[i])
					;
				return finalizerResult;
			}
		}
		DIE("changeRoom");
	}
	while (oldWait > r->cur[i])
		;
	return 0;
}
