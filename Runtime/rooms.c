#include <stdlib.h>
#include <values.h>
#include <strings.h>
#include <assert.h>
#include "rooms.h"

int FetchAndAdd(int *, int);
int CompareAndSwap(int *location, int testValue, int swapValue); /* Returns value in memory location */

static void lockRooms(Rooms_t *r)
{
  while (CompareAndSwap(&r->lock, 0, 1) != 0)
    ;
}

static void unlockRooms(Rooms_t *r)
{
  r->lock = 0;
}

Rooms_t *createRooms(int n) 
{
  Rooms_t *r = (Rooms_t *) malloc(sizeof(Rooms_t));
  assert(n > 0);
  assert(2 * n < MAXINT);
  r->cur = (int *) calloc(n, sizeof(int));
  r->wait = (int *) calloc(n, sizeof(int));
  r->prev = (int *) calloc(n, sizeof(int));
  r->finalizer = (finalizer_t *) calloc(n, sizeof(finalizer_t));
  r->finalizerData = (void **) calloc(n, sizeof(void *));
  r->size = n;
  r->which = -1; 
  r->lock = 0;
  return r;
}

void destroyRooms(Rooms_t *r)
{
  free(r->cur);
  free(r->wait);
  free(r->prev);
  free(r->finalizer);
  free(r->finalizerData);
  free(r);
}

void assignExitCode(Rooms_t *r, int i, finalizer_t f, void *d) 
{
  lockRooms(r);
  r->finalizer[i] = f;
  r->finalizerData[i] = d;
  unlockRooms(r);
}

void assignLogCode(Rooms_t *r, int i, logger_t f, void * d) 
{
  lockRooms(r);
  r->logger = f;
  r->loggerData = d;
  unlockRooms(r);
}

static void transition(Rooms_t *r, int from, int to)
{
  if (r->logger != NULL) {
    lockRooms(r);
    if (r->logger != NULL)
      (*r->logger)(from,to,r->loggerData);
    unlockRooms(r);
  }
}

void enterRoom(Rooms_t *r, int i) 
{
  int oldWait = FetchAndAdd(&r->wait[i],1) + 1;
  while (oldWait > r->cur[i])
    if (CompareAndSwap(&r->which,-1,i) == -1) {
      r->cur[i] = r->wait[i];
      transition(r,-1,i);
      break;
    }
  assert(r->which == i);
}


int exitRoom(Rooms_t *r) 
{
  int wh = r->which;
  int j = FetchAndAdd(&r->prev[wh],1) + 1, k;
  if (j == r->cur[wh]) {
    int finalizerResult = 1, newWh = wh;
    if (r->finalizer[wh] != NULL) {
      lockRooms(r);
      if (r->finalizer[wh] != NULL) 
	finalizerResult = (*r->finalizer[wh])(r->finalizerData[wh]);
      unlockRooms(r);
    }
    for (k = 0; k < r->size; k++) {
      newWh = (newWh + 1) % r->size;
      if (r->cur[newWh] < r->wait[newWh]) {
        r->which = newWh;
        r->cur[newWh] = r->wait[newWh];
	transition(r,wh,newWh);
	return finalizerResult;
      }
    }
    r->which = -1;
    transition(r,wh,-1);
    return finalizerResult;
  }
  return 0;
}


int changeRoom(Rooms_t *r, int i) 
{
  int wh = r->which;
  int oldWait = FetchAndAdd(&r->wait[i],1) + 1;
  int j = FetchAndAdd(&r->prev[wh],1) + 1, k;
  if (j == r->cur[wh]) {
    int finalizerResult = 1, newWh = wh;
    if (r->finalizer[wh] != NULL) {
      lockRooms(r);
      if (r->finalizer[wh] != NULL) 
	finalizerResult = (*r->finalizer[wh])(r->finalizerData[wh]);
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
    assert(0);
  }
  while (oldWait > r->cur[i])
    ;
  return 0;
}


