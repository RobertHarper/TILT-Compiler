#include <strings.h>
#include <stdlib.h>
#include "general.h"
#include "barriers.h"


Barriers_t *createBarriers(int size, int phase)
{
  Barriers_t *b = (Barriers_t *) malloc(sizeof(Barriers_t));
  b->size = size;
  b->phase = phase;
  assert(phase >= 3);
  b->barriers = (long *) calloc(phase, sizeof(long));
  return b;
}

int strongBarrier(Barriers_t *b, int *whichRef)
{
  int which = *whichRef;
  int index = FetchAndAdd(&b->barriers[which], 1);
  *whichRef = (which + 1) % b->phase;
  if (collectDiag >= 2)
    printf("Waiting at barrier %d for %d more processors\n", which, b->size - b->barriers[which]);
  while (b->barriers[which] < b->size) 
    flushStore();
  assert(b->barriers[which] == b->size);
  b->barriers[which ? which - 1 : b->phase - 1] = 0;  /* Reset previous phases's counter */
  flushStore();
  return index;
}

int weakBarrier(Barriers_t *b, int *whichRef)
{
  int which = *whichRef;
  int index = FetchAndAdd(&b->barriers[which], 1);
  *whichRef = (which + 1) % b->phase;
  b->barriers[which ? which - 1 : b->phase - 1] = 0;  /* Reset previous phases's counter */
  flushStore();
  return index;
}

int checkBarrier(Barriers_t *b, int which)
{
  flushStore();
  return (int) b->barriers[which];
}
