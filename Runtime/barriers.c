#include <strings.h>
#include <stdlib.h>
#include "general.h"
#include "thread.h"
#include "barriers.h"


Barriers_t *createBarriers(int numProc, int maxBarrier)
{
  int i, j;
  Barriers_t *b = (Barriers_t *) malloc(sizeof(Barriers_t));
  b->numProcessor = numProc;
  b->maxBarrier = maxBarrier;
  b->phase = maxBarrier;
  b->lastUsed = maxBarrier - 1;
  assert(b->phase >= 3); /* Not strong enough.  Must have 3 strong barriers. */
  b->barriers = (long *) calloc(b->maxBarrier, sizeof(long));
  b->index = (volatile long **) calloc(b->maxBarrier, sizeof(long *));
  for (i=0; i<b->maxBarrier; i++) {
    b->index[i] = (long *) calloc(b->numProcessor, sizeof(long));
    for (j=0; j<b->numProcessor; j++) 
      b->index[i][j] = -1;
  }
  return b;
}

int strongBarrier(Barriers_t *b, Proc_t *proc)
{
  int i;
  int which = proc->barrierPhase;
  int index = FetchAndAdd(&b->barriers[which], 1);
  proc->barrierPhase = (which + 1) % b->phase;
  if (collectDiag >= 2)
    printf("Proc %d: Waiting at strong barrier %d.  %d more processors to pass.\n", 
	   proc->procid, which, b->numProcessor - b->barriers[which]);
  b->index[which][index] = proc->procid;
  while (b->barriers[which] < b->numProcessor)
    memBarrier();
  if (collectDiag >= 2)
    printf("Proc %d: Passing strong barrier %d\n", 
	   proc->procid, which, b->numProcessor - b->barriers[which]);
  assert(b->barriers[which] == b->numProcessor);
  i = b->lastUsed;
  while (i != which) {
    b->barriers[i] = 0;  /* Reset counters */
    i = (i + 1) % b->phase;
  }
  b->lastUsed = which;
  memBarrier();
  
  return index;
}

int weakBarrier(Barriers_t *b, Proc_t *proc)
{
  int i;
  int which = proc->barrierPhase;
  int prev = which ? which - 1 : b->phase - 1;
  int index = FetchAndAdd(&b->barriers[which], 1);
  proc->barrierPhase = (which + 1) % b->phase;
  if (collectDiag >= 2)
    printf("Proc %d: Passing weak barrier %d.   %d more processors to pass\n", 
	   proc->procid, which, b->numProcessor - b->barriers[which]);
  b->index[which][index] = proc->procid;
  memBarrier();
  return index;
}

int checkBarrier(Barriers_t *b, Proc_t *proc)
{
  int which = proc->barrierPhase;
  memBarrier();
  return (int) b->barriers[which];
}
