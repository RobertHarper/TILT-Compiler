#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "gc_para.h"


SharedStack_t *SharedStack_Alloc(int size)
{
  SharedStack_t *ss = (SharedStack_t *) malloc(sizeof(SharedStack_t));
  ss->size = size;
  ss->stack = (ptr_t *) malloc(sizeof(ptr_t) * size);
  ss->cursor = 0;
  ss->Gate = ss->Turn1 = ss->Turn2 = 0;
  return ss;
}

void SynchStart(SharedStack_t *ss)
{
  while (ss->Gate) 
   flushStore();  /* Without this, updates of ss->Gate from other processors are not visible here */
  FetchAndAdd(&ss->Turn1, 1);
  while (ss->Gate) {
    FetchAndAdd(&ss->Turn1, -1);
    while (ss->Gate)
      flushStore();
    FetchAndAdd(&ss->Turn1, 1);
  }
  flushStore();   /* Without this, the change to ss->Turn1 is not visible */
}

void SynchMid(SharedStack_t *ss)
{
  ss->Gate = 1;
  flushStore(); /* for updating ss->Gate */
  FetchAndAdd(&ss->Turn2, 1);
  FetchAndAdd(&ss->Turn1, -1);
  while (ss->Turn1 > 0) 
    flushStore();
}

int isEmptyGlobalStack(SharedStack_t *ss)
{
  return (ss->cursor == 0);
}

/* Returns 1 if gate was closed and global stack empty; i.e. global stack can not become non-empty */
int SynchEnd(SharedStack_t *ss, Thunk_t *thunk)
{
  int i = FetchAndAdd(&ss->Turn2, -1);
  if (i==1) {     /* Last one to decrement */
    int empty = isEmptyGlobalStack(ss);
    if (empty && (thunk != NULL))
      (*thunk)();
    ss->Gate = 0;
    flushStore(); /* for updating ss->Gate */
    return empty;
  }
  return 0;
}

void moveToGlobalStack(SharedStack_t *ss, LocalStack_t *localStack)
{
  int i;
  int oldCursor = FetchAndAdd(&ss->cursor,localStack->cursor);
  for (i=0; i<localStack->cursor; i++) 
    ss->stack[oldCursor+i] = localStack->stack[i];
  localStack->cursor = 0;
  if (ss->cursor >= ss->size) {
    printf("Shared stack overflowed with %d items\n", ss->cursor);
    assert(0);
  }
}

void fetchFromGlobalStack(SharedStack_t *ss, LocalStack_t *localStack, int numToFetch)
{
  int i, localCursor = localStack->cursor;
  int oldCursor = FetchAndAdd(&ss->cursor,-numToFetch);
  /* Handle overreach */
  if (oldCursor < numToFetch) {  
    numToFetch = oldCursor;
    ss->cursor = 0;
  }
  for (i=0; i<numToFetch; i++)
    localStack->stack[localCursor++] = ss->stack[oldCursor - (i + 1)];
  localStack->cursor = localCursor;
}

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
