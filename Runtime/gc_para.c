#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "gc_para.h"


SharedStack_t *SharedStack_Alloc()
{
  SharedStack_t *ss = (SharedStack_t *) malloc(sizeof(SharedStack_t));
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
int SynchEnd(SharedStack_t *ss)
{
  int i = FetchAndAdd(&ss->Turn2, -1);
  if (i==1) {
    ss->Gate = 0;
    flushStore(); /* for updating ss->Gate */
    return isEmptyGlobalStack(ss);
  }
  return 0;
}

void moveToGlobalStack(SharedStack_t *ss, ptr_t *localStack, int *localCursorPtr)
{
  int i;
  int oldCursor = FetchAndAdd(&ss->cursor,*localCursorPtr);
  for (i=0; i<*localCursorPtr; i++) 
    ss->stack[oldCursor+i] = localStack[i];
  *localCursorPtr = 0;
}

void fetchFromGlobalStack(SharedStack_t *ss, ptr_t *localStack, int *localCursorPtr, int numToFetch)
{
  int i, localCursor = *localCursorPtr;
  int oldCursor = FetchAndAdd(&ss->cursor,-numToFetch);
  /* Handle overreach */
  if (oldCursor < numToFetch) {  
    numToFetch = oldCursor;
    ss->cursor = 0;
  }
  for (i=0; i<numToFetch; i++)
    localStack[localCursor++] = ss->stack[oldCursor - (i + 1)];
  *localCursorPtr = localCursor;
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
