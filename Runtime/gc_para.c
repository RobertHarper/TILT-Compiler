#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "gc_para.h"


static ptr_t SharedStack[4096];     /* stack contains gray objects */
static long SharedCursor = 0;
static long Gate = 0, Turn1 = 0, Turn2 = 0;

void SynchStart(void)
{
  while (Gate);
  FetchAndAdd(&Turn1, 1);
  while (Gate)
    {
      FetchAndAdd(&Turn1, -1);
      while (Gate);
      FetchAndAdd(&Turn1, 1);
    }
}

void SynchMid(void)
{
  Gate = 1;
  FetchAndAdd(&Turn2, 1);
  FetchAndAdd(&Turn1, -1);
  while (Turn1 > 0);
}

void SynchEnd(void)
{
  int i = FetchAndAdd(&Turn2, -1);
  if (i==1) {
    Gate = 0;
  }
}

int isEmptyGlobalStack()
{
  return (SharedCursor == 0);
}

void moveToGlobalStack(ptr_t *localStack, int *localCursorPtr)
{
  int i;
  int oldSharedCursor = FetchAndAdd(&SharedCursor,*localCursorPtr);
  for (i=0; i<*localCursorPtr; i++) 
    SharedStack[oldSharedCursor+i] = localStack[i];
  *localCursorPtr = 0;
}

void fetchFromGlobalStack(ptr_t *localStack, int *localCursorPtr, int numToFetch)
{
  int i, localCursor = *localCursorPtr;
  int oldSharedCursor = FetchAndAdd(&SharedCursor,-numToFetch);
  /* Handle overreach */
  if (oldSharedCursor < numToFetch) {  
    numToFetch = oldSharedCursor;
    SharedCursor = 0;
  }
  for (i=0; i<numToFetch; i++)
    localStack[localCursor++] = SharedStack[oldSharedCursor - (i + 1)];
  *localCursorPtr = localCursor;
}
