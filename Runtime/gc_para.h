#ifndef _gc_para_h
#define _gc_para_h

#include "tag.h"

/* Functions associated with TwoRoom_t are not exported */

typedef struct TwoRoom__t
{
  long Gate, Turn1, Turn2;
} TwoRoom_t;

/* Shared stack of objects */

typedef struct SharedObjStack__t
{
  ptr_t *data;          /* Stack contains gray objects; this field placed first for better assembly code */
  long size;            /* Stack size */
  long cursor;          /* Index of first unused stack slot */
  long numStack;   /* Number of local stacks that might be non-empty */
  TwoRoom_t twoRoom;    /* Used to support parallel pushes and parallel pops by forbidding concurrent pushes and pops */
} SharedObjStack_t;

SharedObjStack_t *SharedObjStack_Alloc(int);
int isEmptySharedObjStack(SharedObjStack_t *);  /* Requires access to TwoRoom */
void popSharedObjStack(SharedObjStack_t *, Stack_t *, int);   /* Increments numStack if number of items fetched > 0 */
int pushSharedObjStack(SharedObjStack_t *, Stack_t *);        /* Decrements numStack if number of items returned > 0;
									  Returns 1 if global stack empty and numStack is 0;
									  Opens Gate with SynchEnd */
void resetSharedObjStack(SharedObjStack_t *, int);            /* Resets numStack to given number */
int condPushSharedObjStack(SharedObjStack_t *, Stack_t *);    /* Does not decrement numStack;
								      If global stack empty and numStack is 0, return 0;
								      else perform the transfer and return 1 */

/* Shared stack of objects and regions (pairs of ptr_t) */

typedef struct SharedObjRegionStack__t
{
  SharedObjStack_t objStack;
  mem_t regionStack[1024];
  long regionCursor;
} SharedObjRegionStack_t;

SharedObjRegionStack_t *SharedObjRegionStack_Alloc(int);

int isEmptySharedObjRegionStack(SharedObjRegionStack_t *);  /* Requires access to TwoRoom */
void popObjSharedObjRegionStack(SharedObjRegionStack_t *, Stack_t *, int); /* Analagous to above but emptiness depends on region too */
int pushObjSharedObjRegionStack(SharedObjRegionStack_t *, Stack_t *);     /* Analagous to above but emptiness depends on region too */
void resetSharedObjRegionStack(SharedObjRegionStack_t *, int);                  /* Analagous to above but emptiness depends on region too */   
int condPushObjSharedObjRegionStack(SharedObjRegionStack_t *, Stack_t *);  /* Analagous to above but emptiness depends on region too */

int popRegionSharedObjRegionStack(SharedObjRegionStack_t *, Stack_t *regionStk);  /* Returns 1 if one region actually popped */
int condPushRegionSharedObjRegionStack(SharedObjRegionStack_t *, Stack_t *regionStk); /* Returns 1 if pushed */


/* Barrier synchronization is done by counting up a counter from 0 to the number of threads
   that are being synchronized.  When the barrier is passed, the previous counter
   is reset.  This cyclic resetting works as long as there are at least 3 counters.
*/

/* A synchronous barrier routine - returning the counter value before the increment */
long synchBarrier(long *counter, long barrierSize, long *prevCounter);

/* An asychronous version that allows threads to perform work after it has reached
   the barrier but before the other threads have reached the barrier. */
long asynchReachBarrier(long *counter);
long asynchCheckBarrier(long *counter, long barrierSize, long *prevCounter);  /* returns 1 if all threads have reached barrier */

#endif
