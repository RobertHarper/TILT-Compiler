#ifndef _gc_para_h
#define _gc_para_h

#include "tag.h"
#include "rooms.h"

/* Shared stack of root values, objects, large object segments, stacklets */
typedef struct SharedStack__t
{
  /* YYYYY
     Stack_t  stacklet; 
  Stack_t  globalLoc;
  Stack_t  rootLoc;
  Stack_t  obj;
  Stack_t  segment; 
  */
  LocalWork_t        work;
  volatile long      numLocalStack;   /* Number of local stacks that might be non-empty */
  Rooms_t           *threeRoom;   /* Pops, Pushes, Pushes from GCRelease */
  volatile long      numPush;
  volatile long      numPop;
} SharedStack_t;

SharedStack_t *SharedStack_Alloc(int stackletSize, int globalLocSize, int rootLocSize, int objSize, int segmentSize);
int isEmptySharedStack(SharedStack_t *);                /* Was is (possibly) empty at some point? - Conservative */
void resetSharedStack(SharedStack_t *, LocalWork_t *);          /* Make local stack seem non-empty */
void popSharedStack(SharedStack_t *ss, LocalWork_t *lw,
		    int stackletRequest,
		    int globalLocRequest,
		    int rootLocRequest,
		    int objRequest,
		    int segmentRequest);  /* Increments ss->numStack and lw->hasShared if number of items fetched > 0 */

int pushSharedStack(int conditional,                        /* conditional = 1 for pushes that don't effect termination */
		    SharedStack_t *, LocalWork_t *lw);
                                                            /* Decrements ss->numStack and resets lw->hasShared to 0 if lw->hasShared was 1
							       Returns 1 if global stack empty and numStack is 0 */

#endif
