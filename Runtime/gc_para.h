#ifndef _gc_para_h
#define _gc_para_h

#include "tag.h"
#include "rooms.h"

/* Shared stack of root values, objects, large object segments, stacklets */
typedef struct SharedStack__t
{
  Stack_t stacklet; 
  Stack_t globalLoc;
  Stack_t rootLoc;
  Stack_t obj;
  Stack_t segment; 
  long numLocalStack;   /* Number of local stacks that might be non-empty */
  Rooms_t *threeRoom;   /* Pops, Pushes, Pushes from GCRelease */
} SharedStack_t;

SharedStack_t *SharedStack_Alloc(int stackletSize, int globalLocSize, int rootLocSize, int objSize, int segmentSize);
int isEmptySharedStack(SharedStack_t *);                /* Was is (possibly) empty at some point? - Conservative */
void resetSharedStack(SharedStack_t *, int);            /* Resets numStack to given number */
void popSharedStack(SharedStack_t *, 
		    Stack_t *stacklet, int stackletRequest,
		    Stack_t *globalLoc, int globalLocRequest,
		    Stack_t *rootLoc, int rootLocRequest,
		    Stack_t *obj, int objRequest,
		    Stack_t *segment, int segmentRequest);  /* Increments numStack if number of items fetched > 0 */
int pushSharedStack(int conditional,                        /* conditional = 1 for pushes that don't effect termination */
		    SharedStack_t *, 
		    Stack_t *stacklet, Stack_t *globalLoc, 
		    Stack_t *rootLoc, 
		    Stack_t *obj, Stack_t *segment);        /* Decrements numStack if number of items returned > 0;
							       Returns 1 if global stack empty and numStack is 0 */

#endif
