void SynchStart(void);
void SynchMid(void);
int SynchEnd(void);  /* Returns 1 if gate was closed and global stack empty; i.e. global stack can not become non-empty */

int isEmptyGlobalStack(void);
void moveToGlobalStack(ptr_t *localStack, int *localCursorPtr);
void fetchFromGlobalStack(ptr_t *localStack, int *localCursorPtr, int numToFetch);

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
