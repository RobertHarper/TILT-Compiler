#ifndef _barrier_h
#define _barrier_h

/* There must be at least 3 strong barriers in the cycle for resetting to work. */

typedef struct Barriers__t {
  int numProcessor;            /* Number of processors to synchronize */
  int maxBarrier;              /* Total number of barrier */
  volatile long *barriers;     /* Number of processors at each barrier */
  volatile long **index;       /* index[i][j] is the number of the j-th processor at barrier i */
  volatile int phase;          /* Number of barriers we are using. Must be >= 3.  Reset with lastStrongBarrier. */
  volatile int lastUsed;       /* Last barrier to be used.  We must keep this count in order to reset barriers correctly.
				  Whenever we pass a strong barrier, we clear all barriers from lastUsed up to but
				  excluding the strong barrier and upadte lastUsed to the strong barrier.  No clearing
				  is done on a weak barrier.  */
} Barriers_t;

Barriers_t *createBarriers(int size, int phase);
int strongBarrier(Barriers_t *, Proc_t *); /* Returns index of arrival after all processors have reached barrier */
int weakBarrier(Barriers_t *, Proc_t *);   /* Returns index of arrival immediately - no barrier semantics */
int checkBarrier(Barriers_t *, Proc_t *);  /* Returns next index of arrival immediately */

#endif
