#ifndef _barrier_h
#define _barrier_h

typedef struct Barriers__t {
  int numProcessor;   /* Number of processors to synchronize */
  int maxBarrier;     /* Total number of barrier */
  long *barriers;     /* Number of processors at each barrier */
  int phase;          /* Number of barriers we are using. Must be >= 3.  Reset with lastStrongBarrier. */
} Barriers_t;

Barriers_t *createBarriers(int size, int phase);
int strongBarrier(Barriers_t *, int *which); /* Returns index of arrival after all processors have reached barrier */
int weakBarrier(Barriers_t *, int *which);   /* Returns index of arrival immediately - no barrier semantics */
int checkBarrier(Barriers_t *, int which);  /* Returns next index of arrival immediately */

#endif
