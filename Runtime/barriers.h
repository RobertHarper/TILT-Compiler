#ifndef _barrier_h
#define _barrier_h

typedef struct Barriers__t {
  int size;      /* Number of processors to synchronize */
  int phase;     /* Number of phases >= 3 */
  long *barriers; /* Number of processors at each phase barrier */
} Barriers_t;

Barriers_t *createBarriers(int size, int phase);
int strongBarrier(Barriers_t *, int which); /* Returns index of arrival after all processors have reached barrier */
int weakBarrier(Barriers_t *, int which);   /* Returns index of arrival immediately - no barrier semantics */
int checkBarrier(Barriers_t *, int which);  /* Returns next index of arrival immediately */

#endif
