#ifndef _rooms_h
#define _rooms_h

typedef int (*finalizer_t)(void *);
typedef int (*logger_t)(int, int, void *);

typedef struct Rooms__t {
  int *cur;
  int *wait;
  int *prev;
  finalizer_t *finalizer;
  void **finalizerData;
  logger_t logger;
  void *loggerData;
  int size;
  int which;
  int lock;
} Rooms_t;

Rooms_t* createRooms(int n);
void     destroyRooms(Rooms_t *);
void     assignExitCode(Rooms_t *r, int i, finalizer_t f, void *d);
void     assignLogCode(Rooms_t *r, int i, logger_t f, void * d);
void     enterRoom(Rooms_t *r, int i);
int      exitRoom(Rooms_t *r);  /* Not last to leave: Returns 0
				   Last but no finalizer: Returns 1
				   Last and finalizer: Returns result of call to finalizer
				*/
int      changeRoom(Rooms_t *r, int i);  /* Equivalent to:
					       exitRoom(r); 
					       enterRoom(r,i); 
					    However, guarantees entry into room i immediately if room i
					    is the next room to be active.
					 */
#endif
