/* Thread-safety controlled by instance creation. */

#ifndef _queue_h
#define _queue_h

#include <pthread.h>

struct Queue
{
  pthread_mutex_t* lock;
  pthread_mutex_t* empty_mutex;
  pthread_cond_t* empty_cond;
  pthread_mutex_t* nonempty_mutex;
  pthread_cond_t* nonempty_cond;
  long start;
  long end;
  long len;
  long size;
  void **table;
};

typedef struct Queue Queue_t;

Queue_t    *QueueCreate (int, long); /* thread-safe, size */
void        QueueDestroy(Queue_t *);
int         QueueIsEmpty(Queue_t *);
void       *Dequeue     (Queue_t *);
void       *QueuePop    (Queue_t *);
void       *QueuePopPeek(Queue_t *);
void        Enqueue     (Queue_t *, void *data);
long        QueueLength (Queue_t *);
void       *QueueAccess (Queue_t *, long);
void        QueueSet    (Queue_t *, long, void *);
void        QueueClear  (Queue_t *);
void        QueueWaitEmpty (Queue_t *);
void        QueueWaitNonEmpty (Queue_t *);
#endif
