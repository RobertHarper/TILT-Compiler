#ifndef _queue_h
#define _queue_h

struct Queue
{
  long start;
  long end;
  long len;
  long size;
  void **table;
};

typedef struct Queue Queue_t;

Queue_t    *QueueCreate (long);
void        QueueDestroy(Queue_t *);
int         IsEmpty     (Queue_t *);
void       *Dequeue     (Queue_t *);
void       *QueuePop    (Queue_t *);
void       *QueuePopPeek(Queue_t *);
void        Enqueue     (Queue_t *, void *data);
void        Enqueue2     (Queue_t *, void *data);
void        Enqueue3     (Queue_t *, void *data);
long         QueueLength (Queue_t *);
void       *QueueAccess (Queue_t *, long);
void        QueueSet    (Queue_t *, long, void *);
void        QueueClear  (Queue_t *);

#endif
