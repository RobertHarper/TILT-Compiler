#ifndef _queue_h
#define _queue_h

struct Queue
{
  int start;
  int end;
  int len;
  int size;
  void **table;
};

typedef struct Queue Queue_t;

Queue_t    *QueueCreate (int);
void        QueueDestroy(Queue_t *);
int         IsEmpty     (Queue_t *);
void       *Dequeue     (Queue_t *);
void       *QueuePop    (Queue_t *);
void       *QueuePopPeek(Queue_t *);
void        Enqueue     (Queue_t *, void *data);
int         QueueLength (Queue_t *);
void       *QueueAccess (Queue_t *, int);
void        QueueSet    (Queue_t *, int, void *);
void        QueueClear  (Queue_t *);

#endif
