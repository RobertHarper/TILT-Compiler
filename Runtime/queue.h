/* Thread-safety controlled by instance creation. */

#ifndef _queue_h
#define _queue_h

#include <pthread.h>
#include "general.h"

struct Queue
{
  long start;   /* First entry of queue */
  long end;     /* Index beyond last entry of queue */
                /* Number of entries is  end - start (+ size if end - start is negative);
		   When empty, start = end.  When full with size - 1 entries, start = end + 1. */
  long size;    /* Size of table */
  void **table; /* Table of entries */
};

typedef struct Queue Queue_t;

void        QueueResize (Queue_t *, int additionalSize);  /* not to be called by client code */
Queue_t    *QueueCreate (long);
void        QueueDestroy(Queue_t *);
void       *QueuePop    (Queue_t *);
void       *QueuePopPeek(Queue_t *);
void       *QueueAccess (Queue_t *, long);
void        QueueSet    (Queue_t *, long, void *);
void        QueueClear  (Queue_t *);
void        QueueCopy(Queue_t *target, Queue_t *src);   /* Copies the contents of src into target without changing src */
/* These are so commonly used that they are inlined. */
/* long        QueueLength (Queue_t *); */
/* int         QueueIsEmpty(Queue_t *); */
/* void       *Dequeue     (Queue_t *); */
/* void        Enqueue     (Queue_t *, void *data); */

INLINE1(QueueLength)
INLINE2(QueueLength)
long QueueLength(Queue_t *q)
{
  int diff = q->end - q->start; /* Up to but not including q->size */
  return (diff >= 0) ? diff : diff + q->size;
}

INLINE1(QueueIsEmpty)
INLINE2(QueueIsEmpty)
int QueueIsEmpty(Queue_t *q)
{
  return (q->start == q->end);
}

INLINE1(Dequeue)
INLINE2(Dequeue)
void *Dequeue(Queue_t *q)
{
  void *res;
  if (QueueIsEmpty(q))
    return NULL;
  res = q->table[q->start];
  q->start++;
  if (q->start >= q->size)
    q->start -= q->size;
  return res;
}

INLINE1(Enqueue)
INLINE2(Enqueue)
void Enqueue(Queue_t *q, void *data)
{
  if (QueueLength(q) + 2 >= q->size)  /* + 2 since last entry of table cannot be used */
    QueueResize(q, q->size);
  q->table[q->end++] = data;
  if (q->end >= q->size)
    q->end -= q->size;
}


#endif
