#ifndef _nqueue_h
#define _nqueue_h

#define CreateQueueClass(t) \
struct Queue##t             \
{                           \ 
  int start;                \
  int end;                  \
  int size;                 \
  void **table;             \
};                          \

Queue_t    *Create##t##Queue(int);
void        Destroy##t##Queue(Queue##t *);
int         IsEmpty##t##Queue(Queue##t *);
void       *Dequeue(Queue##t *);
void        Enqueue(Queue##t *, t);


/* XXX is not actually dynamic */

Queue_t *CreateQueue(int size)
{
  Queue_t *res = (Queue_t *)malloc(sizeof(Queue_t));
  int i;

  res->size = size;
  res->table = (void **)malloc(size*sizeof(void *));
  res->start = 0;
  res->end = 0;

  return res;
}


void DestroyQueue(Queue_t *q)
{
  free(q->table);
  free(q);
}

int IsEmpty(Queue_t *q)
{
  return q->start == q->end;
}

void *Dequeue(Queue_t *q)
{
  void *res = NULL;
  if (IsEmpty(q))
    return res;
  res = q->table[q->start];
  q->start = (q->start + 1) % q->size;
  return res;
}

void Enqueue(Queue_t *q, void *data)
{
  if ((q->end + 1) % q->size == q->start)
    {
      printf("Queue full\n");
      assert(FALSE);
    }
  q->table[q->end] = data;
  q->end = (q->end + 1) % q->size;
}

#endif
