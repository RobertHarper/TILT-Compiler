#include "queue.h"
#include <stdlib.h>
#include <assert.h>

Queue_t *QueueCreate(int size)
{
  Queue_t *res = (Queue_t *)malloc(sizeof(Queue_t));
  int i;

  if (size < 10)
    size = 10;
  res->size = size;
  res->table = (void **)malloc(size*sizeof(void *));
  res->start = 0;
  res->end = 0;
  res->len = 0;

  return res;
}


void QueueDestroy(Queue_t *q)
{
  free(q->table);
  free(q);
}

int IsEmpty(Queue_t *q)
{
  return (q->len == 0);
}

void *Dequeue(Queue_t *q)
{
  void *res = NULL;
  if (q->len == 0)
    return res;
  res = q->table[q->start];
  q->start++;
  if (q->start >= q->size)
    q->start -= q->size;
  q->len--;
  return res;
}

void *QueuePop(Queue_t *q)
{
  void *res = NULL;
  int pos = (q->end - 1 + q->size);
  if (pos >= q->size)
    pos -= q->size;
  if (IsEmpty(q))
    return res;
  res = q->table[pos];
  q->end = pos;
  q->len--;
  return res;
}

void *QueuePopPeek(Queue_t *q)
{
  int pos = (q->end - 1 + q->size);
  if (pos >= q->size)
    pos -= q->size;
  if (IsEmpty(q))
    return NULL;
  else
    return q->table[pos];
}

void Enqueue(Queue_t *q, void *data)
{
  /* if table is full, we dynamically resize the queue */
  int qend = q->end, qsize = q->size, qlen = q->len;
  if (qlen + 2 >= qsize)
    {
      void **oldtable = q->table;
      int oldsize = qsize;
      int pos = 0, cur = q->start;
      qsize *= 2;
      q->table = (void **)malloc(sizeof(void *) * qsize);
      while (cur != qend)
	{
	  q->table[pos++] = oldtable[cur];
	  cur++;
	  if (cur >= oldsize)
	    cur -= oldsize;
	}
      qend   = pos;
      q->start = 0;
      q->size  = qsize;
      free(oldtable);
    }
  q->table[qend] = data;
  qend++;
  if (qend >= qsize)
    qend -= qsize;
  q->end = qend;
  q->len = qlen + 1;
}


int QueueLength(Queue_t *q)
{
  return q->len;
}

void *QueueAccess(Queue_t *q, int _pos)
{
  int pos = _pos + q->start;
  if (_pos >= q->len)
    {
      printf("QueueAccess bad\n");
      exit(-1);
    }
  if (pos >= q->size)
    pos -= q->size;
  return q->table[pos];
}

void QueueSet(Queue_t *q, int _pos, void *data)
{
  int pos = _pos + q->start;
  if (_pos >= q->len)
    {
      printf("QueueSet bad\n");
      exit(-1);
    }
  if (pos >= q->size)
    pos -= q->size;
  q->table[pos] = data;
}

void QueueClear(Queue_t *q)
{
#ifdef DEBUG
  memset(q->table,13579,q->size * sizeof(q->table[0]));
#endif
  q->start = 0;
  q->end = 0;
  q->len = 0;
}
