#include "general.h"
#include "queue.h"


Queue_t *QueueCreate(int safe, long size)
{
  Queue_t *res = (Queue_t *)malloc(sizeof(Queue_t));
  long i;

  if (size < 10)
    size = 10;
  res->lock = safe ? malloc(sizeof(pthread_mutex_t)) : NULL;
  res->empty_mutex = safe ? malloc(sizeof(pthread_mutex_t)) : NULL;
  res->empty_cond = safe ? malloc(sizeof(pthread_cond_t)) : NULL;
  res->nonempty_mutex = safe ? malloc(sizeof(pthread_mutex_t)) : NULL;
  res->nonempty_cond = safe ? malloc(sizeof(pthread_cond_t)) : NULL;
  if (safe) {
    pthread_mutex_init(res->lock,NULL);
    pthread_mutex_init(res->empty_mutex,NULL);
    pthread_cond_init(res->empty_cond,NULL);
    pthread_mutex_init(res->nonempty_mutex,NULL);
    pthread_cond_init(res->nonempty_cond,NULL);
  }
  res->size = size;
  res->table = (void **)malloc(size*sizeof(void *));
  res->start = 0;
  res->end = 0;
  res->len = 0;

  return res;
}


void QueueDestroy(Queue_t *q)
{
  if (q->lock)
    pthread_mutex_lock(q->lock);
  pthread_mutex_destroy(q->lock);
  free(q->table);
  free(q);
}

int QueueIsEmpty(Queue_t *q)
{
  return (q->len == 0);
}

static void QueueEnter(Queue_t *q) 
{
  if (q->lock)
    pthread_mutex_lock(q->lock);
}

static void QueueExit(Queue_t *q) 
{
  if (q->lock) {
    pthread_mutex_unlock(q->lock);
    if (QueueIsEmpty(q))
      pthread_cond_broadcast(q->empty_cond);
    else
      pthread_cond_broadcast(q->nonempty_cond);
  }
}

void *Dequeue(Queue_t *q)
{
  void *res = NULL;
  QueueEnter(q);
  if (q->len == 0)
    return res;
  res = q->table[q->start];
  q->start++;
  if (q->start >= q->size)
    q->start -= q->size;
  q->len--;
  QueueExit(q);
  return res;
}

void *QueuePop(Queue_t *q)
{
  int pos;
  void *res = NULL;
  QueueEnter(q);
  pos = (q->end - 1 + q->size);
  if (pos >= q->size)
    pos -= q->size;
  if (QueueIsEmpty(q))
    return res;
  res = q->table[pos];
  q->end = pos;
  q->len--;
  QueueExit(q);
  return res;
}

void *QueuePopPeek(Queue_t *q)
{
  void *res = NULL;
  int pos;
  QueueEnter(q);
  pos = (q->end - 1 + q->size);
  if (pos >= q->size)
    pos -= q->size;
  if (!QueueIsEmpty(q))
    res = q->table[pos];
  QueueExit(q);
  return res;
}

void Enqueue(Queue_t *q, void *data)
{
  /* if table is full, we dynamically resize the queue */
  int qend, qsize, qlen;
  QueueEnter(q);
  qend = q->end;
  qsize = q->size;
  qlen = q->len;
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
  QueueExit(q);
}


long QueueLength(Queue_t *q)
{
  return q->len;
}

void *QueueAccess(Queue_t *q, long _pos)
{
  long pos;
  void *res;
  QueueEnter(q);
  pos = _pos + q->start;
#ifdef DEBUG
  if (_pos >= q->len)
    {
      printf("QueueAccess bad\n");
      QueueExit(-1);
    }
#endif
  if (pos >= q->size)
    pos -= q->size;
  res = q->table[pos];
  QueueExit(q);
  return res;
}

void QueueSet(Queue_t *q, long _pos, void *data)
{
  long pos;
  QueueEnter(q);
  pos = _pos + q->start;
#ifdef DEBUG
  if (_pos >= q->len)
    {
      printf("QueueSet bad\n");
      QueueExit(-1);
    }
#endif
  if (pos >= q->size)
    pos -= q->size;
  q->table[pos] = data;
  QueueExit(q);
}

void QueueClear(Queue_t *q)
{
  QueueEnter(q);
#ifdef DEBUG
  memset(q->table,13579,q->size * sizeof(q->table[0]));
#endif
  q->start = 0;
  q->end = 0;
  q->len = 0;
  QueueExit(q);
}

void QueueWaitEmpty(Queue_t *q) 
{
  if (QueueIsEmpty(q))
    return;
  pthread_cond_wait(q->empty_cond, q->empty_mutex);
}

void QueueWaitNonEmpty(Queue_t *q) 
{
  if (!QueueIsEmpty(q))
    return;
  pthread_cond_wait(q->nonempty_cond, q->nonempty_mutex);
}
