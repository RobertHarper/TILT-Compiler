#include "general.h"
#include "queue.h"


Queue_t *QueueCreate(long size)
{
  Queue_t *res = (Queue_t *)malloc(sizeof(Queue_t));
  long i;

  if (size < 10)
    size = 10;
  res->size = size;
  res->table = (void **)malloc(size*sizeof(void *));
  res->start = 0;
  res->end = 0;

  return res;
}


void QueueDestroy(Queue_t *q)
{
  free(q->table);
  free(q);
}

void *QueuePop(Queue_t *q)
{
  int pos;
  void *res = NULL;
  if (QueueIsEmpty(q))
    return res;
  pos = q->end - 1;
  if (pos < 0)
    pos += q->size;
  res = q->table[pos];
  q->end = pos;
  return res;
}

void *QueuePopPeek(Queue_t *q)
{
  void *res = NULL;
  int pos;
  assert(!QueueIsEmpty(q));
  pos = (q->end - 1 + q->size);
  pos = q->end - 1;
  if (pos < 0)
    pos += q->size;
  res = q->table[pos];
  return res;
}

void QueueResize(Queue_t *q, int addSize)
{
  int oldsize = q->size;
  int newsize = oldsize + addSize;
  void **oldtable = q->table;
  void **newtable = (void **)malloc(sizeof(void *) * newsize);

  int newpos = 0, oldpos = q->start;
  int oldend = q->end;
  while (oldpos != oldend) {
    newtable[newpos++] = oldtable[oldpos++];
    if (oldpos >= oldsize)
      oldpos -= oldsize;
  }
  q->start = 0;
  q->end = newpos;
  q->table = newtable;
  q->size  = newsize;
  free(oldtable);
}


void *QueueAccess(Queue_t *q, long logPos)
{
  int len = QueueLength(q);
  int pos = logPos + q->start;
  void *res;
  assert(logPos < len);
  if (pos >= q->size)
    pos -= q->size;
  res = q->table[pos];
  return res;
}

void QueueSet(Queue_t *q, long logPos, void *data)
{
  long pos;
  assert(logPos < QueueLength(q));
  pos = logPos + q->start;
  if (pos >= q->size)
    pos -= q->size;
  q->table[pos] = data;
}

void QueueClear(Queue_t *q)
{
  q->start = 0;
  q->end = 0;
}

void QueueCopy(Queue_t *target, Queue_t *src)
{
  int i;
  int srcLength = QueueLength(src), targetLength = QueueLength(target);
  
  if (targetLength + srcLength + 1 >= target->size)
    QueueResize(target, target->size + srcLength);
  for (i=0; i<srcLength; i++) {
    int srcPos = src->start + i;
    if (srcPos >= src->size)
      srcPos -= src->size;
    target->table[target->end++] =  src->table[srcPos];
    if (target->end >= target->size)
      target->end -= target->size;
  }
}



void copyStack(Stack_t *from, Stack_t *to)      /* non-destructive operation on from */
{
  int i;
  if (from->cursor + to->cursor + 1 >= to->size)
    resizeStack(to, from->cursor + to->size);
  memcpy(&to->data[to->cursor], &from->data[0], from->cursor * sizeof(ptr_t));
  to->cursor += from->cursor;
  assert(to->cursor < to->size);
}

void transferStack(Stack_t *from, Stack_t *to)  /* destructive operation on from */
{
  copyStack(from,to);
  from->cursor = 0;
}

void allocStack(Stack_t *ostack, int size)
{
  ostack->cursor = 0;
  ostack->size = size;
  ostack->data = (ptr_t *) malloc(size * sizeof(ptr_t));
  memset((void *)ostack->data, 0, size * sizeof(ptr_t));
}

Stack_t *createStack(int size)
{
  Stack_t *res = (Stack_t *) malloc(sizeof(Stack_t));
  allocStack(res, size);
  return res;
}

void resizeStack(Stack_t *ostack, int newSize)
{
  int i;
  ptr_t *newData = (ptr_t *) malloc(newSize * sizeof(ptr_t));
  assert(ostack->cursor < newSize);
  for (i=0; i<ostack->size; i++)
    newData[i] = ostack->data[i];
  free(ostack->data);
  ostack->data = newData;
  ostack->size = newSize;
}

