/* Thread-safety controlled by instance creation. */

#ifndef _queue_h
#define _queue_h

#include <pthread.h>
#include "general.h"
#include "tag.h"

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

INLINE(QueueLength)
long QueueLength(Queue_t *q)
{
  int diff = q->end - q->start; /* Up to but not including q->size */
  return (diff >= 0) ? diff : diff + q->size;
}

INLINE(QueueIsEmpty)
int QueueIsEmpty(Queue_t *q)
{
  return (q->start == q->end);
}

INLINE(Dequeue)
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

INLINE(Enqueue)
void Enqueue(Queue_t *q, void *data)
{
  if (QueueLength(q) + 2 >= q->size)  /* + 2 since last entry of table cannot be used */
    QueueResize(q, q->size);
  q->table[q->end++] = data;
  if (q->end >= q->size)
    q->end -= q->size;
}


typedef struct Stack__t
{
  ptr_t *data;
  long cursor;
  long size;
} Stack_t;


void copyStack(Stack_t *from, Stack_t *to);     /* non-destructive operation on from */
void transferStack(Stack_t *from, Stack_t *to); /* destructive operation on from */
void resizeStack(Stack_t *ostack, int newSize);
void allocStack(Stack_t *ostack, int size);     /* allocate the data portion of an existing stack */
Stack_t *createStack(int size);                 /* make a stack from scratch */

INLINE(resetStack)
void resetStack(Stack_t *oStack)
{
  oStack->cursor = 0;
}

INLINE(lengthStack)
int lengthStack(Stack_t *oStack)
{
  return oStack->cursor;
}

INLINE(pushStack)
void pushStack(Stack_t *oStack, ptr_t item)
{
  /* assert(item != NULL); */
  oStack->data[oStack->cursor++] = item;
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);  /* Enough to maintain invariant that it is not full */
}

INLINE(pushStack2)
void pushStack2(Stack_t *oStack, ptr_t item1, ptr_t item2)
{
  /* assert(item != NULL); */
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
  oStack->data[oStack->cursor++] = item1;
  oStack->data[oStack->cursor++] = item2;
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
}

INLINE(pushStack3)
void pushStack3(Stack_t *oStack, ptr_t item1, ptr_t item2, ptr_t item3)
{
  /* assert(item != NULL); */
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
  oStack->data[oStack->cursor++] = item1;
  oStack->data[oStack->cursor++] = item2;
  oStack->data[oStack->cursor++] = item3;
  if (oStack->cursor == oStack->size)
    resizeStack(oStack, 2 * oStack->size);
}

/* Returns NULL if stack is empty */
INLINE(popStack)
ptr_t popStack(Stack_t *oStack)
{
  if (oStack->cursor) {
    return oStack->data[--oStack->cursor];
  }
  return NULL;
}

/* Returns NULL if stack is empty */
INLINE(peekStack)
ptr_t peekStack(Stack_t *oStack)
{
  if (oStack->cursor) {
    return oStack->data[oStack->cursor-1];
  }
  return NULL;
}

/* Returns NULL if stack is empty */
INLINE(popStack2)
ptr_t popStack2(Stack_t *oStack, ptr_t *item2Ref)
{
  if (oStack->cursor > 1) {
    *item2Ref = oStack->data[--oStack->cursor];  /* In reverse order of push */
    return oStack->data[--oStack->cursor];
  }
  return NULL;
}

/* Returns NULL if stack is empty */
INLINE(popStack3)
ptr_t popStack3(Stack_t *oStack, ptr_t *item2Ref, ptr_t *item3Ref)
{
  if (oStack->cursor > 2) {
    *item3Ref = oStack->data[--oStack->cursor];  /* In reverse order of push */
    *item2Ref = oStack->data[--oStack->cursor];  /* In reverse order of push */
    return oStack->data[--oStack->cursor];
  }
  return NULL;
}

INLINE(isEmptyStack)
int isEmptyStack(Stack_t *oStack)
{
  return oStack->cursor == 0;
}

#endif
