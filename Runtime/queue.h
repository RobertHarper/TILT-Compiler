/* Thread-safety controlled by instance creation. */

#ifndef _queue_h
#define _queue_h

#include <pthread.h>
#include "general.h"
#include "tag.h"

/* A set is a collection of non-NULL values with 2 different sets of operations: stack-like or queue-like. */
typedef struct Set__t
{
  long size;         /* Size of data array */
  ptr_t *data;       /* Array of data elements - layout depends on whether accessed as a stack or queue */
  ptr_t *first;
  ptr_t *last;
  ptr_t *limit;     /* equals s->data + size */
                    /* The data elements are from first to last - 1 with no wraparound.  
		       If first = last, then the set is empty.  
		       If size = last + 1, then the set is full.  The last slot of the data array at last is always NULL.
		       When access is always stack-like, then first will be zero. Stack operations may be used
		       only if no queue-like operations are ever used since stack operations rely on first being zero.

		       The stack-like operations are Push and Pop.  
		           When last + 1 equals size on a Push, the stack is increased in size.
		       The queue-like operations are Push and Dequeue.
		           When last + 1 equals size on a Push, the queue is normalized.
			   If the queue is mostly full (75%), normalization will increase the size.
			   In any case the elements are shifted downwards so that first becomes 0.
		   */
} Set_t;

Set_t*   SetCreate (long initialSize);
void     SetInit (Set_t *, long initialSize);
void     SetDestroy(Set_t *);
void     SetNormalize (Set_t *);                /* There will always be at least 3 slots free after a call to Normalize */
void     SetNormalizeExpand(Set_t *s, int addSize);
void     SetCopy(Set_t *from, Set_t *to);       /* Copies the contents of from into to without changing from */
void     SetTransfer(Set_t *from, Set_t *to);   /* Transfers the contents of from into to, leaving from empty */

INLINE(SetLength)
long SetLength(Set_t *s)
{
  return s->last - s->first;
}

INLINE(SetFullSize)
long SetFullSize(Set_t *s)
{
  return s->size;
}

INLINE(SetReset)
void SetReset(Set_t *s)
{
  s->first = s->data;
  s->last = s->data;
}

INLINE(SetIsEmpty)
int SetIsEmpty(Set_t *s)
{
  return (s->first == s->last);
}

INLINE(SetPush)
void SetPush(Set_t *set, ptr_t item)
{
  fastAssert(item != NULL); 
  if (set->last + 1 >= set->limit)
    SetNormalize(set);
  *(set->last++) = item;
}

INLINE(SetPush2)
void SetPush2(Set_t *set, ptr_t item1, ptr_t item2)
{
  fastAssert(item1 != NULL); 
  fastAssert(item2 != NULL); 
  if (set->last + 2 >= set->limit)
    SetNormalize(set);
  *(set->last++) = item1;
  *(set->last++) = item2;
}

INLINE(SetPush3)
void SetPush3(Set_t *set, ptr_t item1, ptr_t item2, ptr_t item3)
{
  fastAssert(item1 != NULL); 
  fastAssert(item2 != NULL); 
  fastAssert(item3 != NULL); 
  if (set->last + 3 >= set->limit)
    SetNormalize(set);
  *(set->last++) = item1;
  *(set->last++) = item2;
  *(set->last++) = item3;
}

/* Dequeue and SetPop* return NULL if the set is empty */
INLINE(SetDequeue)
ptr_t SetDequeue(Set_t *s)
{
  if (s->first == s->last)
    return NULL;
  return *(s->first++);
}

INLINE(SetPop)
ptr_t SetPop(Set_t *set)
{
  /*  fastAssert(set->first == set->data); */
  if (set->first == set->last) 
    return NULL;
  return *(--set->last);
}

INLINE(SetPop2)
ptr_t SetPop2(Set_t *set, ptr_t *item2Ref)
{
  fastAssert(set->first == set->data);
  if (set->last > set->first + 1) {
    *item2Ref = *(--set->last);  /* In reverse order of push */
    return *(--set->last);
  }
  return NULL;
}

INLINE(SetPop3)
ptr_t SetPop3(Set_t *set, ptr_t *item2Ref, ptr_t *item3Ref)
{
  fastAssert(set->first == set->data);
  if (set->last > set->first + 2) {
    *item3Ref = set->last[-1];  /* In reverse order of push */
    *item2Ref = set->last[-2];  /* In reverse order of push */
    set->last -= 3;
    return *set->last;
  }
  return NULL;
}


#endif
