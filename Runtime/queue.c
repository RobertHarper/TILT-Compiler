#include "general.h"
#include "queue.h"
#include <string.h>


void SetInit(Set_t *set, long size)
{
  set->size = Max(16,size);
  set->data = (ptr_t *)malloc(size*sizeof(ptr_t));
  memset((void *)set->data, 0, size * sizeof(ptr_t));
  set->first = set->data;
  set->last = set->data;
  set->limit = set->data + set->size;
}

Set_t *SetCreate(long size)
{
  Set_t *set = (Set_t *)malloc(sizeof(Set_t));
  SetInit(set, size);
  return set;
}

void SetDestroy(Set_t *s)
{
  free(s->data);
  free(s);
}

void SetNormalizeExpand(Set_t *s, int addSize)
{
  int numElem = s->last - s->first;
  int effSize = numElem + addSize;
  ptr_t *oldData = s->data;
  int newSize = s->size;
  ptr_t *newData = oldData;

  if (4 + effSize >=    s->size ||
      4 * effSize >= 3 * s->size) {
    newSize = 2 * effSize;
    newData = (ptr_t *) malloc(newSize * sizeof(ptr_t));
  }

  memmove(newData, s->first, numElem * sizeof(ptr_t));

  if (oldData != newData)
    free(oldData);
  s->data = newData;
  s->first = s->data;
  s->last = s->data + numElem;
  s->size  = newSize;
  s->limit = s->data + s->size;

  assert(s->last + 3 < s->limit);
}

void SetNormalize(Set_t *s) 
{
  SetNormalizeExpand(s, 0);
}

void SetCopy(Set_t *from, Set_t *to)
{
  int i, srcLength = SetLength(from), targetLength = SetLength(to);
  
  assert(from->first <= from->last);
  assert(to->first <= to->last);
  if (to->first == to->last)
    to->first = to->last = to->data;
  if (targetLength + srcLength + 1 >= to->size)
    SetNormalizeExpand(to, srcLength);
  else if (to->first + srcLength + 1 >= to->limit)
      SetNormalizeExpand(to, 0);
  assert(to->first + srcLength < to->limit);
  memmove(to->last, from->first, srcLength * sizeof(ptr_t));
  to->last += srcLength;
  assert(from->first <= from->last);
  assert(to->first <= to->last);
}

void SetTransfer(Set_t *from, Set_t *to)  /* destructive operation on from */
{
  SetCopy(from,to);
  SetReset(from);
}

