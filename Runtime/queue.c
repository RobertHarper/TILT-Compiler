#include "general.h"
#include "queue.h"
#include <string.h>


void SetInit(Set_t *set, long size)
{
  set->size = Max(16,size);
  set->data = (ptr_t *)malloc(size*sizeof(ptr_t));
  memset((void *)set->data, 0, size * sizeof(ptr_t));
  set->first = 0;
  set->last = 0;
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

  memmove(&newData[0], &oldData[s->first], numElem * sizeof(ptr_t));

  if (oldData != newData)
    free(oldData);
  s->first = 0;
  s->last = numElem;
  s->data = newData;
  s->size  = newSize;

  assert(s->last + 3 < s->size);
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
    to->first = to->last = 0;
  if (targetLength + srcLength + 1 >= to->size)
    SetNormalizeExpand(to, srcLength);
  else if (to->first + srcLength + 1 >= to->size)
      SetNormalizeExpand(to, 0);
  assert(to->first + srcLength < to->size);
  memmove(&to->data[to->last], &from->data[from->first], srcLength * sizeof(ptr_t));
  to->last += srcLength;
  assert(from->first <= from->last);
  assert(to->first <= to->last);
}

void SetTransfer(Set_t *from, Set_t *to)  /* destructive operation on from */
{
  SetCopy(from,to);
  SetReset(from);
}

