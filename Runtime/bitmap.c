#include "bitmap.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#undef DEBUG

#ifdef solaris
#define log_size_long 5
#endif
#ifdef alpha_osf
#define log_size_long 6
#endif

Bitmap_t *CreateBitmap(int size)
{
  Bitmap_t *res = (Bitmap_t *)malloc(sizeof(Bitmap_t));
  res->size = size;
  res->pos  = 0;
  res->data = (unsigned long *)calloc(1 + size / (sizeof(unsigned long) * 8),sizeof(unsigned long));
  res->used = 0;
  assert(8 * (sizeof (unsigned long)) == (1 << log_size_long));
  return res;
}

void DestroyBitmap(Bitmap_t *b)
{
  free(b->data);
  free(b);
}

static int GetBit(unsigned long *data, int i)
{
  int long_pos = i >> (log_size_long);
  int bit_pos = i & (sizeof (unsigned long));
  return (data[long_pos] & (1UL << bit_pos));
}

static void SetBit(unsigned long *data, int i)
{
  int long_pos = i >> (log_size_long);
  int bit_pos = i & (sizeof (unsigned long));
  data[long_pos] |= (1UL << bit_pos);
}


static void ClearBit(unsigned long *data, int i)
{
  int long_pos = i >> (log_size_long);
  int bit_pos = i & (sizeof (unsigned long));
  data[long_pos] &= ~(1UL << bit_pos);
}


int IsSet(Bitmap_t *bmp, unsigned int i)
{
  if (i < bmp->size)
    return GetBit(bmp->data,i);
  return 0;
}

int ClearBitmap(Bitmap_t *bmp)
{
  int i;
  for (i=0; i<bmp->size; i++)
    ClearBit(bmp->data,i);
  bmp->used = 0;
  bmp->pos = 0;
  return 1;
}

int SetBitmapRange(Bitmap_t *bmp, int start, int size)
{
  int i;
  for (i=0; i<size; i++) {
    if (1 || !(GetBit(bmp->data,start+i))) {
      SetBit(bmp->data,start+i);
      bmp->used++;
    }
  }
  return 1;
}

int BitmapSize(Bitmap_t *b)
{
  return b->size;
}

/* XXX this is crummy code */
int AllocBitmapRange(Bitmap_t *bmp, int req_size)
{
  int cur = bmp->pos;
  int dist = 0;
  unsigned long *data = bmp->data;
  if (req_size + bmp->used > bmp->size)
    return -1;
  while (dist < bmp->size)
    {
      int i,found = 1, nextpos=0;
      if (cur + req_size > bmp->size)
	cur = 0;
      for (i=0; i<req_size; i++)
	if (GetBit(data,cur+i))
	  {
	    found = 0;
	    nextpos = cur + i + 1;
	    break;
	  }
      if (!found)
	{
	  dist += nextpos-cur;
	  cur = nextpos;
	  if (cur >= bmp->size)
	    cur -= bmp->size;
	  continue;
	}
      for (i=0; i<req_size; i++)
	if (GetBit(data,cur+i))
	  assert(0);
      for (i=0; i<req_size; i++)
	SetBit(data,cur+i);
      bmp->pos = cur + req_size;
      bmp->used += req_size;
      return cur;
    }
  return -1;
}

