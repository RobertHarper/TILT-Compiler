/* Not thread-safe */
#ifndef _bitmap_h
#define _bitmap_h

struct Bitmap_st
{
  int size;
  int pos;
  unsigned long *data;
  int used;
};

typedef struct Bitmap_st Bitmap_t;

Bitmap_t *CreateBitmap(int size);
void      DestroyBitmap(Bitmap_t *);
int       AllocBitmapRange(Bitmap_t *, int size);
void      DeallocBitmapRange(Bitmap_t *, int start, int size);
int       ClearBitmap(Bitmap_t *);
int       SetBitmapRange(Bitmap_t *, int start, int size);
int       BitmapSize(Bitmap_t *);

#endif 
