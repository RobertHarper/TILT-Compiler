/* Not thread-safe */
#ifndef _bitmap_h
#define _bitmap_h

struct Bitmap_st
{
  long lock;
  int size;
  int pos;
  int used;
  unsigned long *data; 
};

typedef struct Bitmap_st Bitmap_t;

Bitmap_t *CreateBitmap(int size);
void      DestroyBitmap(Bitmap_t *);
int       BitmapSize(Bitmap_t *);
int       ClearBitmap(Bitmap_t *);
int       AllocBitmapRange(Bitmap_t *, int size);                /* multiple callers for allocation */
int       SetBitmapRange(Bitmap_t *, int start, int size);       /* multiple callers for marking */
int       IsSet(Bitmap_t *, unsigned int);                       /* multiple callers for marking */

#endif 
