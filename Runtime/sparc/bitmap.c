#include "s.h"
#include "r.h"
#include "sparc.h"

enum {
	log_size_long = 5,
	mask_long = 31,
};

Bitmap_t*
CreateBitmap(int size)
{
	Bitmap_t *res = (Bitmap_t *)emalloc(sizeof(Bitmap_t));
	res->size = size;
	res->pos = 0;
	res->data = (unsigned long *)ecalloc(
		1 + size / (sizeof(unsigned long) * 8),
		sizeof(unsigned long));
	res->used = 0;
	res->lock = 0;
	assert(8 * (sizeof (unsigned long)) == (1 << log_size_long));
	return res;
}

void
DestroyBitmap(Bitmap_t* b)
{
	efree(b->data);
	efree((void *)b);
}

static int
GetBit(unsigned long* data, int i)
{
	int long_pos = i >> log_size_long;
	int bit_pos = i & mask_long;
	return (data[long_pos] >> bit_pos) & 1UL;
}

static void
SetBit(Bitmap_t* bmp, int i)
{
	int long_pos = i >> log_size_long;
	int bit_pos = i & mask_long;
	assert(long_pos < bmp->size);
	bmp->data[long_pos] |= (1UL << bit_pos);
}

int
IsSet(Bitmap_t* bmp, unsigned int i)
{
	if (i < bmp->size)
		return GetBit(bmp->data,i);
	return 0;
}

int
ClearBitmap(Bitmap_t* bmp)
{
	int i;
	int longSize = DivideUp(bmp->size, 1 << log_size_long);
	for (i=0; i<longSize; i++)
		bmp->data[i] = 0;
	bmp->used = 0;
	bmp->pos = 0;
	return 1;
}

int
BitmapSize(Bitmap_t* b)
{
	return b->size;
}

static void
Lock(Bitmap_t* bmp)
{
	while (!TestAndSet(&bmp->lock))
		;
}

static void
Unlock(Bitmap_t* bmp)
{
	bmp->lock = 0;
}

int
SetBitmapRange(Bitmap_t* bmp, int start, int size)
{
	int i;
	Lock(bmp);
	for (i=0; i<size; i++) {
		if (!(GetBit(bmp->data,start+i))) {
			SetBit(bmp,start+i);
			bmp->used++;
		}
	}
	Unlock(bmp);
	return 1;
}

int
AllocBitmapRange(Bitmap_t* bmp, int req_size)
{
	int cur, result = -1;
	int dist = 0;
	unsigned long *data = bmp->data;

	Lock(bmp);
	cur = bmp->pos;	/* must be after lock */
	if (req_size + bmp->used <= bmp->size)
		while (dist < bmp->size) {
			int i,found = 1, nextpos=0;
			if (cur + req_size > bmp->size)
				cur = 0;
			for (i=0; i<req_size; i++)
				if (GetBit(data,cur+i)) {
					found = 0;
					nextpos = cur + i + 1;
					break;
				}
			if (!found) {
				dist += nextpos-cur;
				cur = nextpos;
				if (cur >= bmp->size)
					cur -= bmp->size;
				continue;
			}
			for (i=0; i<req_size; i++)
				SetBit(bmp,cur+i);
			bmp->pos = cur + req_size;
			bmp->used += req_size;
			result = cur;
			break;
		}
	Unlock(bmp);
	return result;
}
