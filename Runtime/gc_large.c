/* Not thread-safe */
#include "general.h"
#include <sys/time.h>
#include <sys/resource.h>

#include "tag.h"
#include "queue.h"
#include "forward.h"
#include "gc.h"
#include "thread.h"
#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "platform.h"
#include "show.h"

int LargeHeapByte = 16384 * 1024;
static int largebitmapsize = 512;
static Bitmap_t  *allocMap = NULL;
static Bitmap_t  *markMap = NULL;
Heap_t    *largeSpace = NULL;

mem_t gc_large_alloc(Proc_t *proc, int byteLen, Align_t align)
{
  mem_t region, end;
  int bitmapPos;
  int padByteLen = byteLen + ((align == NoWordAlign) ? 0 : 4);
  int chunksNeeded = DivideUp(padByteLen,largebitmapsize);

  assert(byteLen >= 1024);
  bitmapPos = AllocBitmapRange(allocMap,chunksNeeded);
  if (bitmapPos < 0)
    return NULL; /* allocation failed */

  region = largeSpace->bottom + (largebitmapsize * bitmapPos) / (sizeof (val_t));
  end = region + (chunksNeeded * largebitmapsize) / (sizeof (val_t));
  region = AlignMemoryPointer(region, align);
  PadHeapArea(region + byteLen / sizeof(val_t), end);
  return region;
}

void gc_large_startCollect(void)
{
  ClearBitmap(markMap);
}

void gc_large_addRoot(ptr_t obj)
{
  mem_t objStart; 
  int byteLen = objectLength(obj, &objStart);
  int bytePos = (sizeof (unsigned int)) * (objStart - largeSpace->bottom);
  int chunkPos = DivideDown(bytePos, largebitmapsize);
  int chunkLen = DivideUp(byteLen, largebitmapsize);
  if (!IsSet(markMap, chunkPos))
    SetBitmapRange(markMap,chunkPos,chunkLen);
}

void gc_large_endCollect(void)
{
  ClearBitmap(allocMap);
  typed_swap(Bitmap_t *, allocMap, markMap);
}

void gc_large_init(void)
{
  largeSpace = Heap_Alloc(LargeHeapByte, LargeHeapByte);
  allocMap = CreateBitmap(LargeHeapByte / largebitmapsize);
  markMap = CreateBitmap(LargeHeapByte / largebitmapsize);
}

