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
#include "client.h"
#include "show.h"


static int largeheapsize = 16384 * 1024;
static int largebitmapsize = 128;
static Bitmap_t  *largebitmap = NULL;
Heap_t    *large = NULL;
static Queue_t   *largeRoots = NULL;

mem_t gc_large_alloc(Proc_t *proc, int byteLen)
{
  mem_t res = NULL;
  int bitmapPos;
  int byteLenUp = DivideUp(byteLen,largebitmapsize);

  assert(byteLen >= 1024);
  assert(byteLenUp <= largeheapsize);
  bitmapPos = AllocBitmapRange(largebitmap,byteLenUp);

  if (bitmapPos < 0)
    return NULL; /* allocation failed */

  gcstat_normal(proc, byteLenUp, 0 , 0);
  res = large->bottom + (largebitmapsize * bitmapPos) / (sizeof (val_t));
  return res;
}

void gc_large_addRoots(Queue_t *roots)
{
  int i;
  int qlen = QueueLength(largeRoots);
  for (i=0; i<qlen; i++) {
    ptr_t obj = (ptr_t) QueueAccess(largeRoots,i);
    Enqueue(largeRoots, (void *) obj);
  }
}

void gc_large_flush()
{
  int i;
  int qlen = QueueLength(largeRoots);
  ClearBitmap(largebitmap);
  for (i=0; i<qlen; i++) {
    ptr_t obj = (ptr_t) QueueAccess(largeRoots,i);
    mem_t objStart = obj - 1;
    int bytePos = (sizeof (unsigned int)) * (objStart - large->bottom);
    int byteLen = objectLength(obj);
    int chunkPos = DivideDown(bytePos, largebitmapsize);
    int chunkLen = DivideUp(byteLen, largebitmapsize);
    SetBitmapRange(largebitmap,chunkPos,chunkLen);
  }
  QueueClear(largeRoots);
}


void gc_large_init(int threadSafe)
{
  large = Heap_Alloc(largeheapsize,largeheapsize);
  largebitmap = CreateBitmap(largeheapsize / largebitmapsize);
  largeRoots = QueueCreate(threadSafe,100);
}

