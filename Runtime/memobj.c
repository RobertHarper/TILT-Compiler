#include "tag.h"
#include "memobj.h"
#include "bitmap.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <strings.h>
#include "general.h"
#include "til-signal.h"
#include <fcntl.h>
#include "stats.h"

Stacklet_t *Stacklets; /* XXXX should be static */
static StackChain_t *StackChains;
static Heap_t  *Heaps;

int NumHeap       = 20;
int NumStacklet   = 100;
int NumStackChain = 100;

mem_t StopHeapLimit  = (mem_t) 1; /* A user thread heap limit used to indicates that it has been interrupted */
mem_t StartHeapLimit = (mem_t) 2; /* A user thread heap limit used to indicates that it has not been given space */

int StackletSize = 128; /* mesaure in Kb */
int primaryStackletOffset, replicaStackletOffset;

static const int megabyte  = 1024 * 1024;
static const int kilobyte  = 1024;
#ifdef alpha_osf
const mem_t stackstart = (mem_t) (256 * 1024 * 1024);
const mem_t heapstart  = (mem_t) (512 * 1024 * 1024);
#endif
#ifdef solaris 
const mem_t stackstart = (mem_t) (256 * 1024 * 1024);
const mem_t heapstart  = (mem_t) (512 * 1024 * 1024);
#endif
#ifdef rs_aix
const mem_t stackstart = (mem_t) (768 * 1024 * 1024);
const mem_t heapstart  = (mem_t) (780 * 1024 * 1024);
#endif

/* The heaps are managed by a bitmap where each bit corresponds to a 32K chunk of the heap.
   We take 320 megs for heap space which result in 10240 bits.
*/
static int heapspace = 320 * 1024 * 1024;
static int chunksize = 32768;
static Bitmap_t *bmp = NULL;
static int Heapbitmap_bits;



void my_mprotect(int which, caddr_t bottom, int size, int perm)
{
  int status = mprotect(bottom, size, perm);
  if (status) {
    fprintf (stderr, "mprotect %d failed (%d,%d,%d) with %d\n",which,bottom,size,perm,errno);
    assert(0);
  }
}


mem_t my_mmap(caddr_t start, int size, int prot)
{
  static int fd = -1;
  mem_t mapped;
#ifdef solaris
  {
    if (fd == -1)
      if ((fd = open("/dev/zero", O_RDWR)) == -1) {
	fprintf (stderr, "unable to open /dev/zero, errno = %d\n", errno);
	exit(-1);
      }
    mapped = (mem_t) mmap((caddr_t) start, size, prot,
			    MAP_FIXED | MAP_PRIVATE, fd, 0);
  }
#else
  mapped = (mem_t) (mmap((caddr_t) start, size, prot,
			   MAP_ANONYMOUS | MAP_FIXED, fd, 0));
#endif
  if (mapped == (mem_t) -1) {
    fprintf(stderr,"mmap failed with start = %d, size = %d  -->  errno = %d\n",
	    start, size, errno);
    assert(0);
  }
  if (mapped != (mem_t) start) {
      fprintf(stderr,"mmap failed with start = %d, size = %d  -->  mapped = %d\n",
	      start, size, mapped);
      assert(0);
  }
  if (diag)
    fprintf(stderr,"mmap succeeded with start = %d, size = %d, prot = %d\n",
	    start, size, prot);
  return mapped;
}


void StackInitialize(void)
{
  int i;
  Stacklets = (Stacklet_t *)malloc(sizeof(Stacklet_t) * NumStacklet);
  StackChains = (StackChain_t *)malloc(sizeof(StackChain_t) * NumStackChain);
  for (i=0; i<NumStacklet; i++) {
    Stacklets[i].count = 0;
    Stacklets[i].mapped = 0;
  }
  for (i=0; i<NumStackChain; i++)
    StackChains[i].used = 0;
}

void Stacklet_Dealloc(Stacklet_t *stacklet)
{
  assert(stacklet->count > 0);
  stacklet->count--;
}

void Stacklet_KillReplica(Stacklet_t *stacklet)
{
  assert(stacklet->count > 0);
  stacklet->active = 1;
}

static Stacklet_t* Stacklet_Alloc(StackChain_t *stackChain)
{
  int i;
  Stacklet_t *res = NULL;
  int size = StackletSize * kilobyte;  /* for just one of the two: primary and replica */
  int safety = pagesize;

  for (i=0; i<NumStacklet; i++)
    if (Stacklets[i].count == 0) {
      res = &Stacklets[i];
      break;
    }
  assert(res != NULL);

  res->count = 1;
  res->active = 0;
  if (!res->mapped) {
    mem_t start = stackstart + (2 * i * size) / (sizeof (val_t)), middle, end;

    start = my_mmap((caddr_t) start, 2 * size, PROT_READ | PROT_WRITE); /* Might get different base */
    middle = start + size / (sizeof (val_t));
    end = start + 2 * size / (sizeof (val_t));
    res->baseBottom = start + safety / (sizeof (val_t));
    res->baseTop    = middle - safety / (sizeof (val_t));
    res->baseTop    = res->baseTop - ((safety + 128) / sizeof(val_t)); /* Get some initial room in multiples of 64 bytes; 
								  Sparc requires at least 68 byte for the save area */
    my_mprotect(0, (caddr_t) start,                                   safety, PROT_NONE);
    my_mprotect(1, (caddr_t) (middle - (safety / sizeof(val_t))), 2 * safety, PROT_NONE);
    my_mprotect(2, (caddr_t) (end    - (safety / sizeof(val_t))),     safety, PROT_NONE);

    res->callinfoStack = createStack(size / (32 * sizeof (val_t)));
    res->mapped = 1;
  }
  res->baseCursor = res->baseTop;
  res->topRegstate = 0;
  res->bottomRegstate = 0;
  resetStack(res->callinfoStack);
  return res;
}

mem_t StackletPrimaryTop(Stacklet_t *stacklet)
{
  return stacklet->baseTop + (primaryStackletOffset / sizeof(val_t));
}

mem_t StackletPrimaryCursor(Stacklet_t *stacklet)
{
  return stacklet->baseCursor + (primaryStackletOffset / sizeof(val_t));
}


mem_t StackletPrimaryBottom(Stacklet_t *stacklet)
{
  return stacklet->baseBottom + (primaryStackletOffset / sizeof(val_t));
}

Stacklet_t *NewStacklet(StackChain_t *stackChain)
{
  int i;
  Stacklet_t *newStacklet = Stacklet_Alloc(stackChain);
  stackChain->stacklets[stackChain->cursor++] = newStacklet;
  for (i=0; i<stackChain->cursor; i++) 
    assert(stackChain->stacklets[i]->count > 0);
  return newStacklet;
}

/* Copy primary into replica */
void Stacklet_Copy(Stacklet_t *stacklet)
{
  int activeSize = (stacklet->baseTop - stacklet->baseCursor) * sizeof(val_t);
  mem_t primaryBottom = stacklet->baseBottom + (primaryStackletOffset / sizeof(val_t));
  mem_t replicaBottom = stacklet->baseBottom + (replicaStackletOffset / sizeof(val_t));
  mem_t primaryCursor = stacklet->baseCursor + (primaryStackletOffset / sizeof(val_t));
  mem_t replicaCursor = stacklet->baseCursor + (replicaStackletOffset / sizeof(val_t));
  assert(stacklet->count > 0);
  assert(stacklet->baseBottom <= stacklet->baseCursor && stacklet->baseCursor <= stacklet->baseTop);
  memcpy(replicaCursor, primaryCursor, activeSize);
}


/* Given the new sp, establish the most recent stacklet, remembering possible raised exceptions.
   Return the most recent stacklet.
*/
Stacklet_t *EstablishStacklet(StackChain_t *stackChain, mem_t sp)
{
  int i,active;
  mem_t sp_base = sp - primaryStackletOffset / sizeof(val_t);
  for (active=stackChain->cursor-1; active>=0; active--) {
    Stacklet_t *stacklet = stackChain->stacklets[active];
    if (stacklet->baseBottom <= sp_base && sp_base <= stacklet->baseTop) { /* Not cursor since that is not reliable yet */
      stacklet->baseCursor = sp_base;
      break;
    }
  }
  assert(active>=0);
  for (i=active+1; i<stackChain->cursor; i++) {
    Stacklet_Dealloc(stackChain->stacklets[i]);
    stackChain->stacklets[i] = NULL;
  }
  stackChain->cursor = active + 1;
  return stackChain->stacklets[active]; 
}


/* Deallocate most recent stacklet - at least one active must remain */
void PopStacklet(StackChain_t *stackChain)
{
  int active = stackChain->cursor - 1;
  Stacklet_Dealloc(stackChain->stacklets[active]);
  stackChain->stacklets[active] = NULL;
  stackChain->cursor--;
  assert(stackChain->cursor > 0);
}

/* Remove oldest stacklet from stack chain */
void DequeueStacklet(StackChain_t *stackChain)
{
  int i;
  Stacklet_Dealloc(stackChain->stacklets[0]);
  for (i=1; i<stackChain->cursor; i++) {
    stackChain->stacklets[i-1] = stackChain->stacklets[i];
    assert(stackChain->stacklets[i-1]->count > 0);
  }
  stackChain->stacklets[stackChain->cursor-1] = NULL;
  stackChain->cursor--;
  assert(stackChain->cursor>=0);
}

StackChain_t* StackChain_BaseAlloc(void)
{
  int i;
  for (i=0; i<NumStackChain; i++)
    if (!StackChains[i].used) {
      StackChain_t *res = &StackChains[i];
      res->used = 1;
      res->cursor = 0;
      return res;
    }
  assert(0);
}


StackChain_t* StackChain_Copy(StackChain_t *src)
{
  int i;
  StackChain_t* res = StackChain_BaseAlloc();
  assert(src->used);
  res->cursor = src->cursor;
  for (i=0; i<src->cursor; i++) {
    res->stacklets[i] = src->stacklets[i];
    src->stacklets[i]->count++;
    res->stacklets[i]->active = 0;
    Stacklet_Copy(src->stacklets[i]);
  }
  return res;
}

int StackChain_Size(StackChain_t *chain) 
{
  int i, sum = 0;
  for (i=0; i<chain->cursor; i++) {
    Stacklet_t *stacklet = chain->stacklets[i];
    sum += (stacklet->baseTop - stacklet->baseCursor) * sizeof(val_t);
  }
  return sum;
}


StackChain_t* StackChain_Alloc()
{
  StackChain_t* res = StackChain_BaseAlloc();
  NewStacklet(res);
  return res;
}

void StackChain_Dealloc(StackChain_t *stackChain)
{
  int i;
  for (i=0; i<stackChain->cursor; i++)
    Stacklet_Dealloc(stackChain->stacklets[i]);
  stackChain->used = 0;
  stackChain->cursor = 0;
}

Stacklet_t* GetStacklet(mem_t add)
{
  int i;
  mem_t offsetAdd = add - StackletSize * kilobyte / sizeof(val_t);
  for (i=0; i<NumStacklet; i++) {
    Stacklet_t *s = &Stacklets[i];
    if (s->count && 
	((s->baseBottom <= add && s->baseTop >= add) ||
	 (s->baseBottom <= offsetAdd && s->baseTop >= offsetAdd)))
      return s;
  }
  return NULL;
}

Stacklet_t* CurrentStacklet(StackChain_t *stackChain)
{
  Stacklet_t *stacklet = stackChain->stacklets[stackChain->cursor-1];
  assert(stackChain->used);
  assert(stackChain->cursor > 0);
  assert(stacklet->count > 0);
  return stacklet;
}

void HeapInitialize(void)
{
  int i;
  Heaps = (Heap_t *)malloc(sizeof(Heap_t) * NumHeap);
  for (i=0; i<NumHeap; i++) {
    Heap_t *heap = &(Heaps[i]);
    heap->id = i;
    heap->valid = 0;
    heap->top = 0;
    heap->bottom = 0;
    heap->cursor = 0;
    heap->prevCursor = 0;
    heap->mappedTop = 0;
    heap->writeableTop = 0;
    heap->lock = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(heap->lock,NULL);
  }
}


void PadHeapArea(mem_t bottom, mem_t top)
{
  assert(bottom <= top);
  if (bottom < top)
    *bottom = MAKE_SKIP(top - bottom);
}

void GetHeapArea(Heap_t *heap, int size, mem_t *bottom, mem_t *cursor, mem_t *top)
{
  mem_t region = (mem_t) FetchAndAdd((long *)(&heap->cursor), size);
  mem_t newHeapCursor = region + size / sizeof(val_t);
  if (newHeapCursor > heap->top) {
    FetchAndAdd((long *)(&heap->cursor), -size);
    *bottom = *cursor = *top = 0;
  }
  else {
    val_t forceRead = *newHeapCursor;     /* Do most machines have non-blockig read? */
    *bottom = region;
    *cursor = region;
    *top = newHeapCursor;
    PadHeapArea(*bottom,*top);
  }
}

Heap_t* GetHeap(mem_t add)
{
  int i;
  for (i=0; i<NumHeap; i++)
    if (Heaps[i].valid && Heaps[i].id==i &&
	Heaps[i].bottom <= add &&
	Heaps[i].top    >= add)
      return (Heaps+i);
  return NULL;
}

int inSomeHeap(ptr_t v)
{
  int totalHeapSize = chunksize * Heapbitmap_bits; /* in bytes */
  return (v >= heapstart && v < heapstart + (totalHeapSize / sizeof (val_t)));
}

void SetRange(range_t *range, mem_t low, mem_t high)
{
  range->low = low;
  range->high = high;
  range->diff = (high - low) * (sizeof (val_t));
}

Heap_t* Heap_Alloc(int MinSize, int MaxSize)
{
  static int heap_count = 0;
  int stat;
  mem_t cursor;
  Heap_t *res = &(Heaps[heap_count++]);
  int maxsize_pageround = RoundUp(MaxSize,pagesize);
  int maxsize_chunkround = RoundUp(MaxSize,chunksize);

  int chunkstart = AllocBitmapRange(bmp,maxsize_chunkround / chunksize);
  mem_t start = heapstart + (chunkstart * chunksize) / (sizeof (val_t));
  res->size = maxsize_pageround;
  res->bottom = (mem_t) my_mmap((caddr_t) start, maxsize_pageround, PROT_READ | PROT_WRITE);
  res->cursor = res->bottom;
  res->prevCursor = res->bottom;
  res->top    = res->bottom + MinSize / (sizeof (val_t));
  res->writeableTop = res->bottom + maxsize_pageround / (sizeof (val_t));
  res->mappedTop = res->writeableTop;
  SetRange(&(res->range), res->bottom, res->mappedTop);
  res->valid  = 1;
  res->bitmap = paranoid ? CreateBitmap(maxsize_pageround / 4) : NULL;
  res->freshPages = (int *) malloc(DivideUp(maxsize_pageround / pagesize, 32) * sizeof(int));
  bzero(res->freshPages, DivideUp(maxsize_pageround / pagesize, 32) * sizeof(int));
  assert(res->bottom != (mem_t) -1);
  assert(chunkstart >= 0);
  assert(heap_count < NumHeap);
  assert(MaxSize >= MinSize);
  /* Lock down pages and force page-table to be initialized; otherwise, PadHeapArea can often take 0.1 - 0.2 ms. 
     Even with this, there are occasional (but far fewer) page table misses.
   */
  if (geteuid() == 0) {
    stat = mlock((caddr_t) res->bottom, maxsize_pageround); 
    if (stat) {
      printf("mlock failed with errno %d\n", errno);
      assert(0);
    }
  }
  return res;
}

int Heap_ResetFreshPages(Heap_t *h)
{
  int MaxSize = sizeof(val_t) * (h->mappedTop - h->bottom);
  int i, pages = DivideUp(MaxSize,pagesize);
  bzero(h->freshPages, DivideUp(pages, 32) * sizeof(int));
  return pages;
}

int Heap_GetMaximumSize(Heap_t *h)
{
  return (sizeof (val_t)) * (h->mappedTop - h->bottom);
}

int Heap_GetSize(Heap_t *h)
{
  return (sizeof (val_t)) * (h->top - h->bottom);
}

int Heap_GetAvail(Heap_t *h)
{
  return (sizeof (val_t)) * (h->top - h->cursor);
}

void Heap_Reset(Heap_t *h)
{
  h->cursor = h->bottom;
  h->prevCursor = h->bottom;
}

void Heap_Resize(Heap_t *h, long newSize, int reset)
{
  long maxSize = (h->mappedTop - h->bottom) * sizeof(val_t);
  long oldSize = (h->top - h->bottom) * sizeof(val_t);
  long oldWriteableSize = (h->writeableTop - h->bottom) * sizeof(val_t);
  long oldSizeRound = RoundUp(oldSize, pagesize);
  long newSizeRound = RoundUp(newSize, pagesize);

  if (newSize > maxSize) {
    printf("FATAL ERROR in Heap_Resize at GC %d.  Heap size = %d.  Trying to resize to %d\n", NumGC, maxSize, newSize);
    assert(0);
  }
  if (reset) {
    h->cursor = h->bottom;
    h->prevCursor = h->bottom;
  }
  h->top = h->bottom + (newSize / sizeof(val_t));

  if (newSizeRound > oldWriteableSize) {
    my_mprotect(6,(caddr_t) h->writeableTop, newSizeRound - oldWriteableSize, PROT_READ | PROT_WRITE);
    h->writeableTop = h->bottom + newSizeRound / sizeof(val_t);
  }
  else if (paranoid && newSizeRound < oldWriteableSize) {
    my_mprotect(7,(caddr_t) (h->bottom + newSizeRound / sizeof(val_t)), oldWriteableSize - newSizeRound, PROT_NONE);
    h->writeableTop = h->bottom + newSizeRound / sizeof(val_t);
  }
  assert(h->bottom <= h->prevCursor);
  assert(h->prevCursor <= h->cursor);
  assert(h->cursor <= h->top);
  assert(h->top <= h->writeableTop);
  assert(h->writeableTop <= h->mappedTop);
}


mem_t StackError(struct ucontext *ucontext, mem_t badadd)
{
  Stacklet_t *faultstack = 0;
  StackChain_t *faultchain = 0;
  int i;
  mem_t sp = GetSp(ucontext);

  printf("\n------------------StackError---------------------\n");
  printf("sp, badreference:  %u   %u\n",sp,badadd);
  assert(0);
  return sp; /* Bogus!  silence warnings. */
}

extern mem_t datastart;

void memobj_init()
{
  Stacklet_t *s1, *s2;
  Heapbitmap_bits = heapspace / chunksize;
  bmp = CreateBitmap(Heapbitmap_bits);
#ifdef solaris
  assert(pagesize == sysconf(_SC_PAGESIZE));
#else
  assert(pagesize == sysconf(_SC_PAGE_SIZE));
#endif
  StackInitialize();
  HeapInitialize();
  primaryStackletOffset = 0;
  replicaStackletOffset = StackletSize * kilobyte;
  /* So we don't pay mmap for first thread - general case? XXXX */
  { int i;
    Stacklet_t *temp[5];
    for (i=0; i<5; i++)
      temp[i] = Stacklet_Alloc(NULL);
    for (i=0; i<5; i++)
      Stacklet_Dealloc(temp[i]);
  }
}
