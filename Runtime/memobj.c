#include "tag.h"
#include "memobj.h"
#include "bitmap.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include "general.h"
#include "til-signal.h"
#include <fcntl.h>
#include "stats.h"

static MemStack_t *Stacks;
static StackChain_t *StackChains;
static Heap_t  *Heaps;

mem_t StopHeapLimit  = (mem_t) 1; /* A user thread heap limit used to indicates that it has been interrupted */
mem_t StartHeapLimit = (mem_t) 2; /* A user thread heap limit used to indicates that it has not been given space */

int StackSize = 2048; /* mesaure in Kb */
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
   We take 288 megs for heap space which result in 9216 bits.
*/
int pagesize = 0;
static int chunksize = 32768;
static Bitmap_t *bmp = NULL;
static const int Heapbitmap_bits = 9216;



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


void wordset(mem_t start, unsigned long v, size_t sz)
{
  unsigned long *end = (void *)(((unsigned int)start) + sz), *cur;
  for (cur=(unsigned long *)start; cur<end; cur++)
    *cur = v;
}


void StackInitialize(void)
{
  int i;
  Stacks = (MemStack_t *)malloc(sizeof(MemStack_t) * NumStack);
  StackChains = (StackChain_t *)malloc(sizeof(StackChain_t) * NumStackChain);
  for (i=0; i<NumStack; i++) {
    MemStack_t *stack = &(Stacks[i]);
    stack->id = i;
    stack->valid = 0;
    stack->top = 0;
    stack->bottom = 0;
    stack->rawtop = 0;
    stack->rawbottom = 0;
  }
}



MemStack_t* GetStack(mem_t add)
{
  int i;
  for (i=0; i<NumStack; i++) {
    MemStack_t *s = &Stacks[i];
    if (s->valid && 
	s->id == i &&
	s->rawbottom <= add &&
	s->rawtop    >= add)
      return s;
  }
  return NULL;
}

int InStackChain(StackChain_t *sc, mem_t addr) 
{
  int i;
  for (i=0; i<sc->count; i++) {
    fprintf(stderr,"instackchain bottom = %d, addr = %d, top = %d\n",
	  sc->stacks[i]->bottom, addr ,
	   sc->stacks[i]->top);
    if (sc->stacks[i]->bottom <= addr &&
	sc->stacks[i]->top >= addr)
      return 1;
  }
  return 0;
}

StackChain_t* StackChain_Alloc()
{
  static int count = 0;
  StackChain_t *res = &(StackChains[count++]);
  MemStack_t* stack = Stack_Alloc(res);
  assert(count <= NumStackChain);

  res->count = 1;
  res->size = 4;
  res->stacks = (MemStack_t **) malloc(sizeof(MemStack_t *) * res->size);
  res->stacks[0] = stack;
  return res;
}

MemStack_t* Stack_Alloc(StackChain_t *parent)
{
  int i;
  static int count = -1;
  MemStack_t *res = &(Stacks[++count]);
  int size = StackSize * kilobyte;
  mem_t start = stackstart + (count * size) / (sizeof (val_t));
  assert(count < NumStack);

  res->safety = 2 * pagesize;
  res->parent = parent;
  res->rawbottom = my_mmap((caddr_t) start,size,PROT_READ | PROT_WRITE);
  res->rawtop = res->rawbottom + size / (sizeof (val_t));
  res->bottom = res->rawbottom + res->safety / (sizeof (val_t));
  res->top    = res->rawtop    - res->safety / (sizeof (val_t));
  res->valid  = 1;
  assert(res->rawbottom != (mem_t) -1);
  assert(res->rawtop < heapstart);

  my_mprotect(0, (caddr_t) res->rawbottom, res->safety,              PROT_NONE);
  my_mprotect(1, (caddr_t) res->bottom,    size - 2 * res->safety,   PROT_READ | PROT_WRITE);
  my_mprotect(2, (caddr_t) res->top,       res->safety,              PROT_NONE);

  return res;
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
    *bottom = SKIP_TYPE | ((top - bottom) << SKIPLEN_OFFSET);
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
  mem_t cursor;
  Heap_t *res = &(Heaps[heap_count++]);
  int maxsize_pageround = RoundUp(MaxSize,pagesize);
  int maxsize_chunkround = RoundUp(MaxSize,chunksize);

  int chunkstart = AllocBitmapRange(bmp,maxsize_chunkround / chunksize);
  mem_t start = heapstart + (chunkstart * chunksize) / (sizeof (val_t));
  res->bottom = (mem_t) my_mmap((caddr_t) start, maxsize_pageround, PROT_READ | PROT_WRITE);
  res->cursor = res->bottom;
  res->prevCursor = res->bottom;
  res->top    = res->bottom + MinSize / (sizeof (val_t));
  res->writeableTop = res->bottom + maxsize_pageround / (sizeof (val_t));
  res->mappedTop = res->writeableTop;
  SetRange(&(res->range), res->bottom, res->mappedTop);
  res->valid  = 1;
  res->bitmap = paranoid ? CreateBitmap(maxsize_pageround / 4) : NULL;
  assert(res->bottom != (mem_t) -1);
  assert(chunkstart >= 0);
  assert(heap_count < NumHeap);
  assert(MaxSize >= MinSize);
  /* Lock down pages and force page-table to be initialized; otherwise, PadHeapArea can often take 0.1 - 0.2 ms. 
     Even with this, there are occasional (but far fewer) page table misses.
   */
  /*
  mlock((caddr_t) res->bottom, maxsize_pageround); 
  for (cursor = res->bottom; cursor < res->mappedTop; cursor += pagesize / sizeof(val_t))
    *cursor = 0;
    */
  return res;
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
  MemStack_t *faultstack = 0;
  StackChain_t *faultchain = 0;
  int i;
  mem_t sp = GetSp(ucontext);

  printf("\n------------------StackError---------------------\n");
  printf("sp, badreference:  %u   %u\n",sp,badadd);

  faultstack = GetStack((mem_t) badadd);
  if (faultchain == 0)
    return 0;
  faultchain = faultstack->parent;
  for (i=0; i<faultchain->count; i++)
    if (faultstack == faultchain->stacks[i])
      break;

  if (badadd < faultstack->bottom) {
    printf("Underflow occurred - relinking\n\n");
    if (i == (faultchain->count - 1)) {
      MemStack_t *newstack = 0;
      faultstack->used_bottom = sp;
      newstack = Stack_Alloc(faultchain);
      faultchain->stacks[faultchain->count++] = newstack;
      return newstack->top;
    }
    else
      return faultchain->stacks[i+1]->top;
  }
  else {
    if (i > 0) {
      printf("Overflow occurred - relinking \n\n");
      return faultchain->stacks[i-1]->used_bottom;
    }
    else {
      printf("Overflowed bottom stack: FATAL ERROR\n");
      exit(-1);
    }
  }
  exit(-1);
  return 0;
}

extern mem_t datastart;

void memobj_init()
{
  bmp = CreateBitmap(Heapbitmap_bits);
#ifdef solaris
  pagesize = sysconf(_SC_PAGESIZE);
#else
  pagesize = sysconf(_SC_PAGE_SIZE);
#endif
  StackInitialize();
  HeapInitialize();

}
