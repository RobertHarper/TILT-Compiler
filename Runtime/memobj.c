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


static Stack_t *Stacks;
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
   8192 bits gives us 256 megs of heap space. 
*/
int pagesize = 0;
static int chunksize = 32768;
static Bitmap_t *bmp = NULL;
static const int Heapbitmap_bits = 8192;



void my_mprotect(int which, caddr_t bottom, int size, int perm)
{
  int status = mprotect(bottom, size, perm);
  if (status)
    {
      fprintf (stderr, "mprotect %d failed (%d,%d,%d) with %d\n",which,bottom,size,perm,errno);
      assert(0);
    }
  /*
  if (paranoid)
    fprintf (stderr, "mprotect %d succeeded (%d,%d,%d)\n",which,bottom,size,perm);
    */
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
  if (paranoid)
    fprintf(stderr,"mmap succeeded with start = %d, size = %d, prot = %d\n",
	    start, size, prot);
  /*
  if (paranoid) {
    if (prot == (PROT_READ | PROT_WRITE)) {
      int a,b;
      caddr_t end = start + size - 4;
      *((int *)(start)) = 666;
      *((int *)(end)) = 666;
      a = *((int *)(start));
      b = *((int *)(end));
      if (a != 666 || b != 666) {
	fprintf(stderr,"mmap (%d,%d,%d)\n",start,size,prot);
	fprintf(stderr,"written 666 to %u: read back %d\n",start,*((int *)(start)));
	fprintf(stderr,"written 666 to %u: read back %d\n",end,*((int *)(end)));
      }
    }
  }
  */
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
  Stacks = (Stack_t *)malloc(sizeof(Stack_t) * NumStack);
  StackChains = (StackChain_t *)malloc(sizeof(StackChain_t) * NumStackChain);
  for (i=0; i<NumStack; i++) {
    Stack_t *stack = &(Stacks[i]);
    stack->id = i;
    stack->valid = 0;
    stack->top = 0;
    stack->bottom = 0;
    stack->rawtop = 0;
    stack->rawbottom = 0;
  }
}



Stack_t* GetStack(mem_t add)
{
  int i;
  for (i=0; i<NumStack; i++) {
    Stack_t *s = &Stacks[i];
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
  Stack_t* stack = Stack_Alloc(res);
  assert(count <= NumStackChain);

  res->count = 1;
  res->size = 4;
  res->stacks = (Stack_t **) malloc(sizeof(Stack_t *) * res->size);
  res->stacks[0] = stack;
  return res;
}

Stack_t* Stack_Alloc(StackChain_t *parent)
{
  int i;
  static int count = -1;
  Stack_t *res = &(Stacks[++count]);
  int size = StackSize * kilobyte;
  mem_t start = stackstart + (count * size) / (sizeof (val_t));
  assert(count < NumStack);

  res->safety = 2 * pagesize;
  res->parent = parent;
  res->rawbottom = my_mmap((caddr_t) start,size,PROT_READ | PROT_WRITE);
  if (res->rawbottom == (mem_t) -1)
      exit(-1);
  res->rawtop = res->rawbottom + size / (sizeof (val_t));
  res->bottom = res->rawbottom + res->safety / (sizeof (val_t));
  res->top    = res->rawtop    - res->safety / (sizeof (val_t));
  res->valid  = 1;

  if (!(res->rawtop < heapstart))
    fprintf(stderr,"count = %d, res->rawtop , heapstart = %d  %d\n", count,res->rawtop, heapstart);
  assert(res->rawtop < heapstart);

  my_mprotect(0,(caddr_t) res->bottom, (res->top - res->bottom)/ (sizeof (val_t)), PROT_READ | PROT_WRITE);
  my_mprotect(1,(caddr_t) res->rawbottom,         res->safety / (sizeof (val_t)),PROT_NONE);
  my_mprotect(2,(caddr_t) res->rawtop-res->safety,res->safety / (sizeof (val_t)), PROT_NONE);

#ifdef DEBUG
  fprintf(stderr,"Stack Object: bottom = %d,    top = %d\n",res->bottom,res->top);
  fprintf(stderr,"           rawbottom = %d, rawtop = %d\n\n",res->rawbottom,res->rawtop);
#endif
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
    heap->physicalTop = 0;
    heap->lock = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(heap->lock,NULL);
  }
}


void PadHeapArea(mem_t bottom, mem_t top)
{
  assert(bottom <= top);
  if (bottom < top)
    *bottom = SKIP_TAG | ((top - bottom) << SKIPLEN_OFFSET);
}

void GetHeapArea(Heap_t *heap, int size, mem_t *bottom, mem_t *top)
{
  mem_t start, end;
  pthread_mutex_lock(heap->lock);
  start = heap->cursor;
  end = start + size / (sizeof (val_t));
  if (end > heap->top) {
    start = 0;
    end = 0;
  }
  else {
    PadHeapArea(start,end);
    heap->cursor = end;
  }
  *bottom = start;
  *top = end;
  flushStore();
  pthread_mutex_unlock(heap->lock);
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

void Heap_Check(Heap_t *h)
{
  assert(h->bottom <= h->cursor);
  assert(h->cursor <= h->top);
  assert(h->top <= h->physicalTop);
}

Heap_t* Heap_Alloc(int MinSize, int MaxSize)
{
  static int heap_count = 0;

  Heap_t *res = &(Heaps[heap_count++]);
  int maxsize_pageround = RoundUp(MaxSize,pagesize);
  int maxsize_chunkround = RoundUp(MaxSize,chunksize);

  int chunkstart = AllocBitmapRange(bmp,maxsize_chunkround / chunksize);
  mem_t start = heapstart + (chunkstart * chunksize) / (sizeof (val_t));
  res->bottom = (mem_t) my_mmap((caddr_t) start, maxsize_pageround, PROT_READ | PROT_WRITE);
  res->cursor = res->bottom;
  res->top    = res->bottom + MinSize / (sizeof (val_t));
  res->physicalTop = res->bottom + maxsize_pageround / (sizeof (val_t));
  res->range.low = res->bottom;
  res->range.high = res->physicalTop;
  res->valid  = 1;

  assert(res->bottom != (mem_t) -1);
  assert(chunkstart >= 0);
  assert(heap_count < NumHeap);
  assert(MaxSize >= MinSize);

  return res;
}


void Heap_Protect(Heap_t* res)
{
  long size = res->physicalTop - res->bottom;
  assert((size / pagesize * pagesize) == size);
  my_mprotect(6,(caddr_t) res->bottom, size, PROT_NONE);

}

int Heap_GetSize(Heap_t *h)
{
  return (sizeof (val_t)) * (h->top - h->bottom);
}

int Heap_GetAvail(Heap_t *h)
{
  return (sizeof (val_t)) * (h->top - h->cursor);
}

void Heap_Resize(Heap_t *h, long newsize)
{
  int actualSize = (val_t) h->physicalTop - (val_t) h->bottom;
  if (newsize > actualSize) {
    fprintf(stderr,"FATAL ERROR in Heap_Resize at GC %d.  Heap size = %d.  Trying to resize to %d\n",
	    NumGC, actualSize, newsize);
    assert(0);
  }
  h->top = h->bottom + (newsize / (sizeof (val_t))); 
  Heap_Check(h);
}

void Heap_Unprotect(Heap_t *res, long newsize)
{
  long size = res->physicalTop - res->bottom;
  assert((size / pagesize * pagesize) == size);
  my_mprotect(7,(caddr_t) res->bottom, size, PROT_READ | PROT_WRITE);
  Heap_Resize(res, newsize);
}

mem_t StackError(struct ucontext *ucontext, mem_t badadd)
{
  Stack_t *faultstack = 0;
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
      Stack_t *newstack = 0;
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
