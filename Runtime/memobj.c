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



void my_mprotect(int which, caddr_t bottom, int size, int perm)
{
  int v = mprotect(bottom, size, perm);
  if (v)
    {
      printf ("mprotect %d failed (%d,%d,%d) with %d\n",which,bottom,size,perm,errno);
      exit(-1);
    }
}

int my_mmap(caddr_t start, int size, int prot)
{
  static int fd = -1;
  value_t v;
#ifdef solaris
  {
    if (fd == -1)
      if ((fd = open("/dev/zero", O_RDWR)) == -1) {
	printf ("unable to open /dev/zero, errno = %d\n", errno);
	exit(-1);
      }
    v = (value_t) mmap((caddr_t) start,size, prot,
		       MAP_FIXED | MAP_PRIVATE, fd, 0);
  }
#else
  v = mmap((caddr_t) start, size, prot,
	      MAP_ANONYMOUS | MAP_FIXED, fd, 0);
#endif
  if (paranoid) {
    if (prot == (PROT_READ | PROT_WRITE)) {
      int a,b;
      caddr_t end = start + size - 4;
      *((int *)(start)) = 666;
      *((int *)(end)) = 666;
      a = *((int *)(start));
      b = *((int *)(end));
      if (a != 666 || b != 666) {
	printf ("mmap (%d,%d,%d)\n",start,size,prot);
	printf("written 666 to %u: read back %d\n",start,*((int *)(start)));
	printf("written 666 to %u: read back %d\n",end,*((int *)(end)));
      }
    }
  }
  return v;
}

#ifdef SEMANTIC_GARBAGE   
value_t semantic_garbage_offset = 64 * 1024 * 1024;
#endif

void wordset(void *start, unsigned long v, size_t sz)
{
  unsigned long *end = (void *)(((value_t)start) + sz), *cur;
  for (cur=(unsigned long *)start; cur<end; cur++)
    *cur = v;
}

static Stack_t *Stacks;
static StackChain_t *StackChains;
static Heap_t  *Heaps;

value_t StopHeapLimit = 1;  /* A user thread heap limit used to indicates that it has been interrupted */
value_t StartHeapLimit = 2; /* A user thread heap limit used to indicates that it has not been given space */

int StackSize = 512; /* mesaure in Kb */
static const int megabyte  = 1024 * 1024;
static const int kilobyte  = 1024;
#ifdef alpha_osf
static const int stackstart = 256 * 1024 * 1024;
static const int heapstart  = 512 * 1024 * 1024;
#endif
#ifdef solaris 
static const int stackstart = 256 * 1024 * 1024;
static const int heapstart  = 512 * 1024 * 1024;
#endif
#ifdef rs_aix
static const int stackstart = 768 * 1024 * 1024;
static const int heapstart  = 780 * 1024 * 1024;
#endif

int pagesize = 0;
static int chunksize = 32768;
static Bitmap_t *bmp = NULL;
#ifdef SEMANTIC_GARBAGE
static const int Heapbitmap_bits = 1536;
#else
static const int Heapbitmap_bits = 3072;
#endif

void StackInitialize(void)
{
  int i;
  Stacks = (Stack_t *)malloc(sizeof(Stack_t) * NumStack);
  StackChains = (StackChain_t *)malloc(sizeof(StackChain_t) * NumStackChain);
  for (i=0; i<NumStack; i++)
    {
      Stack_t *stack = &(Stacks[i]);
      stack->id = i;
      stack->valid = 0;
      stack->top = 0;
      stack->bottom = 0;
      stack->rawtop = 0;
      stack->rawbottom = 0;
    }
}



Stack_t* GetStack(value_t add)
{
  int i;
  for (i=0; i<NumStack; i++)
    {
      Stack_t *s = &Stacks[i];
      if (s->valid && 
	  s->id == i &&
	  s->rawbottom <= add &&
	  s->rawtop    >= add)
	return s;
    }
  return NULL;
}

int InStackChain(StackChain_t *sc, value_t addr) 
{
  int i;
  for (i=0; i<sc->count; i++) {
    printf("instackchain bottom = %d, addr = %d, top = %d\n",
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
  int start = stackstart + (count * size);
  assert(count < NumStack);

  res->safety = 2 * pagesize;
  res->parent = parent;
  res->rawbottom = my_mmap((caddr_t) start,size,PROT_READ | PROT_WRITE);
  if (res->rawbottom == -1)
      exit(-1);
  res->rawtop    = res->rawbottom + size;
  res->bottom = res->rawbottom + res->safety;
  res->top    = res->rawtop    - res->safety;
  res->valid  = 1;

  if (!(res->rawtop < heapstart))
    printf("count = %d, res->rawtop , heapstart = %d  %d\n", count,res->rawtop, heapstart);
  assert(res->rawtop < heapstart);
#ifdef SEMANTIC_GARBAGE
  wordset((void *)(res->bottom+semantic_garbage_offset),1,
	 res->top-res->bottom);
#endif
  my_mprotect(0,(caddr_t) res->bottom, res->top - res->bottom, PROT_READ | PROT_WRITE);
  my_mprotect(1,(caddr_t) res->rawbottom,         res->safety,PROT_NONE);
  my_mprotect(2,(caddr_t) res->rawtop-res->safety,res->safety,PROT_NONE);

#ifdef DEBUG
  printf("Stack Object: bottom = %d,    top = %d\n",res->bottom,res->top);
  printf("           rawbottom = %d, rawtop = %d\n\n",res->rawbottom,res->rawtop);
#endif
  return res;
}



void HeapInitialize(void)
{
  int i;
  Heaps = (Heap_t *)malloc(sizeof(Heap_t) * NumHeap);
  for (i=0; i<NumHeap; i++)
    {
      Heap_t *heap = &(Heaps[i]);
      heap->id = i;
      heap->valid = 0;
      heap->top = 0;
      heap->bottom = 0;
      heap->alloc_start = 0;
      heap->rawtop = 0;
      heap->rawbottom = 0;
      heap->lock = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
      pthread_mutex_init(heap->lock,NULL);
    }
}



void GetHeapArea(Heap_t *heap, int size, value_t **bottom, value_t **top)
{
  value_t start, end;
  pthread_mutex_lock(heap->lock);
  start = heap->alloc_start;
  end = start + size;
  if (end > heap->top) {
    start = 0;
    end = 0;
  }
  else {
    heap->alloc_start = end;
  }
  *bottom = (value_t *)start;
  *top = (value_t *)end;
  flushStore();
  pthread_mutex_unlock(heap->lock);
}

Heap_t* GetHeap(value_t add)
{
  int i;
  for (i=0; i<NumHeap; i++)
    if (Heaps[i].valid && Heaps[i].id==i &&
	Heaps[i].bottom <= add &&
	Heaps[i].top    >= add)
      return (Heaps+i);
  return NULL;
}

/* we will manage heaps by a bitmap starting from 32meg up to 128M
   which gives us 96M to play with; the bitmap will measure in 32k chunks
   so there will be 3072 bits in the bitmap */
Heap_t* Heap_Alloc(int MinSize, int MaxSize)
{
  static int heap_count = 0;

  Heap_t *res = &(Heaps[heap_count]);

  int safety   = 2 * pagesize;
  int fullsize = MaxSize + 2 * safety + pagesize;
  int fullsize_pageround = RoundUp(fullsize,pagesize);
  int fullsize_chunkround = RoundUp(fullsize,chunksize);

  int chunkstart = AllocBitmapRange(bmp,fullsize_chunkround / chunksize);
  int start = (chunkstart * chunksize) + heapstart;
  assert(chunkstart >= 0);
  assert (heap_count < NumHeap);
  assert(MaxSize >= MinSize);

  res->safety = safety;
  res->rawbottom = (value_t) my_mmap((caddr_t) start,fullsize_pageround, PROT_NONE);
  assert(res->rawbottom != -1);

  res->rawtop = res->rawbottom + fullsize_pageround;
  res->bottom = res->rawbottom + res->safety;
  res->alloc_start = res->bottom;
  res->top    = res->bottom + MinSize;
#ifdef SEMANTIC_GARBAGE
  printf("chunkstart = %d   start=%d res->rawbottom=%d\n",chunkstart,start,res->rawbottom);
  printf("SEMANTIC GARBAGE: memsetting at %d for %d\n",
	 (res->rawbottom+semantic_garbage_offset),fullsize_pageround);
  wordset((void *)(res->bottom+semantic_garbage_offset),1,
	 res->top-res->bottom);
#endif
  my_mprotect(3,(caddr_t) res->bottom, res->top - res->bottom, PROT_READ | PROT_WRITE);
  my_mprotect(4,(caddr_t) res->rawbottom, res->safety,PROT_NONE);
  my_mprotect(5,(caddr_t) (res->rawtop-safety),res->safety,PROT_NONE);

#ifdef DEBUG
    {
      printf("-------HEAP Object allocation----\n");
      printf("unPROTECTing %d %d\n",res->bottom,res->top - res->bottom);
      printf("  PROTECTing %d %d\n",res->rawbottom,safety);
      printf("  PROTECTing %d %d\n",res->rawtop-safety,safety);
      printf("rawtop,rawbot %d %d \n",res->rawtop,res->rawbottom);
      printf("top,bot       %d %d \n",res->top,res->bottom);
    }
#endif
  assert(res->bottom > res->rawbottom);
  assert(res->top    < res->rawtop);
  res->valid  = 1;
  heap_count++;

  return res;
}


void Heap_Protect(Heap_t* res)
{
  long size = res->rawtop - res->rawbottom;
  long roundsize = (size + pagesize - 1) / pagesize * pagesize;
#ifdef DEBUG
  printf("PROTECT: rawbottom, roundsize, rawtop %d %d %d\n",
	 res->rawbottom,roundsize,res->rawtop);
#endif
  my_mprotect(6,(caddr_t) res->rawbottom, roundsize,PROT_NONE);

}

void Heap_Unprotect(Heap_t *res)
{
  long size = res->top - res->bottom;
  long roundsize = (size + pagesize - 1) / pagesize * pagesize;
  if (res-> top >= res->rawtop)
    printf("Error: res->top = %d  res->rawtop = %d\n",res->top,res->rawtop);
  my_mprotect(7,(caddr_t) res->bottom, roundsize, PROT_READ | PROT_WRITE);

}

int Heap_Getsize(Heap_t *res)
{
  int top = res->rawtop - res->safety;
  return top - res->bottom;
}

void Heap_Resize(Heap_t *res, long newsize)
{
  res->top = res->bottom + newsize;
  if (res->top > res->rawtop - res->safety)
    {
      printf("res->bottom, res->rawtop, res->top, newsize  %d %d %d %d\n",
	     res->bottom, res->rawtop, res->top, newsize);
      fprintf(stderr,"FATAL ERROR in Heap_resize\n");
      exit(-1);
    }
}

int StackError(struct ucontext *ucontext, long badadd)
{
  Stack_t *faultstack = 0;
  StackChain_t *faultchain = 0;
  int i;
  long sp = (long)GetSp(ucontext);

  printf("\n------------------StackError---------------------\n");
  printf("sp, badreference:  %u   %u\n",sp,badadd);

  faultstack = GetStack(badadd);
  if (faultchain == 0)
    return 0;
  faultchain = faultstack->parent;
  for (i=0; i<faultchain->count; i++)
    if (faultstack == faultchain->stacks[i])
      break;

  if (badadd < faultstack->bottom) 
      {
	printf("Underflow occurred - relinking\n\n");
	if (i == (faultchain->count - 1))
	  {
	    Stack_t *newstack = 0;
	    faultstack->used_bottom = sp;
	    newstack = Stack_Alloc(faultchain);
	    faultchain->stacks[faultchain->count++] = newstack;
	    return newstack->top;
	  }
	else
	    return faultchain->stacks[i+1]->top;
      }
  else
    {
      if (i > 0)
	{
	  printf("Overflow occurred - relinking \n\n");
	  return faultchain->stacks[i-1]->used_bottom;
	}
      else
	{
	  printf("Overflowed bottom stack: FATAL ERROR\n");
	  exit(-1);
	}
    }
  exit(-1);
  return 0;
}

extern value_t datastart;

void memobj_init()
{
#ifdef SEMANTIC_GARBAGE
  {
    int i;
    value_t global_start = datastart;
    value_t res = (value_t) my_mmap((caddr_t)semantic_garbage_offset,
				    (size_t)semantic_garbage_offset,
				    PROT_READ | PROT_WRITE);
    assert(res == semantic_garbage_offset);
    res = (value_t) my_mmap((caddr_t)global_start + semantic_garbage_offset,
			    (size_t)(4 * 1024 * 1024),
			    PROT_READ | PROT_WRITE);
    assert(res == global_start + semantic_garbage_offset);
    printf("SEMANTIC GARBAGE: about to memset initially\n");
    wordset((void *)(global_start+semantic_garbage_offset),1,(size_t)(4 * 1024 * 1024));
    printf("SEMANTIC GARBAGE: Done memsetting initially\n");
  }
#endif

  bmp = CreateBitmap(Heapbitmap_bits);
#ifdef solaris
  pagesize = sysconf(_SC_PAGESIZE);
#else
  pagesize = sysconf(_SC_PAGE_SIZE);
#endif
  StackInitialize();
  HeapInitialize();

}
