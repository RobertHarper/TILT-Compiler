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

#undef SHOW_MMAP

void my_mprotect(int which, caddr_t bottom, int size, int perm)
{
  int v = mprotect(bottom, size, perm);
  if (v)
    {
      printf ("mprotect %d failed (%d,%d,%d) with %d\n",which,bottom,size,perm,errno);
      exit(-1);
    }
  else
    {
#ifdef SHOW_MMAP
      printf ("mprotect %d succeeded (%d,%d,%d)\n",which,bottom,size,perm);
#endif
    }
}

int my_mmap(caddr_t start, int size, int prot, int flags)
{
  int v = (value_t) mmap(start,size,prot,flags,-1,0);
#ifdef SHOW_MMAP
  printf ("mmap (%d,%d,%d,%d)\n",start,size,prot,flags);
#endif
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

StackObj_t      Stacks[NumStackObj];
static StackChainObj_t StackChains[NumStackChainObj];
static HeapObj_t Heaps[NumHeapObj];

value_t LowHeapLimit = 0;

long StackSize = 2048; /* mesaure in Kb */
static const int megabyte  = 1024 * 1024;
static const int kilobyte  = 1024;
#ifdef alpha_osf
static const int stackstart =  4 * 1024 * 1024;
static const int heapstart  = 16 * 1024 * 1024;
#endif
#ifdef rs_aix
static const int stackstart = 768 * 1024 * 1024;
static const int heapstart  = 780 * 1024 * 1024;
#endif
static int pagesize = 0;
static int chunksize = 32768;
static Bitmap_t *bmp = NULL;
#ifdef SEMANTIC_GARBAGE
static const int Heapbitmap_bits = 1536;
#else
static const int Heapbitmap_bits = 3072;
#endif

void StackInitialize()
{
  int i;
  for (i=0; i<NumStackObj; i++)
    {
      Stacks[i].id = i;
      Stacks[i].valid = 0;
      Stacks[i].top = 0;
      Stacks[i].bottom = 0;
      Stacks[i].rawtop = 0;
      Stacks[i].rawbottom = 0;
    }
}



StackObj_t* GetStack(value_t add)
{
  int i;
  for (i=0; i<NumStackObj; i++)
    {
      StackObj_t *s = &Stacks[i];
      if (s->valid && 
	  s->id == i &&
	  s->rawbottom <= add &&
	  s->rawtop    >= add)
	return s;
    }
  return NULL;
}

int InStackChain(StackChainObj_t *sc, value_t addr) 
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

StackChainObj_t* StackChainObj_Alloc()
{
  static int count = 0;
  StackChainObj_t *res = &(StackChains[count++]);
  StackObj_t* stack = StackObj_Alloc(res);

  res->count = 1;
  res->size = 4;
  res->stacks = (StackObj_t **) malloc(sizeof(StackObj_t *) * res->size);
  res->stacks[0] = stack;
  return res;
}

StackObj_t* StackObj_Alloc(StackChainObj_t *parent)
{
  int i;
  static int count = -1;
  StackObj_t *res = &(Stacks[++count]);
  int size = StackSize * kilobyte;
  int start = stackstart + (count * size);

  res->safety = 2 * pagesize;
  res->parent = parent;
  res->rawbottom = my_mmap((caddr_t) start,size,
			   PROT_READ | PROT_WRITE,MAP_ANONYMOUS | MAP_FIXED);
  if (res->rawbottom == -1)
      exit(-1);
  res->rawtop    = res->rawbottom + size;
  if (!(res->rawtop < heapstart))
    printf("count = %d, res->rawtop , heapstart = %d  %d\n", count,res->rawtop, heapstart);
  assert(res->rawtop < heapstart);
#ifdef SEMANTIC_GARBAGE
  wordset((void *)(res->bottom+semantic_garbage_offset),1,
	 res->top-res->bottom);
#endif
  my_mprotect(1,(caddr_t) res->rawbottom,         res->safety,PROT_NONE);
  my_mprotect(2,(caddr_t) res->rawtop-res->safety,res->safety,PROT_NONE);

  res->bottom = res->rawbottom + res->safety;
  res->top    = res->rawtop    - res->safety;
  res->valid  = 1;

#ifdef DEBUG
  printf("Stack Object: bottom = %d,    top = %d\n",res->bottom,res->top);
  printf("           rawbottom = %d, rawtop = %d\n\n",res->rawbottom,res->rawtop);
#endif
  return res;
}







void HeapInitialize()
{
  int i;
  for (i=0; i<NumHeapObj; i++)
    {
      Heaps[i].id = i;
      Heaps[i].valid = 0;
      Heaps[i].top = 0;
      Heaps[i].bottom = 0;
      Heaps[i].alloc_start = 0;
      Heaps[i].rawtop = 0;
      Heaps[i].rawbottom = 0;
    }
}

HeapObj_t* GetHeap(value_t add)
{
  int i;
  for (i=0; i<NumHeapObj; i++)
    if (Heaps[i].valid && Heaps[i].id==i &&
	Heaps[i].bottom <= add &&
	Heaps[i].top    >= add)
      return (Heaps+i);
  return NULL;
}

/* we will manage heaps by a bitmap starting from 32meg up to 128M
   which gives us 96M to play with; the bitmap will measure in 32k chunks
   so there will be 3072 bits in the bitmap */
HeapObj_t* HeapObj_Alloc(int MinSize, int MaxSize)
{
  static int heap_count = 0;

  HeapObj_t *res = &(Heaps[heap_count]);

  int safety   = 2 * pagesize;
  int fullsize = MaxSize + 2 * safety + pagesize;
  int fullsize_pageround = RoundUp(fullsize,pagesize);
  int fullsize_chunkround = RoundUp(fullsize,chunksize);

  int chunkstart = AllocBitmapRange(bmp,fullsize_chunkround / chunksize);
  int start = (chunkstart * chunksize) + heapstart;
  assert(chunkstart >= 0);
  assert (heap_count < NumHeapObj);
  assert(MaxSize >= MinSize);

  res->safety = safety;
  res->rawbottom = (value_t) my_mmap((caddr_t) start,fullsize_pageround,
				  PROT_NONE,MAP_ANONYMOUS | MAP_FIXED);
  if (res->rawbottom == -1)
      exit(-1);

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


void HeapObj_Protect(HeapObj_t* res)
{
  long size = res->rawtop - res->rawbottom;
  long roundsize = (size + pagesize - 1) / pagesize * pagesize;
#ifdef DEBUG
  printf("PROTECT: rawbottom, roundsize, rawtop %d %d %d\n",
	 res->rawbottom,roundsize,res->rawtop);
#endif
  my_mprotect(6,(caddr_t) res->rawbottom, roundsize,PROT_NONE);

}

void HeapObj_Unprotect(HeapObj_t *res)
{
  long size = res->top - res->bottom;
  long roundsize = (size + pagesize - 1) / pagesize * pagesize;
  if (res-> top >= res->rawtop)
    printf("Error: res->top = %d  res->rawtop = %d\n",res->top,res->rawtop);
  my_mprotect(7,(caddr_t) res->bottom, roundsize, PROT_READ | PROT_WRITE);

}

int HeapObj_Getsize(HeapObj_t *res)
{
  int top = res->rawtop - res->safety;
  return top - res->bottom;
}

void HeapObj_Resize(HeapObj_t *res, long newsize)
{
  res->top = res->bottom + newsize;
  if (res->top > res->rawtop - res->safety)
    {
      printf("res->bottom, res->rawtop, res->top, newsize  %d %d %d %d\n",
	     res->bottom, res->rawtop, res->top, newsize);
      fprintf(stderr,"FATAL ERROR in heapobj_resize\n");
      exit(-1);
    }
}

int StackError(long badadd, long sp)
{
  StackObj_t *faultstack = 0;
  StackChainObj_t *faultchain = 0;
  int i;


  printf("\n------------------StackError---------------------\n");
  printf("sp, badreference:  %d   %d\n",sp,badadd);

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
	    StackObj_t *newstack = 0;
	    faultstack->used_bottom = sp;
	    newstack = StackObj_Alloc(faultchain);
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
				    PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_FIXED);
    assert(res == semantic_garbage_offset);
    res = (value_t) my_mmap((caddr_t)global_start + semantic_garbage_offset,
			    (size_t)(4 * 1024 * 1024),
			    PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_FIXED);
    assert(res == global_start + semantic_garbage_offset);
    printf("SEMANTIC GARBAGE: about to memset initially\n");
    wordset((void *)(global_start+semantic_garbage_offset),1,(size_t)(4 * 1024 * 1024));
    printf("SEMANTIC GARBAGE: Done memsetting initially\n");
  }
#endif

  bmp = CreateBitmap(Heapbitmap_bits);
  pagesize = sysconf(_SC_PAGE_SIZE);
  StackInitialize();
  HeapInitialize();

}
