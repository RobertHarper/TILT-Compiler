/* Not thread-safe */

#ifndef _memobj_h
#define _memobj_h

#include "tag.h"
#include <pthread.h>
#include <ucontext.h>

struct StackChain__t;

struct Stack__t
{
  int id;
  int valid;
  mem_t top;
  mem_t bottom;
  mem_t rawtop;
  mem_t rawbottom;
  mem_t used_bottom;
  long safety;
  struct StackChain__t *parent;
};

typedef struct Stack__t Stack_t;

struct StackChain__t
{
  int size;
  int count;
  Stack_t **stacks;
};

typedef struct StackChain__t StackChain_t;

Stack_t* Stack_Alloc(StackChain_t *);
Stack_t* GetStack(mem_t);
mem_t StackError(struct ucontext *, mem_t);


StackChain_t* StackChain_Alloc(void);
void memobj_init(void);
int InStackChain(StackChain_t*, mem_t);

struct Heap__t
{
  int id;                    /* The ID for the heap object. */
  int valid;                 /* Indicates whether this heap is in current use. */
  mem_t bottom;            /* The physical and logical bottom of the memory region and heap. */
  mem_t top;               /* The logical top of the heap. */
  mem_t actualTop;         /* The physical top of the memory region. */
  mem_t alloc_start;       /* The next allocation point in the logical heap. 
				This must lie between bottom and top.  */
  pthread_mutex_t *lock;     /* Used to synchronize multiple access to heap object. */
};

typedef struct Heap__t Heap_t;

Heap_t* Heap_Alloc(int MinSize, int MaxSize);
Heap_t* GetHeap(ptr_t);
int inSomeHeap(ptr_t v);
void Heap_Protect(Heap_t*);
void Heap_Unprotect(Heap_t*);
void Heap_Resize(Heap_t *res, long newsize);
int Heap_Getsize(Heap_t *res);
void GetHeapArea(Heap_t *heap, int size, mem_t *bottom, mem_t *top);

extern mem_t StartHeapLimit; /* When we don't have a real initial heap limit, use this one */
extern mem_t StopHeapLimit;  /* When heap limit is being used to interrupt a thread, use this one */
extern int pagesize;

#endif

