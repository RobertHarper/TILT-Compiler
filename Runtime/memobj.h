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
  value_t top;
  value_t bottom;
  value_t rawtop;
  value_t rawbottom;
  value_t used_bottom;
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
Stack_t* GetStack(value_t);
int StackError(struct ucontext *, long);


StackChain_t* StackChain_Alloc(void);
void memobj_init(void);
int InStackChain(StackChain_t*, value_t);

struct Heap__t
{
  int id;                    /* The ID for the heap object. */
  int valid;                 /* Indicates whether this heap is in current use. */
  value_t bottom;            /* The physical and logical bottom of the memory region and heap. */
  value_t top;               /* The logical top of the heap. */
  value_t actualTop;         /* The physical top of the memory region. */
  value_t alloc_start;       /* The next allocation point in the logical heap. 
				This must lie between bottom and top.  */
  pthread_mutex_t *lock;     /* Used to synchronize multiple access to heap object. */
};

typedef struct Heap__t Heap_t;

Heap_t* Heap_Alloc(int MinSize, int MaxSize);
Heap_t* GetHeap(value_t);
void Heap_Protect(Heap_t*);
void Heap_Unprotect(Heap_t*);
void Heap_Resize(Heap_t *res, long newsize);
int Heap_Getsize(Heap_t *res);
void GetHeapArea(Heap_t *heap, int size, value_t **bottom, value_t **top);

extern value_t StartHeapLimit; /* When we don't have a real initial heap limit, use this one */
extern value_t StopHeapLimit;  /* When heap limit is being used to interrupt a thread, use this one */
extern int pagesize;

#endif

