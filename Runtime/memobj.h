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

struct range__t
{
  mem_t low;         /* pointers */
  mem_t high;
  unsigned int diff;    /* difference in bytes = 4 * (high - low) */
};

typedef struct range__t range_t;

void SetRange(range_t *range, mem_t low, mem_t high);

/* static inline InRange(mem_t addr, range_t *range)  */
#ifdef alpha_osf
static        int InRange(mem_t addr, range_t *range)
#pragma inline InRange
#else
static inline int InRange(mem_t addr, range_t *range)
#endif
{
  return ((unsigned int)addr - (unsigned int)range->low <= range->diff);
}

#ifdef alpha_osf
static        int NotInRange(mem_t addr, range_t *range) 
#pragma inline NotInRange
#else
static inline int NotInRange(mem_t addr, range_t *range) 
#endif
{
  return ((unsigned int)addr - (unsigned int)range->low > range->diff);
}

struct Heap__t
{
  int id;                  /* The ID for the heap object. */
  int valid;               /* Indicates whether this heap is in current use. */
  mem_t bottom;            /* The physical and logical bottom of the memory region and heap. */
  mem_t top;               /* The logical top of the heap. */
  mem_t physicalTop;       /* The physical top of the memory region. */
  mem_t cursor;            /* The next allocation point in the logical heap. bottom <= cursor <= top */
  struct range__t range;    /* The physical range bottom to physicalTop */
  pthread_mutex_t *lock;   /* Used to synchronize multiple access to heap object. */
};

typedef struct Heap__t Heap_t;

Heap_t* Heap_Alloc(int MinSize, int MaxSize);
Heap_t* GetHeap(ptr_t);
int inSomeHeap(ptr_t v);
void Heap_Check(Heap_t*);
void Heap_Protect(Heap_t*);
void Heap_Resize(Heap_t *res, long newsize);
void Heap_Unprotect(Heap_t *, long newsize);
int Heap_GetSize(Heap_t *res);
int Heap_GetAvail(Heap_t *res);
void PadHeapArea(mem_t bottom, mem_t top);
void GetHeapArea(Heap_t *heap, int size, mem_t *bottom, mem_t *top);

extern mem_t StartHeapLimit; /* When we don't have a real initial heap limit, use this one */
extern mem_t StopHeapLimit;  /* When heap limit is being used to interrupt a thread, use this one */
extern int pagesize;

#endif

