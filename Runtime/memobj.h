/* Not thread-safe */

#ifndef _memobj_h
#define _memobj_h

#include "bitmap.h"
#include "tag.h"
#include "queue.h"
#include <pthread.h>
#include <ucontext.h>

#if (defined solaris)
#define pagesize 8192
#elif (defined alpha_osf)
#define pagesize 8192
#endif

extern int MLStackletSize, CStackletSize;
extern int primaryStackletOffset, replicaStackletOffset;
struct StackChain__t;
/* Inconsistent    - no information on primary or replica
   Pending         - primary not used; replica uninitialized but needs to be copied before primary modified
   Copying         - primary not used; replica snapshot being made, copying from primary snapshot
   InactiveCopied  - primary not used; replica snapshot made (and scanned by end of GC)
   ActiveCopied    - primary used; replica snapshot made (and scanned by end of GC)
*/
typedef enum StackletState__t {Inconsistent, Pending, Copying, InactiveCopied, ActiveCopied} StackletState_t;

/* Each stacklet is actually a pair of stacklets.  The variable stackletOffset indicatse which one we use */
typedef volatile struct Stacklet__t
{
  int  count;                    /* Reference count of how many stack chains I belong to */
  int mapped;                    /* Has memory been mapped to this stacklet */
  StackletState_t state;
 /* Base region of mapped memory with cursor - Other region obtained by adding constant offset. */
  mem_t baseExtendedBottom;      /* True bottom of stack - extra area for C */
  mem_t baseBottom;              /* Normal bottom of stack */
  mem_t baseCursor;
  mem_t baseTop;
  mem_t retadd;                  /* retadd necessary for resumption */
  /* These fields are for scanning the stack */
  mem_t replicaCursor;                 /* These 2 fields are recorded when the replica is copied from the primary */
  mem_t replicaRetadd;                 /*   so that the replica stacklet can be later scanned */
  unsigned int topRegstate;            /* Register state (mask) at top frame */
  unsigned int bottomRegstate;         /* Register state (mask) at bottom frame */
  Stack_t  *callinfoStack;             /* Corresponds to stack frames of this stacklet */
  struct StackChain__t *parent;        /* Stack chain this stacklet belongs to */
} Stacklet_t;

typedef struct StackChain__t
{
  volatile int used;
  volatile int cursor;                  /* Index of first uninitialized stacklet */
  Stacklet_t *stacklets[10];   /* Should be dynamic XXXX */
  volatile void *thread;                /* Really of type Thread_t *  */
} StackChain_t;

StackChain_t* StackChain_Alloc(void *);       /* Obtains an initial stacklet */
void StackChain_Dealloc(StackChain_t *);
StackChain_t* StackChain_Copy(void *, StackChain_t *);  /* Dupliacte stacklets and link to replicas */
int StackChain_Size(StackChain_t *);  /* Total sizes of active area of stacklets */

mem_t StackletPrimaryTop(Stacklet_t *stacklet);
mem_t StackletPrimaryCursor(Stacklet_t *stacklet);
mem_t StackletPrimaryBottom(Stacklet_t *stacklet);

void Stacklet_Dealloc(Stacklet_t *stacklet); /* Decrease reference count; if freed, call dealloc on replica */
Stacklet_t* GetStacklet(mem_t sp);   /* Stack chain can be obatined by looking at parent field */
Stacklet_t* CurrentStacklet(StackChain_t *);  /* Get bottom stacklet of chain */
Stacklet_t *NewStacklet(StackChain_t *); /* Allocate new stacklet to chain */
Stacklet_t *EstablishStacklet(StackChain_t *stackChain, mem_t sp); /* fix stackchain cursor (possible exceptions); return active stacklet */
void PopStacklet(StackChain_t *); /* Pop most recent stacklet - at least one must remain */
mem_t StackError(struct ucontext *, mem_t);
int Stacklet_Copy(Stacklet_t *);   /* Replica area copied from primary area of stacklet; returns whether caller was copier */
void Stacklet_KillReplica(Stacklet_t *);                  /* Mark replica area inconsistent with primary area */
void DequeueStacklet(StackChain_t *stackChain);

void memobj_init(void);

struct range__t
{
  mem_t low;         /* pointers */
  mem_t high;
  unsigned int diff;    /* difference in bytes = 4 * (high - low) */
};

typedef struct range__t range_t;

void SetRange(range_t *range, mem_t low, mem_t high);

/* static inline InRange(mem_t addr, range_t *range)  */
INLINE(InRange)
int InRange(mem_t addr, range_t *range)
{
  return ((unsigned int)addr - (unsigned int)range->low <= range->diff);
}

INLINE(NotInRange)
int NotInRange(mem_t addr, range_t *range) 
{
  return ((unsigned int)addr - (unsigned int)range->low > range->diff);
}

typedef struct Heap__t
{
  int id;                      /* The ID for the heap object. */
  volatile int valid;          /* Indicates whether this heap is in current use. */
  mem_t bottom;                /* The physical and logical bottom of the memory region and heap. */
  volatile mem_t top;          /* The logical top of the heap. */
  volatile mem_t mappedTop;    /* The top of the memory region that is mapped. */
  volatile mem_t writeableTop; /* The top of the memory region that is unprotected. */
  volatile mem_t cursor;       /* The next allocation point in the logical heap. bottom <= cursor <= top */
  volatile int   size;         /* = top - bottom (in bytes) - makes inHeap faster */
  struct range__t range;       /* The physical range bottom to physicalTop */
  pthread_mutex_t *lock;       /* Used to synchronize multiple access to heap object. */
  volatile int    *freshPages; /* Pages not access by collector since start of GC */
  Bitmap_t *bitmap;            /* Stores starts of objects for debugging */
} Heap_t;

INLINE(inHeap)
int inHeap(ptr_t v, Heap_t *heap)
{
  return (((val_t) v - (val_t) heap->bottom) < heap->size);
}

Heap_t* Heap_Alloc(int MinSize, int MaxSize);
Heap_t* GetHeap(ptr_t);
int inSomeHeap(ptr_t v);
void Heap_Check(Heap_t*);
void Heap_Reset(Heap_t *);
struct Proc__t;
int Heap_ResetFreshPages(struct Proc__t *, Heap_t *);
INLINE(Heap_TouchPage)
int Heap_TouchPage(Heap_t *h, mem_t addr) /* Returns 1 if fresh */
{
  int offset = sizeof(val_t) * (addr - h->bottom);
  int page = DivideDown(offset, pagesize);
  return 0;
  /*
  int word = page >> 5;
  int bit = page & 31;
  int mask = 1 << bit;
  int info = h->freshPages[word];
  assert(sizeof(int) == 4);
  h->freshPages[word] = info | mask;
  return !(mask & info);
  */
  /*
  int info = h->freshPages[page];
  h->freshPages[page] = 1;
  return !info;
  */
}

void Heap_Resize(Heap_t *, long newSize, int reset);  /* Resizes the heap, making mprotect calls if paranoid;
							 the cursor is set to bottom if reset is true;
							 if the heap is being shrunk, then reset must be true */
int Heap_GetSize(Heap_t *res);                        /* Current size */
int Heap_GetMaximumSize(Heap_t *res);                 /* Maximum size */
int Heap_GetAvail(Heap_t *res);                       /* Space unused under current size */
int Heap_GetUsed(Heap_t *res);                        /* Space used or allocated to processor */
void PadHeapArea(mem_t bottom, mem_t top);
void GetHeapArea(Heap_t *heap, int size, mem_t *bottom, mem_t *cursor, mem_t *top);

extern mem_t StartHeapLimit; /* When we don't have a real initial heap limit, use this one */
extern mem_t StopHeapLimit;  /* When heap limit is being used to interrupt a thread, use this one */


#endif

