#include "memobj.h"
#include "thread.h"
#include "bitmap.h"

enum GCType { Unknown, Minor, Major, ForcedMajor };

/* GCFromML has a non-standard calling convention */
void GCFromC(Thread_t *, int RequestSizeBytes, int isMajor);

/* These are initialization and finalization routines. */
void gc_init(void);
void gc_finish(void);
void gc_init_Semi(void);
void gc_init_Gen(void);
void gc_init_SemiPara(void);
void gc_finish_Semi(void);
void gc_finish_Gen(void);
void gc_finish_SemiPara(void);

/* Idle (unmapped) processors call the poll function periodically in case there is GC work. */
void gc_poll(SysThread_t *);              /* May return immediately or do some work */
void gc_poll_SemiPara(SysThread_t *);

/* Actual collection routines */

int GCAllocate(SysThread_t *, int);
int GCAllocate_Semi(SysThread_t *, int);
int GCAllocate_Gen(SysThread_t *, int);
int GCAllocate_SemiPara(SysThread_t *, int);

void GC(Thread_t *);  /* Does not return; goes to scheduler; argument may be NULL  */
void GC_Semi(SysThread_t *);
void GC_Gen(SysThread_t *);
void GC_SemiPara(SysThread_t *);
void GC_GenPara(SysThread_t *);

int returnFromGCFromC(Thread_t *);
int returnFromGCFromML(Thread_t *);
int returnFromYield(Thread_t *);

/* Allocating large objects. These routines call may call GCFromC. */

ptr_t alloc_bigintarray_Semi(int byteLen, int value, int tag);
ptr_t alloc_bigintarray_Gen (int byteLen, int value, int tag);
ptr_t alloc_bigintarray_SemiPara(int byteLen, int value, int tag);
ptr_t alloc_bigintarray_GenPara(int byteLen, int value, int tag);

ptr_t alloc_bigptrarray_Semi(int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_Gen (int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_SemiPara(int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_GenPara(int logLen, ptr_t value, int tag);

ptr_t alloc_bigfloatarray_Semi(int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_Gen (int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_SemiPara(int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_GenPara(int logLen, double value, int tag);


extern double MinRatio, MaxRatio;
extern int MinRatioSize, MaxRatioSize;
extern loc_t *writelist_cursor;
extern loc_t *writelist_start;
extern loc_t *writelist_end;
extern unsigned int write_count;
extern int NumLocatives;
extern int NumRoots;
extern int KBytesAllocated, KBytesCollected;
extern int GenKBytesCollected;

long ComputeHeapSize(long oldsize, double oldratio);
void HeapAdjust(int show, unsigned int reqSize, Heap_t **froms, Heap_t *to);

void paranoid_check_stack(char *, Thread_t *, Heap_t *fromspace);
void paranoid_check_global(char *, Heap_t **legalHeaps, Bitmap_t **legalStarts);
Bitmap_t *paranoid_check_heap_without_start(char *, Heap_t *space, Heap_t **legalHeaps);
void paranoid_check_heap_with_start(char *, Heap_t *space, Heap_t **legalHeaps, Bitmap_t **legalStarts);
void debug_and_stat_before(unsigned long *saveregs, long req_size);
void debug_after_collect(Heap_t *nursery, Heap_t *oldspace);
void measure_semantic_garbage_after(void);

