#include "memobj.h"
#include "thread.h"

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
void GC_Semi(SysThread_t *, int request);
void GC_Gen(SysThread_t *, int request, int isMajor);
void GC_SemiPara(SysThread_t *, int request);

int returnFromGCFromC(Thread_t *);
int returnFromGCFromML(Thread_t *);
int returnFromYield(Thread_t *);

/* Allocating large objects. These routines call may call GCFromC. */

value_t alloc_bigintarray_Semi(int byteLen, value_t value, int tag);
value_t alloc_bigintarray_Gen (int byteLen, value_t value, int tag);
value_t alloc_bigintarray_Para(int byteLen, value_t value, int tag);

value_t alloc_bigptrarray_Semi(int logLen, value_t value, int tag);
value_t alloc_bigptrarray_Gen (int logLen, value_t value, int tag);
value_t alloc_bigptrarray_Para(int logLen, value_t value, int tag);

value_t alloc_bigfloatarray_Semi(int logLen, double value, int tag);
value_t alloc_bigfloatarray_Gen (int logLen, double value, int tag);
value_t alloc_bigfloatarray_Para(int logLen, double value, int tag);


extern double MinRatio, MaxRatio;
extern int MinRatioSize, MaxRatioSize;
extern value_t writelist_cursor;
extern value_t writelist_start;
extern value_t writelist_end;
extern value_t write_count;
extern int NumLocatives;
extern int NumRoots;
extern int KBytesAllocated, KBytesCollected;
extern int GenKBytesCollected;

long ComputeHeapSize(long oldsize, double oldratio);
void paranoid_check_stack(Thread_t *, Heap_t *fromspace);
void paranoid_check_heap_global(Heap_t *space, Heap_t **legalHeaps);
void debug_and_stat_before(unsigned long *saveregs, long req_size);
void debug_after_collect(Heap_t *nursery, Heap_t *oldspace);
void measure_semantic_garbage_after(void);

