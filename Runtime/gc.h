#include "memobj.h"
#include "thread.h"

void gc_init(void);
void gc(Thread_t *curThread);
void gc_finish(void);
void poll(void);

/* Specific collectors */
void poll_para(void);
void gc_semi(Thread_t *curThread);
void gc_gen(Thread_t *curThread, int);
void gc_para(Thread_t *curThread);
void gc_init_semi(void);
void gc_init_gen(void);
void gc_init_para(void);
void gc_finish_semi(void);
void gc_finish_gen(void);
void gc_finish_para(void);
void int_alloc_semi(void);
void float_alloc_semi(void);
void ptr_alloc_semi(void);
void int_alloc_gen(void);
void float_alloc_gen(void);
void ptr_alloc_gen(void);
void int_alloc_para(void);
void float_alloc_para(void);
void ptr_alloc_para(void);

extern long YoungHeapByte, MaxHeap, MinHeap;
extern double TargetRatio, MaxRatio;
extern double UpperRatioReward, LowerRatioReward;
extern double TargetSize, SizePenalty;
extern value_t writelist_cursor;
extern value_t writelist_start;
extern value_t writelist_end;
extern value_t write_count;
extern long NumLocatives;
extern long NumRoots;
extern long TotalBytesAllocated, TotalBytesCollected;
extern long TotalGenBytesCollected;

long ComputeHeapSize(long oldsize, double oldratio);
void paranoid_check_stack(Thread_t *, Heap_t *fromspace);
void paranoid_check_heap(Heap_t *fromspace, Heap_t *tospace);
void debug_and_stat_before(unsigned long *saveregs, long req_size);
void debug_after_collect(void);
void measure_semantic_garbage_after(void);


