#include "memobj.h"
#include "thread.h"
#include "bitmap.h"
#include "create.h"
#include "gc_para.h"
extern int NumGC;
extern int pagesize;

/* State the mutator/collector is in.
   The *Pending* states are used to interrupt all processors for the parallel and concurrent collectors.
   The Major type is used for generational collectors.
   The BeginMajor and EndMajor types are used for the generational, concurrent collector.
*/
enum GCStatus { GCOff, GCPendingOn, GCOn, GCPendingOff };
enum GCType { Minor, Major, BeginMajor, EndMajor };  

extern enum GCStatus GCStatus;
extern enum GCType GCType;

/* Heaps and work stacks used by various collectors.  */
extern Heap_t *fromSpace, *toSpace;
extern Heap_t *nursery, *tenuredFrom, *tenuredTo;
extern SharedStack_t *workStack, *majorWorkStack1, *majorWorkStack2;

/* GCFromML has a non-standard calling convention */
void GCFromC(Thread_t *, int RequestSizeBytes, int isMajor);

/* These are initialization and finalization routines. */
void gc_init(void);
void gc_finish(void);
void gc_init_Semi(void);
void gc_init_Gen(void);
void gc_init_SemiPara(void);
void gc_init_GenPara(void);
void gc_init_SemiConc(void);
void gc_init_GenConc(void);
void gc_finish_Semi(void);
void gc_finish_Gen(void);
void gc_finish_SemiPara(void);
void gc_finish_GenPara(void);
void gc_finish_SemiConc(void);
void gc_finish_GenConc(void);

/* Idle (unmapped) processors call the poll function periodically in case there is GC work. */
void gc_poll(SysThread_t *);              /* May return immediately or do some work */
void gc_poll_SemiPara(SysThread_t *);
void gc_poll_GenPara(SysThread_t *);
void gc_poll_SemiConc(SysThread_t *);
void gc_poll_GenConc(SysThread_t *);

/* Actual collection routines */

int GCFromScheduler(SysThread_t *, Thread_t *);  /* Returns whether thread can be mapped */
void GCFromMutator(Thread_t *);                  /* Does not return; goes to scheduler; argument may be NULL  */
void GCRelease(SysThread_t *sysThread);          /* Called by scheduler when a thread is unmapped */

/* Can we continue execution without a stop-and-copy */
int GCTry_Semi(SysThread_t *, Thread_t *);
int GCTry_Gen(SysThread_t *, Thread_t *);
int GCTry_SemiPara(SysThread_t *, Thread_t *);
int GCTry_GenPara(SysThread_t *, Thread_t *);
int GCTry_SemiConc(SysThread_t *, Thread_t *);
int GCTry_GenConc(SysThread_t *, Thread_t *);

/* Perform a stop-and-copy collection */
void GCStop_Semi(SysThread_t *);
void GCStop_Gen(SysThread_t *);
void GCStop_SemiPara(SysThread_t *);
void GCStop_GenPara(SysThread_t *);
/* Concurrent collectors do not have a Stop version */

/* Must be called each time a thread is released */
void GCRelease_SemiConc(SysThread_t *sysThread);
void GCRelease_GenConc(SysThread_t *sysThread);


int returnFromGCFromC(Thread_t *);
int returnFromGCFromML(Thread_t *);
int returnFromYield(Thread_t *);

/* Allocating large objects. These routines call may call GCFromC. */

ptr_t alloc_bigintarray_Semi(int byteLen, int value, int tag);
ptr_t alloc_bigintarray_Gen (int byteLen, int value, int tag);
ptr_t alloc_bigintarray_SemiPara(int byteLen, int value, int tag);
ptr_t alloc_bigintarray_GenPara(int byteLen, int value, int tag);
ptr_t alloc_bigintarray_SemiConc(int byteLen, int value, int tag);
ptr_t alloc_bigintarray_GenConc(int byteLen, int value, int tag);


ptr_t alloc_bigptrarray_Semi(int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_Gen (int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_SemiPara(int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_GenPara(int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_SemiConc(int logLen, ptr_t value, int tag);
ptr_t alloc_bigptrarray_GenConc(int logLen, ptr_t value, int tag);

ptr_t alloc_bigfloatarray_Semi(int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_Gen (int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_SemiPara(int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_GenPara(int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_SemiConc(int logLen, double value, int tag);
ptr_t alloc_bigfloatarray_GenConc(int logLen, double value, int tag);


extern double MinRatio, MaxRatio;
extern int MinRatioSize, MaxRatioSize;
extern int NumRoots, NumWrites, NumLocatives;
extern int KBytesAllocated, KBytesCollected;
extern int GenKBytesCollected;

long ComputeHeapSize(long oldsize, double oldratio);
void HeapAdjust(int show, unsigned int reqSize, Heap_t **froms, Heap_t *to);

/* Make sure all the pointer values in the stack/globals are in the legal heaps */
void paranoid_check_stack(char *label, Thread_t *thread, Heap_t **legalHeaps, Bitmap_t **legalStarts);
void paranoid_check_global(char *, Heap_t **legalHeaps, Bitmap_t **legalStarts);
Bitmap_t *paranoid_check_heap_without_start(char *, Heap_t *space, Heap_t **legalHeaps);
void paranoid_check_heap_with_start(char *, Heap_t *space, Heap_t **legalHeaps, Bitmap_t **legalStarts);
void paranoid_check_all(Heap_t *firstPrimary, Heap_t *secondPrimary,
			Heap_t *firstReplica, Heap_t *secondReplica);

void measure_semantic_garbage_after(void);

