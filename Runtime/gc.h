#ifndef _gc_h
#define _gc_h

#include "memobj.h"
#include "thread.h"
#include "bitmap.h"
#include "create.h"
#include "barriers.h"
#include "gc_para.h"

extern Statistic_t        heapSizeStatistic;    
extern Statistic_t        minorSurvivalStatistic;
extern Statistic_t        majorSurvivalStatistic;

extern int NumGC;
extern int forceMirrorArray, mirrorGlobal, mirrorArray;  /* Are we flipping globals and arrays? */
extern int primaryGlobalOffset, replicaGlobalOffset;     /* Used by concurrent collector to support global root redirection */
extern int primaryArrayOffset, replicaArrayOffset;       /* Used by generational, concurrent collector to support atomic redirection */
extern double pauseWarningThreshold;
extern int warnThreshold;
extern double warnUtil;
extern int addOldStackletOnUnderflow;
extern int accountingLevel;

/* States of the collector */
typedef enum GCStatus__t { GCOff, GCPendingAgressive, GCAgressive, GCPendingOn, GCOn, GCPendingOff } GCStatus_t;
typedef enum GCType__t   { Minor, Major } GCType_t;
                            /* Semi and SemiPara has no state variables */
extern volatile GCType_t GCType;     /* Used by Gen, GenPara, GenConc */
extern volatile GCStatus_t GCStatus; /* Used by SemiConc, GenConc */

/* Array value */
typedef enum Field__t {PointerField, IntField, DoubleField, MirrorPointerField, OldPointerField} Field_t; 
typedef struct ArraySpec__t {
  Field_t type;
  int     elemLen;  /* bytes for intField, words for pointerField, doublewords for doubleField */
  int     byteLen;  /* (+ 3) / 4 * 4 for intField, * 4 for pointerField, * 8 for doubleField */
  val_t   intVal;
  volatile ptr_t   pointerVal;
  double  doubleVal;
} ArraySpec_t;

typedef enum Align__t {NoWordAlign, OddWordAlign, EvenWordAlign} Align_t;

#pragma INLINEP(AlignMemoryPointer)
static INLINE
mem_t AlignMemoryPointer(mem_t alloc, Align_t align)
{
  int curEven;
  if (align == NoWordAlign)
    return alloc;
  curEven = (((val_t)(alloc)) & 7) == 0;
  if ((align == OddWordAlign && curEven) ||
      (align == EvenWordAlign && !curEven))
    *(alloc++) = MAKE_SKIP(1);
  return alloc;
}

#pragma INLINEP(UnusedProcAlloc)
static INLINE
int UnusedProcAlloc(Proc_t *proc)
{
  return sizeof(val_t) * (proc->allocLimit - proc->allocCursor);
}

mem_t AllocFromThread(Thread_t *thread, int bytesToAlloc, Align_t align);             /* bytesToAlloc does not include alignment */
mem_t AllocFromHeap(Heap_t *heap, Thread_t *thread, int bytesToAlloc, Align_t align); /* bytesToAlloc does not include alignment */

/* Heaps and work stacks used by various collectors.  */
extern Heap_t *fromSpace, *toSpace;                   /* The 2 semispaces or the tenured area of a generational collector */
extern Heap_t *nursery;                               /* Used by the generational collector */
extern SharedStack_t *workStack;                      /* Used by all parallel/concurrent collectors */
extern Barriers_t *barriers;                          /* Used by all parallel/concurrent collectors */

/* GCFromML has a non-standard calling convention */
void GCFromC(Thread_t *, int RequestSizeBytes, int isMajor);

/* Is there enough allocation and write buffer space in processor to map thread? */
int GCSatisfiable(Proc_t *proc, Thread_t *th);

/* Assign heap to allocation area of a processor - if heap is null, fields set to StartHeapLimit - must be null for multiple processors*/
void ResetAllocation(Proc_t *proc, Heap_t *h);

/* These are initialization and finalization routines. */
void GCInit(void);
void GCInit_Help(double defaultMinRatio, double defaultMaxRatio, 
		 int defaultMinRatioSize, int defaultMaxRatioSize);
void GCInit_Semi(void);
void GCInit_Gen(void);
void GCInit_SemiPara(void);
void GCInit_GenPara(void);
void GCInit_SemiConc(void);
void GCInit_GenConc(void);

/* Idle (unmapped) processors call the poll function periodically in case there is GC work. */
void GCPoll(Proc_t *);              /* May return immediately or do some work */
void GCPoll_SemiPara(Proc_t *);
void GCPoll_GenPara(Proc_t *);
void GCPoll_SemiConc(Proc_t *);
void GCPoll_GenConc(Proc_t *);

/* Actual collection routines */

void GCFromScheduler(Proc_t *, Thread_t *);  
void GCFromMutator(Thread_t *);                  /* Does not return; goes to scheduler; argument may be NULL  */
void GCReleaseThread(Proc_t *proc);          /* Called by scheduler when a thread is unmapped */

/* The collector functions */
void GC_Semi(Proc_t *, Thread_t *);
void GC_SemiPara(Proc_t *, Thread_t *);
void GC_SemiConc(Proc_t *, Thread_t *);
void GC_Gen(Proc_t *, Thread_t *);
void GC_GenPara(Proc_t *, Thread_t *);
void GC_GenConc(Proc_t *, Thread_t *);


/* Must be called each time a thread is released */
void GCRelease_Semi(Proc_t *proc);
void GCRelease_Gen(Proc_t *proc);
void GCRelease_SemiPara(Proc_t *proc);
void GCRelease_GenPara(Proc_t *proc);
void GCRelease_SemiConc(Proc_t *proc);
void GCRelease_GenConc(Proc_t *proc);

int returnToML(Thread_t *, mem_t linkValue);
int returnFromGCFromC(Thread_t *);
int returnFromGCFromML(Thread_t *);
int returnFromYield(Thread_t *);

/* Allocating large arrays. These routines call may call GCFromC. */
ptr_t AllocBigArray_Semi(Proc_t *, Thread_t *, ArraySpec_t *);
ptr_t AllocBigArray_Gen (Proc_t *, Thread_t *, ArraySpec_t *);
ptr_t AllocBigArray_SemiPara(Proc_t *, Thread_t *, ArraySpec_t *);
ptr_t AllocBigArray_GenPara(Proc_t *, Thread_t *, ArraySpec_t *);
ptr_t AllocBigArray_SemiConc(Proc_t *, Thread_t *, ArraySpec_t *);
ptr_t AllocBigArray_GenConc(Proc_t *, Thread_t *, ArraySpec_t *);

extern double MinRatio, MaxRatio;
extern int MinRatioSize, MaxRatioSize;
extern long NumRoots, NumContentions, NumWrites, NumLocatives;
extern int GenKBytesCollected;
extern int minOffRequest, maxOffRequest, minOnRequest; 
extern int copyPageSize;                 /* Size of area allocated for a collector */
extern int copyCheckSize;                /* Amount limit is increment for creation of gray regions */
extern int copyChunkSize;                /* Minimum size of object that causes direct allocation from heap if local pool empty */
extern int threadFetchSize;              /* Number of thread/stackchains to fetch */
extern int globalLocFetchSize;           /* Number of globals to fetch from global pool */
extern int rootLocFetchSize;             /* Number of root locs to fetch from global pool */
extern int objFetchSize;                 /* Number of objects to fetch from global pool */
extern int grayRegionFetchSize;          /* Number of gray regions to fetch from global pool */
extern int segFetchSize;                 /* Number of (large object) segments to fetch from global pool */
extern int backObjFetchSize;             /* Number of back objects to fetch from global pool */
extern int backLocFetchSize;             /* Number of back locations to fetch from global pool */
extern int doCopyCopySync;               
extern int doAgressive, doMinorAgressive;
extern double useLastUtil, targetUtil;
extern int localWorkSize;
extern int arraySegmentSize;             /* If zero, not splitting large arrays.
					    An array of more than arraySegmentSize bytes is considered large and
					    broken into segments for incremental copying.
					    Each segment (except possibly the last) is of size arraySegmentSize. */
extern int ordering;                 /* Implicit queue, explicit stack, explicit queue */
extern int forceSpaceCheck;          /* Do space check even when not necessary */
extern double CollectionRate;   /* Ratio of coll rate to alloc rate */
extern int grayAsReplica;            /* In concurrent collector, store gray set as replicas (with backpointers) */

extern double objCopyWeight;  
extern double objScanWeight;
extern double fieldCopyWeight;  
extern double fieldScanWeight;
extern double ptrFieldScanWeight;
extern double pageWeight;
extern double rootWeight;
extern double globalWeight;
extern double stackSlotWeight;

/* The amount "request" is added to what is considered live.  
   The amount "withhold" is subtracted from both spaces for computation of liveness ratio.
   The fraction "reserve" is reserved for concurrent collector.
*/
void HeapAdjust1(int request, int unused, int withhold,  double rate, int phases, Heap_t *from1, Heap_t *to);
void HeapAdjust2(int request, int unused, int withhold,  double rate, int phases,  Heap_t *from1, Heap_t *from2, Heap_t *to);
int expandedToReduced(int size, double rate, int phases);
int reducedToExpanded(int size, double rate, int phases);

/* Make sure all the pointer values in the stack/globals are in the legal heaps */
void paranoid_check_all(Heap_t *firstPrimary, Heap_t *secondPrimary,
			Heap_t *firstReplica, Heap_t *secondReplica, 
			Heap_t *largeSpace);

void measure_semantic_garbage_after(void);

#endif
