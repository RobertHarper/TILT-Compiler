#ifndef _gc_h
#define _gc_h

#include "memobj.h"
#include "thread.h"
#include "bitmap.h"
#include "create.h"
#include "gc_para.h"

extern int NumGC;
extern int forceMirrorArray, mirrorGlobal, mirrorArray;  /* Are we flipping globals and arrays? */
extern int primaryGlobalOffset, replicaGlobalOffset;     /* Used by concurrent collector to support global root redirection */
extern int primaryArrayOffset, replicaArrayOffset;       /* Used by generational, concurrent collector to support atomic redirection */

/* State of the collector */
typedef enum GCStatus__t { GCOff, GCPendingOn, GCOn, GCPendingOff } GCStatus_t;
typedef enum GCType__t   { Minor, Major } GCType_t;
                            /* Semi and SemiPara has no state variables */
extern GCType_t GCType;     /* Used by Gen, GenPara, GenConc */
extern GCStatus_t GCStatus; /* Used by SemiConc, GenConc */

/* Array value */
typedef enum Field__t {PointerField, IntField, DoubleField, MirrorPointerField, OldPointerField} Field_t; 
typedef struct ArraySpec__t {
  Field_t type;
  int     elemLen;  /* bytes for intField, words for pointerField, doublewords for doubleField */
  int     byteLen;  /* (+ 3) / 4 * 4 for intField, * 4 for pointerField, * 8 for doubleField */
  val_t   intVal;
  ptr_t   pointerVal;
  double  doubleVal;
} ArraySpec_t;

typedef enum Align__t {NoWordAlign, OddWordAlign, EvenWordAlign} Align_t;

void AlignMemoryPointer(mem_t *allocRef, Align_t align);
mem_t AllocFromThread(Thread_t *thread, int bytesToAlloc, Align_t align);             /* bytesToAlloc does not include alignment */
mem_t AllocFromHeap(Heap_t *heap, Thread_t *thread, int bytesToAlloc, Align_t align); /* bytesToAlloc does not include alignment */

/* Heaps and work stacks used by various collectors.  */
extern Heap_t *fromSpace, *toSpace;                   /* The 2 semispaces or the tenured area of a generational collector */
extern Heap_t *nursery;                               /* Used by the generational collector */
extern SharedStack_t *workStack;                      /* Used by all parallel/concurrent collectors */

/* GCFromML has a non-standard calling convention */
void GCFromC(Thread_t *, int RequestSizeBytes, int isMajor);

/* These are initialization and finalization routines. */
void GCInit(void);
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

int GCFromScheduler(Proc_t *, Thread_t *);  /* Returns whether thread can be mapped */
void GCFromMutator(Thread_t *);                  /* Does not return; goes to scheduler; argument may be NULL  */
void GCRelease(Proc_t *proc);          /* Called by scheduler when a thread is unmapped */

/* Can we continue execution without a stop-and-copy */
int GCTry_Semi(Proc_t *, Thread_t *);
int GCTry_Gen(Proc_t *, Thread_t *);
int GCTry_SemiPara(Proc_t *, Thread_t *);
int GCTry_GenPara(Proc_t *, Thread_t *);
int GCTry_SemiConc(Proc_t *, Thread_t *);
int GCTry_GenConc(Proc_t *, Thread_t *);

/* Perform a stop-and-copy collection */
void GCStop_Semi(Proc_t *);
void GCStop_Gen(Proc_t *);
void GCStop_SemiPara(Proc_t *);
void GCStop_GenPara(Proc_t *);
/* Concurrent collectors do not have a Stop version */

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
extern int minOffRequest, minOnRequest;  /* Mutator handed multiples of this amount of space for parallel and concurrent collectors */
extern int threadFetchSize;              /* Number of thread/stackchains to fetch */
extern int globalLocFetchSize;           /* Number of globals to fetch from global pool */
extern int rootLocFetchSize;             /* Number of root locs to fetch from global pool */
extern int objFetchSize;                 /* Number of objects to fetch from global pool */
extern int segFetchSize;                 /* Number of (large object) segments to fetch from global pool */
extern int doCopyCopySync;               
extern int localWorkSize;
extern int arraySegmentSize;             /* If zero, not splitting large arrays.
					    An array of more than arraySegmentSize bytes is considered large and
					    broken into segments for incremental copying.
					    Each segment (except possibly the last) is of size arraySegmentSize. */

extern double minorCollectionRate;   /* Ratio of minor coll rate to alloc rate */
extern double majorCollectionRate;   /* Ratio of major coll rate to alloc rate */

extern double objCopyWeight;  
extern double objScanWeight;
extern double fieldCopyWeight;  
extern double fieldScanWeight;
extern double pageWeight;
extern double globalWeight;
extern double stackSlotWeight;

long ComputeHeapSize(long oldsize, double oldratio);
double HeapAdjust1(unsigned int reqSize, Heap_t *from1, Heap_t *to);
double HeapAdjust2(unsigned int reqSize, Heap_t *from1, Heap_t *from2, Heap_t *to);

/* Make sure all the pointer values in the stack/globals are in the legal heaps */
void paranoid_check_all(Heap_t *firstPrimary, Heap_t *secondPrimary,
			Heap_t *firstReplica, Heap_t *secondReplica, 
			Heap_t *largeSpace);

void measure_semantic_garbage_after(void);

#endif
