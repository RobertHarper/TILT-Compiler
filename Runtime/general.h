#ifndef _general_h
#define _general_h

#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif
#ifdef solaris
#include "interface_solaris.h"
#endif

#undef GCTABLE_HASENTRYID
#undef PARANOID
#undef STACKDEBUG
#define WRITE
#define OLD_ALLOC

#define Semispace 0
#define Generational 1
#define SemispaceParallel 2
#define GenerationalParallel 3
#define SemispaceConcurrent 4
#define GenerationalConcurrent 5
#define SemispaceExplicit 6

#define DefaultOrder 0
#define ImplicitOrder 1
#define QueueOrder 2
#define StackOrder 3
#define HybridOrder 4

#ifndef _asm_

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef DEBUG
#define fastAssert(x)  assert(x)
#else
#define fastAssert(x) 
#endif

#define DivideUp(a,div) (((int)(a) + (div) - 1) / (div))
#define DivideDown(a,div) ((int)(a)  / (div))
#define RoundUp(x,mult) (((int)(x) + (mult) - 1) / (mult) * (mult))
#define RoundDown(x,mult) (((int)(x) / (mult)) * (mult))
#define Max(a,b) ((a) > (b) ? (a) : (b))
#define Min(a,b) ((a) < (b) ? (a) : (b))
#define typed_swap(t,a,b) { t swap_temp = a; a = b; b = swap_temp; }
#define arraysize(a) (sizeof(a)/sizeof(*(a)))
#define BUG(x) {printf(x); exit(-1); }

void init_int(int *, int);
void init_double(double *, double);
long FetchAndAdd(volatile long *, int);
long TestAndSet(volatile long *);
int CompareAndSwap(volatile int *location, int testValue, int swapValue); /* Returns value in memory location */
void memOrder(void);
void memBarrier(void);

extern int checkAtGC;
extern int LEAST_GC_TO_CHECK;
extern int SHOW_GCSTATS;
extern int SHOW_GCFORWARD;
extern int SHOW_GCDEBUG;
extern int SHOW_GCERROR;
extern int SHOW_HEAPS;
extern int SHOW_GLOBALS;
extern int NurseryByte, MinHeapByte, MaxHeapByte;
extern double TargetRatio;
extern int StackSize;
extern int save_rate;
extern int use_stack_gen;
extern int collector_type;
extern int paranoid;
extern int debug;
extern int debugStack;
extern int verbose;
extern int timeDiag;
extern int diag;
extern int collectDiag;
extern int threadDiag;
extern int NumProc, RotateProc;
extern int NumThread;
extern int NumStack;
extern int NumStackChain;
extern int NumHeap;
extern int NumGC, NumMajorGC;
extern int noSharing, noWorkTrack;
extern int relaxed;

#endif


#endif


