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
#undef SEMANTIC_GARBAGE 
#define WRITE
#define OLD_ALLOC

#define Semispace 0
#define Generational 1
#define Parallel 2

#ifndef _asm_

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define DivideUp(a,div) (((a) + (div) - 1) / (div))
#define RoundUp(x,mult) (((x) + (mult) - 1) / (mult) * (mult))
#define RoundDown(x,mult) ((x) / (mult) * (mult))
#define typed_swap(t,a,b) { t swap_temp = a; a = b; b = swap_temp; }

int FetchAndAdd(int *, int);
int TestAndSet(int *);

extern int LEAST_GC_TO_CHECK;
extern int SHOW_GCSTATS;
extern int SHOW_GCFORWARD;
extern int SHOW_GCDEBUG;
extern int SHOW_GCERROR;
extern int SHOW_HEAPS;
extern int MinHeap, MaxHeap;
extern double TargetRatio;
extern int StackSize;
extern int YoungHeapByte;
extern int save_rate;
extern int use_stack_gen;
extern int collector_type;
extern int paranoid;
extern int diag;
extern int NumSysThread;
extern int NumThread;
extern int NumStack;
extern int NumStackChain;
extern int NumHeap;
extern int NumGC, NumMajorGC;
extern int shortSummary;
#endif


#endif


