#ifndef _general_h
#define _general_h

#define DivideUp(a,div) (((a) + (div) - 1) / (div))
#define RoundUp(x,mult) (((x) + (mult) - 1) / (mult) * (mult))
#define RoundDown(x,mult) ((x) / (mult) * (mult))
#define typed_swap(t,a,b) { t swap_temp = a; a = b; b = swap_temp; }

#undef DIAG
#undef PARANOID
#undef DEBUG
#undef SUPPRESS

/* #define SEMANTIC_GARBAGE */

#undef GCTABLE_HASENTRYID

#define NumStackObj      12
#define NumStackChainObj 4
#define NumHeapObj       20
#define RuntimeGlobalDataSize 1024
#define NumThreadObj 100

#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif

#endif


