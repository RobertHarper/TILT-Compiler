#include "s.h"
#include "r.h"
#include "sparc.h"

enum {
	RuntimeGlobalDataSize = 256,
};

/* these are defined by the linker */
extern unsigned long _edata;
extern unsigned long _fdata;
extern unsigned long _etext;
extern unsigned long _ftext;
extern unsigned long _fbss;
extern unsigned long _end;

mem_t datastart, dataend;
mem_t textstart, textend;

val_t
GetGlobal(ptr_t globalLoc)
{
	return globalLoc[primaryGlobalOffset / sizeof(val_t)];
}

/* Little allocation area for data allocated by the runtime. */
mem_t RuntimeGlobalData_Start;
mem_t RuntimeGlobalData_Cur;
mem_t RuntimeGlobalData_End;

void
global_init(void)
{
	RuntimeGlobalData_Start =
		(mem_t)emalloc(sizeof(unsigned int) * RuntimeGlobalDataSize);
	RuntimeGlobalData_Cur = RuntimeGlobalData_Start;
	RuntimeGlobalData_End = RuntimeGlobalData_Start + RuntimeGlobalDataSize;
	datastart = (mem_t) firstdata;
	dataend   = (mem_t) &_edata;
	textstart = (mem_t) firsttext;
	textend   = (mem_t) &_etext;
}
