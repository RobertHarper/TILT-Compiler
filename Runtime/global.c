#include "tag.h"
#include "global.h"
#include "memobj.h"
#include "thread.h"
#include "create.h"
#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif

#include "general.h"

#ifdef rs_aix
#define _fdata _data
#define _ftext _text
#endif
/* these next two are defined by the linker */
extern unsigned long _edata;
extern unsigned long _fdata;
extern unsigned long _etext;
extern unsigned long _ftext;
extern unsigned long _fbss;
extern unsigned long _end;

value_t DivideByZeroExn = 0;
value_t OverflowExn = 0;
value_t datastart, dataend;
double FPTOFROMINT;

/* Little allocation area for data allocated by the runtime. */
static value_t RuntimeGlobalData_Start;
value_t RuntimeGlobalData_Cur;
value_t RuntimeGlobalData_End;

int IsCompileGlobalData(value_t addr)
{
  int res = (datastart <= addr) && (addr <= dataend);
  return res;
}


int IsRuntimeGlobalData(value_t addr)
{
  return ((RuntimeGlobalData_Start <= addr) &&
	  (addr < RuntimeGlobalData_End));
}

int IsGlobalData(value_t addr)
{
  return (IsCompileGlobalData(addr) || 
	  IsRuntimeGlobalData(addr));
}

int IsConstructorData(value_t addr)
{
  return (addr < 256);
}



void global_init()
{
  value_t fields[3];
  int masks[1];
  int RuntimeGlobalDataSize = 1024;
  char *RuntimeGlobalData = (char *) malloc(sizeof(char) * RuntimeGlobalDataSize);

  RuntimeGlobalData_Start = (value_t) RuntimeGlobalData;
  RuntimeGlobalData_Cur = (value_t) RuntimeGlobalData;
  RuntimeGlobalData_End = RuntimeGlobalData_Start + RuntimeGlobalDataSize;

  fields[0] = MLEXN_DIVZERO;
  fields[1] = 0;
  masks[0] = 0;
  DivideByZeroExn = alloc_record(fields,masks,2);

  fields[0] = MLEXN_OVERFLOW;
  fields[1] = 0;
  masks[0] = 0;
  OverflowExn  = alloc_record(fields,masks,2);
  
  datastart = (value_t) &_fdata;
  dataend   = (value_t) &_end;  
  /* _edata does not seem to work at all, in fact, it's less then _fdata */

}
