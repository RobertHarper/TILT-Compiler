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

ptr_t DivideByZeroExn = 0;
ptr_t OverflowExn = 0;
mem_t datastart, dataend;
mem_t textstart, textend;

/* Little allocation area for data allocated by the runtime. */
static mem_t RuntimeGlobalData_Start;
mem_t RuntimeGlobalData_Cur;
mem_t RuntimeGlobalData_End;

int IsText(ptr_t addr)
{
  int res = (textstart <= addr) && (addr <= textend);
  return res;
}

int IsCompileGlobalData(ptr_t addr)
{
  int res = (datastart <= addr) && (addr <= dataend);
  return res;
}


int IsRuntimeGlobalData(ptr_t addr)
{
  return ((RuntimeGlobalData_Start <= addr) &&
	  (addr < RuntimeGlobalData_End));
}

int IsGlobalData(ptr_t addr)
{
  return (IsCompileGlobalData(addr) || 
	  IsRuntimeGlobalData(addr));
}



#ifdef solaris
extern unsigned long firstdata;
extern unsigned long firsttext;
#endif

void global_init()
{
  val_t fields[3];
  int masks[1];
  int RuntimeGlobalDataSize = 256;
  mem_t RuntimeGlobalData = (mem_t) malloc(sizeof(unsigned int) * RuntimeGlobalDataSize);

  RuntimeGlobalData_Start = (mem_t) RuntimeGlobalData;
  RuntimeGlobalData_Cur = RuntimeGlobalData_Start;
  RuntimeGlobalData_End = RuntimeGlobalData_Start + RuntimeGlobalDataSize;

  fields[0] = MLEXN_DIVZERO;
  fields[1] = 0;
  masks[0] = 0;
  DivideByZeroExn = alloc_record(fields,masks,2);

  fields[0] = MLEXN_OVERFLOW;
  fields[1] = 0;
  masks[0] = 0;
  OverflowExn  = alloc_record(fields,masks,2);
  
#ifdef alpha_osf
  datastart = (mem_t) &_fdata;
  dataend   = (mem_t) &_edata;  
  textstart = (mem_t) &_ftext;
  textend   = (mem_t) &_etext;
#endif

#ifdef solaris
  datastart = (mem_t) firstdata;
  dataend   = (mem_t) &_edata;
  textstart = (mem_t) firsttext;
  textend   = (mem_t) &_etext;  
#endif

}
