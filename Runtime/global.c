#include "general.h"
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
#include "stack.h"

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

mem_t datastart, dataend;
mem_t textstart, textend;

extern val_t Div_r_INT, Overflow_r_INT;   /* Must use & to get global's address */
  
ptr_t getOverflowExn(void)
{
  return (ptr_t) GetGlobal(&Div_r_INT);
}

ptr_t getDivExn(void)
{
  return (ptr_t) GetGlobal(&Overflow_r_INT);
}



/* Little allocation area for data allocated by the runtime. */
mem_t RuntimeGlobalData_Start;
mem_t RuntimeGlobalData_Cur;
mem_t RuntimeGlobalData_End;

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
