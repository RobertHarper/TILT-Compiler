#ifndef _global_h
#define _global_h

/* From Basis - access is safe only though GetGlobal */
val_t GetGlobal(ptr_t);

ptr_t getOverflowExn(void);
ptr_t getDivExn(void);

extern mem_t RuntimeGlobalData_Start, RuntimeGlobalData_Cur, RuntimeGlobalData_End; 
extern mem_t datastart, dataend;
extern mem_t textstart, textend;

void global_init(void);

INLINE(IsText)
int IsText(vptr_t addr)
{
  return (textstart <= addr && addr <= textend);
}


INLINE(IsGlobalData)
int IsGlobalData(vptr_t addr)
{
  return (datastart <= addr && addr <= dataend);
	  /* ||	  (RuntimeGlobalData_Start <= addr && addr <= RuntimeGlobalData_End) */
}

INLINE(IsTagData)
int IsTagData(ptr_t addr)
{
  return (addr <= (ptr_t) 256);
}


#endif
