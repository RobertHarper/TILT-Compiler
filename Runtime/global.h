#ifndef _global_h
#define _global_h

/* From Basis - access is safe only though GetGlobal */
val_t GetGlobal(ptr_t);

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
  unsigned long a = (unsigned long)addr;
  return a <= 256;
}


#endif
