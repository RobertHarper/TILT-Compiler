#ifndef _global_h
#define _global_h

/*  global_init should be called once; the rest are thread-safe */

extern ptr_t DivideByZeroExn;
extern ptr_t OverflowExn;
/* extern mem_t RuntimeGlobalData_Start, RuntimeGlobalData_Cur, RuntimeGlobalData_End; */
extern mem_t datastart, dataend;
extern mem_t textstart, textend;

void global_init(void);

INLINE1(IsText)
INLINE2(IsText)
int IsText(ptr_t addr)
{
  return (textstart <= addr && addr <= textend);
}


INLINE1(IsGlobalData)
INLINE2(IsGlobalData)
int IsGlobalData(ptr_t addr)
{
  return (datastart <= addr && addr <= dataend);
	  /* ||	  (RuntimeGlobalData_Start <= addr && addr <= RuntimeGlobalData_End) */
}

INLINE1(IsTagData)
INLINE2(IsTagData)
int IsTagData(ptr_t addr)
{
  return (addr <= (ptr_t) 256);
}


#endif
