/*  global_init should be called once; the rest are thread-safe */

void global_init(void);
int  IsGlobalData(ptr_t);

static int IsTagData(ptr_t addr)
{
  return (addr <= (ptr_t) 256);
}

extern ptr_t DivideByZeroExn;
extern ptr_t OverflowExn;

extern mem_t RuntimeGlobalData_Cur;
extern mem_t RuntimeGlobalData_End;
