/*  global_init should be called once; the rest are thread-safe */

void global_init(void);
int  IsGlobalData(ptr_t);
int  IsConstructorData(ptr_t addr);

extern ptr_t DivideByZeroExn;
extern ptr_t OverflowExn;

extern mem_t RuntimeGlobalData_Cur;
extern mem_t RuntimeGlobalData_End;
