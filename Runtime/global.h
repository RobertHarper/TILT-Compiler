/*  global_init should be called once; the rest are thread-safe */

void global_init(void);
int  IsGlobalData(value_t);
int  IsConstructorData(value_t addr);

extern value_t DivideByZeroExn;
extern value_t OverflowExn;

extern value_t RuntimeGlobalData_Cur;
extern value_t RuntimeGlobalData_End;
