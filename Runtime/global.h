int  IsGlobalData(value_t);
int IsConstructorData(value_t addr);
void global_init();

extern value_t DivideByZeroExn;
extern value_t OverflowExn;
