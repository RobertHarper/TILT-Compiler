void gc_init();
void gc_handler(unsigned long *saveregs, long sp, long ret_add, long req_size, int isMajor);

void gcstat_finish();
