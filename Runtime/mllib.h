#ifndef _mllib_h
#define _mllib_h

void mllib_init(void);
int ml_output(val_t _des, ptr_t mlstring);
val_t ml_lookahead(val_t des);
int ml_open_in(ptr_t mlstring);
int ml_open_out(ptr_t mlstring);
void ml_close_in(val_t des);
int ml_close_out(val_t des);
int ml_flush_out(val_t des);
int ml_end_of_stream(val_t des);
ptr_t ml_input(val_t _des, val_t numtoread);
val_t ml_input1(val_t _des);
extern int exncounter;

#endif
