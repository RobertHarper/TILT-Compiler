#ifndef _mllib_h
#define _mllib_h

void mllib_init();
int ml_output(value_t _des, value_t mlstring);
value_t ml_lookahead(value_t des);
int ml_open_in(value_t mlstring);
int ml_open_out(value_t mlstring);
void ml_close_in(value_t des);
int ml_close_out(value_t des);
int ml_flush_out(value_t des);
int ml_end_of_stream(value_t des);
value_t ml_input(value_t _des, value_t numtoread);
value_t ml_input1(value_t _des);
extern int exncounter;

#endif
