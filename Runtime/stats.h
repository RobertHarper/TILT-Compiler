/* Not thread-safe */

#ifndef _stats_h
#define _stats_h

#include <sys/time.h>
#include <sys/resource.h>


struct timer_st
{
  int on;
  double user, sys;
  struct rusage cur_usage;
};

typedef struct timer_st timer_mt;

void reset_timer(timer_mt *);
void start_timer(timer_mt *);
void stop_timer(timer_mt *);
double getuser_timer(timer_mt *);
double getsys_timer(timer_mt *);
double gettotal_timer(timer_mt *);
void show_timer(timer_mt *t, char *describe);

void stats_init(void);
void stats_finish(void);
void stats_finish_thread(timer_mt *stack, timer_mt *gctime, timer_mt *majorgctime);

#endif
