/* Not thread-safe */

#ifndef _stats_h
#define _stats_h

#ifdef alpha
#include <sys/sysmisc.h>	/* for struct timespec */
#endif
#include <sys/time.h>
#include <sys/resource.h>

extern int information;
extern char *historyFile;

/* Note that timer_t is a type already defined by the C library */
typedef struct Timer__t
{
  int on;              /* Is timer on */
  double last;         /* Last time in milliseconds */
  struct timespec tp;
} Timer_t;

void reset_timer(Timer_t *);
void start_timer(Timer_t *);
double lap_timer(Timer_t *);
void restart_timer(Timer_t *);
void stop_timer(Timer_t *);

typedef struct Statistic__t
{
  double last, min, max, sum;            /* Running statistics without subsampling */
  int count;
} Statistic_t;

void reset_statistic(Statistic_t *);
void add_statistic(Statistic_t *, double);



typedef struct Histogram__t
{
  Statistic_t stat;                     /* The number of data points is stored in stat */
  double bucketStart[38];
  double bucketEnd[38];
  int bucket[38];                       
                                        /* 38 buckets like this:
					   1 for <.001; 
					   6 for .001 to .01 in this pattern .001 < .002 < .003 < .004 < .006 < .008 < .010;
					   6 for .01 to .1; 
					   6 for .1 to 1; 
					   6 for 1 to 10; 
					   6 for 10 to 100; 
					   6 for 100 to 1000; 
					   1 for > 1000 */
} Histogram_t;

void reset_histogram(Histogram_t *);
void add_histogram(Histogram_t *, double);


/* A sequence of numbers (times) s1, s2, ... , each associated with a bit-value on/off, is fed
   to this data structure and it computes the smallest fraction of on to off values in any given
   window of size w.  That is, it computes values

   s_x1 + s_x2 + ... + s_xm    with x1 < x2 < ...
   ------------------------ 
   s_y1 + s_y2 + ... + s_ym    with y1 < y2 < ...

   where the x and y values are distinct and form a contiguous sequence and so that

   w - s_max(xm,yn) > s_x1 + s_x2 + ... + s_xm + s_y1 + s_y2 + ... + s_ym > w


*/
   
typedef struct WindowQuotient__t   /* quotient = on / off */
{
  int    granularity;             /* number of parts millisecond is divided into */
  int    size;
  double onRemain, offRemain;
  char   *data;                   /* Each char indicates state of 1 ms / granularity */
  int    first;                   /* first occupied slot */
  int    last;                    /* first unused slot */
  int    numWindows;
  int    windowSize[20];          /* in units of granularity * ms */
  int    onSum[20];               /* sum of active data values that are on */
  int    offSum[20];              /* sum of active data values that are off */
  int    start[20];               /* first slot for this window size */
  Statistic_t stat[20];
} WindowQuotient_t;

void reset_windowQuotient(WindowQuotient_t *, int fineness);
void add_windowQuotient(WindowQuotient_t *, double, int on);
double get_prewindow(WindowQuotient_t *wq, int which, double neededOnTime);

/* Initializing timers; show statistics */
void add_statString(char *);
void stats_init(void);
void stats_finish(void);

void resetTimeList(void);
double addTimeList(void *proc, int which, int data);
int showTimeList(double min);
#endif
