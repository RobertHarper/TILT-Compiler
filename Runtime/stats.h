/* Not thread-safe */

#ifndef _stats_h
#define _stats_h

#include <sys/time.h>
#include <sys/resource.h>

extern int information;

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
  double min, max, sum;            /* Running statistics without subsampling */
  int count;
} Statistic_t;

void reset_statistic(Statistic_t *);
void add_statistic(Statistic_t *, double);

/* The history buffer is fixed in size and so the history is recorded so that,
   on overflow, the history acts as if there is subsampling.  "dataCursor"
   increments by 1 each time a new data point comes in.  If dataCursor
   is a multiple of "2^logFrequency", then the data is stored into "history[historyCursor]"
   and "historyCursor" is incremented.  When "historyCursor" hits 8192, the overflow
   causes 3 things:
   
     (1) "logFrequency" increments by 1
     (2) "historyCursor" is set to 4196
     (3) each element at "history[2*i]" is moved to "history[i]" for 0 <= i < 4196

   At any point in time, "history" contains data evenly sampled at the frequency "2^logFrequency"
   and the number of data.  We are also guaranteed that "history" is at least half full
   (except initially when there are few datapoints).

*/
typedef struct History__t
{
  Statistic_t stat;                     /* The number of data points is stored in stat */
  int historyCursor;                    /* The next field in history[] to fill */
  int logFrequency;                     /* Binary logarithm of frequency we are sampling data at */
  double history[8192];
} History_t;

void reset_history(History_t *);
void add_history(History_t *, double);
void add_history_brief(History_t *, double);


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
  int    windowSize[20];          /* in tens of microseconds */
  int    onSum[20];               /* sum of active data values that are on */
  int    offSum[20];              /* sum of active data values that are off */
  int    start[20];               /* first slot for this window size */
  Statistic_t stat[20];
} WindowQuotient_t;

void reset_windowQuotient(WindowQuotient_t *, int fineness);
void add_windowQuotient(WindowQuotient_t *, double, int on);

/* Initializing timers; show statistics */
void add_statString(char *);
void stats_init(void);
void stats_finish(void);

void resetTimeList(void);
double addTimeList(void *proc, int which, int data);
int showTimeList(double min);
#endif
