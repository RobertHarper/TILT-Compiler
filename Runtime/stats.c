#include <values.h>
#include "general.h"
#include "tag.h"
#include "memobj.h"
#include "stats.h"
#include "gcstat.h"
#include "gc.h"
#include "thread.h"
#include "stack.h"
#include <string.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/resource.h>
#if (defined alpha_osf)
int ftime(struct timeb *tp);   /* This should be in sys/timeb.h but isn't on the Alpha */
#endif

extern int use_stack_gen;


static struct timespec start_tp, stop_tp;
static struct rusage start_rusage, stop_rusage;

int shortSummary;
int	prof_fd = 1;
static double time_diff(),time2double();
static double eps=1e-7;


static double tp_diff (struct timespec *tp1, struct timespec *tp2)
{ 
  return (tp1->tv_sec - tp2->tv_sec) + ((double) (tp1->tv_nsec - tp2->tv_nsec)) / 1e9;
}

void reset_timer(Timer_t *t)
{
  t->on =  0;
  t->last = 0.0;
}

void start_timer(Timer_t *t)
{
  assert(t->on == 0);
  t->on = 1;
  clock_gettime(CLOCK_REALTIME, &t->tp);
}

void stop_timer(Timer_t *t)
{
  struct timespec tp2;
  assert(t->on == 1);
  clock_gettime(CLOCK_REALTIME, &tp2);
  t->last = 1000.0 * tp_diff(&tp2, &t->tp);
  t->on = 0;
}

void restart_timer(Timer_t *t)
{
  struct timespec tp2;
  assert(t->on == 1);
  clock_gettime(CLOCK_REALTIME, &tp2);
  t->last = 1000.0 * tp_diff(&tp2, &t->tp);
  t->tp.tv_sec = tp2.tv_sec;
  t->tp.tv_nsec = tp2.tv_nsec;
}

void reset_statistic(Statistic_t *s)
{
  s->min = s->max = s->sum = 0;
  s->count = 0;
}

void add_statistic(Statistic_t *s, double data)
{
  s->count++;
  if (s->count == 1)
    s->min = data;
  s->min = (s->min < data) ? s->min : data;
  s->max = (s->max > data) ? s->max : data;
  s->sum += data;
}

void reset_history(History_t *h)
{
  reset_statistic(&h->stat);
  h->historyCursor = 0;
  h->logFrequency = 0;
}

void add_history_brief(History_t *h, double data)
{
  add_statistic(&h->stat, data);
}

void add_history(History_t *h, double data)
{
  int i;
  int historySize = sizeof(h->history) / sizeof(double);
  if (((h->stat.count >> h->logFrequency) << h->logFrequency) == h->stat.count) {
    h->history[h->historyCursor++] = data;
  }
  if (h->historyCursor >= historySize) {
    h->logFrequency++;
    h->historyCursor = historySize / 2;
    for (i=0; i<historySize / 2; i++)
      h->history[i] = h->history[2*i];
  }
  add_statistic(&h->stat, data);
}

void reset_histogram(Histogram_t *h)
{
  int i;
  double base;
  reset_statistic(&h->stat);
  for (i=0; i<38; i++)
    h->bucket[i] = 0;
  h->bucketStart[0] = 0.0;
  h->bucketEnd[0] = 1.0;
  h->bucketStart[37] = 1.0;
  h->bucketEnd[37] = MAXDOUBLE;
  for (i=0, base = 0.001; i<6; i++, base *= 10.0) {
    int start = 1 + 6 * i;
    h->bucketStart[start+0] = base;
    h->bucketEnd[start+0]   = 2.0 * base;
    h->bucketStart[start+1] = 2.0 * base;
    h->bucketEnd[start+1]   = 3.0 * base;
    h->bucketStart[start+2] = 3.0 * base;
    h->bucketEnd[start+2]   = 4.0 * base;
    h->bucketStart[start+3] = 4.0 * base;
    h->bucketEnd[start+3]   = 6.0 * base;
    h->bucketStart[start+4] = 6.0 * base;
    h->bucketEnd[start+4]   = 8.0 * base;
    h->bucketStart[start+5] = 8.0 * base;
    h->bucketEnd[start+5]   = 10.0 * base;
  }
}

void add_histogram(Histogram_t *h, double data)
{
  int which = 0;
  assert(sizeof(h->bucket) == 38 * sizeof(int));
  if (data < 0.001)
    which = 0;
  else if (data > 1000)
    which = 37;
  else {
    which = 1;
    while (data < 0.001) {
      which += 6;
      data /= 10.0;
    }
    while (!(h->bucketStart[which] <= data &&
	     data < h->bucketEnd[which])) {
      which++;
      assert(which < 38);
    }
  }
  assert(0 <= which && which < 38);
  h->bucket[which]++;
}


int doubleCompare(const void *a, const void *b)
{
  return (*(double *)a) > (*(double *)b);
}

static void show_statistic(char *str, Statistic_t *s, double totalSum)
{
  double partialSum = s->sum / 1000.0;
  totalSum /= 1000.0;
  printf("         %s (s) = %9.2lf (%4.1f%%)  Cnt = %6d   Min/Avg/Max (ms) = %4.3lf < %4.3lf < %4.3lf\n",
	 str, partialSum, (100.0 * partialSum) / (totalSum + eps), 
	 s->count, s->min, s->sum/s->count, s->max);
}

static void show_history(char *name, History_t *h)
{
  int historySize = sizeof(h->history) / sizeof(double);
  int dataSize = h->logFrequency ? historySize : h->historyCursor;
  int numToShow = 5;
  int i;
  qsort(h->history, dataSize, sizeof(double), doubleCompare);
  printf("         %s (ms) : ", name);
  for (i=0; i<dataSize; i++) {
    if (i < numToShow)
      printf("%4.2lf < ", h->history[i]);
    if (i == numToShow)
      printf(" skip %d ", dataSize - 2 * numToShow);
    if (i > dataSize - numToShow - 1)
      printf(" < %4.2lf", h->history[i]);
  }
  printf("\n");
}

static void show_histogram(char *name, Histogram_t *h)
{
  int i;
  for (i=0; i<38; i++) {
    if (i != 0 && (i % 6 == 0))
      printf("%6s","");
    printf("[%.3f,",h->bucketStart[i]);
    if (h->bucketEnd[i] == MAXDOUBLE)
      printf("INF");
    else 
      printf("%.3f", h->bucketEnd[i]);
    printf(") = %d  ", h->bucket[i]);
    if (i != 0 && i != 36 && (i % 6 == 5))
      printf("\n");
  }
}

void stats_init()
{   
  getrusage(RUSAGE_SELF,&start_rusage);
  clock_gettime(CLOCK_REALTIME, &start_tp);
}

void stats_finish()
{ 
  int i;
  double elapsed;
  double AvgStackDepth = TotalStackDepth/((double)NumGC+eps);
  double AvgNewStackDepth = TotalNewStackDepth/((double)NumGC+eps);
  double AvgStackFrameSize = TotalStackSize /(double)(TotalStackDepth+eps);
  long bytesAllocated = 0, kbytesAllocated = 0, bytesCopied = 0, kbytesCopied = 0;
  long NumCopied = 0, NumShared = 0, NumContention = 0, NumRoots = 0, NumLocatives = 0, NumWrites = 0;

  getrusage(RUSAGE_SELF,&stop_rusage);
  clock_gettime(CLOCK_REALTIME, &stop_tp);
  elapsed = tp_diff(&stop_tp, &start_tp);

  for (i=0; i<NumProc; i++) {
    Proc_t *proc = getNthProc(i);
    bytesAllocated += proc->bytesAllocated;
    kbytesAllocated += proc->kbytesAllocated;
    bytesCopied += proc->bytesCopied;
    kbytesCopied += proc->kbytesCopied;
    NumCopied += proc->numCopied;
    NumShared += proc->numShared;
    NumContention += proc->numContention;
    NumRoots += proc->numRoot;
    NumLocatives += proc->numLocative;
    NumWrites += proc->numWrite;
  }
  kbytesAllocated += (bytesAllocated / 1024);
  kbytesCopied += (bytesCopied / 1024);
  bytesAllocated %= 1024;
  bytesCopied %= 1024;

  printf("\n\n");

  if (!shortSummary) {
    for (i=0; i<NumProc; i++) {
      Proc_t *proc = getNthProc(i);
      printf("Proc #%d: Alloc         = %9d kb       Copy          = %9d kb\n",
	     i, proc->kbytesAllocated, proc->kbytesCopied);
      printf("         Total       (s) = %9.2lf\n", proc->totalTimer.last / 1000.0);
      show_statistic("  Scheduler", &proc->schedulerStatistic, proc->totalTimer.last);
      show_statistic("  Mutator  ", &proc->mutatorStatistic, proc->totalTimer.last);
      show_statistic("  GCOff    ", &proc->gcOffStatistic, proc->totalTimer.last);
      show_statistic("  GCOn     ", &proc->gcOnHistory.stat, proc->totalTimer.last);
      show_statistic("    GCGlob ", &proc->gcGlobalStatistic, proc->gcOnHistory.stat.sum);
      show_statistic("    GCStack", &proc->gcStackStatistic, proc->gcOnHistory.stat.sum);
      show_history("GCOn Hist   ", &proc->gcOnHistory);
      show_histogram("GCOn Hist   ", &proc->gcOnHistogram);
    }
  }

  printf("TIME(s): Total time    = %9.3lf s\n", elapsed);
  printf("GC:      GC_Method     = %9s          stackMethod   = %9s\n"
	 "         Alloc         = %9d kb       Copy          = %9d kb\n"
	 "         NumGC         = %9d          NumMajorGC    = %9d\n",
	 (collector_type == Semispace) ? "Semi" : 
	 ((collector_type == Generational) ? "Gen " : 
	  ((collector_type == SemispaceParallel) ? "SemiPara" :
	   ((collector_type == GenerationalParallel) ? "GenPara" : 
	    ((collector_type == SemispaceConcurrent) ? "SemiConc" :
	     ((collector_type == GenerationalConcurrent) ? "GenConc" : "????"))))),
	 use_stack_gen?"Gener":"Normal",
	 kbytesAllocated,             kbytesCopied,
	 NumGC,                           NumMajorGC);
  printf("         NumRoot       = %9d          NumCopied     = %9d\n"
	 "         NumShared     = %9d          NumContention = %9d\n", 
	 NumRoots, NumCopied, NumShared, NumContention);
  if (collector_type == Generational)
    printf("         NumWrites       = %9d        NumLocs = %d\n",
	   NumWrites, NumLocatives);
  printf(  "STACK:   maxDepth      = %9d          avgFrameSize  = %8.2lf b\n",
	   MaxStackDepth,                   AvgStackFrameSize);
  if(use_stack_gen)	   
    printf("         avgDepth      = %9.1lf      newDepth   = %5.1lf\n",
	   AvgStackDepth,                   AvgNewStackDepth);
  else
    printf("         avgDepth      = %9.1lf\n",  AvgStackDepth);
  printf("THREAD:  Total Threads = %9d          Max Threads   = %9d\n",
	 thread_total(),thread_max());
  printf("MEM(K):  maxPhysMem    = %9d          sharedMem     = %9d\n"
	 "         unsharedData  = %9d          unsharedStk   = %9d\n",
	 stop_rusage.ru_maxrss,	          stop_rusage.ru_ixrss,
	 stop_rusage.ru_idrss,	          stop_rusage.ru_isrss);
  printf("PAGING:  minorPageFault= %9d          majorPageFault= %9d     swaps = %d\n"
	 "PROCESS: invCtxtSwap   = %9d          volCtxtSwap   = %9d\n",
	 stop_rusage.ru_minflt,	stop_rusage.ru_majflt,   stop_rusage.ru_nswap,
	 stop_rusage.ru_nvcsw,  stop_rusage.ru_nivcsw);
  
  printf("MISC:    GCTable       = %9d b  \n"
	 "         SMLGlobal     = %9d b  \n"
	 "         GlobalTable   = %9d b        MutableTable = %9d b\n",
	 GCTableSize,                    
	 SMLGlobalSize,                   
	 GlobalTableSize,    MutableTableSize);

}

