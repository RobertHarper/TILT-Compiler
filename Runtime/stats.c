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
#ifdef sparc
int getrusage(int who, struct rusage *rusage);  /* Not in header files for some reason */
int ftime(struct timeb *tp);   /* Not in header file */
#endif

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

double lap_timer(Timer_t *t)
{
  struct timespec tp2;
  assert(t->on == 1);
  clock_gettime(CLOCK_REALTIME, &tp2);
  t->last = 1000.0 * tp_diff(&tp2, &t->tp);
  return t->last;
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
  h->bucketEnd[0] = 0.001;
  h->bucketStart[37] = 1000.0;
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
  add_statistic(&h->stat,data);
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
  if (s->sum == 0.0)
    return;
  if (totalSum >= 0.0)
    printf("         %s (s) = %8.2lf (%4.1f%%)    Cnt = %6d      Min/Avg/Max (ms) = %5.2f < %5.2f < %6.2f\n",
	   str, partialSum, (100.0 * partialSum) / (totalSum + eps), 
	   s->count, s->min, s->sum/s->count, s->max);
  else
    printf("         %s                         Cnt = %6d      Min/Avg/Max      = %5.2f < %5.2f < %6.2f\n",
	   str, s->count, s->min, s->sum/s->count, s->max);
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

static void show_bucket_end(double v)
{
  if (v == MAXDOUBLE)
    printf(" INF");
  else if (v >= 1.0 || v < 0.001)
    printf("%4.0f",v);
  else if (v >= 0.001) {
    char buf[16];
    sprintf(buf,"%.3g",v);
    printf("%4s",&buf[1]);
  }
    
}

static void show_bucket(Histogram_t *h, int i)
{
  printf("[");
  show_bucket_end(h->bucketStart[i]);
  printf(",");
  show_bucket_end(h->bucketEnd[i]);
  printf(") = %4d", h->bucket[i]);
}

static void show_histogram(char *name, Histogram_t *h)
{
  int i, j;
  if (h->stat.sum == 0.0)
    return;
  printf("         %10s (ms) :    ", name);
  if (h->bucket[0]) {
    show_bucket(h,0);
    printf("            ");
  }
  if (h->bucket[37])
    show_bucket(h,37);
  printf("\n");
  for (i=0; i<6; i++) {
    int significant = 0;
    for (j=0; j<6; j++) 
      if (h->bucket[6*i+j+1])
	significant = 1;
    if (!significant)
      continue;
    printf("            ");
    for (j=0; j<6; j++) {
      show_bucket(h,6*i+j+1);
      printf("   ");
    }
    printf("\n");
  }
}

int statStringCursor = 0;
char statString[2000000];
void add_statString(char *msg)
{
  strcat(&statString[statStringCursor], msg);
  statStringCursor += strlen(msg);
  assert(statStringCursor < sizeof(statString));
}

void stats_init()
{   
  statString[0] = 0;
  getrusage(RUSAGE_SELF,&start_rusage);
  clock_gettime(CLOCK_REALTIME, &start_tp);
}

const char *collectorTypeString()
{
  return (collector_type == Semispace) ? "Semi" : 
    ((collector_type == Generational) ? "Gen" : 
     ((collector_type == SemispaceParallel) ? "SemiPara" :
      ((collector_type == GenerationalParallel) ? "GenPara" : 
       ((collector_type == SemispaceConcurrent) ? "SemiConc" :
	((collector_type == GenerationalConcurrent) ? "GenConc" : "????")))));
}



void stats_finish()
{ 
  int i;
  FILE *fd;
  double elapsed;
  double AvgStackDepth = TotalStackDepth/((double)NumGC+eps);
  double AvgNewStackDepth = TotalNewStackDepth/((double)NumGC+eps);
  double AvgStackFrameSize = TotalStackSize /(double)(TotalStackDepth+eps);
  double bytesAllocated = 0.0, bytesCopied = 0.0;
  long NumCopied = 0, NumShared = 0, NumContention = 0, NumRoots = 0, NumLocatives = 0, NumWrites = 0;

  getrusage(RUSAGE_SELF,&stop_rusage);
  clock_gettime(CLOCK_REALTIME, &stop_tp);
  elapsed = tp_diff(&stop_tp, &start_tp);

  for (i=0; i<NumProc; i++) {
    Proc_t *proc = getNthProc(i);
    bytesAllocated += proc->bytesAllocatedStatistic.sum;
    bytesCopied += proc->bytesCopiedStatistic.sum;
    NumCopied += proc->numCopied;
    NumShared += proc->numShared;
    NumContention += proc->numContention;
    NumRoots += proc->numRoot;
    NumLocatives += proc->numLocative;
    NumWrites += proc->numWrite;
  }

  printf("\n\n");

  if (!shortSummary) {
    for (i=0; i<NumProc; i++) {
      Proc_t *proc = getNthProc(i);
      printf("PROC #%d: Allocated       = %8.0f kb         Copied    = %8.0f kb\n",
	     i, proc->bytesAllocatedStatistic.sum / 1024.0, proc->bytesCopiedStatistic.sum / 1024.0);
      printf("         Total       (s) = %8.2lf\n", proc->totalTimer.last / 1000.0);
      show_statistic(" Scheduler ", &proc->schedulerStatistic, proc->totalTimer.last);
      show_statistic(" Mutator   ", &proc->mutatorHistogram.stat, proc->totalTimer.last);
      show_statistic(" GCNone    ", &proc->gcNoneStatistic, proc->totalTimer.last);
      show_statistic(" GCWork    ", &proc->gcWorkHistogram.stat, proc->totalTimer.last);
      show_statistic("  GCGlob   ", &proc->gcGlobalStatistic, proc->gcWorkHistogram.stat.sum);
      show_statistic("  GCStack  ", &proc->gcStackStatistic, proc->gcWorkHistogram.stat.sum);
      show_statistic("  GCMajor  ", &proc->gcMajorWorkHistogram.stat, proc->gcWorkHistogram.stat.sum);
      show_statistic("  GCFlipOn ", &proc->gcFlipOnHistogram.stat, proc->gcWorkHistogram.stat.sum);
      show_statistic("  GCFlipOff", &proc->gcFlipOffHistogram.stat, proc->gcWorkHistogram.stat.sum);
      show_histogram(" GCWork Histogram", &proc->gcWorkHistogram);
      show_histogram(" GCMajorWork Hist", &proc->gcMajorWorkHistogram);
      show_histogram(" GCFlipOff   Hist", &proc->gcFlipOffHistogram);
      show_histogram(" GCFlipOn    Hist", &proc->gcFlipOnHistogram);
      /*      show_histogram("Mutator Histogram", &proc->mutatorHistogram); */
      show_statistic("MinSurvRate", &proc->minorSurvivalStatistic, -1.0);
      show_statistic("MajSurvRate", &proc->majorSurvivalStatistic, -1.0);
      show_statistic("HeapSize   ", &proc->heapSizeStatistic, -1.0);
    }
  }

  printf("GC:    GCMethod      = %8s     Allocated    = %9.0f kb   NumCopied    = %8d    MaxStkDepth  = %4d\n"
	 "       StackMethod   = %8s     Copied       = %9.0f kb   NumShared    = %8d    AvgStkDepth  = %4.0f\n",
	 collectorTypeString(),          bytesAllocated / 1024.0, NumCopied,      MaxStackDepth,
	 useGenStack?"Gener":"Normal", bytesCopied / 1024.0,    NumShared,      AvgStackDepth); 
  printf("       NumGC         = %8d     NumRoot      = %9d      NumConflict  = %8d    AvgFrameSize = %4.0f\n"
	 "       NumMajorGC    = %8d     NumWrite     = %9d      NumLocative  = %8d\n",
	 NumGC,                          NumRoots,                NumContention,   AvgStackFrameSize,
	 NumMajorGC,                     NumWrites,               NumLocatives);
  if(useGenStack)
    printf("       newStkDepth   = %4.0f\n",  AvgNewStackDepth);
  printf("MISC:  Total time    = %8.2f s   maxPhysMem   = %9d      minPageFault = %8d    invCtxtSwap  = %5d\n"
         "       Total Threads = %8d     sharedMem    = %9d      majPageFault = %8d    volCtxtSwap  = %5d\n"
	 "       Max Threads   = %8d     unsharedData = %9d                                 swapping     = %5d\n"
	 "                                    unsharedStk  = %9d\n",
	 elapsed, 	 stop_rusage.ru_maxrss,	 stop_rusage.ru_minflt,	stop_rusage.ru_nvcsw,  
	 thread_total(), stop_rusage.ru_ixrss,   stop_rusage.ru_majflt, stop_rusage.ru_nivcsw,
	 thread_max(),	 stop_rusage.ru_idrss,                          stop_rusage.ru_nswap,
	                 stop_rusage.ru_isrss);
  /*
    printf("MISC:    GCTable       = %9d b  \n"
	 "         SMLGlobal     = %9d b  \n"
	 "         GlobalTable   = %9d b        MutableTable = %9d b\n",
	 GCTableSize,                    
	 SMLGlobalSize,                   
	 GlobalTableSize,    MutableTableSize);
  */


  fd = fopen("runStats", "w");
  i = fwrite(statString,1,statStringCursor,fd);
  assert(statStringCursor == i);
  fclose(fd);
}

/* --------------- Debugging Stuff ---------------- */
static double times[1000];
static int which[1000];
static int data[1000];
int cursor = 0;

void resetTimeList()
{
  cursor = 0;
}

void addTimeList(void *procVoid, int w, int d)
{
  Proc_t *proc = (Proc_t *)procVoid;
  which[cursor] = w;
  data[cursor] = d;
  times[cursor] = segmentTime(proc);
  cursor++;
  assert(cursor < 1000);
}

int showTimeList(double min)
{
  int i;
  if (cursor == 0 || times[cursor-1] < min)
    return 0;
  for (i=0; i<cursor; i++) {
    printf("%2d - %5d  %4.2lf ms   ", which[i], data[i], times[i]);
    if ((i % 6) == 5)
      printf("\n");
  }
  printf("\n\n");
  return 1;
}

