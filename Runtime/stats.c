#include <values.h>
#include "math.h"
#include "general.h"
#include "tag.h"
#include "memobj.h"
#include "stats.h"
#include "gcstat.h"
#include "gc.h"
#include "thread.h"
#include "stack.h"
#include <string.h>
#include <strings.h>
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

int information = 1;
int	prof_fd = 1;
static double time_diff(void),time2double(void);
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

/* --------------- Debugging Stuff ---------------- */
static double times[2000];
static int which[2000];
static int data[2000];
int cursor = 0;

void initTimeList(void)
{
  memset(times, 0, sizeof(times));
  memset(which, 0, sizeof(which));
  memset(data, 0, sizeof(data));
}

void resetTimeList(void)
{
  cursor = 0;
}

int showTimeList(double min)
{
  int i;
  if (cursor == 0 || times[cursor-1] < min)
    return 0;
  for (i=0; i<cursor; i++) {
    printf("%2d - %5d  %4.2lf ms     ", which[i], data[i], times[i]);
    if ((i % 5) == 4)
      printf("\n");
  }
  printf("\n\n");
  return 1;
}

double addTimeList(void *procVoid, int w, int d)
{
  Proc_t *proc = (Proc_t *)procVoid;
  double t = nonMutatorTime(proc);
  which[cursor] = w;
  data[cursor] = d;
  times[cursor] = t;
  cursor++;
  if (cursor >= sizeof(which) / sizeof(int)) {
    printf("Time list overflowed with %d entries:\n", sizeof(which) / sizeof(int));
    showTimeList(0.0);
    assert(0);
  }
  return t;
}

static void show_time_statistic_header(void)
{
  printf("                                Sum (s)        Count     Min(ms)   Avg(ms)  Max(ms)\n");
  printf("         --------------|-----------------------------------------------------------\n");
}

static void show_time_statistic(char *str, Statistic_t *s, double totalSum)
{
  double percent;
  double partialSum = s->sum / 1000.0;
  totalSum /= 1000.0;
  percent = (100.0 * partialSum) / totalSum;

  if (s->sum == 0.0)
    return;
  printf("         %s |   %8.3f", str, partialSum);
  if (totalSum >= 0.0) {
    if (percent == 100.0)
      printf(" ( 100%%)");
    else 
      printf(" (%4.1f%%)", percent);
  }
  else
    printf("        ");
  printf("   %6d      %6.2f    %6.2f   %6.2f\n", s->count, s->min, s->sum/s->count, s->max);
}

static void show_double(double f)
{
  printf((f >= 1000.0) ? "%6.0f" : 
	 ((f >= 100.0) ? "%6.1 f" : 
	  ((f >= 1.0) ? "%6.2f" : "%6.3f")) , f);
}

static void show_statistic(char *str, Statistic_t *s)
{
  if (s->count > 0) {
    printf("%s", str);
    show_double(s->min);
    printf(" < ");
    show_double(s->sum/s->count);
    printf(" < ");
    show_double(s->max);
  }
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
  else if (v == 1000.0)
    printf(" 1K");
  else if (v >= 1.0 || v < 0.001)
    printf("%3.0f",v);
  else if (v >= 0.001) {
    char buf[16];
    sprintf(buf,"%.3g",v);
    printf("%3s",&buf[1]);
  }
    
}

static void show_bucket(Histogram_t *h, int i)
{
  printf("[");
  show_bucket_end(h->bucketStart[i]);
  printf(",");
  show_bucket_end(h->bucketEnd[i]);
  printf("] = %5d", h->bucket[i]);
}

static void show_histogram(char *name, Histogram_t *h)
{
  int i, j;
  if (h->stat.sum == 0.0)
    return;
  printf("  %10s :    ", name);
  if (h->bucket[0]) {
    show_bucket(h,0);
    printf("     ");
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
    printf("     ");
    for (j=0; j<6; j++) {
      show_bucket(h,6*i+j+1);
      printf("   ");
    }
    printf("\n");
  }
}


void reset_windowQuotient(WindowQuotient_t *wq, int fineness)
{
  int i;
  if (fineness == 0) {
    wq->numWindows = 5;
    wq->granularity = 50;
    wq->size = 2 * 40 * wq->granularity;  /* Twice largest window */
  }
  else {
    wq->numWindows = 14;
    wq->granularity = 5;
    wq->size = 2 * 4000 * wq->granularity;
  }

  wq->data = (char *) malloc(sizeof(float) * wq->size);
  wq->first = 0;
  wq->last = 0;
  wq->onRemain = 0.0;
  wq->offRemain = 0.0;
  assert(wq->data != NULL);

  for (i=0; i<wq->numWindows; i++) {
    wq->start[i] = 0;
    wq->onSum[i] = 0;
    wq->offSum[i] = 0;
    reset_statistic(&wq->stat[i]);
  }

  if (fineness == 0) {
    wq->windowSize[0]  =    5 * wq->granularity;
    wq->windowSize[1]  =   10 * wq->granularity;
    wq->windowSize[2]  =   20 * wq->granularity;
    wq->windowSize[3]  =   30 * wq->granularity;
    wq->windowSize[4]  =   40 * wq->granularity;
  }
  else if (fineness == 1) {
    wq->windowSize[0]  =   50 * wq->granularity;
    wq->windowSize[1]  =   60 * wq->granularity;
    wq->windowSize[2]  =   70 * wq->granularity;
    wq->windowSize[3]  =   80 * wq->granularity;
    wq->windowSize[4]  =   90 * wq->granularity;
    wq->windowSize[5]  =  100 * wq->granularity;
    wq->windowSize[6]  =  150 * wq->granularity;
    wq->windowSize[7]  =  200 * wq->granularity;
    wq->windowSize[8]  =  250 * wq->granularity;
    wq->windowSize[9]  =  500 * wq->granularity;
    wq->windowSize[10] = 1000 * wq->granularity;
    wq->windowSize[11] = 2000 * wq->granularity;
    wq->windowSize[12] = 3000 * wq->granularity;
    wq->windowSize[13] = 4000 * wq->granularity;
  }
}

/* Although there are an infinite number of windows of a given size sz, we need to consider
   only a finite number to obtain the range of the quotients.  Also, we only need to keep
   enough data points so that an amount just larger than the window is spanned.  Any data
   before that is uneffected by future data and should have been computed already.
   The question reduces to a series of times with an associated on/off bit and obtaining
   the quotients of all windows in that range of time.

   For each window size w:
   (1) onSum_w + offSum_w == SUM (data[i])    - firstExtra_w  < windowSize_w
                           start_w <= i < end

*/


INLINE(add_windowQuotient_addHelp)
void add_windowQuotient_addHelp(WindowQuotient_t *wq, int i)
{
  /*  assert (wq->onSum[i] >= 0);
  assert (wq->offSum[i] >= 0);
  assert (wq->onSum[i] + wq->offSum[i] == wq->windowSize[i]);
*/
  double q = wq->onSum[i] / ((double)wq->windowSize[i]);
  add_statistic(&wq->stat[i], q);
}

void check(WindowQuotient_t *wq, int i)
{
  int c = wq->start[i], on = 0, off = 0;
  while (c != wq->last) {
    if (wq->data[c]) { on++; } else { off++; }
    c++;
    if (c >= wq->size) 
      c = 0;
  }
  printf("on/off = %d %d     onSum/offSum = %d  %d\n", on, off, wq->onSum[i], wq->offSum[i]);
  if (on != wq->onSum[i] ||
      off != wq->offSum[i])
    assert(0);
}

/* Time in ms */
void add_windowQuotient(WindowQuotient_t *wq, double time, int on)
{
  int i, w;
  double units = wq->granularity * time;
  int ticks = (int) units;
  
  assert(time >= 0.0);
  if (on) { wq->onRemain += (units - ticks); }
  else { wq->offRemain += (units - ticks); }
  if (on && wq->onRemain >= 1.0) {
    wq->onRemain -= 1.0;
    ticks++;
  }
  else if (!on && wq->offRemain >= 1.0) {
    wq->offRemain -= 1.0;
    ticks++;
  }
  ticks = Min(ticks, wq->size / 2);  /* Half of size is greater than largest window */
  if (ticks == 0)
    return;
  /*
  if (wq->numWindows > 0)
    printf("adding %d  ticks  %d\n", ticks, on);
  for (w=0; w<wq->numWindows; w++) 
    check(wq,w);
    */
  for (i=0; i < ticks; i++) {
    wq->data[wq->last] = on;
    wq->last = (wq->last + 1 == wq->size) ? 0 : wq->last + 1;
  }
  
  for (w=0; w<wq->numWindows; w++) {
    int need = wq->windowSize[w] - (wq->onSum[w] + wq->offSum[w]);
    int add = (need < ticks) ? need : ticks;
    int transfer = ticks - add;
    int *onRef = &wq->onSum[w];
    int *offRef = &wq->offSum[w];
    int *startRef = &wq->start[w];

  assert (*onRef >= 0);
    if (on) {
      wq->onSum[w] += add;
      for (i=0; i<transfer; i++) {
	int firstOn = wq->data[(*startRef)++];
	*startRef = (*startRef == wq->size) ? 0 : *startRef;
	if (!firstOn) {
	  (*onRef)++; 
	  (*offRef)--;
	}
	if (firstOn != (wq->data[*startRef]))
	  add_windowQuotient_addHelp(wq,w);
      }
    }
    else {
      wq->offSum[w] += add;
      for (i=0; i<transfer; i++) {
	int firstOn = wq->data[(*startRef)++];
	*startRef = (*startRef == wq->size) ? 0 : *startRef;
	if (firstOn) {
	  (*offRef)++;
	  (*onRef)--; 
	}
	if (firstOn != (wq->data[*startRef]))
	  add_windowQuotient_addHelp(wq,w);
      }
    }
    /*
check(wq,w);
printf("\n");
*/
    if (transfer > 0)
      add_windowQuotient_addHelp(wq,w);
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

void stats_init(void)
{   
  statString[0] = 0;
  initTimeList();
  getrusage(RUSAGE_SELF,&start_rusage);
  clock_gettime(CLOCK_REALTIME, &start_tp);
}

const char *collectorTypeString(void)
{
  return (collector_type == Semispace) ? "Semi" : 
    ((collector_type == Generational) ? "Gen" : 
      ((collector_type == SemispaceParallel) ? "SemiPara" :
       ((collector_type == GenerationalParallel) ? "GenPara" : 
	((collector_type == SemispaceConcurrent) ? "SemiConc" :
	 ((collector_type == GenerationalConcurrent) ? "GenConc" : "????")))));
}

const char* copyCopyString(void)
{
  int needCC = (collector_type == SemispaceParallel ||
		collector_type == SemispaceConcurrent ||
		collector_type == GenerationalParallel ||
		collector_type == GenerationalConcurrent);
  if (needCC) {
    if (doCopyCopySync)
      return "Needed";
    return "Omitted";
  }
  else return "Unneeded";
}

const char *orderString(void)
{
  return (ordering == DefaultOrder) ? "Default" : 
    ((ordering == ImplicitOrder) ? "Implicit" : 
    ((ordering == QueueOrder) ? "Queue" : 
     ((ordering == StackOrder) ? "Stack" : 
     ((ordering == HybridOrder) ? "Hybrid" : "????"))));
}




void stats_finish(void)
{ 
  int i;
  FILE *fd;
  double elapsed;
  double AvgStackDepth = TotalStackDepth/((double)NumGC+eps);
  double AvgNewStackDepth = TotalNewStackDepth/((double)NumGC+eps);
  double AvgStackFrameSize = TotalStackSize /(double)(TotalStackDepth+eps);
  double bytesAllocated = 0.0, bytesCopied = 0.0, bytesReplicated = 0.0;
  long NumCopied = 0, NumShared = 0, NumContention = 0, NumRoots = 0, NumLocatives = 0, NumWrites = 0;

  getrusage(RUSAGE_SELF,&stop_rusage);
  clock_gettime(CLOCK_REALTIME, &stop_tp);
  elapsed = tp_diff(&stop_tp, &start_tp);

  for (i=0; i<NumProc; i++) {
    Proc_t *proc = getNthProc(i);
    bytesReplicated += proc->bytesReplicatedStatistic.sum;
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

  if (information >= 1) {
    for (i=0; i<NumProc; i++) {
      Proc_t *proc = getNthProc(i);
      printf("\n");
      printf("PROC #%d: Allocated  = %8.0f kb\n", i, proc->bytesAllocatedStatistic.sum / 1024.0);
      printf("         Copied     = %8.0f kb\n", proc->bytesCopiedStatistic.sum / 1024.0); 
      printf("         Work       = %8.0f kw\n", proc->workStatistic.sum / 1024.0);
      show_time_statistic_header();
      printf("         Total         |   %8.3f\n", proc->totalTimer.last / 1000.0);
      show_time_statistic("  Accounting ", &proc->accountingStatistic, proc->totalTimer.last);
      show_time_statistic("  Scheduler  ", &proc->schedulerStatistic, proc->totalTimer.last);
      show_time_statistic("  Mutator    ", &proc->mutatorHistogram.stat, proc->totalTimer.last);
      show_time_statistic("  GC         ", &proc->gcStatistic, proc->totalTimer.last);
      show_time_statistic("  Idle       ", &proc->idleStatistic, proc->totalTimer.last);
      show_time_statistic("Pause        ", &proc->gcPauseHistogram.stat, proc->totalTimer.last);
      if (proc->utilizationQuotient1.stat[0].count > 0) {
	WindowQuotient_t *wq1 = &proc->utilizationQuotient1;
	WindowQuotient_t *wq2 = &proc->utilizationQuotient2;
	int i, total = wq1->numWindows + wq2->numWindows;
	int rows = (total + 2) / 3;
	printf("\n");
	for (i=0; i<rows; i++) {
	  int j = i;
	  WindowQuotient_t *wq = (i < wq1->numWindows) ? wq1 : wq2;
	  if (j >= wq1->numWindows)
	    j -= wq1->numWindows;
	  printf("      %6.1f ms util:  ", (double) wq->windowSize[j] / wq->granularity);
	  show_statistic("", &wq->stat[j]);

	  j = i + rows;
	  wq = (j < wq1->numWindows) ? wq1 : wq2;
	  if (j >= wq1->numWindows)
	    j -= wq1->numWindows;
	  if (j < wq->numWindows) {
	    printf("      %6.1f ms util:  ", (double) wq->windowSize[j] / wq->granularity);
	    show_statistic("", &wq->stat[j]);
	  }

	  j = i + 2 * rows;
	  wq = (j < wq1->numWindows) ? wq1 : wq2;
	  if (j >= wq1->numWindows)
	    j -= wq1->numWindows;
	  if (j < wq->numWindows) {
	    printf("      %6.1f ms util:  ", (double) wq->windowSize[j] / wq->granularity);
	    show_statistic("", &wq->stat[j]);
	  }

	  printf("\n");
	}
      }
      if (information >= 2) {
	printf("\n");
	show_time_statistic("  GCIdle     ", &proc->gcIdleStatistic, proc->gcStatistic.sum);
	show_time_statistic("  GCWork     ", &proc->gcWorkStatistic, proc->gcStatistic.sum);
	show_time_statistic("  GCGlobal   ", &proc->gcGlobalStatistic, proc->gcStatistic.sum);
	show_time_statistic("  GCStack    ", &proc->gcStackStatistic, proc->gcStatistic.sum);
	show_time_statistic("  GCWrite    ", &proc->gcWriteStatistic, proc->gcStatistic.sum);
	show_time_statistic("  GCReplicate", &proc->gcReplicateStatistic, proc->gcStatistic.sum);
	show_time_statistic("  GCOther    ", &proc->gcOtherStatistic, proc->gcStatistic.sum);
      }
      if (information >= 3) {
	printf("\n");
	show_time_statistic("GCFlipBoth   ", &proc->gcFlipBothHistogram.stat, proc->gcStatistic.sum);
	show_time_statistic("GCFlipOn     ", &proc->gcFlipOnHistogram.stat, proc->gcStatistic.sum);
	show_time_statistic("GCFlipOff    ", &proc->gcFlipOffHistogram.stat, proc->gcStatistic.sum);
	show_histogram(" Pause   Histogram (ms)", &proc->gcPauseHistogram);
	show_histogram(" Time / Work  Hist (ms/kw)", &proc->timeDivWorkHistogram);
	show_histogram(" GCFlipBoth   Hist (ms)", &proc->gcFlipOffHistogram);
	show_histogram(" GCFlipOff    Hist (ms)", &proc->gcFlipOffHistogram);
	show_histogram(" GCFlipOn     Hist (ms)", &proc->gcFlipOnHistogram);
	/*      show_histogram("Mutator Histogram", &proc->mutatorHistogram); */
      }
    }
  }

  printf("\n");
  printf("GC:    GCMethod      = %12s      StackMethod = %s\n",
	 collectorTypeString(), useGenStack?"Gener":"Normal");
  printf("       Ordering   = %8s        ForceSpaceCheck = %s   CopyCopy = %8s     WorkSharing = %s     WorkTrack = %s   Relaxed = %s\n",
	 orderString(),  forceSpaceCheck ? "Yes" : "No",
	 copyCopyString(), noSharing ? "No" : "Yes", noWorkTrack ? "No" : "Yes", relaxed ? "Yes" : "No");
  printf("       MajorCollectionRate = %.2f  MinorCollectionRate = %.2f\n",
       majorCollectionRate, minorCollectionRate);
  printf("       Allocated     = %9.0f kb      Copied     = %9.0f kb    Replicated  = %9.0f kb\n",
	 bytesAllocated / 1024.0, bytesCopied / 1024.0,  bytesReplicated/1024.0 + 0.001);
  printf("       NumGC         = %6d            NumMajorGC = %3d\n"
	 "       AvgFramsSize  = %4.0f         AvgStkDepth  = %4.0f           MaxStkDepth  = %4d    newStkDepth = %4.0f\n",
	 NumGC, NumMajorGC, 
	 AvgStackFrameSize, AvgStackDepth, MaxStackDepth, AvgNewStackDepth);
  if (information >= 1)
    printf("       NumCopied     = %8d     NumRoot      = %9d      NumConflict  = %8d\n"
	   "       NumShared     = %8d     NumWrite     = %9d      NumLocative  = %8d\n",
	   NumCopied,                     NumRoots,                NumContention,   
	   NumShared,                     NumWrites,               NumLocatives);
  show_statistic("       MinSurvRate   ", &minorSurvivalStatistic);
  printf("\n");
  show_statistic("       MajSurvRate   ", &majorSurvivalStatistic);
  printf("\n");
  show_statistic("       HeapSize(kb)  ", &heapSizeStatistic);
  printf("\n");

  printf("\n");
  printf("MISC:  Total time    = %8.2f s   maxPhysMem   = %9d      minPageFault = %8d    invCtxtSwap  = %5d\n"
         "       Total Threads = %8d     sharedMem    = %9d      majPageFault = %8d    volCtxtSwap  = %5d\n"
	 "       Processors    = %8d\n"
	 "       Max Threads   = %8d     unsharedData = %9d                                 swapping     = %5d\n"
	 "                                    unsharedStk  = %9d\n",
	 elapsed, 	 stop_rusage.ru_maxrss,	 stop_rusage.ru_minflt,	stop_rusage.ru_nvcsw,  
	 thread_total(), stop_rusage.ru_ixrss,   stop_rusage.ru_majflt, stop_rusage.ru_nivcsw,
	 NumProc,
	 thread_max(),	 stop_rusage.ru_idrss,                          stop_rusage.ru_nswap,
	                 stop_rusage.ru_isrss);

  if (workStack != NULL)
    printf("       Shared Stack Push = %6d    Shared Stack Pops = %6d\n",
	   workStack->numPush, 	 workStack->numPop);
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

