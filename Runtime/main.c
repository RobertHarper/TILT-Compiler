#include "general.h"
#include <sys/resource.h>
#include <sys/user.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>

#include "queue.h"
#include "tag.h"
#include "memobj.h"
#include "exn.h"
#include "general.h"
#include "gc.h"
#include "gc_large.h"
#include "thread.h" 
#include "platform.h"
#include "til-signal.h"
#include "stats.h"
#include "global.h"
#include "stack.h"

int LEAST_GC_TO_CHECK = 0;
int checkAtGC = 0;

void setCommandLine(char* cmd, char** argv);

int process_bool(int *var, char *item, char *option)
{
  int match = !strcasecmp(item,option);
  if (match) 
    *var = 1;
  return match;
}

int process_string(char *var, char *item, char *option)
{
  int len = strlen(item);
  int prefix_match = !strncmp(item,option,len);
  int optlen = strlen(option); 
  if (!prefix_match || (optlen<=len) || (option[len] != '='))
    return 0;
  strcpy(var,option+len+1);
  return 1;
}

int process_long(long *var, char *item, char *option)
{
  int itemLen = strlen(item);
  int optLen = strlen(option); 
  int prefixMatch = !strncmp(item,option,itemLen);
  if (prefixMatch) {
    if (optLen == itemLen) {
      *var = 1;    /* Special case to act like bool too */
      return 1;
    }
    else if (optLen>itemLen && (option[itemLen] == '=')) {
      *var = atol(&option[itemLen+1]);
      return 1;
    }
  }
  return 0;
}

int process_int(int *var, char *item, char *option)
{
  long temp = *var;
  int status = process_long(&temp,item,option);
  *var = (int) temp;
  return status;
}

int process_byte(int *var, char *item, char *option)
{
  char optionByte[100];
  long temp = *var;
  int status = process_long(&temp,item,option);
  *var = (int) (1024 * temp);
  if (status)
    return status;
  sprintf(optionByte,"%sbyte",option);
  status = process_long(&temp,item,optionByte);
  *var = (int) temp;
  return status;
}

int process_double(double *var, char *item, char *option)
{
  int len = strlen(item);
  int prefix_match = !strncmp(item,option,len);
  int optlen = strlen(option); 
  double possval = 0.0;
  if (!prefix_match || (optlen<=len) || (option[len] != '='))
    return 0;
  /*  For some bizarre reason, atof will NOT work here, it returns 1.0 or 0.0 */
  sscanf(option+len+1,"%lf",&possval);
#ifdef DEBUG
  printf("Setting item %s to %lf\n",item,possval);
#endif
  *var = possval;
  return 1;
}

struct option_entry {
  int type; /* 0 for bool, 1 for int, 2 for long, 3 for double, 4 for byte measreuements */
  char *name; 
  void *item;
  char *description;
};

extern int largeheapsize;

extern int cacheSize, cacheSize2, fetchSize; /* XXXXXX debugging */

static int stackOrder = 0, queueOrder = 0, hybridOrder = 0, doSpaceCheck = 0;
static int help=0, semi=0, gen=0, semipara=0, genpara=0, semiconc = 0, genconc = 0;
static int FixheapByte = 0, RelaxFixheapByte = 0;
struct option_entry table[] = 
  {0, "help", &help, "Print help info but do not execute program",
   0, "semi", &semi, "Use the semispace garbage collector",
   0, "gen", &gen,   "Use the generational garbage collector",
   0, "semipara", &semipara, "Use the semispace, parallel garbage collector",
   0, "genpara", &genpara, "Use the generational, parallel garbage collector",
   0, "semiconc", &semiconc, "Use the semispace, concurrent garbage collector",
   0, "genconc", &genconc, "Use the generational, concurrent garbage collector",
   0, "forceMirrorArray", &forceMirrorArray, "Force collector to use mirrored pointer arrays",
   0, "useGenStack", &useGenStack, "Use generational stack tracing",
   1, "paranoid", &paranoid, "Run in paranoid mode every nth GC",
   0, "verbose", &verbose, "Be verbose when paranoid",
   0, "diag", &diag, "Run in diagnostic mode",
   1, "collectDiag", &collectDiag, "Run in collector diagnostic mode",
   0, "timeDiag", &timeDiag, "Run in time-diagnostic mode",
   0, "threadDiag", &threadDiag, "Show thread-related diagnostic messages",
   0, "warnThreshold", &warnThreshold, "Show information if pause exceeds threshold",
   3, "warnUtil", &warnUtil, "Show information if utilization falls below given fraction at 10 ms level",
   0, "debugStack", &debugStack, "Show scanning of stack frames",
   0, "gcstats", &SHOW_GCSTATS, "Show GC statistics during execution",
   0, "gcdebug", &SHOW_GCDEBUG, "Show GC debugging information during execution",
   0, "gcforward", &SHOW_GCFORWARD, "Show object forwarding infomation during GC",
   0, "gcerror", &SHOW_GCERROR, "Show GC errors",
   0, "showheaps", &SHOW_HEAPS, "Show heaps before and after each GC",
   0, "showglobals", &SHOW_GLOBALS, "Show globals before and after each GC",
   1, "showatgc", &LEAST_GC_TO_CHECK, "Show heaps starting at this GC",
   1, "checkatgc", &checkAtGC, "Check heaps starting at this GC",
   1, "stackletSize", &MLStackletSize, "Stack size of thread stacklets measured in Kbytes",
   1, "proc", &NumProc, "Use this many processors",
   1, "rotateProc", &RotateProc, "Skip this many processors before beginning assignment",
   1, "usageCount", &usageCount, "Update usage whenever this many gray objects processed",
   4, "largeheap", &LargeHeapByte, "Set large object heap size in bytes",
   4, "minheap", &MinHeapByte, "Set minimum size of heap in bytes",
   4, "maxheap", &MaxHeapByte, "Set maximum size of heap in bytes",
   4, "nursery", &NurseryByte, "Set size of nursery in bytes",
   4, "fixheap", &FixheapByte, "Set the size of heap in kbytes",
   4, "relaxFixheap", &RelaxFixheapByte, "Set the size of heap in Kbytes when concurrent collector off",
   1, "minratio", &MinRatio, "Set the minimum ratio of of live objects to all objects",
   1, "maxratio", &MaxRatio, "Set the maximum ratio of of live objects to all objects",
   1, "minOffRequest", &minOffRequest, "Minimum size of mutator request when collector is off",
   1, "maxOffRequest", &maxOffRequest, "Maximum size of mutator request when collector is off",
   1, "minOnRequest", &minOnRequest, "Minimum size of mutator request when collector is on",
   1, "copyPageSize", &copyPageSize, "Minimum size of copying area for the multi-collector case",
   1, "copyChunkSize", &copyChunkSize, "Minimum size of objects that are copied into area directly allocated from shared pool",
   1, "objFetchSize", &objFetchSize, "Number of items to fetch from the shared work stack",
   1, "localWorkSize", &localWorkSize, "Number of items to work on from local shared stack before accessing shared work stack",
   1, "doCopyCopySync", &doCopyCopySync, "Perform copy-copy synchronization for parallel/concurrent collectiors",
   1, "noSharing", &noSharing, "Do not use the shared stack to distribute work",
   1, "noWorkTrack", &noWorkTrack, "Do not update or test for work done.  Should be used only in addition to noSharing",
   1, "doAgressive", &doAgressive, "Use 2 phase concurrent collection",
   1, "doMinorAgressive", &doMinorAgressive, "Use 2 phase concurrent collection for minor collections",
   1, "doStableEfficiency", &doStableEfficiency, "Do more/less work when efficiency is higher/lower",
   3, "collectionRate", &CollectionRate, "Rate of concurrent collector",
   3, "objCopyWeight", &objCopyWeight, "Weight given to cost of copying an object",
   3, "objScanWeight", &objScanWeight, "Weight given to cost of scanning an object",
   3, "fieldCopyWeight", &fieldCopyWeight, "Weight given to cost of copying a field",
   3, "fieldScanWeight", &fieldScanWeight, "Weight given to cost of scanning a field",
   1, "stackOrder", &stackOrder, "Use stack ordering (use pop) on local work set",
   1, "queueOrder", &queueOrder, "Use queue ordering (use dequeue) on local work set",
   1, "hybridOrder", &hybridOrder, "Use a set of gray regions",
   1, "forceSpaceCheck", &doSpaceCheck, "Perform space check on allocating space for copying",
   1, "grayAsReplica", &grayAsReplica, "In concurrent collectors, store gray set as replica (with backptrs)",
#ifdef solaris
   1, "perfType", &perfType, "Type of performance counters",
#endif

   /* Debuggnig XXXXX */
   1, "fetchSize", &fetchSize, "",
   1, "cacheSize", &cacheSize, "",
   1, "cacheSize2", &cacheSize2, "",

   1, "doShowHistory", &doShowHistory, "Show history of states for each processor",
   1, "info", &information, "Level of information to print"};

void process_option(int argc, char **argv)
{
  char **cur;
  int i;
  for (cur=argv+1; *cur != NULL; cur++)
    {
      int matched = 0;
      char *poss_option = *cur;
      char *option = poss_option+1;
      if (*poss_option != '@')
	/* First non-option signals start of user-program options.*/
	break;
      for (i=0; i<arraysize(table); i++) {
	switch (table[i].type) {
	  case 0 : {
	    matched = process_bool(table[i].item, table[i].name, option);
	    break;
	  }
	  case 1 : {
	    matched = process_int(table[i].item, table[i].name, option);
	    break;
	  }
	  case 2 : {
	    matched = process_long(table[i].item, table[i].name, option);
	    break;
	  }
	  case 3 : {
	    matched = process_double(table[i].item, table[i].name, option);
	    break;
	  }
	  case 4 : {
	    matched = process_byte(table[i].item, table[i].name, option);
	    break;
	  }
	  default: {
	    printf("Unknown type for option entry %d", table[i].name);
	    assert(0);
	  }
	}
	if (matched) break;
      }
      if (!matched) {
	printf("Unknown option argument '%s'\n",option);
	assert(0);
      }
    }
  setCommandLine(argv[0], cur);
  if (semi) collector_type = Semispace;
  if (gen) collector_type = Generational;
  if (semipara) collector_type = SemispaceParallel;
  if (genpara) collector_type = GenerationalParallel;
  if (semiconc) collector_type = SemispaceConcurrent;
  if (genconc) collector_type = GenerationalConcurrent;
  if (stackOrder) ordering = StackOrder;
  if (queueOrder) ordering = QueueOrder;
  if (hybridOrder) ordering = HybridOrder;
  if (doSpaceCheck) forceSpaceCheck = 1;
  if (RelaxFixheapByte) {
    assert(collector_type == SemispaceConcurrent ||
	   collector_type == GenerationalConcurrent);
    relaxed = 1;
    MinHeapByte = MaxHeapByte = RelaxFixheapByte;
  }
  if (FixheapByte) MinHeapByte = MaxHeapByte = FixheapByte;
  if (help) {
    printf("Boolean options are activated like this: @diag\n");
    printf("Int, long, and double options are activated like this: @nursery=512\n");
    printf("The following options are available.\n");
    for (i=0; i<arraysize(table); i++) {
      char *type;
      printf("%12s : ", table[i].name);
      switch (table[i].type) {
	case 0 : printf("bool = %s", *(int *)table[i].item ? "true" : "false"); break;
	case 1 : printf("int = %d", *(int *)(table[i].item)); break;
	case 2 : printf("long = %d", *(long *)(table[i].item)); break;
	case 3 : printf("double = %lf", *(double *)(table[i].item)); break;
	default : printf("Unknown type!!!", table[i].name); assert(0);
      }
      printf(" -- %s\n", table[i].description);
    }
    exit(-1);
  }
}

void init_int(int *x, int y)
{
  if (*x == 0)
    *x = y;
}

void init_double(double *x, double y)
{
  if (*x == 0.0)
    *x = y;
}

extern ptr_t LINKUNIT_unit;

int main(int argc, char **argv)
{
  int i;
  Thread_t *th, *th2;

  process_option(argc,argv);

  stats_init();
  platform_init();
  memobj_init();
  signal_init();
  thread_init();
  global_init(); 
  exn_init();
  stack_init();  /* must follow thread_init */
  GCInit();

  thread_go((ptr_t) GetGlobal((ptr_t) &LINKUNIT_unit));
  stats_finish();
  return 0;
}



