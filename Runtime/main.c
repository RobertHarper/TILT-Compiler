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
#include "platform.h"
#include "gc.h"
#include "thread.h" 
#include "til-signal.h"
#include "stats.h"
#include "global.h"
/* #include "mllib.h" */
#include "client.h"

int ThreadedVersion = THREADED_VERSION;
int LEAST_GC_TO_CHECK = 0;

int NumHeap       = 20;
int NumStack      = 100;
int NumStackChain = 100;
int NumThread     = 100;
int NumSysThread  = 1;

int process_bool(int *var, char *item, char *option)
{
  int match = !strcmp(item,option);
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
#ifdef DEBUG
  printf("Setting item %s to %s\n",item,var);
#endif
  return 1;
}

int process_long(long *var, char *item, char *option)
{
  int len = strlen(item);
  int prefix_match = !strncmp(item,option,len);
  int optlen = strlen(option); 
  long possval = 0;
  if (!prefix_match || (optlen<=len) || (option[len] != '='))
    return 0;
  possval = atol(option+len+1);
#ifdef DEBUG
  printf("Setting item %s to %d\n",item,possval);
#endif
  *var = possval;
  return 1;
}

int process_int(int *var, char *item, char *option)
{
  long temp = *var;
  int status = process_long(&temp,item,option);
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
  int type; /* 0 for bool, 1 for int, 2 for long, 3 for double */
  char *name; 
  void *item;
  char *description;
};

static int help=0, semi=0, gen=0, semipara=0, genpara=0, semiconc = 0, genconc = 0, fixheap=0, youngheap=0;
struct option_entry table[] = 
  {0, "help", &help, "Print help info but do not execute program",
   0, "semi", &semi, "Use the semispace garbage collector",
   0, "gen", &gen,   "Use the generational garbage collector",
   0, "semipara", &semipara, "Use the semispace, parallel garbage collector",
   0, "genpara", &genpara, "Use the generational, parallel garbage collector",
   0, "semiconc", &semiconc, "Use the semispace, concurrent garbage collector",
   0, "genconc", &genconc, "Use the generational, concurrent garbage collector",
   0, "paranoid", &paranoid, "Run in paranoid mode",
   0, "verbose", &verbose, "Be verbose when paranoid",
   0, "diag", &diag, "Run in diagnostic mode",
   0, "debugStack", &debugStack, "Show scanning of stack frames",
   0, "gcstats", &SHOW_GCSTATS, "Show GC statistics during execution",
   0, "gcdebug", &SHOW_GCDEBUG, "Show GC debugging information during execution",
   0, "gcforward", &SHOW_GCFORWARD, "Show object forwarding infomation during GC",
   0, "gcerror", &SHOW_GCERROR, "Show GC errors",
   0, "showheaps", &SHOW_HEAPS, "Show heaps before and after each GC",
   0, "showglobals", &SHOW_GLOBALS, "Show globals before and after each GC",
   1, "showatgc", &LEAST_GC_TO_CHECK, "Check/show heaps starting at this GC",
   1, "stacksize", &StackSize, "Stack size of threads measured in Kbytes",
   1, "proc", &NumSysThread, "Use this many processors",
   1, "minheap", &MinHeap, "Set minimum size of heap in Kbytes",
   1, "maxheap", &MaxHeap, "Set maximum size of heap in Kbytes",
   1, "fixheap", &fixheap, "Set the size of heap in Kbytes",
   1, "nursery", &youngheap, "Set size of nursery in Kbytes",
   1, "nurserybyte", &YoungHeapByte, "Set size of nursery in bytes",
   1, "minratio", &MinRatio, "Set the minimum ratio of of live objects to all objects",
   1, "maxratio", &MaxRatio, "Set the maximum ratio of of live objects to all objects",
   0, "short", &shortSummary, "Print short summary of execution"};

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
	{
	  printf("Ignoring non-option argument '%s'\n",poss_option);
	  continue;
	}
      for (i=0; i<sizeof(table) / sizeof(struct option_entry); i++) {
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
  if (semi) collector_type = Semispace;
  if (gen) collector_type = Generational;
  if (semipara) collector_type = SemispaceParallel;
  if (genpara) collector_type = GenerationalParallel;
  if (semiconc) collector_type = SemispaceConcurrent;
  if (genconc) collector_type = GenerationalConcurrent;
  if (fixheap) MinHeap = MaxHeap = fixheap;
  if (youngheap) YoungHeapByte = 1024 * youngheap;
  if (help) {
    printf("Boolean options are activated like this: @diag\n");
    printf("Int, long, and double options are activated like this: @nursery=512\n");
    printf("The following options are available.\n");
    for (i=0; i<sizeof(table) / sizeof(struct option_entry); i++) {
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
  stack_init();
  gc_init();

  thread_go((ptr_t *)(&client_entry),module_count);
  stats_finish();
  return 0;
}



