#include "tag.h"
#include "memobj.h"
#include "mlclient.h"
#include "thread.h"
#include <sys/resource.h>
#include <sys/user.h>
#include <assert.h>
#include "exn.h"
#include "general.h"
#include <string.h>
#include "platform.h"

int ThreadedVersion = THREADED_VERSION;



extern int   client_entry;
extern int module_count;
extern long SHOW_GCSTATS;
extern long SHOW_GCFORWARD;
extern long SHOW_GCDEBUG;
extern long SHOW_GCERROR;
extern long SHOW_HEAPS;
extern long MinHeap, MaxHeap;
extern double TargetRatio;
extern long StackSize;
extern long generational_flag;
extern long YoungHeapByte;
extern long save_rate;
extern long use_stack_gen;

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

void process_option(int argc, char **argv)
{
  char **cur;
  for (cur=argv+1; *cur != NULL; cur++)
    {
      char *poss_option = *cur;
      char *option = poss_option+1;
      char buf[100];
      if (*poss_option != '@')
	{
	  printf("Ignoring non-option argument '%s'\n",poss_option);
	  continue;
	}
      if (process_string(buf,"GC_METHOD",option))
	{
	  if (!strcmp(buf,"semi"))
	    generational_flag = 0;
	  continue;
	}
      if (process_long(&SHOW_GCSTATS,"SHOW_GCSTATS",option))  continue;
      if (process_long(&SHOW_GCDEBUG,"SHOW_GCDEBUG",option))  continue;
      if (process_long(&SHOW_GCFORWARD,"SHOW_GCFORWARD",option))  continue;
      if (process_long(&SHOW_GCERROR,"SHOW_GCERROR",option))  continue;
      if (process_long(&SHOW_HEAPS,"SHOW_HEAPS",option))      continue;
      if (process_long(&StackSize,"StackSize",option))       continue;
      if (process_double(&TargetRatio,"TargetRatio",option)) continue;
      if (process_long(&MinHeap,"MinHeap",option))           continue;
      if (process_long(&MaxHeap,"MaxHeap",option))           continue;
      {
	long FixHeap = 0;
	if (process_long(&FixHeap,"FixHeap",option))
	  {
	    MinHeap = MaxHeap = FixHeap;
	    continue;
	  }
      }
      if (process_long(&YoungHeapByte,"YoungHeapByte",option))   continue;
      {
	long YoungHeap = 0;
	if (process_long(&YoungHeap,"YoungHeap",option))         
	  {
	    YoungHeapByte = 1024 * YoungHeap;
	    continue;
	  }
      }
      if (process_long(&save_rate,"save_rate",option))           continue;
      if (process_long(&use_stack_gen,"use_stack_gen",option))   continue;
      printf("Unknown option argument '%s'\n",option);
    }
}

int main(int argc, char **argv)
{
  int i;
  ThreadObj_t *th, *th2;

#ifdef
  printf("Entered main\n");
#endif
  process_option(argc,argv);

  stats_init();
  platform_init();
  memobj_init();
  stack_init();
  mllib_init();
  signal_init();
  global_init(); 
  exn_init();
  thread_init();
  gc_init();

  th = thread_create((value_t)(&client_entry),module_count);
  thread_insert(th);
  thread_go();
  assert(FALSE);
  return -1;
}



