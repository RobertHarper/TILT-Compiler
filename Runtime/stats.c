#include <stdio.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "general.h"
#include "tag.h"
#include "stats.h"
#include "gcstat.h"


extern long generational_flag;
extern long use_stack_gen;

#ifdef HEAPPROFILE
extern Object_Profile_t allocated_object_profile;
extern Object_Profile_t collected_object_profile;
#endif

int	prof_fd = 1;
static double time_diff(),time2double();
static struct rusage start,finish;
timer_mt gctime, stacktime, majorgctime;
extern long TotalBytesAllocated, TotalBytesCollected;
extern TotalStackDepth, MaxStackDepth, TotalStackSize, TotalNewStackDepth;
extern int NumGC, NumMajorGC;
extern long GCTableSize;
extern long CodeSize;
extern long SMLGlobalSize;
extern long GlobalTableSize;
extern long MutableTableSize;
extern int write_count;
extern long NumLocatives;
extern long NumRoots;

extern long SemanticGarbageSize;

void stats_init()
{   
  getrusage(RUSAGE_SELF,&start);
  reset_timer(&gctime);
  reset_timer(&stacktime);
}

void stats_finish()
{ 
  double all,userall, sysall, nongc_user, nongc_sys, nongc_all;
  double gc_all, gc_user, gc_sys;
  double gc_copy, gc_stack, majorgc_copy;
  double gc_stack_user, gc_stack_sys, gc_copy_user, gc_copy_sys;
  double eps=1e-5;
  double AvgStackDepth = TotalStackDepth/((double)NumGC+eps);
  double AvgNewStackDepth = TotalNewStackDepth/((double)NumGC+eps);
  double AvgStackFrameSize = TotalStackSize /(double)(TotalStackDepth+eps);

  getrusage(RUSAGE_SELF,&finish);
  userall = time_diff(&(start.ru_utime),&(finish.ru_utime));
  sysall = time_diff(&(start.ru_stime),&(finish.ru_stime));
  all = userall + sysall;
  nongc_user = userall-getuser_timer(&gctime);
  nongc_sys = sysall-getsys_timer(&gctime);
  nongc_all = nongc_user + nongc_sys;
  gc_all = gettotal_timer(&gctime);
  gc_user = getuser_timer(&gctime);
  gc_sys = getsys_timer(&gctime);
  majorgc_copy = gettotal_timer(&majorgctime);
  gc_stack = gettotal_timer(&stacktime);
  gc_copy = gc_all - gc_stack;
  gc_stack_user = getuser_timer(&stacktime);
  gc_stack_sys = getsys_timer(&stacktime);
  gc_copy_user = gc_user - gc_stack_user;
  gc_copy_sys = gc_sys - gc_stack_sys;

#ifdef SEMANTIC_GARBAGE
  printf("SemanticGarbageSize = %d\n",SemanticGarbageSize);
#endif

#ifdef SUPPRESS
  return;
#endif



  printf("\n\n");
  printf("TIME(s): total_time                = %6.3lf\n"
	 "          >total-user_time         = %6.3lf(%4.2lf%%)\n"
         "           total-sys_time          = %6.3lf(%4.2lf%%)\n",
	 all,
	 userall,        (100.0*userall)    /(all+eps),
	 sysall,         (100.0*sysall)     /(all+eps));
#ifndef FAST_DIAG
  printf("          >client_time             = %6.3lf(%4.2lf%%)\n"
	 "             client-user_time      = %6.3lf(%4.2lf%%)\n"
         "             client-sys_time       = %6.3lf(%4.2lf%%)\n"
	 "           gc_time                 = %6.3lf(%4.2lf%%)\n"
         "            >gc-user_time          = %6.3lf(%4.2lf%%)\n"
	 "             gc-sys_time           = %6.3lf(%4.2lf%%)\n"
	 "            >majorgc-copy_time     = %6.3lf(%4.2lf%%)\n"
	 "            >gc-copy_time          = %6.3lf(%4.2lf%%)\n"
	 "                gc-copy-user_time  = %6.3lf(%4.2lf%%)\n"
	 "                gc-copy-sys_time   = %6.3lf(%4.2lf%%)\n"
	 "             gc-stack_time         = %6.3lf(%4.2lf%%)\n"
	 "                gc-stack-user_time = %6.3lf(%4.2lf%%)\n"
	 "                gc-stack-sys_time  = %6.3lf(%4.2lf%%)\n",
	 nongc_all,      (100.0*nongc_all)   /(all+eps),
	 nongc_user,     (100.0*nongc_user)  /(nongc_all+eps),
	 nongc_sys,      (100.0*nongc_sys)   /(nongc_all+eps),
	 gc_all,         (100.0*gc_all)      /(all+eps),
         gc_user, 	 (100.0*gc_user)/(gc_all+eps),
         gc_sys, 	 (100.0*gc_sys)/(gc_all+eps),
         majorgc_copy, 	 (100.0*majorgc_copy)/(gc_all+eps),
         gc_copy, 	 (100.0*gc_copy)     /(gc_all+eps),
         gc_copy_user,   (100.0*gc_copy_user)    /(gc_copy+eps),
         gc_copy_sys, 	 (100.0*gc_copy_sys)     /(gc_copy+eps),
         gc_stack, 	 (100.0*gc_stack)    /(gc_all+eps),
         gc_stack_user,  (100.0*gc_stack_user)    /(gc_stack+eps),
         gc_stack_sys, 	 (100.0*gc_stack_sys)    /(gc_stack+eps));

  printf("GC(K):   heap_method   = %9s    stack_method  = %s\n"
         "         bytes_alloced = %9ld    bytes_copied  = %d\n"
	 "         NumGC         = %9d    NumMajorGC    = %d\n",
	 generational_flag?"Gener":"Semi",
	 use_stack_gen?"Gener":"Normal",
	 TotalBytesAllocated,             TotalBytesCollected,
	 NumGC,                           NumMajorGC);
    printf("         Num_of_roots = %9d\n", NumRoots);
  if (generational_flag)
    printf("         Num_of_writes = %9d    Num_of_Locs = %d\n",
	   write_count, NumLocatives);
  printf(  "STACK(K):max_stack_depth = %7d    stk_frame_sz  = %4.2lf\n",
	   MaxStackDepth,                   AvgStackFrameSize);
  if(use_stack_gen)	   
    printf("         avg_stack_depth = %7.1lf    new stack dep = %5.1lf\n",
	   AvgStackDepth,                   AvgNewStackDepth);
  else
    printf("         avg_stack_depth = %7.1lf\n",  AvgStackDepth);
  printf("MEM(K):  max_phys_mem  = %9d" "    shared_mem    = %d\n"
	 "         unshared_data = %9d" "    unshared_stk  = %d\n"
	 "PAGING:  page_reclaims = %9d" "    page_faults   = %d     swaps = %d\n"
	 "PROCESS: inv_cont_sws  = %9d" "    vol_cont_sws  = %d\n",

	 finish.ru_maxrss,	          finish.ru_ixrss,
	 finish.ru_idrss,	          finish.ru_isrss,
	 finish.ru_minflt,	          finish.ru_majflt,   finish.ru_nswap,
	 finish.ru_nvcsw,	          finish.ru_nivcsw);
  printf("MISC:    GCTableSize   = %9d    CodeSize = %d\n"
	 "         SMLGlobalSize = %9d    \n"
	 "         GlobalTableSz = %9d    MutableTableSz = %d\n",
	 GCTableSize,                     CodeSize,
         SMLGlobalSize,                   
	 GlobalTableSize,    MutableTableSize);
#endif
#ifdef HEAPPROFILE
  show_gcstats(&allocated_object_profile, &collected_object_profile);
#endif
  printf("DONE:\n");
}

static double time2double (struct timeval *t)
{ 
  return((double) t->tv_sec + (double) t->tv_usec/1000000.0); 
}

static double time_diff (struct timeval *start,
		  struct timeval *finish)
{ 
  return(time2double(finish)-time2double(start));
}


void reset_timer(timer_mt *t)
{
  t->user = t->sys = 0.0;
  t->on =  0;
}

void start_timer(timer_mt *t)
{
  if (t->on)
    {
      printf("ERROR: Can't start a timer that is already started\n");
      exit(-1);
    }
  t->on = 1;
  getrusage(RUSAGE_SELF,&(t->cur_usage));
}

void stop_timer(timer_mt *t)
{
  struct rusage finish;
  double user_inc, sys_inc;
  if (!t->on)
    {
      printf("ERROR: Can't stop a timer that has not been started\n");
      exit(-1);
    }
  getrusage(RUSAGE_SELF,&finish);
  user_inc = time_diff(&(t->cur_usage.ru_utime),&(finish.ru_utime));
  sys_inc = time_diff(&(t->cur_usage.ru_stime),&(finish.ru_stime));
  t->user += user_inc;
  t->sys += sys_inc;
  t->on = 0;
}

double getuser_timer(timer_mt *t)
{
  return t->user;
}

double getsys_timer(timer_mt *t)
{
  return t->sys;
}

double gettotal_timer(timer_mt *t)
{
  return t->sys + t->user;
}

void show_timer(timer_mt *t, char *describe)
{
  printf("%-25s =  %6.3f   (%6.3lf,%6.3lf)\n",
	 describe,gettotal_timer(t),
	 getuser_timer(t), getsys_timer(t));
}
