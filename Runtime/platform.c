/* This file ripped off from /usr/local/lib/tools/memsys.anal.c running OSF_1 v2 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "thread.h"
#include "platform.h"
#include <strings.h>
#include <fcntl.h>
#include <unistd.h>
#include "perfmon.h"

#define WB_MAX            5
#define TB_MAX           32
#define LINE_MIN         32
#define PCACHE_MAX    16384
#define BCACHE_MAX  4194304
#define PAGE_SIZE      8192
#define PAGE_SHIFT       13

struct platformType {
  char  *model;        /* Model number                                   */
  int   itbSize;       /* User translation buffer entries                */
  int   dtbSize;       /* number of entries in the translation buffer    */
  float tbService;     /* Cycles to servie translation buffer miss       */
  int   wbSize;        /* number of entries in the on-chip write buff    */
  int   icacheSize;    /* Size(bytes) of the on chip instruction cache   */
  int   icacheShift;   /* log 2 of the instruction cache line size       */
  int   dcacheSize;    /* Size(bytes) of the on chip data cache          */
  int   dcacheShift;   /* log 2 of the data cache line size              */
  int   bcacheSize;    /* Size(bytes) of the board cache                 */
  int   bcacheShift;   /* log 2 of the board cache line size             */
  float cycleTime;     /* Processor cycle time in nanoseconds            */
  float iMissService;  /* Average service time of instruction cache miss */
  float dMissService;  /* Average service time of data cache miss        */
  float dWriteService; /* Average service time of data cache board write */
  float bMissService;  /* Average service time of board cache miss       */
  float bVictimService;/* Average service time of victim only            */
} platforms[] = {
/*
 *  first item acts as the default
 */            /* chip specific paramteters                   */  /* platform specific parameters */
"DEC3000_500", 8, 32, 50.0, 4, 8192, 5, 8192, 5, 524288, 5, 6.66,  66.6,  66.6,  99.9, 260.0, 190.0,
"DEC3000_300", 8, 32, 50.0, 4, 8192, 5, 8192, 5, 262144, 5, 6.66, 106.6, 106.6, 133.3, 450.0, 400.0,
"DEC3000_400", 8, 32, 50.0, 4, 8192, 5, 8192, 5, 524288, 5, 7.50,  75.0,  75.0, 112.5, 292.5, 207.5,
"DEC3000_600", 8, 32, 50.0, 4, 8192, 5, 8192, 5,2097152, 5, 5.71,  57.1,  57.1,  85.7, 260.0, 190.0,
"DEC4000",     8, 32, 50.0, 4, 8192, 5, 8192, 5,1048576, 5, 6.25,  50.0,  50.0,  75.0, 182.0,   0.0,
"DEC7000",     8, 32, 50.0, 4, 8192, 5, 8192, 5,4194304, 6, 5.50,  82.5,  82.5,  82.5, 400.0,   0.0,
"DEC10000",    8, 32, 50.0, 4, 8192, 5, 8192, 5,4194304, 6, 5.00,  75.0,  75.0,  75.0, 400.0,   0.0,
#if 0
"DEC2100_A50", 8, 32, 50.0, 4,16384, 5,16384, 5,2097152, 5, 6.66,  66.6,  66.6,  99.9, 260.0, 190.0, /* 1 */
#endif
           "", 0,  0,  0.0, 0,    0, 0,    0, 0,      0, 0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0
	   };

/* 1: DEC2100_A50 is the AlphaStations 250 4/266 which uses a 266MHz Alpha
   21064A chip.  The cache sizes (in bytes) are correct.  Everything else
   is copied from DEC3000_500 and ignored by TILT.
*/

typedef struct platformType PlatformType;


void GetPlatform(PlatformType *platform)
{
  FILE *filePlatform;
  char model[100];
  int i = -1;

  filePlatform = popen("/usr/sbin/sizer -c","r");
  if (filePlatform != NULL)
    {
      fscanf(filePlatform,"cpu \"%[^\"]s",model);
      for (i = 0; strcmp(model,platforms[i].model)!=0 &&
	     platforms[i].model[0] != '\0'; i++);
      if (platforms[i].model[0] == '\0')
	  i = -1;
    }
  if (i == -1)
    {
      i = 0;
      fprintf(stderr,"Defaulting to model %s\n",platforms[i].model);
    }
  *platform = platforms[i];
  fclose(filePlatform);
}

static PlatformType platform;

#ifdef alpha_osf
int GetBcacheSize(void) { return platform.bcacheSize; }
int GetIcacheSize(void) { return platform.icacheSize; }
int GetDcacheSize(void) { return platform.dcacheSize; }
#endif

#ifdef rs_aix
int GetBcacheSize(void) { return 256 * 1024; }
int GetIcacheSize(void) { return 8 * 1024; }
int GetDcacheSize(void) { return 8 * 1024; }
#endif

#ifdef solaris
int GetBcacheSize(void) { return 512 * 1024; }
int GetIcacheSize(void) { return 8 * 1024; }
int GetDcacheSize(void) { return 8 * 1024; }


enum PerfType {NoPerf = -1, UserBasic = 0, SysBasic, BothBasic, UserDCache, SysDCache, UserECache, SysECache, UserEWrite, UserESnoop};
int perfType = (int) NoPerf;
int hasPerfMon = 0;

int initializePerfMon(Proc_t *proc)
{
  int fd, rc;
  unsigned long long tmp;

  /* Establish Perf Mon */
  if (hasPerfMon == -1)
    return 0;
  fd = open("/dev/perfmon", O_RDONLY);
  if (fd == -1) {
    if (diag)
      fprintf(stderr,"Cannot open /dev/perfmon\n");
    hasPerfMon = -1;
    return 0;
  }
  hasPerfMon = 1;

  switch (perfType) {
    case NoPerf: return 0;
    case UserBasic:  tmp = PCR_S0_CYCLE_CNT | PCR_S1_INSTR_CNT | PCR_USER_TRACE; break;
    case SysBasic:   tmp = PCR_S0_CYCLE_CNT | PCR_S1_INSTR_CNT | PCR_SYS_TRACE; break;
    case BothBasic:  tmp = PCR_S0_CYCLE_CNT | PCR_S1_INSTR_CNT | PCR_USER_TRACE | PCR_SYS_TRACE; break;
    case UserDCache: tmp = PCR_S0_DC_READ | PCR_S0_DC_WRITE | PCR_S1_DC_READ_HIT | PCR_S1_DC_WRITE_HIT | PCR_USER_TRACE; break;
    case SysDCache:  tmp = PCR_S0_DC_READ | PCR_S0_DC_WRITE | PCR_S1_DC_READ_HIT | PCR_S1_DC_WRITE_HIT | PCR_SYS_TRACE; break;
    case UserECache: tmp = PCR_S0_EC_REF | PCR_S1_EC_HIT | PCR_USER_TRACE; break;
    case SysECache:  tmp = PCR_S0_EC_REF | PCR_S1_EC_HIT | PCR_SYS_TRACE; break;
    case UserEWrite: tmp = PCR_S0_EC_WRITE_RO | PCR_S1_EC_WRITEBACK | PCR_USER_TRACE; break;
    case UserESnoop: tmp = PCR_S0_EC_SNOOP_INV | PCR_S1_EC_SNOOP_COPYBCK | PCR_USER_TRACE; break;
  }
  rc = ioctl(fd, PERFMON_SETPCR, &tmp);
  if (rc < 0) {
    perror("ioctl(PERFMON_SETPCR)");
    exit(1);
  }
  ioctl(fd, PERFMON_FLUSH_CACHE); /* XXX */
  return 1;
}

void resetPerfMon(Proc_t *proc)
{
  unsigned register long long pic;
  int i;
  assert(hasPerfMon == 1);
  if (perfType == NoPerf)
    return;
  for (i=0; i<arraysize(proc->pic0); i++)
    proc->pic0[i] = proc->pic1[i] = proc->pic2[i] = proc->pic3[i] = 0;
  proc->picCursor = 0;
  clr_pic();
  cpu_sync();
  pic = get_pic();
  proc->last0 = extract_pic0(pic);
  proc->last1 = extract_pic1(pic);
}

void lapPerfMon(Proc_t *proc, int which)
{
  unsigned register long long pic;
  unsigned long cur0, cur1;
  assert(hasPerfMon == 1);
  if (perfType == NoPerf)
    return;
  pic = get_pic();
  cur0 = extract_pic0(pic);
  cur1 = extract_pic1(pic);
  (which ? proc->pic2[proc->picCursor] : proc->pic0[proc->picCursor]) += cur0 - proc->last0;
  (which ? proc->pic3[proc->picCursor] : proc->pic1[proc->picCursor]) += cur1 - proc->last1;
  if (which == 0)
    proc->picCursor++;
  proc->last0 = cur0;
  proc->last1 = cur1;
  assert(proc->picCursor < arraysize(proc->pic0));
}

void showPerfMon(Proc_t *proc, int which)
{
  int i;
  assert(hasPerfMon == 1);
  if (perfType == NoPerf)
    return;
  printf("\nProc %d    PerfMon: %s\n",proc->procid, which == 0 ? "PRIMARY" : (which == 1 ? "ALTERNATE" : "BOTH"));
  switch (perfType) {
    case UserBasic: 
      printf("    |    User  |    User  |  User\n");
      printf("Seg |   Cycle  |   Instr  |   CPI\n");
      break;
    case BothBasic: 
      printf("    |    Both  |    Both  |  Both\n");
      printf("Seg |   Cycle  |   Instr  |   CPI\n");
      break;
    case SysBasic:
      printf("    |     Sys  |     Sys  |   Sys   |     Sys  |     Sys\n");
      printf("Seg |   Cycle  |   Instr  |   CPI   |   Cycle  |   Instr\n");
      break;
    case UserDCache:
      printf("    |    User  |    User  |    User  | User %%\n");
      printf("Seg |    Dacc  |    Dhit  |  D miss  | D Rate\n");
      break;
    case SysDCache:
      printf("    |     Sys  |     Sys  |     Sys  | Sys %%\n");
      printf("Seg |    Dacc  |    Dhit  |  D miss  | D Rate\n");
      break;
    case UserECache:
      printf("    |    User  |    User  |    User  | User %%\n");
      printf("Seg |    Eacc  |    Ehit  |  E miss  | E Rate\n");
      break;
    case SysECache:
      printf("    |     Sys  |     Sys  |     Sys  | Sys %%\n");
      printf("Seg |    Eacc  |    Ehit  |  E miss  | E Rate\n");
      break;
    case UserEWrite:
      printf("    |    User  |    User\n");
      printf("Seg | Ehit-RO  | EWrBack\n");
      break;
    case UserESnoop:
      printf("    |    User  |      User\n");
      printf("Seg | ESnpInv  | ESnpCpyBk\n");
      break;
    }
  printf("===========================================\n");
  for (i=0; i<proc->picCursor+1; i++) {
    unsigned long count0 = (which == 0) ? proc->pic0[i] : proc->pic2[i];
    unsigned long count1 = (which == 0) ? proc->pic1[i] : proc->pic3[i];
    if (i < proc->picCursor) {
      proc->pic0[proc->picCursor] += proc->pic0[i];
      proc->pic1[proc->picCursor] += proc->pic1[i];
      proc->pic2[proc->picCursor] += proc->pic2[i];
      proc->pic3[proc->picCursor] += proc->pic3[i];
      printf("%3d | ", i);
    }
    else
      printf("All | ");
    if (which == 2)
      assert(perfType == SysBasic);
    switch (perfType) {
      case UserBasic: 
      case BothBasic: 
	printf("%7d  | %7d  |   %4.2f\n", count0, count1, ((double) count0) / count1);
	break;
      case SysBasic:
	if (which == 2)
	  printf("%7d  | %7d  |   %4.2f  | %7d  | %7d  |  %4.2f\n", 
		 proc->pic0[i], proc->pic1[i], ((double) proc->pic0[i]) / proc->pic1[i],
		 proc->pic2[i], proc->pic3[i], ((double) proc->pic2[i]) / proc->pic3[i]);
	else 
	  printf("%7d  | %7d  |   %4.2f\n", count0, count1, ((double) count0) / count1);
	break;
      case UserDCache:
      case SysDCache:
	printf("%7d  | %7d  | %7d  |   %4.2f\n", count0, count1, count0 - count1, 100.0 * count1 / count0);
	break;
      case UserECache:
      case SysECache:
	printf("%7d  | %7d  | %7d  |   %4.2f\n", count0, count1, count0 - count1, 100.0 * count1 / count0);
	break;
      case UserEWrite:
	printf("%7d  | %7d\n", count0, count1);
	break;
      case UserESnoop:
	printf("%7d  | %7d\n", count0, count1);
	break;
    }
  }
}

/*
void testPerfMon(void)
{
  resetPerfMon();
  lapPerfMon();
  showPerfMon(1);
  resetPerfMon();
}
*/

#else
int perfType;
int initializePerfMon(Proc_t * proc) {return 1;}
void resetPerfMon(Proc_t * proc) {}
void startAlternatePerfMon(Proc_t * proc) {}
void stopAlternatePerfMon(Proc_t * proc) {}
void lapPerfMon(Proc_t * proc, int which) {}
void showPerfMon(Proc_t *proc, int which) {}
#endif



void platform_init(void)
{
#ifdef alpha_osf
  GetPlatform(&platform); 
#endif
}


