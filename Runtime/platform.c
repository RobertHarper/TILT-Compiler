/* This file ripped off from /usr/local/lib/tools/memsys.anal.c running OSF_1 v2 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
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
           "", 0,  0,  0.0, 0,    0, 0,    0, 0,      0, 0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0
	   };

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
int GetBcacheSize() { return platform.bcacheSize; }
int GetIcacheSize() { return platform.icacheSize; }
int GetDcacheSize() { return platform.dcacheSize; }
#endif

#ifdef rs_aix
int GetBcacheSize() { return 256 * 1024; }
int GetIcacheSize() { return 8 * 1024; }
int GetDcacheSize() { return 8 * 1024; }
#endif

#ifdef solaris
int GetBcacheSize() { return 512 * 1024; }
int GetIcacheSize() { return 8 * 1024; }
int GetDcacheSize() { return 8 * 1024; }

static unsigned long last0, last1;
long pic0[1024], pic1[1024], picCursor = 0;
long pic2[1024], pic3[1024];  /* Splitting into two areas */

enum PerfType {UserBasic = 0, SysBasic, BothBasic, UserDCache, SysDCache, UserECache, SysECache, UserEWrite, UserESnoop};
int perfType = (int) UserBasic;

void initializePerfMon()
{
  int fd, rc;
  unsigned long long tmp;

  /* XXX Need to bind processor? */
  fd = open("/dev/perfmon", O_RDONLY);
  if (fd == -1) {
    perror("open(/dev/perfmon)");
    exit(1);
  }


  switch (perfType) {
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
  ioctl(fd, PERFMON_FLUSH_CACHE);
}



void resetPerfMon()
{
  unsigned register long long pic;
  int i;
  for (i=0; i<(sizeof(pic0) / sizeof(long)); i++)
    pic0[i] = pic1[i] = pic2[i] = pic3[i] = 0;
  picCursor = 0;
  clr_pic();
  cpu_sync();
  pic = get_pic();
  last0 = extract_pic0(pic);
  last1 = extract_pic1(pic);
}

void update(int primary)
{
  unsigned register long long pic;
  unsigned long cur0, cur1;
  pic = get_pic();
  cur0 = extract_pic0(pic);
  cur1 = extract_pic1(pic);
  (primary ? pic0[picCursor] : pic2[picCursor]) += cur0 - last0;
  (primary ? pic1[picCursor] : pic3[picCursor]) += cur1 - last1;
  last0 = cur0;
  last1 = cur1;
  assert(picCursor < (sizeof(pic0) / sizeof(long)));
}

void startAlternatePerfMon()
{
  update(1);
}

void stopAlternatePerfMon()
{
  update(0);
}

void lapPerfMon()
{
  update(1);
  picCursor++;
  assert(picCursor < (sizeof(pic0) / sizeof(long)));
}

void showPerfMon(int which)
{
  int i;
  printf("\nPerfMon: %s\n", which == 0 ? "PRIMARY" : (which == 1 ? "ALTERNATE" : "BOTH"));
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
  for (i=0; i<picCursor; i++) {
    unsigned long count0 = (which == 0) ? pic0[i] : pic2[i];
    unsigned long count1 = (which == 0) ? pic1[i] : pic3[i];
    if (which == 2)
      assert(perfType == SysBasic);
    switch (perfType) {
      case UserBasic: 
      case BothBasic: 
	printf("%3d | %7d  | %7d  |   %4.1f\n", i, count0, count1, ((double) count0) / count1);
	break;
      case SysBasic:
	if (which == 2)
	  printf("%3d | %7d  | %7d  |   %4.1f  | %7d  | %7d  |  %4.1f\n", i, 
		 pic0[i], pic1[i], ((double) pic0[i]) / pic1[i],
		 pic2[i], pic3[i], ((double) pic2[i]) / pic3[i]);
	else 
	  printf("%3d | %7d  | %7d  |   %4.1f\n", i, count0, count1, ((double) count0) / count1);
	break;
      case UserDCache:
      case SysDCache:
	printf("%3d | %7d  | %7d  | %7d  |   %4.1f\n", i, count0, count1, count0 - count1, 100.0 * count1 / count0);
	break;
      case UserECache:
      case SysECache:
	printf("%3d | %7d  | %7d  | %7d  |   %4.1f\n", i, count0, count1, count0 - count1, 100.0 * count1 / count0);
	break;
      case UserEWrite:
	printf("%3d | %7d  | %7d\n", i, count0, count1);
	break;
      case UserESnoop:
	printf("%3d | %7d  |   %7d\n", i, count0, count1);
	break;
    }
  }
}

void testPerfMon()
{
  resetPerfMon();
  lapPerfMon();
  showPerfMon(1);
  resetPerfMon();
}

#endif



void platform_init()
{
#ifdef alpha_osf
  GetPlatform(&platform); 
#endif
}


