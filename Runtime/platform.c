/* This file ripped off from /usr/local/lib/tools/memsys.anal.c running OSF_1 v2 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "platform.h"


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
GetBcacheSize() { return platform.bcacheSize; }
GetIcacheSize() { return platform.icacheSize; }
GetDcacheSize() { return platform.dcacheSize; }
#endif

#ifdef rs_aix
GetBcacheSize() { return 256 * 1024; }
GetIcacheSize() { return 8 * 1024; }
GetDcacheSize() { return 8 * 1024; }
#endif

void platform_init()
{
#ifdef alpha_osf
  GetPlatform(&platform); 
#endif
}
