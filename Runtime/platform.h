/* These are not thread-safe */

/* This file ripped off from /usr/local/lib/tools/platform.h running OSF_1 v2 */
#ifndef _platform_h
#define _platform_h

void platform_init(void);
int GetBcacheSize(void); /* secondary cache size */
int GetIcacheSize(void); /* primary icache size */
int GetDcacheSize(void); /* primary dcache size */
int GetPhysicalPages(void); /* main memory size */ 

/* Only defined for Sparc - otherwise, no-op */
extern int perfType;
int initializePerfMon(Proc_t *);   /* Must be called by each processor - returns 0 if perfmon not installed */
void resetPerfMon(Proc_t *);
void startAlternatePerfMon(Proc_t *); 
void stopAlternatePerfMon(Proc_t *); 
void lapPerfMon(Proc_t *, int);    /* 0 for primary, 1 for alternate */
void showPerfMon(Proc_t *, int);   /* Show accumulated statistics - 0, 1, or 2 */

#endif 

