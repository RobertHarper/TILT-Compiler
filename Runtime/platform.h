/* These are not thread-safe */

/* This file ripped off from /usr/local/lib/tools/platform.h running OSF_1 v2 */
#ifndef _platform_h
#define _platform_h

void platform_init(void);
int GetBcacheSize(void); /* secondary cache size */
int GetIcacheSize(void); /* primary icache size */
int GetDcacheSize(void); /* primary dcache size */

#ifdef solaris
extern int perfType;
void initializePerfMon();   /* Must be called per processor */
void resetPerfMon();
void startAlternatePerfMon(); 
void stopAlternatePerfMon(); 
void lapPerfMon();
void showPerfMon(int);   /* Show accumulated statistics */
void testPerfMon();
#endif

#endif 

