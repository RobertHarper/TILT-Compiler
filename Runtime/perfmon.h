#ifndef _PERFMON_H
#define _PERFMON_H


#ifndef _ASM
#ifdef solaris
#include <sys/ioccom.h>
#endif
/*
  #ifdef alpha_osf
  #include <sys/ioctl.h>
  #endif
*/

#define PERFMON_SETPCR			_IOW('P', 1, unsigned long long)
#define PERFMON_GETPCR			_IOR('P', 2, unsigned long long)
#define PERFMON_FLUSH_CACHE		_IO('P', 3)
#define PERFMON_FLUSH_ALL_CACHES	_IO('P', 4)
#endif /* ! _ASM */

#define PCR_PRIV_MODE	    	0x1

#define PCR_SYS_TRACE	    	(0x1 << 1)
#define PCR_USER_TRACE	    	(0x1 << 2)

#define PCR_S0_CYCLE_CNT    	(0x0 << 4)
#define PCR_S0_INSTR_CNT    	(0x1 << 4)
#define PCR_S0_STALL_IC_MISS 	(0x2 << 4)
#define PCR_S0_STALL_STORBUF 	(0x3 << 4)
#define PCR_S0_IC_REF	    	(0x8 << 4)
#define PCR_S0_DC_READ	    	(0x9 << 4)
#define PCR_S0_DC_WRITE	    	(0xa << 4)
#define PCR_S0_STALL_LOAD   	(0xb << 4)
#define PCR_S0_EC_REF	    	(0xc << 4)
#define PCR_S0_EC_WRITE_RO  	(0xd << 4)
#define PCR_S0_EC_SNOOP_INV 	(0xe << 4)
#define PCR_S0_EC_READ_HIT  	(0xf << 4)

#define PCR_S1_CYCLE_CNT    	(0x0 << 11)
#define PCR_S1_INSTR_CNT    	(0x1 << 11)
#define PCR_S1_STALL_MISPRED 	(0x2 << 11)
#define PCR_S1_STALL_FPDEP	(0x3 << 11)
#define PCR_S1_IC_HIT		(0x8 << 11)
#define PCR_S1_DC_READ_HIT	(0x9 << 11)
#define PCR_S1_DC_WRITE_HIT	(0xa << 11)
#define PCR_S1_LOAD_STALL_RAW	(0xb << 11)
#define PCR_S1_EC_HIT		(0xc << 11)
#define PCR_S1_EC_WRITEBACK	(0xd << 11)
#define PCR_S1_EC_SNOOP_COPYBCK	(0xe << 11)
#define PCR_S1_EC_IC_HIT	(0xf << 11)

#ifndef _ASM

#ifdef _KERNEL

extern void pm_set_rdtick_mode(int);
extern int pm_get_rdtick_mode(void);
extern void pm_set_pcr(unsigned long long);
extern unsigned long long pm_get_pcr(void);

#endif /* _KERNEL */

extern unsigned long long get_tick();
extern void cpu_sync(void);
extern void clr_pic(void);
extern unsigned long long get_pic(void);
extern unsigned long get_pic0(void);
extern unsigned long get_pic1(void);
extern unsigned long extract_pic0(unsigned long long);
extern unsigned long extract_pic1(unsigned long long);

#endif /* ! _ASM */

#endif /* ! _PERFMON_H */
