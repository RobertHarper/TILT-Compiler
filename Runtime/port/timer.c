/* ../../Basis/useless-timer.sml */

#include "s.h"
#include "r.h"
#include <sys/resource.h>
#include <sys/timeb.h>

#ifdef __SunOS5_5_1__
	/* Missing prototypes. */
	int getrusage(int, struct rusage*);
	int ftime(struct timeb*);
#endif

/*int * int * int * int*/cresult
posix_time_getrusage_self(void)
{
	val_t fields[4];
	struct rusage rusage;
	ptr_t r;
	if (getrusage(RUSAGE_SELF, &rusage) == -1) {
		return Error(SysErr(errno));
	}
	fields[0] = (val_t) rusage.ru_utime.tv_sec;
	fields[1] = (val_t) rusage.ru_utime.tv_usec;
	fields[2] = (val_t) rusage.ru_stime.tv_sec;
	fields[3] = (val_t) rusage.ru_stime.tv_usec;
	r = alloc_record(fields,0,4);
	return NormalPtr(r);
}

intpair
posix_time_ftime(void)
{
	struct timeb tp;
	ftime(&tp);
	return alloc_intint(tp.time, tp.millitm);
}

