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

/*
	Rusagerep must agree with
	type rusagerep in ../../Basis/externtys.sml
*/

static ptr_t
Rusagerep(struct rusage* ru)
{
	val_t fields[4];
	fields[0] = (val_t) ru->ru_utime.tv_sec;
	fields[1] = (val_t) ru->ru_utime.tv_usec;
	fields[2] = (val_t) ru->ru_stime.tv_sec;
	fields[3] = (val_t) ru->ru_stime.tv_usec;
	return alloc_record(fields, arraysize(fields));
}

ptr_t
timer_getrusage_self(cerr er)
{
	struct rusage rusage;
	ptr_t r;
	if(getrusage(RUSAGE_SELF, &rusage) == -1)
		send_errno(er,errno);
	r = Rusagerep(&rusage);
	return r;
}

intpair
timer_ftime(unit ignored)
{
	struct timeb tp;
	ftime(&tp);
	return alloc_intint(tp.time, tp.millitm);
}

