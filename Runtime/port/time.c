/* ../../Basis/OS/time.sml */

#include "s.h"
#include "r.h"
#include <sys/time.h>

/*int * int*/cresult
posix_time_gettimeofday(void)
{
	struct timeval tp;
	struct timezone tzp;
	if (gettimeofday(&tp, &tzp) == -1)
		return Error(SysErr(errno));
	else {
		ptr_t pair = alloc_intint(tp.tv_sec,tp.tv_usec);
		return NormalPtr(pair);
	}
}
