/* ../../Basis/OS/time.sml */

#include "s.h"
#include "r.h"
#include <sys/time.h>

intpair
time_gettimeofday(cerr er)
{
	struct timeval tp;
	struct timezone tzp;
	ptr_t pair;
	if (gettimeofday(&tp, &tzp) == -1)
		send_errno(er, errno);
	pair = alloc_intint(tp.tv_sec,tp.tv_usec);
	return pair;
}
