/* ../../Basis/Numeric/math64c.sml */

#include "s.h"
#include "r.h"
#include <math.h>

/* XXX: Why doesn't ML call the libc log function directly? */
double
ln(double arg)
{
	return log(arg);
}
