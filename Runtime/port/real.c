/* ../../Basis/Numeric/real64.sml */

#include "s.h"
#include "r.h"

/* extract the exponent */
int
real_logb(double arg)
{
	int biasedExp = 0;
	int temp[2];
	assert((2 * (sizeof(unsigned int))) == sizeof(double));
	*((double *)temp) = arg;
	biasedExp = (int)(temp[1]);
	biasedExp -= 1023;
	return biasedExp;
}

/*
	XXX: double real_scalb(double m, int e);
*/
