/*
	../../Basis/Numeric/tiltfc.int
	../../Basis/Numeric/tiltfc.sml
*/

#include "s.h"
#include "r.h"
#include <ieeefp.h>

ptr_t
getfc(unit ignored)
{
	fp_rnd mode = fpgetround();
	int r;
	switch(mode){
	case FP_RZ:	r = TO_ZERO; break;
	case FP_RN:	r = TO_NEAREST; break;
	case FP_RP:	r = TO_POSINF; break;
	case FP_RM:	r = TO_NEGINF; break;
	default:	DIE("bogus value from fpgetround");
	}
	return alloc_intint((val_t)r, (val_t)DOUBLE);
}

/*unit*/cresult
setfc(int r, int p)
{
	fp_rnd mode;
	switch(r){
	case TO_NEAREST:	mode = FP_RN; break;
	case TO_ZERO:	mode = FP_RZ; break;
	case TO_POSINF:	mode = FP_RP; break;
	case TO_NEGINF:	mode = FP_RM; break;
	default:
		return Error(SysErr_fmt("setfc: bad rounding mode: %d",r));
	}
	if(p != DOUBLE)
		return Error(SysErr_fmt("setfc: bad precision: %d",p));
	fpsetround(mode);
	return NormalPtr(empty_record);
}
