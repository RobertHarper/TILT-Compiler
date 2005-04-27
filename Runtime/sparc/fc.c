/* ../../Basis/Numeric/tiltfc.sml */

#include "s.h"
#include "r.h"
#include <ieeefp.h>

static char Eround[] = "getfc: bogus value from fpgetround";
static char Emode[] = "setfc: bad rounding mode";
static char Eprecision[] = "setfc: bad precision";

ptr_t
getfc(cerr er)
{
	fp_rnd mode = fpgetround();
	int r;
	switch(mode){
	case FP_RZ:	r = TO_ZERO; break;
	case FP_RN:	r = TO_NEAREST; break;
	case FP_RP:	r = TO_POSINF; break;
	case FP_RM:	r = TO_NEGINF; break;
	default:	send_errmsg(er,Eround);
	}
	return alloc_intint((val_t)r, (val_t)DOUBLE);
}

unit
setfc(cerr er, int r, int p)
{
	fp_rnd mode;
	switch(r){
	case TO_NEAREST:	mode = FP_RN; break;
	case TO_ZERO:	mode = FP_RZ; break;
	case TO_POSINF:	mode = FP_RP; break;
	case TO_NEGINF:	mode = FP_RM; break;
	default:
		send_errmsg(er,Emode);
		return empty_record;
	}
	if(p != DOUBLE){
		send_errmsg(er,Eprecision);
		return empty_record;
	}
	fpsetround(mode);
	return empty_record;
}
