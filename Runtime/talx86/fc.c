/*
	../../Basis/Numeric/tiltfc.int
	../../Basis/Numeric/tiltfc.sml
*/

#include "s.h"
#include "r.h"

/*
	We use the macros _FPU_GETCW and _FPU_SETCW
	just to get the inline assembler syntax right.
*/
#include <fpu_control.h>

enum {
	/* x86 FPU control word */
	RMask	=	3<<10,	/* rounding control */
		RN	=	0<<10,
		RZ	=	3<<10,
		RPI	=	2<<10,
		RNI	=	1<<10,
	PMask	=	3<<8,	/* precision control */
		PS	=	0<<8,
		PD	=	2<<8,
		PE	=	3<<8,
};

static int
fnstcw(void)
{
	fpu_control_t mode;
	_FPU_GETCW(mode);	/* ie, FNSTCW */
	return mode;
}

static void
fldcw(int cw)
{
	fpu_control_t mode = cw;
	_FPU_SETCW(mode);	/* ie, FLDCW */
}

ptr_t
getfc(unit ignored)
{
	int c = fnstcw();
	int r, p;

	switch(c & RMask){
	case RN:	r = TO_NEAREST; break;
	case RZ:	r = TO_ZERO; break;
	case RPI:	r = TO_POSINF; break;
	default:	r = TO_NEGINF; break;
	}
	switch(c & PMask){
	case PS:	p = SINGLE; break;
	case PD:	p = DOUBLE; break;
	default:	p = EXTENDED; break;
	}
	return alloc_intint((val_t)r, (val_t)p);
}

/*unit*/cresult
setfc(int r, int p)
{
	int c = fnstcw() & (~(PMask|RMask));
	int rb, pb;
	switch(r){
	case TO_NEAREST:	rb = RN; break;
	case TO_ZERO:	rb = RZ; break;
	case TO_POSINF:	rb = RPI; break;
	case TO_NEGINF:	rb = RNI; break;
	default:
		return Error(SysErr_fmt("setfc: bad rounding mode: %d",r));
	}
	switch(p){
	case SINGLE:	pb = PS; break;
	case DOUBLE:	pb = PD; break;
	case EXTENDED:	pb = PE; break;
	default:
		return Error(SysErr_fmt("setfc: bad precision: %d",p));
	}
	fldcw(c | rb | pb);
	return NormalPtr(empty_record);
}
