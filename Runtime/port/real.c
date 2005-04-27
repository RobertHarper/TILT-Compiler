/* ../../Basis/Numeric/real64.sml */

#include "s.h"
#include "r.h"
#include <math.h>	/* for ldexp */

/* extract the exponent */
int
real_logb(double arg)
{
	int biasedExp = 0;
	int temp[2];
	assert((2 * (sizeof(int))) == sizeof(double));
	*((double *)temp) = arg;
	biasedExp = (int)(temp[1]);
	biasedExp -= 1023;
	return biasedExp;
}

#if 0
/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */
ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	PUSHL	REGOFF(4,stdarg)		/* Get copy of scalar. */
	SARL	(IMMED(1),REGOFF(0,REG(esp)))	/* Untag it. */
	FILDL	REGOFF(0,REG(esp))			/* Load it ... */
/*	fstp	FP_REG(st)(1) */		/* ... into 1st FP reg. */
/*	ADDL	(IMMED(4), REG(esp)) */		/* Discard copy of scalar. */

	MOVL	(REGOFF(0,stdarg), temp)	/* Get pointer to real. */
	fld	REAL8 PTR 0 [temp]		/* Load it into temp. */

	fscale				/* Multiply exponent by scalar. */
	MOVL	(IMMED(DESC_reald), REGOFF(0,allocptr))
	fstp	REAL8 PTR 4 [allocptr]	/* Store resulting float. */
	ADDL	(IMMED(4),allocptr)	/* Allocate word for tag. */
	MOVL	(allocptr, stdarg)	/* Return a pointer to the float. */
	ADDL	(IMMED(8), allocptr)	/* Allocate room for float. */
	fstp	REAL8 PTR 0 [esp]	/* ?? */
	ADDL	(IMMED(4),REG(esp))	/* discard copy of scalar */
	CONTINUE

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */

ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	PUSH_L(REGOFF(4,stdarg))		/* Get copy of scalar. */
	SAR_L(CONST(1), REGIND(ESP))	/* Untag it. */
	FILD_L(REGIND(ESP))			/* Load it ... */
/*	fstp	%st(1) */		/* ... into 1st FP reg. */
	MOV_L(REGIND(stdarg), temp)		/* Get pointer to real. */
	FLD_D(REGIND(temp))			/* Load it into temp. */

	FSCALE				/* Multiply exponent by scalar. */
	MOV_L(CONST(DESC_reald), REGIND(allocptr))
	FSTP_D(REGOFF(4,allocptr))		/* Store resulting float. */
	ADD_L(CONST(4), allocptr)	/* Allocate word for tag. */
	MOV_L(allocptr, stdarg)		/* Return a pointer to the float. */
	ADD_L(CONST(8), allocptr)	/* Allocate room for float. */
	FSTP_D(REGIND(ESP))			
	ADD_L(CONST(4), ESP)		/* Discard copy of scalar. */
	CONTINUE

END

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 */
ML_CODE_HDR(scalb_a)
	CHECKLIMIT(scalb_a_limit)
	ld	[STDARG+4],TMPREG1   /* tmpreg1 gets scale (second arg) */
	sra	TMPREG1,1,TMPREG1	    /* cvt scale to sparc int */
	ld	[STDARG],STDARG   /* stdarg gets real (first arg) */
	ld	[STDARG],TMPREG4	    /* tmpreg4 gets high word of real value. */
	set	0x7ff00000,TMPREG2    /* tmpreg2 gets exponent mask. */
	andcc	TMPREG4,TMPREG2,TMPREG3   /* extract exponent into tmpreg3. */
	be	1f		    /* if 0 then return same */
	nop
	srl	TMPREG3,20,TMPREG3	    /* cvt exp to int (delay slot). */
	addcc	TMPREG3,TMPREG1,TMPREG1	    /* tmpreg1 = exp + scale */
	ble	under		    /* if new exp <= 0 then underflow */
	nop
	cmp	TMPREG1,2047	    /* if new exp >= 2047 then overflow */
	bge	over
	nop
	andn	TMPREG4,TMPREG2,TMPREG4   /* mask out old exponent. */
	sll	TMPREG1,20,TMPREG1	    /* shift new exp to exponent position. */
	or	TMPREG4,TMPREG1,TMPREG4   /* set new exponent. */
	ld	[STDARG+4],TMPREG1   /* tmpreg1 gets low word of real value. */
7:
#ifdef ALIGN_REALDS
	or	ALLOCPTR,0x4,ALLOCPTR	    /* desc is unaliged */
#endif
	st	TMPREG4,[ALLOCPTR+4] /* allocate the new real value */
	st	TMPREG1,[ALLOCPTR+8]
	set	DESC_reald,TMPREG1
	st	TMPREG1,[ALLOCPTR]
	add	ALLOCPTR,4,STDARG /* set result. */
	inc	12,ALLOCPTR	    /* allocptr += 3 */
1:	CONTINUE

over:				/* handle overflow */
	t	ST_INT_OVERFLOW	    /* generate an Overflow exn.  We do this */
	/* never get here */	    /* via a trap to produce a SIGOVFL */

under:				/* handle underflow */
	set	0,TMPREG4
	set	0,TMPREG1
	ba	7b
	nop

/*
** scalb : real * int -> real
**	scalb(x,y) = x * 2^y
*/
ML_CODE_HDR(scalb_a)
	CHECKLIMIT(scalb_v_limit)
	lwz	atmp1,4(stdarg)		/* atmp1 := y */
	srawi	atmp1,atmp1,1		/* atmp1 := machine int y */
	lwz	stdarg,0(stdarg)	/* stdarg := x */
	lwz	atmp2,0(stdarg)		/* atmp2 := MSW(x) */
	lis	r0,0x7ff0		/* r0 := 0x7ff0,0000 */
	and.	atmp3,atmp2,r0		/* atmp3 := atmp2 & 0x7ff00000 */
	bt	CR0_EQ,scalb_all_done
	
	srawi	atmp3,atmp3,20		/* atmp3 := ieee(exp) */
	add.	atmp1,atmp1,atmp3	/* scale exponent */
	bt	CR0_LT,scalb_underflow

	cmpi	CR0,atmp1,2047		/* max. ieee(exp) */
	bf	CR0_LT,scalb_overflow

	not	r0,r0			/* r0 := not(r0) */
	and	atmp2,atmp2,r0		/* atmp2 := high mantessa bits + sign */
	slwi	atmp1,atmp1,20		/* atmp1 := new exponent */
	or	atmp1,atmp1,atmp2	/* atmp1 := new MSB(x) */
	lwz	atmp2, 4(stdarg)	

scalb_write_out:
	stw	atmp1, 4(allocptr)
	stw	atmp2, 8(allocptr)
	li	atmp3, DESC_reald
	stw	atmp3, 0(allocptr)
	addi	stdarg,allocptr,4
	addi	allocptr,allocptr,12

scalb_all_done:
	CONTINUE

scalb_underflow:
	li	atmp1,0
	li	atmp2,0
	b	scalb_write_out

LABEL(scalb_overflow)
	mtfsb1 	3

ML_CODE_HDR(scalb_a)
	CHECKLIMIT(FUN_MASK)
	lw 	atmp1,4(stdarg)		/* get tagged n */
	sra	atmp1,1			/* get real n */
	beqz	atmp1,9f		/* if zero, return the old float */
	lw	ptrtmp,0(stdarg)	/* get pointer to float */
	lw 	atmp2,BIGPART(ptrtmp)	/* most significant part */
	srl 	atmp2,20		/* throw out 20 low bits */
	andi	atmp2,0x07ff		/* clear all but 11 low bits */
	add	atmp3,atmp2,atmp1	/* new := old + n */
	blt	atmp3,1,under		/* punt if underflow */
	bgt	atmp3,2046,over		/* or overflow */
	xor	atmp3,atmp2		/* at3 = new xor old */
	sll	atmp3,20		/* put exponent in right position */
	lw	atmp2,BIGPART(ptrtmp)	/* most significant word */
	xor	atmp2,atmp3		/* change to new exponent */
	sw	atmp2,BIGPART+4(allocptr)	/* save */
	lw 	atmp2,LITTLEPART(ptrtmp) /* get least significant word */
	sw	atmp2,LITTLEPART+4(allocptr)	/* save lsw */
8:	li	atmp4,DESC_reald        /* make descriptor */
	sw	atmp4,0(allocptr)	/* save descriptor */
	add	stdarg,allocptr,4	/* get pointer to new float */
	add	allocptr,12		/* point to new free word */
        CONTINUE

9:	lw	stdarg,0(stdarg)	/* get old float */
	CONTINUE

over:	li	atmp3,0x7fffffff
	add	atmp3,atmp3		/* generate overflow exception */

under:	sw	zero,4(allocptr)		/* return 0.0 */
	sw	zero,8(allocptr)
	b	8b

ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	ldl	PTRTMP,0(STDARG)	/* address of float */
	ldq	ATMP2,0(PTRTMP)	/* get float */
	ldl	ATMP1,4(STDARG)	/* get tagged n */
	sra	ATMP1,1,ATMP1		/* real n */
	beq	ATMP1,9f		/* branch if n=0 */
	sra	ATMP2,52,ATMP3	/* shift out fraction of float */
	and	ATMP3,0xfff,ATMP3	/* just exponent of float */
	addq	ATMP3,ATMP1,ATMP3	/* n + exponent */
	ble	ATMP3,6f		/* branch if underflow */
	sll	ATMP1,52,ATMP1	/* n in exponent position */
	addqv	ATMP2,ATMP1,ATMP1	/* add n to exponent, with overflow */
3:	/* return float in atmp1 */
	or	ALLOCPTR,4,ALLOCPTR	/* unalign allocptr to align float */
	mov	DESC_reald,ATMP2	/* get desc */
	stl	ATMP2,0(ALLOCPTR)	/* store desc */
	stq	ATMP1,4(ALLOCPTR)	/* store float */
	addq	ALLOCPTR,4,STDARG	/* return boxed float */
	addq	ALLOCPTR,12,ALLOCPTR	/* set allocptr */
	CONTINUE
6:	/* underflow -- return zero */
	mov	0,ATMP1
	br	3b
9:	/* n=0  --  return original float */
	mov	PTRTMP,STDARG
	CONTINUE

#endif

/* x * 2^y. */
double
real_scalb(double x, int y)
{
	/*
		Just an approximation.
	*/
	return ldexp(x,y);
}
