#define ZERO	0
#define EXNPTR	1
#define THREADPTR	2
#define ALLOCPTR	4
#define ALLOCLIMIT	5
#define CFIRSTARG	8
#define RESULT	8
#define CSECONDARG	9
#define CTHIRDARG	10
#define CFOURTHARG	11
#define CFIFTHARG	12
#define CSIXTHARG	13
#define SP	14
#define RA	15 
#define EXNARG	15
#define ASMTMP	16
#define ASMTMP2	17

#define EXNPTR_REG	%r1
#define THREADPTR_REG	%r2
#define ALLOCPTR_REG	%r4
#define ALLOCLIMIT_REG	%r5
#define CFIRSTARG_REG	%r8
#define RESULT_REG	%r8
#define CSECONDARG_REG	%r9
#define CTHIRDARG_REG	%r10
#define SP_REG	%r14
#define RA_REG	%r15
#define EXNARG_REG	%r15
#define ASMTMP_REG	%r16
#define ASMTMP2_REG	%r17

/* Must agree with Thread_t. */
#define ALLOCPTR_DISP	(4 * ALLOCPTR)
#define ALLOCLIMIT_DISP	(4 * ALLOCLIMIT)
#define SP_DISP	(4 * SP)
#define RA_DISP	(4 * RA)
#define EXNARG_DISP	(4 * EXNARG)
#define ASMTMP_DISP	(4 * ASMTMP)
#define ASMTMP2_DISP	(4 * ASMTMP2)
