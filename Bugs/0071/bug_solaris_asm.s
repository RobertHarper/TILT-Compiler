#include <sys/trap.h>

! unit my_openf(word ignored)
	
	.section ".text"
	.globl	my_openf
	
	.proc	07
	.align  4
my_openf:
	! flushw -- avoids error
	! ta	ST_FLUSH_WINDOWS -- avoids error
	save	%sp, -120, %sp				! -112 abort, -120 illegal
	sethi	%hi(filename), %o0
	or	%o0, %lo(filename), %o0			! first argument  "/dev/null"
	clr	%o1					! second argument 0
	clr	%o2					! third argument  0
	call	open
	nop						! delay slot empty
	call	abort
	nop						! delay slot empty
	.size my_openf,(.-my_openf)

	.data
filename:
	.ascii "/dev/null\000"
	