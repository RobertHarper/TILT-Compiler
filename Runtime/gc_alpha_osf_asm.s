 # Assumes that the thread pointer points to a structure whose first 64 quads are
 # the general and floating point register set and that the thread pointer register
 # is unmodified by call to gc_raw	 
		
#define _asm_
#include "general.h"
#include "thread.h"
			
		
	.text	
	.align	4
	.globl	gc_raw
	.globl	float_alloc_raw
	.globl	int_alloc_raw
	.globl	ptr_alloc_raw
	.globl	context_restore
	.globl	old_alloc
	.globl	cur_alloc_ptr
	.globl	save_regs
	.globl	load_regs
	
 # ----------------- save_regs---------------------------------
 # saves entire register set except for the return address register
 # does not use a stack frame or change any registers
 # ----------------------------------------------------------
	.ent	save_regs
	.frame $sp, 0, $26
save_regs:	
.set noat
	stq	$0, (THREADPTR_SYMREG)
	stq	$1, 8(THREADPTR_SYMREG)
	stq	$2, 16(THREADPTR_SYMREG)
	stq	$3, 24(THREADPTR_SYMREG)
	stq	$4, 32(THREADPTR_SYMREG)
	stq	$5, 40(THREADPTR_SYMREG)
	stq	$6, 48(THREADPTR_SYMREG)
	stq	$7, 56(THREADPTR_SYMREG)
	stq	$8, 64(THREADPTR_SYMREG)
	stq	$9, 72(THREADPTR_SYMREG)
	stq	$10, 80(THREADPTR_SYMREG)
	stq	$11, 88(THREADPTR_SYMREG)
	stq	$12, 96(THREADPTR_SYMREG)
	stq	$13, 104(THREADPTR_SYMREG)  
	stq	$14, 112(THREADPTR_SYMREG)
	stq	$15, 120(THREADPTR_SYMREG)
	stq	$16, 128(THREADPTR_SYMREG)
	stq	$17, 136(THREADPTR_SYMREG)
	stq	$18, 144(THREADPTR_SYMREG)
	stq	$19, 152(THREADPTR_SYMREG)
	stq	$20, 160(THREADPTR_SYMREG)
	stq	$21, 168(THREADPTR_SYMREG)
	stq	$22, 176(THREADPTR_SYMREG)
	stq	$23, 184(THREADPTR_SYMREG)
	stq	$24, 192(THREADPTR_SYMREG)
	stq	$25, 200(THREADPTR_SYMREG)
	stq	$27, 216(THREADPTR_SYMREG)
	stq	$28, 224(THREADPTR_SYMREG)
	stq	$29, 232(THREADPTR_SYMREG)
	stq	$30, 240(THREADPTR_SYMREG)
	stq	$31, 248(THREADPTR_SYMREG)

	stt	$f0, 256(THREADPTR_SYMREG)   
	stt	$f1, 264(THREADPTR_SYMREG)
	stt	$f2, 272(THREADPTR_SYMREG)
	stt	$f3, 280(THREADPTR_SYMREG)
	stt	$f4, 288(THREADPTR_SYMREG)
	stt	$f5, 296(THREADPTR_SYMREG)
	stt	$f6, 304(THREADPTR_SYMREG)
	stt	$f7, 312(THREADPTR_SYMREG)
	stt	$f8, 320(THREADPTR_SYMREG)   
	stt	$f9, 328(THREADPTR_SYMREG)
	stt	$f10, 336(THREADPTR_SYMREG)
	stt	$f11, 344(THREADPTR_SYMREG)
	stt	$f12, 352(THREADPTR_SYMREG)
	stt	$f13, 360(THREADPTR_SYMREG)
	stt	$f14, 368(THREADPTR_SYMREG)
	stt	$f15, 376(THREADPTR_SYMREG)
	stt	$f16, 384(THREADPTR_SYMREG)
	stt	$f17, 392(THREADPTR_SYMREG)
	stt	$f18, 400(THREADPTR_SYMREG)
	stt	$f19, 408(THREADPTR_SYMREG)
	stt	$f20, 416(THREADPTR_SYMREG)
	stt	$f21, 424(THREADPTR_SYMREG)
	stt	$f22, 432(THREADPTR_SYMREG)
	stt	$f23, 440(THREADPTR_SYMREG)
	stt	$f24, 448(THREADPTR_SYMREG)
	stt	$f25, 456(THREADPTR_SYMREG)
	stt	$f26, 464(THREADPTR_SYMREG)
	stt	$f27, 472(THREADPTR_SYMREG)
	stt	$f28, 480(THREADPTR_SYMREG)
	stt	$f29, 488(THREADPTR_SYMREG)
	stt	$f30, 496(THREADPTR_SYMREG)
	stt	$f31, 504(THREADPTR_SYMREG)

	ret	$31, ($26), 1	
.set at
	.end	save_regs	

 # ----------------- load_regs---------------------------------
 # loads entire register set except for the return address register
 # does not use a stack frame or change any registers
 # ----------------------------------------------------------
	.ent	load_regs
	.frame $sp, 0, $26
load_regs:	
.set noat
	ldq	$0, (THREADPTR_SYMREG)
	ldq	$1, 8(THREADPTR_SYMREG)
	ldq	$2, 16(THREADPTR_SYMREG)
	ldq	$3, 24(THREADPTR_SYMREG)
	ldq	$4, 32(THREADPTR_SYMREG)
	ldq	$5, 40(THREADPTR_SYMREG)
	ldq	$6, 48(THREADPTR_SYMREG)
	ldq	$7, 56(THREADPTR_SYMREG)
	ldq	$8, 64(THREADPTR_SYMREG)
	ldq	$9, 72(THREADPTR_SYMREG)
	ldq	$10, 80(THREADPTR_SYMREG)
	ldq	$11, 88(THREADPTR_SYMREG)
	ldq	$12, 96(THREADPTR_SYMREG)
	ldq	$13, 104(THREADPTR_SYMREG)  
	ldq	$14, 112(THREADPTR_SYMREG)
	ldq	$15, 120(THREADPTR_SYMREG)
	ldq	$16, 128(THREADPTR_SYMREG)
	ldq	$17, 136(THREADPTR_SYMREG)
	ldq	$18, 144(THREADPTR_SYMREG)
	ldq	$19, 152(THREADPTR_SYMREG)
	ldq	$20, 160(THREADPTR_SYMREG)
	ldq	$21, 168(THREADPTR_SYMREG)
	ldq	$22, 176(THREADPTR_SYMREG)
	ldq	$23, 184(THREADPTR_SYMREG)
	ldq	$24, 192(THREADPTR_SYMREG)
	ldq	$25, 200(THREADPTR_SYMREG)
	ldq	$27, 216(THREADPTR_SYMREG)
	ldq	$28, 224(THREADPTR_SYMREG)
	ldq	$29, 232(THREADPTR_SYMREG)
	ldq	$30, 240(THREADPTR_SYMREG)
	ldq	$31, 248(THREADPTR_SYMREG)

	ldt	$f0, 256(THREADPTR_SYMREG)   
	ldt	$f1, 264(THREADPTR_SYMREG)
	ldt	$f2, 272(THREADPTR_SYMREG)
	ldt	$f3, 280(THREADPTR_SYMREG)
	ldt	$f4, 288(THREADPTR_SYMREG)
	ldt	$f5, 296(THREADPTR_SYMREG)
	ldt	$f6, 304(THREADPTR_SYMREG)
	ldt	$f7, 312(THREADPTR_SYMREG)
	ldt	$f8, 320(THREADPTR_SYMREG)   
	ldt	$f9, 328(THREADPTR_SYMREG)
	ldt	$f10, 336(THREADPTR_SYMREG)
	ldt	$f11, 344(THREADPTR_SYMREG)
	ldt	$f12, 352(THREADPTR_SYMREG)
	ldt	$f13, 360(THREADPTR_SYMREG)
	ldt	$f14, 368(THREADPTR_SYMREG)
	ldt	$f15, 376(THREADPTR_SYMREG)
	ldt	$f16, 384(THREADPTR_SYMREG)
	ldt	$f17, 392(THREADPTR_SYMREG)
	ldt	$f18, 400(THREADPTR_SYMREG)
	ldt	$f19, 408(THREADPTR_SYMREG)
	ldt	$f20, 416(THREADPTR_SYMREG)
	ldt	$f21, 424(THREADPTR_SYMREG)
	ldt	$f22, 432(THREADPTR_SYMREG)
	ldt	$f23, 440(THREADPTR_SYMREG)
	ldt	$f24, 448(THREADPTR_SYMREG)
	ldt	$f25, 456(THREADPTR_SYMREG)
	ldt	$f26, 464(THREADPTR_SYMREG)
	ldt	$f27, 472(THREADPTR_SYMREG)
	ldt	$f28, 480(THREADPTR_SYMREG)
	ldt	$f29, 488(THREADPTR_SYMREG)
	ldt	$f30, 496(THREADPTR_SYMREG)
	ldt	$f31, 504(THREADPTR_SYMREG)

	ret	$31, ($26), 1	
.set at
	.end	load_regs	
		
 # ----------------- gc_raw ---------------------------------
 # return address comes in normal return address register
 # request size come in at heap limit
 # does not use a stack frame 
 # ----------------------------------------------------------
	.ent	gc_raw
	.frame $sp, 0, $26
	.prologue 0
gc_raw:
.set noat
	stq	$26, 8*26(THREADPTR_SYMREG)	# note that this is return address of gc_raw
	bsr	save_regs
	br	$gp, gc_raw_getgp
gc_raw_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self
	ldl	$at, sysThread_disp(THREADPTR_SYMREG) # get system thread pointer
	ldl	$sp, ($at)		        # run on system thread stack
	mov	THREADPTR_SYMREG, $16		# pass user thread pointer as arg
.set at
	jsr	$26, gc				# no need to restore $gp after this call
	ldgp	$gp, 0($26)			# compute correct gp for self	
.set noat
	bsr	load_regs                       # THREADPTR_SYMREG is a callee-save register
	ldq	$26, 8*26(THREADPTR_SYMREG)	# note that this is return address of gc_raw
	ret	$31, ($26), 1	
.set at			
	.end	gc_raw

 # --------------- float_alloc_raw ----------------------
 # return address comes in normal return address register
 # array logical size come in at temp register
 # array initial value come in at ftemp register
 # optional profile tag comes in at alloc_limit register
 # result goes back in temp	
 # ------------------------------------------------------
	.ent	float_alloc_raw 
	.frame $sp, 0, $26
	.prologue 0
float_alloc_raw:
.set noat
	stq	$26, 208(THREADPTR_SYMREG)	# note that this is return address of float_alloc_raw
	bsr	save_regs
.set at
	br	$gp, float_alloc_raw_getgp
float_alloc_raw_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	$26, float_alloc
	ldgp	$gp, 0($26)			# compute correct gp for self	
.set noat
	bsr	load_regs
	ldq	$26, 208(THREADPTR_SYMREG)	# note that this is return address of float_alloc_raw
	ret	$31, ($26), 1	
.set at			
	.end	float_alloc_raw

 # ----------------- old_alloc --------------------------
 # return address comes in $26
 # request size come in at heap limit
 # ------------------------------------------------------
	.ent	old_alloc
	.frame $sp, 640, $at
.set noat
old_alloc:
	stq	$gp, -8($sp)    # save caller gp
	br	$gp, old_alloc_dummy
old_alloc_dummy:
	ldgp	$gp, 0($gp)     # get self gp so we can access globals
				# return address passed in at $26

	stq	$0, -32($sp)    # we some scratch regs
	stq	$27, -24($sp)    # we some scratch regs
	stl	ALLOCLIMIT_SYMREG, -16($sp)	# save req size
	
 	lda	$0, old_alloc_limit
 	ldl	$27, 0($0)      # at this point $27 is top/limit
	lda	$0, old_alloc_ptr
	ldl	$at, 0($0)			# at this point $at is ptr/start
	addl	ALLOCLIMIT_SYMREG, $at, ALLOCLIMIT_SYMREG	# heaplimit contains address to check against limit
	

	cmpule	$27, ALLOCLIMIT_SYMREG, $at
	beq     $at, old_alloc_ok
old_alloc_bad:
        jsr	abort
.globl old_alloc_ok
old_alloc_ok:
	# we must make sure we won't be asked to old_alloc more than we can give
	# it suffices to make sure that there is less space in the nursery than
	# in the old alloc area
	subl	$27, ALLOCLIMIT_SYMREG, $27   # $27 contains space left in old_alloc area
	lda	$0, nursery
	ldl	$0, 0($0)		 # $0 is nursery
	ldl	ALLOCLIMIT_SYMREG, 8($0)      # at this point ALLOCLIMIT_SYMREG is restored to heaplimit
	subl	ALLOCLIMIT_SYMREG, $11, $at	 # $at has less space between limit and at
	cmple   $at, $27, $at
	bne	$at, no_limit_reset
	addl	$11, $27, ALLOCLIMIT_SYMREG
no_limit_reset:
	ldl	$at, -16($sp)
	subl	ALLOCLIMIT_SYMREG, $at, ALLOCLIMIT_SYMREG
	subl	ALLOCLIMIT_SYMREG, $at, ALLOCLIMIT_SYMREG
	# return old_alloc_ptr at ALLOCPTR_SYMREG;  code retrieves normal from cur_alloc_ptr
	lda	$0, cur_alloc_ptr
	stl	ALLOCPTR_SYMREG, 0($0)
	lda	$0, old_alloc_ptr
	ldl	ALLOCPTR_SYMREG, 0($0)		# at this point $at is ptr/start
	ldq	$27, -24($sp)			# restore scractch regs
	ldq	$0, -32($sp)
	ldq	$gp, -8($sp)
	ret	$31, ($26), 1
.set at
	.end	old_alloc


 # ----------------- int_alloc_raw ----------------------
 # return address comes in normal return address register
 # array logical size come in at temp register
 # array initial value come in at temp2 register
 # optional profile tag comes in at alloc_limit register
 # result goes back in temp	
 # ------------------------------------------------------
	.ent	int_alloc_raw
	.frame $sp, 0, $26
	.prologue 0
int_alloc_raw:
.set noat
	stq	$26, 208(THREADPTR_SYMREG)	# note that this is return address of int_alloc_raw
	bsr	save_regs
.set at
	br	$gp, int_alloc_raw_getgp
int_alloc_raw_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	$26, int_alloc
	ldgp	$gp, 0($26)			# compute correct gp for self	
.set noat
	bsr	load_regs
	ldq	$26, 208(THREADPTR_SYMREG)	# note that this is return address of int_alloc_raw
	ret	$31, ($26), 1	
.set at			
	.end	int_alloc_raw


 # ----------------- ptr_alloc_raw ----------------------
 # return address comes in normal return address register
 # array logical size come in at temp register
 # array initial value come in at temp2 register
 # optional profile tag comes in at alloc_limit register
 # result goes back in temp	
 # ------------------------------------------------------
	.ent	ptr_alloc_raw
	.frame $sp, 0, $26
	.prologue 0
ptr_alloc_raw:
.set noat
	stq	$26, RA_DISP(THREADPTR_SYMREG)	# note that this is return address of ptr_alloc_raw
	bsr	save_regs
.set at
	br	$gp, ptr_alloc_raw_getgp
ptr_alloc_raw_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	$26, ptr_alloc
	ldgp	$gp, 0($26)			# compute correct gp for self	
.set noat
	bsr	load_regs
	ldq	$26, RA_DISP(THREADPTR_SYMREG)	# note that this is return address of ptr_alloc_raw
	ret	$31, ($26), 1	
.set at			
	.end	ptr_alloc_raw


 # --------------------------------------------------------
 # Called from the runtime with the thread pointer argument
 # --------------------------------------------------------
	.ent	context_restore	
.set noat
context_restore:
	mov	$16, THREADPTR_SYMREG
	bsr 	load_regs
	ldq	$26, RA_DISP(THREADPTR_SYMREG)
	ret	$31, ($26), 1	
.set at
	.end	context_restore	


.data
cur_alloc_ptr:	
	.long	0
	.long	0