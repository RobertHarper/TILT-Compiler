 # (1) Assumes that the thread pointer points to a structure containing
 # 32 longs constituting the integer register set followed by 32 doubles
 # constituting the floating-point register set
 # (2) Assumes that the thread pointer is unmodified by call to gc_raw	  
		
#define _asm_
#include "general.h"
#include "thread.h"
			
		
	.text	
	.align	4
	.globl	gc_raw
	.globl	context_restore
	.globl	old_alloc
	.globl	cur_alloc_ptr
	.globl	save_regs
	.globl	save_iregs
	.globl	load_regs
	.globl	load_iregs
	.globl	save_regs_forC
	.globl	load_regs_forC
	
 # ----------------- save_regs---------------------------------
 # ----------------- save_iregs--------------------------------
 # save_regs saves entire register set (excluding the return address register)
 # save_iregs saves only integer register set (excluding the return address register)
 # does not use a stack frame or change any registers
 # ----------------------------------------------------------
	.ent	save_regs
	.frame $sp, 0, $26
save_regs:
save_regs_forC:	
.set noat
	stt	$f0, 256(THREADPTR_REG)   
	stt	$f1, 264(THREADPTR_REG)
	stt	$f2, 272(THREADPTR_REG)
	stt	$f3, 280(THREADPTR_REG)
	stt	$f4, 288(THREADPTR_REG)
	stt	$f5, 296(THREADPTR_REG)
	stt	$f6, 304(THREADPTR_REG)
	stt	$f7, 312(THREADPTR_REG)
	stt	$f8, 320(THREADPTR_REG)   
	stt	$f9, 328(THREADPTR_REG)
	stt	$f10, 336(THREADPTR_REG)
	stt	$f11, 344(THREADPTR_REG)
	stt	$f12, 352(THREADPTR_REG)
	stt	$f13, 360(THREADPTR_REG)
	stt	$f14, 368(THREADPTR_REG)
	stt	$f15, 376(THREADPTR_REG)
	stt	$f16, 384(THREADPTR_REG)
	stt	$f17, 392(THREADPTR_REG)
	stt	$f18, 400(THREADPTR_REG)
	stt	$f19, 408(THREADPTR_REG)
	stt	$f20, 416(THREADPTR_REG)
	stt	$f21, 424(THREADPTR_REG)
	stt	$f22, 432(THREADPTR_REG)
	stt	$f23, 440(THREADPTR_REG)
	stt	$f24, 448(THREADPTR_REG)
	stt	$f25, 456(THREADPTR_REG)
	stt	$f26, 464(THREADPTR_REG)
	stt	$f27, 472(THREADPTR_REG)
	stt	$f28, 480(THREADPTR_REG)
	stt	$f29, 488(THREADPTR_REG)
	stt	$f30, 496(THREADPTR_REG)
	stt	$f31, 504(THREADPTR_REG)
save_iregs:	
	stq	$0, (THREADPTR_REG)
	stq	$1, 8(THREADPTR_REG)
	stq	$2, 16(THREADPTR_REG)
	stq	$3, 24(THREADPTR_REG)
	stq	$4, 32(THREADPTR_REG)
	stq	$5, 40(THREADPTR_REG)
	stq	$6, 48(THREADPTR_REG)
	stq	$7, 56(THREADPTR_REG)
	stq	$8, 64(THREADPTR_REG)
	stq	$9, 72(THREADPTR_REG)
	stq	$10, 80(THREADPTR_REG)
	stq	$11, 88(THREADPTR_REG)
	stq	$12, 96(THREADPTR_REG)
	stq	$13, 104(THREADPTR_REG)  
	stq	$14, 112(THREADPTR_REG)
	stq	$15, 120(THREADPTR_REG)
	stq	$16, 128(THREADPTR_REG)
	stq	$17, 136(THREADPTR_REG)
	stq	$18, 144(THREADPTR_REG)
	stq	$19, 152(THREADPTR_REG)
	stq	$20, 160(THREADPTR_REG)
	stq	$21, 168(THREADPTR_REG)
	stq	$22, 176(THREADPTR_REG)
	stq	$23, 184(THREADPTR_REG)
	stq	$24, 192(THREADPTR_REG)
	stq	$25, 200(THREADPTR_REG)
	# skip return address register
	stq	$27, 216(THREADPTR_REG)
	stq	$28, 224(THREADPTR_REG)
	stq	$29, 232(THREADPTR_REG)
	stq	$30, 240(THREADPTR_REG)
	stq	$31, 248(THREADPTR_REG)

	ret	$31, ($26), 1	
.set at
	.end	save_regs	

 # ----------------- load_regs---------------------------------
 # ----------------- load_iregs---------------------------------
 # load_regs loads entire register set (excluding return address register)
 # load_iregs loads integer register set (excluding return address register)
 # does not use a stack frame or change any registers
 # ----------------------------------------------------------
	.ent	load_regs
	.frame $sp, 0, $26
load_regs:	
.set noat
	ldt	$f0, 256(THREADPTR_REG)   
	ldt	$f1, 264(THREADPTR_REG)
	ldt	$f2, 272(THREADPTR_REG)
	ldt	$f3, 280(THREADPTR_REG)
	ldt	$f4, 288(THREADPTR_REG)
	ldt	$f5, 296(THREADPTR_REG)
	ldt	$f6, 304(THREADPTR_REG)
	ldt	$f7, 312(THREADPTR_REG)
	ldt	$f8, 320(THREADPTR_REG)   
	ldt	$f9, 328(THREADPTR_REG)
	ldt	$f10, 336(THREADPTR_REG)
	ldt	$f11, 344(THREADPTR_REG)
	ldt	$f12, 352(THREADPTR_REG)
	ldt	$f13, 360(THREADPTR_REG)
	ldt	$f14, 368(THREADPTR_REG)
	ldt	$f15, 376(THREADPTR_REG)
	ldt	$f16, 384(THREADPTR_REG)
	ldt	$f17, 392(THREADPTR_REG)
	ldt	$f18, 400(THREADPTR_REG)
	ldt	$f19, 408(THREADPTR_REG)
	ldt	$f20, 416(THREADPTR_REG)
	ldt	$f21, 424(THREADPTR_REG)
	ldt	$f22, 432(THREADPTR_REG)
	ldt	$f23, 440(THREADPTR_REG)
	ldt	$f24, 448(THREADPTR_REG)
	ldt	$f25, 456(THREADPTR_REG)
	ldt	$f26, 464(THREADPTR_REG)
	ldt	$f27, 472(THREADPTR_REG)
	ldt	$f28, 480(THREADPTR_REG)
	ldt	$f29, 488(THREADPTR_REG)
	ldt	$f30, 496(THREADPTR_REG)
	ldt	$f31, 504(THREADPTR_REG)
load_iregs:	
	ldq	$0, (THREADPTR_REG)
load_regs_forC:	
	ldq	$1, 8(THREADPTR_REG)
	ldq	$2, 16(THREADPTR_REG)
	ldq	$3, 24(THREADPTR_REG)
	ldq	$4, 32(THREADPTR_REG)
	ldq	$5, 40(THREADPTR_REG)
	ldq	$6, 48(THREADPTR_REG)
	ldq	$7, 56(THREADPTR_REG)
	ldq	$8, 64(THREADPTR_REG)
	ldq	$9, 72(THREADPTR_REG)
	ldq	$10, 80(THREADPTR_REG)
	ldq	$11, 88(THREADPTR_REG)
	ldq	$12, 96(THREADPTR_REG)
	ldq	$13, 104(THREADPTR_REG)  
	ldq	$14, 112(THREADPTR_REG)
	ldq	$15, 120(THREADPTR_REG)
	ldq	$16, 128(THREADPTR_REG)
	ldq	$17, 136(THREADPTR_REG)
	ldq	$18, 144(THREADPTR_REG)
	ldq	$19, 152(THREADPTR_REG)
	ldq	$20, 160(THREADPTR_REG)
	ldq	$21, 168(THREADPTR_REG)
	ldq	$22, 176(THREADPTR_REG)
	ldq	$23, 184(THREADPTR_REG)
	ldq	$24, 192(THREADPTR_REG)
	ldq	$25, 200(THREADPTR_REG)
	# skip return address register
	ldq	$27, 216(THREADPTR_REG)
	ldq	$28, 224(THREADPTR_REG)
	ldq	$29, 232(THREADPTR_REG)
	ldq	$30, 240(THREADPTR_REG)
	ldq	$31, 248(THREADPTR_REG)

	ret	$31, ($26), 1	
.set at
	.end	load_regs	
		
 # ----------------- gc_raw ---------------------------------
 # return address comes in normal return address register
 # temp register contains heap pointer + request size
 # does not use a stack frame 
 # ----------------------------------------------------------
	.ent	gc_raw
	.frame $sp, 0, $26
	.prologue 0
gc_raw:
.set noat
	stq	$26, RA_DISP(THREADPTR_REG)	# note that this is return address of gc_raw
	bsr	save_regs
	br	$gp, gc_raw_getgp
gc_raw_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self
	ldl	$at, sysThread_disp(THREADPTR_REG) # get system thread pointer
	ldl	$sp, ($at)		        # run on system thread stack
	mov	THREADPTR_REG, $16		# pass user thread pointer as arg
.set at
	jsr	$26, gc				# no need to restore $gp after this call
	ldgp	$gp, 0($26)			# compute correct gp for self	
.set noat
	bsr	load_regs                       # THREADPTR_REG is a callee-save register
	ldq	$26, RA_DISP(THREADPTR_REG)	# note that this is return address of gc_raw
	ret	$31, ($26), 1	
.set at			
	.end	gc_raw


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
	stl	ALLOCLIMIT_REG, -16($sp)	# save req size
	
 	lda	$0, old_alloc_limit
 	ldl	$27, 0($0)      # at this point $27 is top/limit
	lda	$0, old_alloc_ptr
	ldl	$at, 0($0)			# at this point $at is ptr/start
	addl	ALLOCLIMIT_REG, $at, ALLOCLIMIT_REG	# heaplimit contains address to check against limit
	

	cmpule	$27, ALLOCLIMIT_REG, $at
	beq     $at, old_alloc_ok
old_alloc_bad:
        jsr	abort
.globl old_alloc_ok
old_alloc_ok:
	# we must make sure we won't be asked to old_alloc more than we can give
	# it suffices to make sure that there is less space in the nursery than
	# in the old alloc area
	subl	$27, ALLOCLIMIT_REG, $27   # $27 contains space left in old_alloc area
	lda	$0, nursery
	ldl	$0, 0($0)		 # $0 is nursery
	ldl	ALLOCLIMIT_REG, 8($0)      # at this point ALLOCLIMIT_REG is restored to heaplimit
	subl	ALLOCLIMIT_REG, $11, $at	 # $at has less space between limit and at
	cmple   $at, $27, $at
	bne	$at, no_limit_reset
	addl	$11, $27, ALLOCLIMIT_REG
no_limit_reset:
	ldl	$at, -16($sp)
	subl	ALLOCLIMIT_REG, $at, ALLOCLIMIT_REG
	subl	ALLOCLIMIT_REG, $at, ALLOCLIMIT_REG
	# return old_alloc_ptr at ALLOCPTR_REG;  code retrieves normal from cur_alloc_ptr
	lda	$0, cur_alloc_ptr
	stl	ALLOCPTR_REG, 0($0)
	lda	$0, old_alloc_ptr
	ldl	ALLOCPTR_REG, 0($0)		# at this point $at is ptr/start
	ldq	$27, -24($sp)			# restore scractch regs
	ldq	$0, -32($sp)
	ldq	$gp, -8($sp)
	ret	$31, ($26), 1
.set at
	.end	old_alloc



 # --------------------------------------------------------
 # Called from the runtime with the thread pointer argument
 # --------------------------------------------------------
	.ent	context_restore	
.set noat
context_restore:
	mov	$16, THREADPTR_REG
	bsr 	load_regs
	ldq	$26, RA_DISP(THREADPTR_REG)
	ret	$31, ($26), 1	
.set at
	.end	context_restore	


.data
cur_alloc_ptr:	
	.long	0
	.long	0