 # start_client makes assumption about how to invoke closures and thread pointer structure
	
#define _asm_
#include "general.h"
#include "thread.h"
		
	.text	
	.align	4
 	.globl	start_client
 	.globl	start_client_retadd_val
	.globl  global_exnrec
        .globl  GetRpcc
	.globl	raise_exception_raw
	.globl	Overflow_exncon
	.globl	Divide_exncon
	.globl  FetchAndAdd
	.globl  TestAndSet
	.globl  Yield
	
 # ----------------------------------------------------------------------------	
 # FetchAndAdd takes the address of the variable to be incremented and the increment
 # Returns the pre-incremented value
 # ----------------------------------------------------------------------------
        .ent FetchAndAdd
FetchAndAdd:	
.set noat
	ldl_l	$at, ($16)
	addq	$at, $17, $18
	stl_c	$18, ($16)
	beq	$18, FetchAndAdd	# might need to retry
	mov	$at, $0
        ret     $31, ($26), 1	
.set at
        .end FetchAndAdd

 # ----------------------------------------------------------------------------	
 # TestAndSet takes the address of the variable to be test-and-set
 # If the value was 0, it is set to 1.  Returns 1.
 # If the value was 1, it is unchanged.  Returns 0.
 # If the value is not 0 or 1, then the result is unpredictable.
 # If the reservation fails, value is unchaged. Returns 0.
 # ----------------------------------------------------------------------------
        .ent TestAndSet
TestAndSet:	
.set noat
	ldl_l	$at, ($16)
	bne	$at, AlreadySet	# test
	lda	$0, 1($31)
	stl_c	$0, ($16)	# try to set
        ret     $31, ($26), 1		
AlreadySet:
	stl_c	$at, ($16)	# cancel reservation
	mov	$31, $0
        ret     $31, ($26), 1	
.set at
        .end TestAndSet
			
 # ----------------------------------------------------------------------------	
 # one might call getrpcc twice and take the different between the two results
 # to obtain the cycles used; remember to subtract 5 from the result
 # return the rpcc in standard result register $0 (a 32-bit-quantity)
 # GetRpcc returns the contents of the cycle count for this process
 # this is some multiple(in range 1..16) of the number of cycles
 # ----------------------------------------------------------------------------	
        .ent GetRpcc
GetRpcc:
        rpcc    $0
        sll     $0,   32,  $1
        addq    $0,   $1,  $0
        srl     $0,   32,  $0
        ret     $31, ($26), 1
        .end GetRpcc


	
 # ------------------------ start_client  -------------------------------------
 # first C arg = current thread pointer
 # second C arg = client_entry (array of starting addresss)
 # third C arg = number of starting address in array client_entry
 # ----------------------------------------------------------------------------
	.ent	start_client 
start_client:
 	ldgp	$gp, 0($27)	# get self gp
	mov	$16,THREADPTR_SYMREG                                 # initialize thread ptr outside loop
			        # $17 = client_entry current
	s4addq	$18, $17, $19   # client_entry end
	ldq	ALLOCPTR_SYMREG, ALLOCPTR_DISP(THREADPTR_SYMREG)     # initialize heap ptr outside loop
	ldq	ALLOCLIMIT_SYMREG, ALLOCLIMIT_DISP(THREADPTR_SYMREG) # initizlize heap limit outside loop
	ldq	$sp, SP_DISP(THREADPTR_SYMREG) # fetch stack argument
 	lda	$sp, -32($sp)	# switch to new stack and allocate a little space
 	stq	$17, 0($sp)	# save current thunk
 	stq	$19, 8($sp)	# save limit thunk
thunk_loop:
 # nuke regs for debugging
	lda	$0,  1200($31)
	lda	$1,  1201($31)
	lda	$2,  1202($31)
	lda	$3,  1203($31)
	lda	$4,  1204($31)
	lda	$5,  1205($31)
	lda	$6,  1206($31)
	lda	$7,  1207($31)
	lda	$8,  1208($31)
	stl	$31, notinml_disp(THREADPTR_SYMREG)
.set noat
 	ldq	$17, 0($sp)	# fetch current thunk
	ldl	$at, ($17)	# fetch current thunk address
	ldl	$27, ($at)	# fetch code pointer
	ldl	$0, 4($at)	# fetch type env
	ldl	$2, 8($at)	# fetch term env
	lda	EXNPTR_SYMREG, global_exnrec # install global handler
	jsr	$26,  ($27)	# jump to thunk
start_client_retadd_val:	
	br	$26, dummy
dummy:	ldgp	$gp, 0($26)
 # returned from client
	ldq	$17, 0($sp)     # fetch current thunk
	addq	$17, 4, $17	# update current thunk
 	stq	$17, 0($sp)	# save current thunk	
	ldq	$19, 8($sp)	# fetch thunk limit
	cmplt	$17, $19, $at
	bne	$at, thunk_loop
	lda	$at, 1($31)
	stl	$at, notinml_disp(THREADPTR_SYMREG)
	bsr	save_regs	# need to save register set to get alloction pointer into thread state
	ldl	$at, sysThread_disp(THREADPTR_SYMREG) # get system thread pointer
	ldl	$sp, ($at)		        # run on system thread stack	
	br	$gp, start_client_getgp
start_client_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	Finish
	lda	$16, $$errormsg
	jsr	printf
	jsr	abort
	ldq	$26, 0($sp)
	lda	$sp, 320($sp)
	ret	$31, ($26), 1
	.end	start_client
.set at
	
 # ---------------------------
 # Yield
 # ---------------------------	
	.ent	Yield
	.frame $sp, 0, $26
	.prologue 0
Yield:
.set noat
	stq	$26, 208(THREADPTR_SYMREG)	# note that this is return address of Yield
	bsr	save_regs
	br	$gp, Yield_getgp
Yield_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	ldl	$at, sysThread_disp(THREADPTR_SYMREG) # get system thread pointer
	ldl	$sp, ($at)		        # run on system thread stack
	jsr	$26, YieldRest			# no need to restore $gp after this call
	ldgp	$gp, 0($26)			# compute correct gp for self	
	bsr	load_regs			# THREADPTR_SYMREG is a callee-save register
	ldq	$26, 208(THREADPTR_SYMREG)	# note that this is return address of Yield
	ret	$31, ($26), 1	
.set at			
	.end	Yield






 # ------------------------------------------------------------
 # global_exnhandler when all else fails
 # saves all registers and calls C function toplevel_exnhandler
 # ------------------------------------------------------------
	.ent	global_exnhandler
global_exnhandler:
	br	$gp, global_exn_handler_dummy
global_exn_handler_dummy:	
	ldgp	$gp, 0($gp)
	lda	$sp, -320($sp)
.set noat
	stq	$0, 16($sp)
	stq	$1, 24($sp)
	stq	$2, 32($sp)
	stq	$3, 40($sp)
	stq	$4, 48($sp)
	stq	$5, 56($sp)
	stq	$6, 64($sp)
	stq	$7, 72($sp)
	stq	$8, 80($sp)
	stq	$9, 88($sp)
	stq	$10, 96($sp)
	stq	$11, 104($sp)
	stq	$12, 112($sp)
	stq	$13, 120($sp)
	stq	$14, 128($sp)
	stq	$15, 136($sp)
	stq	$16, 144($sp)
	stq	$17, 152($sp)
	stq	$18, 160($sp)
	stq	$19, 168($sp)
	stq	$20, 176($sp)
	stq	$21, 184($sp)
	stq	$22, 192($sp)
	stq	$23, 200($sp)
	stq	$24, 208($sp)
	stq	$25, 216($sp)
	stq	$26, 224($sp)
	stq	$27, 232($sp)
	stq	$28, 240($sp)
	stq	$29, 248($sp)
	stq	$30, 256($sp)	
	stq	$31, 264($sp)
	lda	$16, 16($sp)
	lda	$27, toplevel_exnhandler
	bsr	toplevel_exnhandler
	jsr	abort
	.end	global_exnhandler

 # ------------------------------------------------------------
 # first C arg = where regs are
 # second C arg = exn argument
 # third C arg = exn code handler address
 # ------------------------------------------------------------
	.ent	raise_exception_raw
raise_exception_raw:
	lda	$sp, -64($sp)	# allocate frame
	stq	$16, 8($sp)	# save where regs are
	stq	$17, 16($sp)	# save the exn value
	stq	$18, 24($sp)	# save handler address
	br	$gp, restore_dummy
restore_dummy:	
	ldgp	$gp, 0($gp)	# get own gp
.set noat
				# restore address from argument
	ldq	$0, 0($16)
	ldq	$1, 8($16)
	ldq	$2, 16($16)
	ldq	$3, 24($16)
	ldq	$4, 32($16)
	ldq	$5, 40($16)
	ldq	$6, 48($16)
	ldq	$7, 56($16)
	ldq	$8, 64($16)
	ldq	$9, 72($16)	# exn ptr must be restored
	ldq	$10, 80($16)	# alloc limit must be restored
	ldq	$11, 88($16)	# alloc ptr must be restored
	ldq	$12, 96($16)
	ldq	$13, 104($16)
	ldq	$14, 112($16)
	ldq	$15, 120($16)
				# regs 16 to 21 are C arg regs and need not be restored
	ldq	$22, 176($16)	
	ldq	$23, 184($16)
	ldq	$24, 192($16)	
	ldq	$25, 200($16)	
				# skip exn argument
	ldq	$27, 216($16)
	ldq	$28, 224($16)
				# $29 is gp
				# $30 is sp
				# $31 is zero
	
	ldq	$26, 16($sp)	# restore exn arg
	ldq	$at, 24($sp)	# restore where to go
	ldq	$sp, 240($16)	# restore SP from saved regs at end
	mov	$at, $27
	jmp	$31, ($at), 1
.set at
	.end	raise_exception_raw

	.data
Overflow_exncon:
	.long	Prelude_31_Overflow_1
Divide_exncon:
	.long	Prelude_57_Div_1

	
	.data

 # a triple to represent the top-level exn record
	.align 4
	.long   (3 << 27) + (0 << 3) + 0
global_exnrec:
	.long	global_exnhandler
	.long   0
	.long	0

	.align 4
$$errormsg:
	.ascii "Thread Finish returned!!!\n\000"

	.align 4
$$global_exnmsg:
	.ascii "Runtime Error: Uncaught exception!!!\n\000"
	

