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
	stq	EXNARG_SYMREG, EXNARG_DISP(THREADPTR_SYMREG)
	bsr	save_regs
	mov	THREADPTR_SYMREG, $16
	bsr	toplevel_exnhandler
	jsr	abort
	.end	global_exnhandler

 # ------------------------------------------------------------
 # first C arg = thread structure
 # second C arg = exn argument	;  will eventually pass in return address
 # ------------------------------------------------------------
	.ent	raise_exception_raw
raise_exception_raw:
	mov	$16, THREADPTR_SYMREG	# restore thread point
	mov	$17, $16	# save the exn value;  load_regs_forC does not change $16
	br	$gp, restore_dummy
restore_dummy:	
	ldgp	$gp, 0($gp)	# get own gp
.set noat
				# restore address from argument
	bsr	load_regs_forC
	stq	$18, 24($sp)	# save handler address
	mov	$16, EXNARG_SYMREG	# restore exn arg - which is same as $26 
	ldq	$27, 0(EXNPTR_SYMREG)	# fetch exn handler code
				# no need to pop frame as handler will change sp
	jmp	$31, ($27), 1
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
	

