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
	.globl  Spawn
	.globl  scheduler
	.globl	flushStore

 # XXX dummy function	
        .ent flushStore
flushStore:
        ret     $31, ($26), 1
	.end flushStore
	
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
 # ----------------------------------------------------------------------------
	.ent	start_client 
start_client:
 	ldgp	$gp, 0($27)				# get self gp
	mov	$16,THREADPTR_REG			# initialize thread ptr outside loop
	addq	THREADPTR_REG, MLsaveregs_disp, $0	# use ML save area of thread pointer structure
	bsr	load_regs				# restore dedicated pointers like
							# heap pointer, heap limit, and stack pointer
							# don't need to restore return address
	ldq	$1, MLsaveregs_disp+8($0)		# restore $1 which is not restored by load_regs
	ldq	$0, MLsaveregs_disp($0)			# restore $0 which was used as arg to load_regs
	br	$gp, start_client_getgp1
start_client_getgp1:	
	ldgp	$gp, 0($gp)				# fix $gp
	br	$31, after_loop
thunk_loop:
	stq	$31, notinml_disp(THREADPTR_REG)
.set noat
 	ldl	$16, nextThunk_disp(THREADPTR_REG)	# fetch nextThunk
	addl	$16, 1, $17				# increment and
 	stl	$17, nextThunk_disp(THREADPTR_REG)	#   save nextThunk
 	ldq	$17, thunk_disp(THREADPTR_REG)		# fetch thunks
	s4addq	$16, $17, $16				# $16 holds current thunk' address
	ldl	$at, ($16)				# fetch current thunk
	ldl	$27, ($at)				# fetch code pointer
	ldl	$0, 4($at)				# fetch type env
	ldl	$2, 8($at)				# fetch term env
	lda	EXNPTR_REG, global_exnrec		# install global handler
	stl	$sp, 4(EXNPTR_REG)			# initialize the stack pointer
	jsr	$26,  ($27)				# jump to thunk
start_client_retadd_val:	
	br	$gp, start_client_getgp2
start_client_getgp2:	
	ldgp	$gp, 0($gp)
after_loop:	
	ldl	$16, nextThunk_disp(THREADPTR_REG)	# fetch nextThunk
	ldl	$17, numThunk_disp(THREADPTR_REG)	# fetch numThunk
	cmplt	$16, $17, $at
	bne	$at, thunk_loop				# execute if nextThunk < numThunk
	lda	$at, 1($31)
	stl	$at, notinml_disp(THREADPTR_REG)
	addq	THREADPTR_REG, MLsaveregs_disp, $0
	bsr	save_regs				# need to save register set to get 
							#    alloction pointer into thread state
	ldl	$at, sysThread_disp(THREADPTR_REG)	# get system thread pointer
	ldl	$sp, ($at)				# run on system thread stack	
	jsr	Finish
	lda	$16, $$errormsg				# should not return from Finish
	jsr	printf
	jsr	abort
	.end	start_client
.set at
	
 # ------------------------------------------------------------------------------------
 # Yield is called by mutator like a C function so save_regs_MLtoC has just been called
 # ------------------------------------------------------------------------------------
	.ent	Yield
	.frame $sp, 0, $26
	.prologue 0
Yield:
.set noat
	br	$gp, Yield_getgp
Yield_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self to we can jsr	
	ldl	$at, sysThread_disp(THREADPTR_REG) # get system thread pointer
	ldl	$sp, ($at)		        # run on system thread stack
	jsr	$26, YieldRest			# no need to restore $gp after this call
	br	$gp, Yield_getgp2
Yield_getgp2:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	abort
.set at			
	.end	Yield


	.ent	Spawn
	.frame $sp, 0, $26
	.prologue 0
Spawn:	
.set noat
	stq	$26, 208(THREADPTR_REG)	# note that this is return address of Spawn
	br	$gp, Spawn_getgp
Spawn_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	ldl	$at, sysThread_disp(THREADPTR_REG) # get system thread pointer
	ldl	$sp, ($at)		        # run on system thread stack
	jsr	$26, SpawnRest			# no need to restore $gp after this call
	ldgp	$gp, 0($26)			# compute correct gp for self	
	bsr	load_regs			# THREADPTR_REG is a callee-save register
	ldq	$26, 208(THREADPTR_REG)		# note that this is return address of Spawn
	ret	$31, ($26), 1	
.set at			
	.end	Spawn

	.ent	scheduler
	.frame $sp, 0, $26
	.prologue 0
scheduler:
	br	$gp, scheduler_getgp
scheduler_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self		
.set noat
	ldl	$sp, (CFIRSTARG_REG)	        # run on system thread stack
	jsr	$26, schedulerRest		# no need to restore $gp after this call
	br	$gp, scheduler_getgp2
scheduler_getgp2:	
	ldgp	$gp, 0($gp)			# compute correct gp for self			
	jsr	abort
.set at			
	.end	scheduler






 # ------------------------------------------------------------
 # global_exnhandler when all else fails
 # saves all registers and calls C function toplevel_exnhandler
 # ------------------------------------------------------------
	.ent	global_exnhandler
global_exnhandler:
	br	$gp, global_exn_handler_dummy
global_exn_handler_dummy:	
	ldgp	$gp, 0($gp)
	ldl	$sp, 4(EXNPTR_REG)
	stq	EXNARG_REG, EXNARG_DISP(THREADPTR_REG)
	bsr	save_regs
	mov	THREADPTR_REG, $16
	lda	$27, toplevel_exnhandler
	bsr	toplevel_exnhandler
	jsr	abort
	.end	global_exnhandler

 # ------------------------------------------------------------
 # first C arg = thread structure
 # second C arg = exn argument	;  will eventually pass in return address
 # ------------------------------------------------------------
	.ent	raise_exception_raw
raise_exception_raw:
	mov	$16, THREADPTR_REG	# restore thread point
	mov	$17, $16		# save the exn value;  load_regs_MLtoC does not change $16
	br	$gp, restore_dummy
restore_dummy:	
	ldgp	$gp, 0($gp)		# get own gp
.set noat
					# restore address from argument
	bsr	load_regs_MLtoC
	stq	$18, 24($sp)		# save handler address
	mov	$16, EXNARG_REG		# restore exn arg - which is same as $26 
	ldq	$27, 0(EXNPTR_REG)	# fetch exn handler code
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
	.long   (0 << 24) + (4 << 3) + 0
global_exnrec:
	.long	global_exnhandler
	.long   0
	.long   0	
	.long	0

	.align 4
$$errormsg:
	.ascii "Thread Finish returned!!!\n\000"

	.align 4
$$global_exnmsg:
	.ascii "Runtime Error: Uncaught exception!!!\n\000"
	

