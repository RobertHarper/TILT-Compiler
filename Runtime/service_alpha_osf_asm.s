
 # start_client makes assumption about how to invoke closures and thread pointer structure
	
 # Code that transitions from ML to C must set the thread-specific value notInML (used by signal
 # handlers).  Code that transitions from C to ML must clear notInML.

#define _asm_
#include "general.h"
#include "thread.h"
		
	.text	
	.align	4
 	.globl	start_client
	.globl  global_exnrec
        .globl  GetRpcc
	.globl	raise_exception_raw
	.globl  CompareAndSwap
	.globl  FetchAndAdd
	.globl  TestAndSet
	.globl  Yield
	.globl  Spawn
	.globl  scheduler
	.globl	memOrder
	.globl	memBarrier

 # XXX dummy functions
        .ent memOrder
memOrder:	
        ret     $31, ($26), 1
	.end memOrder

        .ent memBarrier
memBarrier:	
        ret     $31, ($26), 1
	.end memBarrier
	
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
	mov	1, $0
	stl_c	$0, ($16)	# try to set
        ret     $31, ($26), 1		
AlreadySet:
	stl_c	$at, ($16)	# cancel reservation
	mov	$31, $0
        ret     $31, ($26), 1	
.set at
        .end TestAndSet
			
 # ----------------------------------------------------------------------------	
 # CompareAndSwap takes an address, a test value, and a new value
 # If the address contains the test value, it is changed to the new value
 # In any case, the address's old value is returned
 # ----------------------------------------------------------------------------
        .ent CompareAndSwap
CompareAndSwap:	
.set noat
	ldl_l	$0, ($16)
	cmpeq	$0, $17, $at
	beq	$at, NotEqual
	mov	$18, $19		# need temp $19 since stl_c modifies register
	stl_c	$19, ($16)		# try to set
	beq	$19, CompareAndSwap	# must retry
        ret     $31, ($26), 1		
NotEqual:
	mov	$0, $19			# need temp $19 since stl_c modifies register
	stl_c	$19, ($16)		# cancel reservation by writing old value
        ret     $31, ($26), 1	
.set at
        .end CompareAndSwap
			

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
 # second C arg = thunk
 # ----------------------------------------------------------------------------
	.ent	start_client 
start_client:
.set noat
 	ldgp	$gp, 0($27)				# get self gp
	mov	$16,THREADPTR_REG			# initialize thread ptr
	mov	$17,$1					# save thunk
	addq	THREADPTR_REG, MLsaveregs_disp, $0	# use ML save area of thread pointer structure
	bsr	load_regs				# restore dedicated pointers like
							# heap pointer, heap limit, and stack pointer
							# don't need to restore return address
	mov	$1, $at					# restore thunk to temp
	ldq	$1, MLsaveregs_disp+8($0)		# restore $1 which is not restored by load_regs
	ldq	$0, MLsaveregs_disp($0)			# restore $0 which was used as arg to load_regs
	br	$gp, start_client_getgp1
start_client_getgp1:	
	ldgp	$gp, 0($gp)				# fix $gp
	stq	$31, notinml_disp(THREADPTR_REG)	# entering ML
	ldl	$27, ($at)				# fetch code pointer
	ldl	$0, 4($at)				# fetch type env
	ldl	$1, 8($at)				# fetch term env
	lda	EXNPTR_REG, global_exnrec		# install global handler
	stl	$sp, 4(EXNPTR_REG)			# initialize the stack pointer
	jsr	$26,  ($27)				# jump to thunk
	br	$gp, start_client_getgp2		# returning from mutator
start_client_getgp2:	
	ldgp	$gp, 0($gp)				# fix gp
	mov	1, $at
	stq	$at, notinml_disp(THREADPTR_REG)	# returning from ML
	addq	THREADPTR_REG, MLsaveregs_disp, $0
	bsr	save_regs				# need to save register set to get 
							#    alloction pointer into thread state
	ldq	$at, proc_disp(THREADPTR_REG)	# get system thread pointer
	ldq	$sp, ($at)				# run on system thread stack	
	jsr	Finish
	br	$gp, start_client_getgp3
start_client_getgp3:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	abort
	.end	start_client
.set at
	
 # ------------------------------------------------------------------------------------
 # Yield is called by mutator like a C function so save_regs_MLtoC will take care of
 # ML -> C transition work for us.
 # ------------------------------------------------------------------------------------
	.ent	Yield
	.frame $sp, 0, $26
	.prologue 0
Yield:
.set noat
	br	$gp, Yield_getgp
Yield_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self to we can jsr	
	ldq	$at, proc_disp(THREADPTR_REG) # get system thread pointer
	ldq	$sp, ($at)		        # run on system thread stack
	jsr	$26, YieldRest			# no need to restore $gp after this call
	br	$gp, Yield_getgp2
Yield_getgp2:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	abort
.set at			
	.end	Yield


 # ------------------------------------------------------------------------------------
 # Spawn is called by mutator like a C function so save_regs_MLtoC will take care of
 # ML -> C transition work for us.
 # ------------------------------------------------------------------------------------
	.ent	Spawn
	.frame $sp, 0, $26
	.prologue 0
Spawn:	
.set noat
	stq	$26, 208(THREADPTR_REG)	# note that this is return address of Spawn
	br	$gp, Spawn_getgp
Spawn_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	ldq	$at, proc_disp(THREADPTR_REG) # get system thread pointer
	ldq	$sp, ($at)		        # run on system thread stack
	jsr	$26, SpawnRest			# no need to restore $gp after this call
	ldgp	$gp, 0($26)			# compute correct gp for self	
	bsr	load_regs			# THREADPTR_REG is a callee-save register
	ldq	$26, 208(THREADPTR_REG)		# note that this is return address of Spawn
	ret	$31, ($26), 1	
.set at			
	.end	Spawn

 # -------------------------------------------------------------------------------
 # Scheduler is called by a C function with the Proc_t * pointer.
 # We switch to processor's stack and then call schedulerRest.
 # -------------------------------------------------------------------------------
	.ent	scheduler
	.frame $sp, 0, $26
	.prologue 0
scheduler:
	br	$gp, scheduler_getgp
scheduler_getgp:	
	ldgp	$gp, 0($gp)			# compute correct gp for self		
.set noat
	ldl	$sp, (CFIRSTARG_REG)	        # run on system thread stack  XXX:	ldq?
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
	br	$gp, global_exn_handler_getgp1
global_exn_handler_getgp1:	
	ldgp	$gp, 0($gp)					# fix $gp
	ldl	$sp, 4(EXNPTR_REG)
	stq	EXNARG_REG, EXNARG_DISP(THREADPTR_REG)
	mov	1, $0
	stq	$0, notinml_disp(THREADPTR_REG)			# returning from ML
	addq	THREADPTR_REG, MLsaveregs_disp, $0		# use ML save area of thread pointer
	bsr	save_regs
	mov	THREADPTR_REG, $16
	lda	$27, toplevel_exnhandler
	bsr	toplevel_exnhandler
	br	$gp, global_exn_handler_getgp2
global_exn_handler_getgp2:	
	ldgp	$gp, 0($gp)			# compute correct gp for self	
	jsr	abort
	.end	global_exnhandler

 # ------------------------------------------------------------
 # first C arg = thread structure
 # second C arg = exn argument	;  will eventually pass in return address
 # ------------------------------------------------------------
	.ent	raise_exception_raw
raise_exception_raw:
	mov	$16, THREADPTR_REG			# restore thread pointer
	mov	$17, $1					# save the exn value;  load_regs does not change $1
	addq    THREADPTR_REG, MLsaveregs_disp, $0	# use ML save area of thread pointer structure
	br	$gp, restore_dummy
restore_dummy:	
	ldgp	$gp, 0($gp)				# get own gp
	stq	$31, notinml_disp(THREADPTR_REG)	# entering ML
.set noat
	bsr	load_regs
	mov	$1, EXNARG_REG				# restore exn arg from $1 temp (unmodified by load_regs)
	ldq	$0, MLsaveregs_disp+0*8(THREADPTR_REG)	# restore $0 which was used as arg to load_regs
	ldq	$1, MLsaveregs_disp+1*8(THREADPTR_REG)	# restore $1 which was used to save exn arg unmodified by load_regs
							# don't need to restore r26 and r29 due to load_regs
							# at this point, all registers restored
	br	$gp, restore_dummy2			# Fix gp
restore_dummy2:
	ldgp	$gp, 0($gp)
	lda	ASMTMP2_REG, primaryStackletOffset
	ldl	ASMTMP2_REG, (ASMTMP2_REG)
	ldl	ASMTMP_REG, 4(EXNPTR_REG)	# fetch sp in handler
	addl	ASMTMP_REG, ASMTMP2_REG, $sp	# restore sp		
	ldl	$27, 0(EXNPTR_REG)	# fetch pc of handler
	jmp	$31, ($27), 1		# jump without link
.set at
	.end	raise_exception_raw

	
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
