	
 ! start_client makes assumption about how to invoke closures and thread pointer structure
	
#define _asm_
#include "general.h"
#include "thread.h"
		
	.section ".text"
	.align	4
 	.globl	start_client
 	.globl	start_client_retadd_val
	.globl  global_exnrec
        .globl  GetTick
	.globl	raise_exception_raw
	.globl	Overflow_exncon
	.globl	Divide_exncon
	.globl  FetchAndAdd
	.globl  TestAndSet
	.globl  Yield
	
 ! ----------------------------------------------------------------------------	
 ! FetchAndAdd takes the address of the variable to be incremented (%o0) and the increment (%o1)
 ! Returns the pre-incremented value (%o0)
 ! Don't use register windows;  unused parameters slots used as temp (%o2 and %o3)
 ! ----------------------------------------------------------------------------
        .proc	07
	.align	4  
FetchAndAdd:	
	ld	[%o0], %o2
	add	%o2, %o1, %o3
	cas	[%o0], %o2, %o3
	cmp	%o2, %o3
	bne	FetchAndAdd
	nop
	mov	%o2, %o0
        retl
	nop
        .size FetchAndAdd,(.-FetchAndAdd)

 ! ----------------------------------------------------------------------------	
 ! TestAndSet takes the address of the variable to be test-and-set
 ! If the value was 0, it is set to 1.  Returns 1.
 ! If the value was 1, it is unchanged.  Returns 0.
 ! If the value is not 0 or 1, then the result is unpredictable.
 ! Under the Sparc, this sequence requires no retry.
 ! Don't use register windows	;  parameters come in and result leaves in %o0; %o1 is scratch
 ! ----------------------------------------------------------------------------
        .proc	07
	.align  4
TestAndSet:
	mov	1, %o1
	cas	[%o0], %g0, %o1
	not	%o1, %o0
        retl     
	nop
	.size   TestAndSet,(.-TestAndSet)


 ! ----------------------------------------------------------------------------	
 ! one might call GetTick twice and take the different between the two results
 ! to obtain the number of intervening cycles used
 ! Done use register windows;  result leaves in %o0
 ! ----------------------------------------------------------------------------	
        .proc	07
	.align  4
GetTick:	
        rd	%tick, %o0
        retl
	nop
        .size   GetTick,(.-GetTick)

	
 ! ------------------------ start_client  -------------------------------------
 ! first C arg = current thread pointer
 ! second C arg = client_entry (array of starting addresss)
 ! third C arg = number of starting address in array client_entry
 ! ----------------------------------------------------------------------------
	.proc	07
	.align	4
start_client:
	mov	%o0, THREADPTR_SYMREG		! initialize thread ptr outside loop
	st	%o1, [THREADPTR_SYMREG + ASMTMP_DISP]
	st	%o2, [THREADPTR_SYMREG + ASMTMP2_DISP]
	call	load_iregs			! restore dedicated pointers like
						! heap pointer, heap limit, and stack pointer
	nop
	sub	SP_SYMREG, 96, SP_SYMREG	! allocate a little space on systhread stack
	ld	[THREADPTR_SYMREG + ASMTMP_DISP], %o0	! %o0 = client_entry current
	ld	[THREADPTR_SYMREG + ASMTMP2_DISP], %o1
	sll	%o1, 2, %o1
	add	%o0, %o1, %o1			! %o1 = client_entry end
 	st	%o0, [SP_SYMREG+64]		! save current thunk
 	st	%o1, [SP_SYMREG+68]		! save limit thunk
thunk_loop:
	st	%g0, [THREADPTR_SYMREG + notinml_disp]
 	ld	[SP_SYMREG+64], %o0		! fetch current thunk
	ld	[%o0], ASMTMP_SYMREG		! fetch current thunk address
	ld	[ASMTMP_SYMREG + 0], LINK_SYMREG ! fetch code pointer
	ld	[ASMTMP_SYMREG + 4], %o0	! fetch type env
	ld	[ASMTMP_SYMREG + 8], %o2	! fetch term env
        sethi   %hi(global_exnrec),EXNPTR_SYMREG
	or      EXNPTR_SYMREG,%lo(global_exnrec),EXNPTR_SYMREG  ! install global handler
	jmpl	LINK_SYMREG, %o7		! jump to thunk
	nop
start_client_retadd_val:			! used by stack.c
 ! returned from client
	ld	[SP_SYMREG + 64], %o0		! fetch current thunk
	add	%o0, 4, %o0			! update current thunk
 	st	%o0, [SP_SYMREG + 64]	! save current thunk	
	ld	[SP_SYMREG + 68], %o1		! fetch thunk limit
	cmp	%o0, %o1
	bl	thunk_loop
	nop
	mov	1, ASMTMP_SYMREG
	st	ASMTMP_SYMREG, [THREADPTR_SYMREG + notinml_disp]
	call	save_regs		! need to save register set to get alloction pointer into thread state
	nop
	ld	[THREADPTR_SYMREG + sysThread_disp], ASMTMP_SYMREG	! get system thread pointer
	ld	[ASMTMP_SYMREG], SP_SYMREG				! run on system thread stack	
	call	Finish
	nop
        sethi   %hi($$errormsg),%o0
	or      %o0,%lo($$errormsg),%o0  ! install global handler
	call	printf
	nop
	call	abort
	nop
	.size	start_client,(.-start_client)

	
 ! ---------------------------
 ! Yield
 ! ---------------------------	
	.proc	07
	.align	4
Yield:
	st	LINK_SYMREG, [THREADPTR_SYMREG + LINK_DISP]    ! note that this is return address of Yield
	call	save_regs
	nop
	ld	[THREADPTR_SYMREG + sysThread_disp],ASMTMP_SYMREG ! get system thread pointer into temp
	ld	[ASMTMP_SYMREG], SP_SYMREG      ! run on system thread stack
	call	YieldRest			! no need to restore $gp after this call
	nop
	mov	%o0, THREADPTR_SYMREG		! user thread pointer returned
	call	load_regs			
	nop
	ld	[THREADPTR_SYMREG + LINK_DISP], LINK_SYMREG	   ! note that this is return address of Yield
	retl
	nop
	.size	Yield,(.-Yield)






 ! ------------------------------------------------------------
 ! global_exnhandler when all else fails
 ! saves all registers and calls C function toplevel_exnhandler with thread pointer
 ! ------------------------------------------------------------
	.proc	07
	.align	4
global_exnhandler:
	st	EXNARG_SYMREG, [THREADPTR_SYMREG + EXNARG_DISP]    ! note that this is return address of Yield
	call	save_regs
	nop
	mov	THREADPTR_SYMREG, %o0
	call	toplevel_exnhandler
	nop
	call	abort
	nop
	.size	global_exnhandler,(.-global_exnhandler)

 ! ------------------------------------------------------------
 ! first C arg = thread structure
 ! second C arg = exn argument	;  will eventually pass in return address
 ! third C arg = exn code handler address
 ! Don't use register window
 ! ------------------------------------------------------------
	.proc	07
	.align	4
raise_exception_raw:
	mov	%o0, THREADPTR_SYMREG		! save where regs are
	mov	%o1, RESULT_SYMREG	! load_Iregs_forC does not change this reg
	st	%o2, [SP_SYMREG + 24]		! save handler address
	call	load_regs_forC
	nop
	mov	RESULT_SYMREG, EXNARG_SYMREG ! restore exn arg
	ld	[EXNPTR_SYMREG], ASMTMP_SYMREG	! fetch exn handler code
	jmpl	ASMTMP_SYMREG, %g0		! jump not return and do not link
	nop
	.size	raise_exception_raw,(.-raise_exception_raw)

	.data
Overflow_exncon:
	.word	Prelude_31_Overflow_1
Divide_exncon:
	.word	Prelude_57_Div_1

	
	.data

 ! a triple to represent the top-level exn record
	.align 4
	.word   (3 << 27) + (0 << 3) + 0
global_exnrec:
	.word	global_exnhandler
	.word   0
	.word	0
	.type   global_exnrec,#object
	.size   global_exnrec,4
	
	.align 4
$$errormsg:
	.ascii "Thread Finish returned!!!\n\000"

	.align 4
$$global_exnmsg:
	.ascii "Runtime Error: Uncaught exception!!!\n\000"
	

