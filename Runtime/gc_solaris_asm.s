
 ! (1) Assumes that the thread pointer points to a structure containing
 ! 32 longs constituting the integer register set followed by 16 doubles
 ! constituting the floating-point register set
 ! (2) Assumes that the thread pointer is unmodified by call to GCFromML

 ! We maintain the invariant that there is only one valid register window while the mutator is
 ! running.  That is, the C parts of the runtime may use register windows (SAVE and RESTORE) but we
 ! compensate with FLUSHW.
	
 ! Code that transitions from ML to C must set the thread-specific value notInML (used by signal
 ! handlers).  Code that transitions from C to ML must clear notInML and perform FLUSHW.
	
#define _asm_
#include "general.h"
#include "thread.h"
		
	.section ".text"
 ! helper functions used locally and in service_solaris_asm.s
	.globl	load_regs
	.globl	save_regs
	.globl	GCFromML
	.globl	RestoreStackFromML
	.globl	NewStackletFromML
	.globl	PopStackletFromML
	.globl	GCFromC
	.globl	returnFromGCFromML
	.globl	returnFromGCFromC
	.globl	returnFromYield
	.globl	returnToML
	.globl	save_regs_MLtoC
	.globl	load_regs_MLtoC


 ! ---------------------------------------------------------------------------------------------
 ! save_regs stores the integer register (exceptions noted) into the save area pointed to by r1.
 ! It will then return to the caller by using r15.	
 ! The save area begins with the GP area and then the FP area.   
 !
 ! r0 is skipped because it is the zero register
 ! r1 is skipped because it is used as the argument register to the save area
 ! r2 is skipped because it is the THREADPTR_REG
 ! r3, r6, r7 are skipped because they are reserved for library or app software (C compiler)
 ! r4 is skipped because so the caller can use it as a temp register
 ! r15 is skipped because it is our return address
 ! The called must take care to save r1, r4, and r15 manually
 ! ---------------------------------------------------------------------------------------------
save_regs:	
 !	stw	%r0, [%r1+0]	     g0 - no std role                           
 !	stw	%r1 , [%r1+4]	   ! g1 - no std role(volatile);     EXNPTR
 !	stw	%r2 , [%r1+8]	   ! g2 - reserved for app software; THREADPTR
 ! 	stw	%r3 , [%r1+12]       g3 - reserved for app software
 !	stw	%r4 , [%r1+16]     ! g4 - no std role (volatile);    ALLOCPTR   
 	stw	%r5 , [%r1+20]	   ! g5 - no std role (volatile);    ALLOCLIMIT           
 !	stw	%r6 , [%r1+24]       g6 - reserved for system software          
 !	stw	%r7 , [%r1+28]       g7 - reserved for system software          
 	stw	%r8 , [%r1+32]
	stw	%r9 , [%r1+36]
	stw	%r10, [%r1+ 40]
	stw	%r11, [%r1+ 44]
	stw	%r12, [%r1+ 48]
	stw	%r13, [%r1+ 52]
	stw	%r14, [%r1+ 56]	    ! o6 - stack pointer is saved
 !	stw	%r15, [%r1+ 60]     ! o7 - return address/link register
	stw	%r16, [%r1+ 64]
	stw	%r17, [%r1+ 68]
	stw	%r18, [%r1+ 72]
	stw	%r19, [%r1+ 76]
	stw	%r20, [%r1+ 80]
	stw	%r21, [%r1+ 84]
	stw	%r22, [%r1+ 88]
	stw	%r23, [%r1+ 92]
 	stw	%r24, [%r1+ 96] 
 	stw	%r25, [%r1+ 100]
 	stw	%r26, [%r1+ 104]
 	stw	%r27, [%r1+ 108]
 	stw	%r28, [%r1+ 112]
 	stw	%r29, [%r1+ 116]
 	stw	%r30, [%r1+ 120]	! i6 - frame pointer is saved
 	stw	%r31, [%r1+ 124]
	std	%f0 , [%r1 + 128]
	std	%f2 , [%r1 + 136]
	std	%f4 , [%r1 + 144]
	std	%f6 , [%r1 + 152]
	std	%f8 , [%r1 + 160]
	std	%f10, [%r1 + 168]
	std	%f12, [%r1 + 176]
	std	%f14, [%r1 + 184]
	std	%f16, [%r1 + 192]
	std	%f18, [%r1 + 200]
	std	%f20, [%r1 + 208]
	std	%f22, [%r1 + 216]
	std	%f24, [%r1 + 224]
	std	%f26, [%r1 + 232]
	std	%f28, [%r1 + 240]
	std	%f30, [%r1 + 248]
	std	%f32, [%r1 + 256]
	std	%f34, [%r1 + 264]
	std	%f36, [%r1 + 272]
	std	%f38, [%r1 + 280]
	std	%f40, [%r1 + 288]
	std	%f42, [%r1 + 296]
	std	%f44, [%r1 + 304]
	std	%f46, [%r1 + 312]
	std	%f48, [%r1 + 320]
	std	%f50, [%r1 + 328]
	std	%f52, [%r1 + 336]
	std	%f54, [%r1 + 344]
	std	%f56, [%r1 + 352]
	std	%f58, [%r1 + 360]
	std	%f60, [%r1 + 368]
	std	%f62, [%r1 + 376]	
	ld	[%r2+MLsaveregs_disp+8], %o0	! use g8/o0 as temp to perform consistency check
	cmp	%r2, %o0			! check that g2 consistent with saved version
	be	save_regs_ok
	nop
	mov	%r2, %o1			! pass the inconsistent versions to save_regs_fail
	call	save_regs_fail
	nop
save_regs_ok:	
	ld	[%r1+32], %o0			! restore g8/o0
	stbar
	retl
	nop
	.size	save_regs,(.-save_regs)


 ! ---------------------------------------------------------------------------------------------
 ! load_regs restores the integer register (exceptions noted) from the save area pointed to by r1.
 ! It will then return to the caller by using r15.	
 ! The save area begins with the GP area and then the FP area.   
 !	
 ! See the comment for save_regs for why these register are skipped:	
 ! r0, r1, r2, r3, r6, r7, r15 are skipped
 ! The caller must take care to restore r1, r4, and r15 manually
 ! ---------------------------------------------------------------------------------------------
	.proc	07
	.align  4
load_regs:	
 !	ld	[%r1+0], %r0       ! g0 - no std role
 !	ld	[%r1+0], %r1       ! g1 - no std role(volatile);     EXNPTR
 !	ld	[%r1+0], %r2       ! g2 - reserved for app software; THREADPTR
 !	ld	[%r1+12], %r3      ! g3 - reserved for app software
 !	ld	[%r1+16], %r4      ! g4 - no std role (volatile);    ALLOCPTR
 	ld	[%r1+20], %r5      ! g5 - no std role (volatile);    ALLOCLIMIT
 !	ld	[%r1+24], %r6      ! g6 - reserved for system software      
 !	ld	[%r1+28], %r7      ! g7 - reserved for system software          
	ld	[%r1+32], %r8
	ld	[%r1+36], %r9 
	ld	[%r1+ 40], %r10
	ld	[%r1+ 44], %r11
	ld	[%r1+ 48], %r12
	ld	[%r1+ 52], %r13
	ld	[%r1+ 56], %r14    ! o6 - stack pointer
 !	ld	[%r1+ 60], %r15    ! o7 - return address/link register
	ld	[%r1+ 64], %r16
	ld	[%r1+ 68], %r17
	ld	[%r1+ 72], %r18
	ld	[%r1+ 76], %r19
	ld	[%r1+ 80], %r20
	ld	[%r1+ 84], %r21
	ld	[%r1+ 88], %r22
	ld	[%r1+ 92], %r23
 	ld	[%r1+ 96], %r24 
 	ld	[%r1+ 100], %r25
 	ld	[%r1+ 104], %r26
 	ld	[%r1+ 108], %r27
 	ld	[%r1+ 112], %r28
 	ld	[%r1+ 116], %r29
  	ld	[%r1+ 120], %r30   !  i6 - frame pointer
 	ld	[%r1+ 124], %r31
	ldd	[%r1 + 128], %f0 
	ldd	[%r1 + 136], %f2 
	ldd	[%r1 + 144], %f4 
	ldd	[%r1 + 152], %f6 
	ldd	[%r1 + 160], %f8 
	ldd	[%r1 + 168], %f10
	ldd	[%r1 + 176], %f12
	ldd	[%r1 + 184], %f14
	ldd	[%r1 + 192], %f16
	ldd	[%r1 + 200], %f18
	ldd	[%r1 + 208], %f20
	ldd	[%r1 + 216], %f22
	ldd	[%r1 + 224], %f24
	ldd	[%r1 + 232], %f26
	ldd	[%r1 + 240], %f28
	ldd	[%r1 + 248], %f30
	ldd	[%r1 + 256], %f32
	ldd	[%r1 + 264], %f34
	ldd	[%r1 + 272], %f36
	ldd	[%r1 + 280], %f38
	ldd	[%r1 + 288], %f40
	ldd	[%r1 + 296], %f42
	ldd	[%r1 + 304], %f44
	ldd	[%r1 + 312], %f46
	ldd	[%r1 + 320], %f48
	ldd	[%r1 + 328], %f50
	ldd	[%r1 + 336], %f52
	ldd	[%r1 + 344], %f54
	ldd	[%r1 + 352], %f56
	ldd	[%r1 + 360], %f58
	ldd	[%r1 + 368], %f60
	ldd	[%r1 + 376], %f62
	ld	[%r2+MLsaveregs_disp+8], %o0	! use g8/o0 as temp to perform consistency check
	cmp	%r2, %o0			! check that g2 consistent with saved version
	be	load_regs_ok
	nop
	mov	%r2, %o1			! pass the inconsistent versions to save_regs_fail
	call	load_regs_fail
	nop
load_regs_ok:	
	ld	[%r1+32], %o0			! restore g8/o0
	stbar
	retl
	nop
	.size	load_regs,(.-load_regs)


 ! ----------------- GCFromML -------------------------------
 ! return address comes in normal return address register
 ! temp register contains request size + heap pointer
 ! does not use a stack frame
 ! ----------------------------------------------------------
	.proc	07
	.align  4
GCFromML:	
	stw	%r1 , [THREADPTR_REG+MLsaveregs_disp+4]		! we save r1, r4, r15 manually 
	stw	%r4 , [THREADPTR_REG+MLsaveregs_disp+16]	
	stw	RA_REG, [THREADPTR_REG + MLsaveregs_disp + RA_DISP]
	mov	1, %r1
	st	%r1, [THREADPTR_REG + notinml_disp]		! leaving ML
	add	THREADPTR_REG, MLsaveregs_disp, %r1		! use ML save area of thread pointer
	call	save_regs					
	nop
	sub	ASMTMP_REG, ALLOCPTR_REG, ASMTMP_REG		! compute how many bytes requested
	stw	ASMTMP_REG, [THREADPTR_REG + requestInfo_disp]	! record bytes needed
	mov	GCRequestFromML, ASMTMP_REG
	stw	ASMTMP_REG, [THREADPTR_REG + request_disp]	! record that this is an MLtoGC request
	ld	[THREADPTR_REG + proc_disp], ASMTMP_REG		! must use temp so SP always correct
	ld	[ASMTMP_REG], SP_REG				! run on system thread stack
	mov	THREADPTR_REG, CFIRSTARG_REG			! pass user thread pointer as arg
	call	GCFromMutator					! call runtime GC
	nop
	call	abort
	nop
	.size GCFromML,(.-GCFromML)

 ! ----------------- RestoreStackFromML ----------------------------------------------------------------
 ! This routine is called when raising an exception sets %sp outside the top stacklet.
 ! -----------------------------------------------------------------------------------------------------
	.proc	07
	.align  4
RestoreStackFromML:	
	stw	%r1 , [THREADPTR_REG+MLsaveregs_disp+4]		! we save r1, r4, r15 manually 
	stw	%r4 , [THREADPTR_REG+MLsaveregs_disp+16]	
	stw	RA_REG, [THREADPTR_REG + MLsaveregs_disp + RA_DISP]
	mov	1, %r1
	st	%r1, [THREADPTR_REG + notinml_disp]		! leaving ML
	add	THREADPTR_REG, MLsaveregs_disp, %r1		! use ML save area of thread pointer
	call	save_regs					
	nop
	mov	THREADPTR_REG, CFIRSTARG_REG			! pass user thread pointer
	ld	[THREADPTR_REG + proc_disp], ASMTMP_REG		! must use temp so SP always safe
	ld	[ASMTMP_REG], SP_REG				! run on system thread stack
	call	RestoreStackFromMutator				! call runtime
	nop
	call	abort
	nop
	.size RestoreStackFromML,(.-RestoreStackFromML)

 ! ----------------- NewStackletFromML --------------------------------------------------------------------
 ! When an ML function A calls B, the prolog of B checks if a new stacklet is necessary.  If so, this routine is called.
 ! Rra:	 The return address back to B comes in the normal return address register
 ! Rat2: The return address of B back to A is saved in Rat2
 ! Rat:	 Rat (normal temp) contains offset above highest argument retrieved from previous frame for additional arguments
 ! -----------------------------------------------------------------------------------------------------
	.proc	07
	.align  4
NewStackletFromML:	
	stw	%r1 , [THREADPTR_REG+MLsaveregs_disp+4]		! we save r1, r4, r15 manually 
	stw	%r4 , [THREADPTR_REG+MLsaveregs_disp+16]	
	stw	RA_REG, [THREADPTR_REG + MLsaveregs_disp + RA_DISP]
	mov	1, %r1
	st	%r1, [THREADPTR_REG + notinml_disp]		! leaving ML
	add	THREADPTR_REG, MLsaveregs_disp, %r1		! use ML save area of thread pointer
	call	save_regs					
	nop
	mov	THREADPTR_REG, CFIRSTARG_REG			! pass user thread pointer as arg
	mov	ASMTMP_REG, CSECONDARG_REG			! pass max offset we need to copy
	ld	[THREADPTR_REG + proc_disp], ASMTMP_REG		! must use temp so SP always correct
	ld	[ASMTMP_REG], SP_REG				! run on system thread stack
	call	NewStackletFromMutator				! call runtime GC
	nop
	call	abort
	nop
	.size NewStackletFromML,(.-NewStackletFromML)


 ! ----------------- PopStackletFromML --------------------------------------------------------------------
 ! return address comes in normal return address register
 ! does not use a stack frame 
 ! -----------------------------------------------------------------------------------------------------
	.proc	07
	.align  4
PopStackletFromML:	
	stw	%r1 , [THREADPTR_REG+MLsaveregs_disp+4]		! we save r1, r4, r15 manually 
	stw	%r4 , [THREADPTR_REG+MLsaveregs_disp+16]	
	stw	RA_REG, [THREADPTR_REG + MLsaveregs_disp + RA_DISP]	
	mov	1, %r1
	st	%r1, [THREADPTR_REG + notinml_disp]		! leaving ML
	add	THREADPTR_REG, MLsaveregs_disp, %r1		! use ML save area of thread pointer
	call	save_regs					
	nop
	mov	THREADPTR_REG, CFIRSTARG_REG			! pass user thread pointer as arg
	ld	[THREADPTR_REG + proc_disp], ASMTMP_REG		! must use temp so SP always correct
	ld	[ASMTMP_REG], SP_REG				! run on system thread stack
	call	PopStackletFromMutator				! call runtime GC
	nop
	call	abort
	nop
	.size PopStackletFromML,(.-PopStackletFromML)		
	
 ! -------------------------------------------------------------------------------
 ! returnToML is called from the runtime with
 ! thread pointer as 1st argument
 ! link value/return address as 2nd argument - this may or may not be the same saveregs[RA]
 ! -------------------------------------------------------------------------------
	.proc	07
	.align  4
returnToML:
	flushw	
	mov	CFIRSTARG_REG, THREADPTR_REG		! restore THREADPTR_REG
	mov	CSECONDARG_REG, %r4			! use r4 as temp for return address
	st	%g0, [THREADPTR_REG + notinml_disp]	! returning to ML
	add	THREADPTR_REG, MLsaveregs_disp, %r1	! use ML save area of thread pointer structure
	call	load_regs				! don't need to save return address
	nop						
	mov	%r4, ASMTMP2_REG
	ld	[%r1+60], %r15				! restore r15 which we do no use to return to
	ld	[%r1+16], %r4				! restore r4 which we use as temp	
	ld	[%r1+RA_DISP], RA_REG		! restore return address register but not used to get back to ML
	ld	[THREADPTR_REG+MLsaveregs_disp+4], %r1	! restore r1 which was used as arg to load_regs
	jmpl	ASMTMP2_REG+8, %g0			! use delay slot to restore ASMTMP2
	ld	[THREADPTR_REG+MLsaveregs_disp+ASMTMP2_DISP], ASMTMP2_REG
	call	abort
	nop	
	.size returnToML,(.-returnToML)

 ! -------------------------------------------------------------------------------
 ! returnFromGCFromML is called from the runtime with the thread pointer argument
 ! thread pointer as 1st argument
 ! Note returnToML does the normal C -> ML transition work for us.
 ! -------------------------------------------------------------------------------
	.proc	07
	.align  4
returnFromGCFromML:
	ld	[CFIRSTARG_REG+RA_DISP], CSECONDARG_REG
	ba	returnToML
	nop
	call	abort
	nop	
	.size returnFromGCFromML,(.-returnFromGCFromML)
	
 ! ----------------- gcFromC ---------------------------------
 ! gcFromC is called from the runtime system with 3 arguments:	
 !	thread pointer, request size, a bool for majorGC
 ! gcFromC does not use a stack frame 
 ! ----------------------------------------------------------
	.proc	07
	.align  4
GCFromC:
	mov	CFIRSTARG_REG, THREADPTR_REG
	stw	%r1 , [THREADPTR_REG+Csaveregs_disp+4]		! we save r1, r4, r15 manually 
	stw	%r4 , [THREADPTR_REG+Csaveregs_disp+16]	
	stw	RA_REG, [THREADPTR_REG + Csaveregs_disp + RA_DISP]	
								! don't change notInML
	add	THREADPTR_REG, Csaveregs_disp, %r1		! use C save area of thread pointer
	call	save_regs					
	nop
	stw	CSECONDARG_REG, [THREADPTR_REG + requestInfo_disp] ! record bytes needed
	mov	GCRequestFromC, ASMTMP_REG
	stw	ASMTMP_REG, [THREADPTR_REG + request_disp]	! record that this is an CtoGC request
	cmp	%g0, CTHIRDARG_REG
	beq	MinorGCFromC
	nop
	mov	MajorGCRequestFromC, ASMTMP_REG
	stw	ASMTMP_REG, [THREADPTR_REG + request_disp]	! record that this is an CtoGC request	
MinorGCFromC:	
	ld	[THREADPTR_REG + proc_disp], ASMTMP_REG		! must use temp so SP always correct
	ld	[ASMTMP_REG], SP_REG				! run on system thread stack
	mov	THREADPTR_REG, CFIRSTARG_REG			! pass user thread pointer as arg
	call	GCFromMutator					! call runtime GC
	nop
	call	abort
	nop
	.size GCFromC,(.-GCFromC)

 ! -------------------------------------------------------------------------------
 ! returnFromGCFromC is called from the runtime with the thread pointer argument
 ! -------------------------------------------------------------------------------
	.proc	07
	.align  4
returnFromGCFromC:
	mov	CFIRSTARG_REG, THREADPTR_REG		! restore THREADPTR_REG
							! don't change notInML
	add	THREADPTR_REG, Csaveregs_disp, %r1	! use C save area of thread pointer structure
	call	load_regs				! don't need to save return address
	nop						
	ld	[%r1+16], %r4				! restore r4 which we use as temp
	ld	[%r1+RA_DISP], RA_REG		! restore return address back to C code
	ld	[THREADPTR_REG+Csaveregs_disp+4], %r1	! restore r1 which was used as arg to load_regs
	ld	[THREADPTR_REG+Csaveregs_disp+8], THREADPTR_REG	! restore C's $r2 which does not hold threadptr
	retl
	nop
	call	abort
	nop	
	.size returnFromGCFromC,(.-returnFromGCFromC)

 ! ------------------------------------------------------------------------------------
 ! returnFromYield is called from the runtime with the thread pointer argument
 ! Yield was called from ML code as a C function so load_regs_MLtoC will take care of
 ! C -> ML transition work for us.
 ! -------------------------------------------------------------------------------------
	.proc	07
	.align  4
returnFromYield:
	mov	CFIRSTARG_REG, THREADPTR_REG
	mov	THREADPTR_REG, %l0			! Calls from C expect thread reg in %l0
	ld	[THREADPTR_REG + MLsaveregs_disp + SP_DISP], SP_REG	
	ld	[THREADPTR_REG + MLsaveregs_disp + RA_DISP], RA_REG
	retl
	nop
	call	abort
	nop
	.size returnFromYield,(.-returnFromYield)
	
	
 ! ----------------- save_regs_MLtoC -------------------------
 ! This is called using the C calling convention.
 ! This routine does not use a stack frame.
 ! The THREADPTR_REG will be moved to %l0 so that C will not modify it.
 ! ----------------------------------------------------------		
	.proc	07
	.align  4
save_regs_MLtoC:
	stw	%r1 , [THREADPTR_REG+MLsaveregs_disp+4]	! save_regs does not save r1 (used for arg)
	add	THREADPTR_REG, MLsaveregs_disp, %r1	! use ML save area of thread pointer structure	
	stw	%r4, [%r1+16]				! save_regs does not save r4
							!    so we can use this as temp if we save it
	mov	%o7, %r4				! use r4 to hold return address
	add	%o7, 8, %o7				! we want C to return to load_regs_MLtoC
							! and NOT to the C call again so we skip
							! 2 instructions (call and nop)
	stw	%o7, [%r1+60]                           ! save_regs does not save o7
	call	save_regs
	nop
	mov	%r4, %o7				! restore return address
	mov	1, %r4
	st	%r4, [THREADPTR_REG + notinml_disp]	! leaving ML
	ld	[%r1+16], %r4				! restore r4 which we use as temp
	ld	[THREADPTR_REG+MLsaveregs_disp+4], %r1	! restore r1 which was used as arg
	mov	THREADPTR_REG, %l0			! when calling C code, %l0 (but not %g2) is unmodified
	retl
	nop
        .size save_regs_MLtoC,(.-save_regs_MLtoC)

 ! ----------------- load_regs_MLtoC -------------------------
 ! This is called using the C calling convention.
 ! This routine does not use a stack frame.
 ! The THREADPTR_REG is actually in %l0 when this function is called.
 ! It will restore all registers EXCEPT the result registers.
 ! ----------------------------------------------------------		
	.proc	07
	.align  4
load_regs_MLtoC:
	flushw
	mov	%l0, THREADPTR_REG			! restore THREADPTR_REG
	st	%g0, [THREADPTR_REG + notinml_disp]	! entering ML
	add	THREADPTR_REG, MLsaveregs_disp, %r1	! use ML save area of thread pointer structure
	stw	%o0, [%r1 + 32]				! overwrite GP result register
	std	%f0, [%r1 + 128]			! overwrite FP result register	
	mov	%o7, %r4				! save our return address
	call	load_regs
	nop
	mov	%r4, %o7				! restore return address used for call to load_regs
	ld	[%r1+16], %r4				! restore r4 which we use as temp
	ld	[THREADPTR_REG+MLsaveregs_disp+4], %r1	! restore r1 which was used as arg to load_regs
	retl
	nop
        .size load_regs_MLtoC,(.-load_regs_MLtoC)	
