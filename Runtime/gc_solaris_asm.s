 ! (1) Assumes that the thread pointer points to a structure containing
 ! 32 longs constituting the integer register set followed by 16 doubles
 ! constituting the floating-point register set
 ! (2) Assumes that the thread pointer is unmodified by call to gc_raw	 
		
#define _asm_
#include "general.h"
#include "thread.h"
			
		
	.section ".text"
	.globl	gc_raw
	.globl	returnFromGC
	.globl	returnFromYield
	.globl	old_alloc
	.globl	cur_alloc_ptr
	.globl	save_regs
	.globl	load_regs
	.globl	save_regs_forC
	.globl	load_regs_forC
	
save_fregs:	
	std	%f0 , [THREADPTR_REG + 128]
save_most_fregs:	
	std	%f2 , [THREADPTR_REG + 136]
	std	%f4 , [THREADPTR_REG + 144]
	std	%f6 , [THREADPTR_REG + 152]
	std	%f8 , [THREADPTR_REG + 160]
	std	%f10, [THREADPTR_REG + 168]
	std	%f12, [THREADPTR_REG + 176]
	std	%f14, [THREADPTR_REG + 184]
	std	%f16, [THREADPTR_REG + 192]
	std	%f18, [THREADPTR_REG + 200]
	std	%f20, [THREADPTR_REG + 208]
	std	%f22, [THREADPTR_REG + 216]
	std	%f24, [THREADPTR_REG + 224]
	std	%f26, [THREADPTR_REG + 232]
	std	%f28, [THREADPTR_REG + 240]
	std	%f30, [THREADPTR_REG + 248]
	std	%f32, [THREADPTR_REG + 256]
	std	%f34, [THREADPTR_REG + 264]
	std	%f36, [THREADPTR_REG + 272]
	std	%f38, [THREADPTR_REG + 280]
	std	%f40, [THREADPTR_REG + 288]
	std	%f42, [THREADPTR_REG + 296]
	std	%f44, [THREADPTR_REG + 304]
	std	%f46, [THREADPTR_REG + 312]
	std	%f48, [THREADPTR_REG + 320]
	std	%f50, [THREADPTR_REG + 328]
	std	%f52, [THREADPTR_REG + 336]
	std	%f54, [THREADPTR_REG + 344]
	std	%f56, [THREADPTR_REG + 352]
	std	%f58, [THREADPTR_REG + 360]
	std	%f60, [THREADPTR_REG + 368]
	std	%f62, [THREADPTR_REG + 376]	
	retl
	nop
	.size	save_fregs,(.-save_fregs)
	.size	save_most_fregs,(.-save_most_fregs)
	
 !	save iregs all iregs skipping r1 and r2	
save_most_iregs:	
 !	stw	%r0, [THREADPTR_REG+0]         g0 - no std role                           
 ! 	stw	%r3 , [THREADPTR_REG+12]       g3 - reserved for app software
 	stw	%r4 , [THREADPTR_REG+16]     ! g4 - no std role (volatile);    ALLOCPTR   
 	stw	%r5 , [THREADPTR_REG+20]     ! g5 - no std role (volatile);    ALLOCLIMIT           
 !	stw	%r6 , [THREADPTR_REG+24]       g6 - reserved for system software          
 !	stw	%r7 , [THREADPTR_REG+28]       g7 - reserved for system software          
 	stw	%r8 , [THREADPTR_REG+32]
	stw	%r9 , [THREADPTR_REG+36]
	stw	%r10, [THREADPTR_REG+ 40]
	stw	%r11, [THREADPTR_REG+ 44]
	stw	%r12, [THREADPTR_REG+ 48]
	stw	%r13, [THREADPTR_REG+ 52]
	stw	%r14, [THREADPTR_REG+ 56]
	! skip return address/link register
	stw	%r16, [THREADPTR_REG+ 64]
	stw	%r17, [THREADPTR_REG+ 68]
	stw	%r18, [THREADPTR_REG+ 72]
	stw	%r19, [THREADPTR_REG+ 76]
	stw	%r20, [THREADPTR_REG+ 80]
	stw	%r21, [THREADPTR_REG+ 84]
	stw	%r22, [THREADPTR_REG+ 88]
	stw	%r23, [THREADPTR_REG+ 92]
 	stw	%r24, [THREADPTR_REG+ 96] 
 	stw	%r25, [THREADPTR_REG+ 100]
 	stw	%r26, [THREADPTR_REG+ 104]
 	stw	%r27, [THREADPTR_REG+ 108]
 	stw	%r28, [THREADPTR_REG+ 112]
 	stw	%r29, [THREADPTR_REG+ 116]
						! skip frame pointer so it remains valid for OS
 	stw	%r31, [THREADPTR_REG+ 124]
	stbar
	retl
	nop
	.size	save_most_iregs,(.-save_most_iregs)
					
 ! ----------------- save_regs---------------------------------
 ! saves entire register set except for the return address register
 ! does not use a stack frame or change any registers
 ! ----------------------------------------------------------
	.proc	07
	.align  4
save_regs:
 	stw	%r1 , [THREADPTR_REG+4]		! g1 - no std role(volatile);     EXNPTR
						! g2 - reserved for app software; THREADPTR
	ld	[THREADPTR_REG+8], %r1		! use g1 as temp
	cmp	%r1, %r2			! check that g2 consistent with pointed to struct
	be	save_regs_ok
	nop
	mov	%r1, %o0
	mov	%r2, %o1
	call	save_regs_fail
	nop
save_regs_ok:
	mov	%o7, %r1			! use g1 to hold return address
	call	save_most_iregs
	nop
	call	save_fregs
	nop
	mov	%r1, %o7			! restore return address
	ld	[THREADPTR_REG+4], %r1		! restore g1 which we use as temp
	retl
	nop
        .size save_regs,(.-save_regs)


save_regs_forC:		
	stw	%r1 , [THREADPTR_REG+4]		! g1 - no std role(volatile);     EXNPTR
						! g2 - reserved for app software; THREADPTR
	ld	[THREADPTR_REG+8], %r1		! use g1 as temp
	cmp	%r1, %r2			! check that g2 consistent with pointed to struct
	be	save_regs_forC_ok
	nop
	mov	%r1, %o0
	mov	%r2, %o1
	call	save_regs_fail
	nop
save_regs_forC_ok:
	mov	%o7, %r1			! use g1 to hold return address
	call	save_most_iregs
	nop
	call	save_most_fregs
	nop
	mov	%r1, %o7			! restore return address
	ld	[THREADPTR_REG+4], %r1		! restore g1 which we use as temp
	mov	1, %l0
	st	%l0, [THREADPTR_REG + notinml_disp]	! set notInML to one
	mov	THREADPTR_REG, %l0		! when calling C code, %l0 (but not %g2) is unmodified
	retl
	nop	
        .size save_regs_forC,(.-save_regs_forC)

	.proc	07
	.align  4
load_fregs:	
	ldd	[THREADPTR_REG + 128], %f0 
load_most_fregs:	
	ldd	[THREADPTR_REG + 136], %f2 
	ldd	[THREADPTR_REG + 144], %f4 
	ldd	[THREADPTR_REG + 152], %f6 
	ldd	[THREADPTR_REG + 160], %f8 
	ldd	[THREADPTR_REG + 168], %f10
	ldd	[THREADPTR_REG + 176], %f12
	ldd	[THREADPTR_REG + 184], %f14
	ldd	[THREADPTR_REG + 192], %f16
	ldd	[THREADPTR_REG + 200], %f18
	ldd	[THREADPTR_REG + 208], %f20
	ldd	[THREADPTR_REG + 216], %f22
	ldd	[THREADPTR_REG + 224], %f24
	ldd	[THREADPTR_REG + 232], %f26
	ldd	[THREADPTR_REG + 240], %f28
	ldd	[THREADPTR_REG + 248], %f30
	ldd	[THREADPTR_REG + 256], %f32
	ldd	[THREADPTR_REG + 264], %f34
	ldd	[THREADPTR_REG + 272], %f36
	ldd	[THREADPTR_REG + 280], %f38
	ldd	[THREADPTR_REG + 288], %f40
	ldd	[THREADPTR_REG + 296], %f42
	ldd	[THREADPTR_REG + 304], %f44
	ldd	[THREADPTR_REG + 312], %f46
	ldd	[THREADPTR_REG + 320], %f48
	ldd	[THREADPTR_REG + 328], %f50
	ldd	[THREADPTR_REG + 336], %f52
	ldd	[THREADPTR_REG + 344], %f54
	ldd	[THREADPTR_REG + 352], %f56
	ldd	[THREADPTR_REG + 360], %f58
	ldd	[THREADPTR_REG + 368], %f60
	ldd	[THREADPTR_REG + 376], %f62
	retl
	nop
	.size	load_fregs,(.-load_fregs)
	.size	load_most_fregs,(.-load_most_fregs)	

 !	load all regs but r1, r2, r8
load_most_iregs:	
 !	ld	[THREADPTR_REG+0], %r0         g0 - no std role              
 !	ld	[THREADPTR_REG+12], %r3        g3 - reserved for app software
 	ld	[THREADPTR_REG+16], %r4      ! g4 - no std role (volatile);    ALLOCPTR
 	ld	[THREADPTR_REG+20], %r5      ! g5 - no std role (volatile);    ALLOCLIMIT
 !	ld	[THREADPTR_REG+24], %r6        g6 - reserved for system software      
 !	ld	[THREADPTR_REG+28], %r7        g7 - reserved for system software          
	ld	[THREADPTR_REG+36], %r9 
	ld	[THREADPTR_REG+ 40], %r10
	ld	[THREADPTR_REG+ 44], %r11
	ld	[THREADPTR_REG+ 48], %r12
	ld	[THREADPTR_REG+ 52], %r13
	ld	[THREADPTR_REG+ 56], %r14
						! skip return address/link register
	ld	[THREADPTR_REG+ 64], %r16
	ld	[THREADPTR_REG+ 68], %r17
	ld	[THREADPTR_REG+ 72], %r18
	ld	[THREADPTR_REG+ 76], %r19
	ld	[THREADPTR_REG+ 80], %r20
	ld	[THREADPTR_REG+ 84], %r21
	ld	[THREADPTR_REG+ 88], %r22
	ld	[THREADPTR_REG+ 92], %r23
 	ld	[THREADPTR_REG+ 96], %r24 
 	ld	[THREADPTR_REG+ 100], %r25
 	ld	[THREADPTR_REG+ 104], %r26
 	ld	[THREADPTR_REG+ 108], %r27
 	ld	[THREADPTR_REG+ 112], %r28
 	ld	[THREADPTR_REG+ 116], %r29
						! skip frame pointer so it remains valid for OS
 	ld	[THREADPTR_REG+ 124], %r31
	retl
	nop
        .size load_most_iregs,(.-load_most_iregs)

			
 ! ----------------- load_regs---------------------------------
 ! loads entire register set except for the return address register
 ! does not use a stack frame or change any registers
 ! ----------------------------------------------------------
load_regs:	
						! g2 - reserved for app software; THREADPTR
	ld	[THREADPTR_REG+8], %r1       ! use g1 as temp for thread-structure's copy of g2
	cmp	%r1, %r2			! check that g2 consistent with temp
	be	load_regs_ok
	nop
	mov	%r1, %o0
	mov	%r2, %o1
	call	load_regs_fail
	nop
load_regs_ok:
	mov	%o7, %r1			! save return address in g1 temp
	call	load_fregs
	nop
	call	load_most_iregs
	nop	
	ld	[THREADPTR_REG+32], %r8	! load_most_iregs skips r8 so load_regs_forC can use it
	mov	%r1, %o7			! restore return address
						! g1 - no std role(volatile);     EXNPTR
	ld	[THREADPTR_REG+4], %r1       ! used g1 as temp;  restore now
	retl
	nop
        .size load_regs,(.-load_regs)


load_regs_forC:
	mov	%l0, THREADPTR_REG
	st	%g0, [THREADPTR_REG + notinml_disp]	! set notInML to zero
						! g2 - reserved for app software; THREADPTR
	ld	[THREADPTR_REG+8], %r1       ! use g1 as temp for thread-structure's copy of g2
	cmp	%r1, %r2			! check that g2 consistent with temp
	be	load_regs_forC_ok
	nop
	mov	%r1, %o0
	mov	%r2, %o1
	call	load_regs_fail
	nop
load_regs_forC_ok:
	mov	%o7, %r1			! save return address in g1 temp
	call	load_most_fregs
	nop
	call	load_most_iregs
	nop	
	mov	%r1, %o7			! restore return address
						! g1 - no std role(volatile);     EXNPTR
	ld	[THREADPTR_REG+4], %r1       ! used g1 as temp;  restore now
	retl
	nop
        .size load_regs_forC,(.-load_regs_forC)
		
		
 ! ----------------- gc_raw ---------------------------------
 ! return address comes in normal return address register
 ! temp register contains request size + heap pointer
 ! does not use a stack frame 
 ! ----------------------------------------------------------
	.proc	07
	.align  4
gc_raw:
	flushw
	stw	LINK_REG, [THREADPTR_REG + LINK_DISP] ! save link value
	call	save_regs
	nop
	ld	[THREADPTR_REG + ASMTMP_DISP], ASMTMP_REG
	ld	[THREADPTR_REG + ALLOCPTR_DISP], ALLOCPTR_REG
	sub	ASMTMP_REG, ALLOCPTR_REG, ASMTMP_REG
	stw	ASMTMP_REG, [THREADPTR_REG + request_disp]
	ld	[THREADPTR_REG + sysThread_disp], CFIRSTARG_REG ! use CFIRSTARG as temp
	ld	[CFIRSTARG_REG], SP_REG 	        ! run on system thread stack
	mov	THREADPTR_REG, CFIRSTARG_REG		! pass user thread pointer as arg
	call	gc					! call runtime GC
	nop
	call	abort
	nop
	.size gc_raw,(.-gc_raw)


 ! ----------------- old_alloc --------------------------
 ! return address comes in $26
 ! request size come in at heap limit
 ! ------------------------------------------------------
	.proc	07
	.align  4
old_alloc:
	call	abort
	nop


	.proc	07
	.align  4
load_regs2:	
	ld	[THREADPTR_REG+32], %r8 
 !	ld	[THREADPTR_REG+0], %r0         g0 - no std role              
 	ld	[THREADPTR_REG+4], %r1	! g1 - no std role(volatile);     EXNPTR
 	ld	[THREADPTR_REG+8], %r2	! g2 - reserved for app software; THREADPTR 
 !	ld	[THREADPTR_REG+12], %r3        g3 - reserved for app software
 	ld	[THREADPTR_REG+16], %r4      ! g4 - no std role (volatile);    ALLOCPTR
 	ld	[THREADPTR_REG+20], %r5      ! g5 - no std role (volatile);    ALLOCLIMIT
 !	ld	[THREADPTR_REG+24], %r6        g6 - reserved for system software      
 !	ld	[THREADPTR_REG+28], %r7        g7 - reserved for system software          
	ld	[THREADPTR_REG+36], %r9 
	ld	[THREADPTR_REG+ 40], %r10
	ld	[THREADPTR_REG+ 44], %r11
	ld	[THREADPTR_REG+ 48], %r12
	ld	[THREADPTR_REG+ 52], %r13
	ld	[THREADPTR_REG+ 56], %r14
	! skip return address/link register
	ld	[THREADPTR_REG+ 64], %r16
	ld	[THREADPTR_REG+ 68], %r17
	ld	[THREADPTR_REG+ 72], %r18
	ld	[THREADPTR_REG+ 76], %r19
	ld	[THREADPTR_REG+ 80], %r20
	ld	[THREADPTR_REG+ 84], %r21
	ld	[THREADPTR_REG+ 88], %r22
	ld	[THREADPTR_REG+ 92], %r23
  	ld	[THREADPTR_REG+ 96], %r24 
  	ld	[THREADPTR_REG+ 100], %r25
  	ld	[THREADPTR_REG+ 104], %r26
  	ld	[THREADPTR_REG+ 108], %r27
  	ld	[THREADPTR_REG+ 112], %r28
  	ld	[THREADPTR_REG+ 116], %r29
 !	ld	[THREADPTR_REG+ 120], %r30
 ! 	ld	[THREADPTR_REG+ 124], %r31

	retl
	nop
	
        .size load_regs2,(.-load_regs2)
		
 ! --------------------------------------------------------
 ! Called from the runtime with the thread pointer argument
 ! --------------------------------------------------------
	.proc	07
	.align  4
returnFromGC:
	flushw
	mov	CFIRSTARG_REG, THREADPTR_REG
 	call 	load_regs
 	nop
	ld	[THREADPTR_REG + LINK_DISP], LINK_REG
	retl
	nop
	call	abort
	nop	
	.size returnFromGC,(.-returnFromGC)

 ! --------------------------------------------------------
 ! Called from the runtime with the thread pointer argument
 ! --------------------------------------------------------
	.proc	07
	.align  4
returnFromYield:
	flushw	
	mov	CFIRSTARG_REG, THREADPTR_REG
	mov	THREADPTR_REG, %l0			! Calls from C expect thread reg in %l0
	ld	[THREADPTR_REG + SP_DISP], SP_REG	
	ld	[THREADPTR_REG + LINK_DISP], LINK_REG
	retl
	nop
	call	abort
	nop
	.size returnFromYield,(.-returnFromYield)
		
.data
.align 4
cur_alloc_ptr:	
	.word	0
	.word	0

