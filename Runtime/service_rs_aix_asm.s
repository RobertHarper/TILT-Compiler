.set r0,0; .set SP,1; .set RTOC,2; .set r3,3; .set r4,4
.set r5,5; .set r6,6; .set r7,7; .set r8,8; .set r9,9
.set r10,10; .set r11,11; .set r12,12; .set r13,13; .set r14,14
.set r15,15; .set r16,16; .set r17,17; .set r18,18; .set r19,19
.set r20,20; .set r21,21; .set r22,22; .set r23,23; .set r24,24
.set r25,25; .set r26,26; .set r27,27; .set r28,28; .set r29,29
.set r30,30; .set r31,31
.set fp0,0; .set fp1,1; .set fp2,2; .set fp3,3; .set fp4,4
.set fp5,5; .set fp6,6; .set fp7,7; .set fp8,8; .set fp9,9
.set fp10,10; .set fp11,11; .set fp12,12; .set fp13,13; .set fp14,14
.set fp15,15; .set fp16,16; .set fp17,17; .set fp18,18; .set fp19,19
.set fp20,20; .set fp21,21; .set fp22,22; .set fp23,23; .set fp24,24
.set fp25,25; .set fp26,26; .set fp27,27; .set fp28,28; .set fp29,29
.set fp30,30; .set fp31,31
.set MQ,0; .set XER,1; .set FROM_RTCU,4; .set FROM_RTCL,5; .set FROM_DEC,6
.set LR,8; .set CTR,9; .set TID,17; .set DSISR,18; .set DAR,19; .set TO_RTCU,20
.set TO_RTCL,21; .set TO_DEC,22; .set SDR_0,24; .set SDR_1,25; .set SRR_0,26
.set SRR_1,27
.set BO_dCTR_NZERO_AND_NOT,0; .set BO_dCTR_NZERO_AND_NOT_1,1
.set BO_dCTR_ZERO_AND_NOT,2; .set BO_dCTR_ZERO_AND_NOT_1,3
.set BO_IF_NOT,4; .set BO_IF_NOT_1,5; .set BO_IF_NOT_2,6
.set BO_IF_NOT_3,7; .set BO_dCTR_NZERO_AND,8; .set BO_dCTR_NZERO_AND_1,9
.set BO_dCTR_ZERO_AND,10; .set BO_dCTR_ZERO_AND_1,11; .set BO_IF,12
.set BO_IF_1,13; .set BO_IF_2,14; .set BO_IF_3,15; .set BO_dCTR_NZERO,16
.set BO_dCTR_NZERO_1,17; .set BO_dCTR_ZERO,18; .set BO_dCTR_ZERO_1,19
.set BO_ALWAYS,20; .set BO_ALWAYS_1,21; .set BO_ALWAYS_2,22
.set BO_ALWAYS_3,23; .set BO_dCTR_NZERO_8,24; .set BO_dCTR_NZERO_9,25
.set BO_dCTR_ZERO_8,26; .set BO_dCTR_ZERO_9,27; .set BO_ALWAYS_8,28
.set BO_ALWAYS_9,29; .set BO_ALWAYS_10,30; .set BO_ALWAYS_11,31
.set CR0_LT,0; .set CR0_GT,1; .set CR0_EQ,2; .set CR0_SO,3
.set CR1_FX,4; .set CR1_FEX,5; .set CR1_VX,6; .set CR1_OX,7
.set CR2_LT,8; .set CR2_GT,9; .set CR2_EQ,10; .set CR2_SO,11
.set CR3_LT,12; .set CR3_GT,13; .set CR3_EQ,14; .set CR3_SO,15
.set CR4_LT,16; .set CR4_GT,17; .set CR4_EQ,18; .set CR4_SO,19
.set CR5_LT,20; .set CR5_GT,21; .set CR5_EQ,22; .set CR5_SO,23
.set CR6_LT,24; .set CR6_GT,25; .set CR6_EQ,26; .set CR6_SO,27
.set CR7_LT,28; .set CR7_GT,29; .set CR7_EQ,30; .set CR7_SO,31
.set TO_LT,16; .set TO_GT,8; .set TO_EQ,4; .set TO_LLT,2; .set TO_LGT,1


 	.globl	.start_client
	.globl  global_exnrec
        .globl  GetRpcc
	.globl	.raise_exception_raw
	.globl	Overflow_exncon
#	.globl	Divide_exncon
	.globl	.context_restore
#	.globl  overflow_handler
	.extern NOTINML
        .extern .abort{PR}
        .extern .thread_finish{PR}
        .extern .toplevel_exnhandler{PR}	
.toc
T.NOTINML:	     .tc     NOTINML{TC}, NOTINML
T.global_exnrec:     .tc     global_exnrec{TC}, global_exnrec
	
	.csect  general_data{RW}
#	.globl	Prelude_31_Overflow_1
#	.globl	Prelude_57_Div_1
#Overflow_exncon:	
#	.long	Prelude_31_Overflow_1
#Divide_exncon:		
#	.long	Prelude_57_Div_1
global_exnrec:		
	.long	global_exnhandler
	.long	0
	.long	0
	
	
	.csect  general_code{PR}
	.align 3	
GetRpcc:
        cal     r3,-1(r0)
        bl      .abort{PR}	# no rpcc on aix
	cror	r31, r31, r31
			
global_exnhandler:
	# don't bother saving return address because we are not coming back here		
	subi	SP, SP, 320
 # save registers to stack, starting at 56
	stw	r0, 56(SP)
	addi	r0, SP, 640	# we need to save sp of the caller of GC
	stw	r0,  60(SP)
	stw	RTOC, 64(SP)
	stw	r3, 68(SP)
	stw	r4, 72(SP)
	stw	r5, 76(SP)
	stw	r6, 80(SP)
	stw	r7, 84(SP)
	stw	r8, 88(SP)
	stw	r9, 92(SP)
	stw	r10, 96(SP)
	stw	r11, 100(SP)
	stw	r12, 104(SP)
	stw	r13, 108(SP)
	stw	r14, 112(SP)
	stw	r15, 116(SP)
	stw	r16, 120(SP)
	stw	r17, 124(SP)
	stw	r18, 128(SP)
	stw	r19, 132(SP)
	stw	r20, 136(SP)
	stw	r21, 140(SP)
	stw	r22, 144(SP)
	stw	r23, 148(SP)
	stw	r24, 152(SP)
	stw	r25, 156(SP)
	stw	r26, 160(SP)			
	stw	r27, 164(SP)
	stw	r28, 168(SP)
	stw	r29, 172(SP)
	stw	r30, 176(SP)
	stw	r31, 180(SP)
	cal	r3, 56(SP)	
	bl	.toplevel_exnhandler{PR}
	cror	r31, r31, r31	
        bl      .abort{PR}	# should not return from toplevel_exnhandler
	cror	r31, r31, r31	
	
overflow_handler:	
        cal     r3,-1(r0)
        bl      .abort{PR}	# overflow handler not implemented XXX
	cror	r31, r31, r31	
		
			
.start_client:
	mtfsb1	30		# set the 30th and 31st bit of FP-control status register
	mtfsb1	31		#   so we have rounding towards negative infinite when we do fctiw
	stw	r31, 8(SP)	# save ret address
    	subi	SP, SP, 320
	mfspr	r31, LR
	stw	r3, 24(SP)	# save arg0: new stack
	stw	r4, 28(SP)	# save arg1: alloc ptr val
	stw	r5, 32(SP)	# save arg2: alloc limit val
	stw	r6, 36(SP)	# save arg3: client_entry
	stw	r7, 40(SP)	# save arg4: num_add
	lil	r29, 0
	stw	r29, 48(SP)	# initialize current thunk to run
	lwz	r28, 28(SP)	# initilize heap ptr/limit outside loop
	lwz	r26, 32(SP)
thunk_loop:
	lil	r21, 1200	# nuke regs for debugging
	lil	r22, 1201
	lil	r23, 1202
	lil	r24, 1203
	lil	r25, 1204
	lil	r27, 1206
	l	r31, 36(SP)	# fetch start_client array address
	l	r29, 48(SP)	# fetch current thunk counter
	mulli	r29, r29, 4	# compute offset for int-sized things
	add	r29, r29, r31	# compute array item address
	l	r29, 0(r29)	# fetch current thunk address
	l	r31, 24(SP)	# fetch stack argument
				# note that first 12 bytes used by callee
	stw	SP, -8(r31)	# save own stack pointer on new stack
	cal	SP, -32(r31)	# set SP to new stack, below where old SP was saved
	l	r27, T.global_exnrec(RTOC)	# load global_exnrec address
	l	r31, T.NOTINML(RTOC)		# going to ML!!!
	lil	r30, 0
	stw	r30, 0(r31)			# NOTINML is 0 now
	mtspr	LR, r29
	blrl					# cross-TOC call to ML
	cror	r31, r31, r31			# returning from mutator
	l	SP, 24(SP)	# restore own stack
	l	r29, 48(SP)	# fetch current thunk counter
	addi	r29, r29, 1	# increment counter
	stw	r29, 48(SP)	# save current thunk counter
	l	r31, 40(SP)	# fetch counter limit
	cmp	0, r29, r31
	blt	0, thunk_loop	# branch back if counter<limit
 # save registers to stack, starting at 56
	stw	r0, 56(SP)
	addi	r0, SP, 640	# we need to save sp of the caller of GC
	stw	r0,  60(SP)
	stw	RTOC, 64(SP)
	stw	r3, 68(SP)
	stw	r4, 72(SP)
	stw	r5, 76(SP)
	stw	r6, 80(SP)
	stw	r7, 84(SP)
	stw	r8, 88(SP)
	stw	r9, 92(SP)
	stw	r10, 96(SP)
	stw	r11, 100(SP)
	stw	r12, 104(SP)
	stw	r13, 108(SP)
	stw	r14, 112(SP)
	stw	r15, 116(SP)
	stw	r16, 120(SP)
	stw	r17, 124(SP)
	stw	r18, 128(SP)
	stw	r19, 132(SP)
	stw	r20, 136(SP)
	stw	r21, 140(SP)
	stw	r22, 144(SP)
	stw	r23, 148(SP)
	stw	r24, 152(SP)
	stw	r25, 156(SP)
	stw	r26, 160(SP)			
	stw	r27, 164(SP)
	stw	r28, 168(SP)
	stw	r29, 172(SP)
	stw	r30, 176(SP)
	stw	r31, 180(SP)

	cal	r3, 56(SP)		# thread_finish wants saveregs
        bl      .thread_finish{PR}	# we are done
	cror	r31, r31, r31
        bl      .abort{PR}	# should not come back from thread_finish
	cror	r31, r31, r31
        addi    SP, SP, 320	# deallocate frame and return
	lwz     r31, 8(SP)	# restore return address
        mtspr   LR, r31
        blr
		

.raise_exception_raw:
        subi	SP, SP, 64
	stw	r3, 24(SP)	# save where regs are
	stw	r4, 28(SP)	# save exn value
	stw	r5, 32(SP)	# save handler address

	lwz	r0, 0(r3)	# restore registers
				# skip SP
				# skip RTOC
				# skip C regs (r3 to r8)
	lwz	r9, 36(r3)
	lwz	r10, 40(r3)
	lwz	r11, 44(r3)
	lwz	r12, 48(r3)
	lwz	r13, 52(r3)
	lwz	r14, 56(r3)			
	lwz	r15, 60(r3)
	lwz	r16, 64(r3)
	lwz	r17, 68(r3)
	lwz	r18, 72(r3)
	lwz	r19, 76(r3)
	lwz	r20, 80(r3)
	lwz	r21, 84(r3)
	lwz	r22, 88(r3)
	lwz	r23, 92(r3)
	lwz	r24, 96(r3)			
	lwz	r25, 100(r3)
	lwz	r26, 104(r3)	# must restoer alloc limit ptr
	lwz	r27, 108(r3)	# must restoer exn ptr
	lwz	r28, 112(r3)	# must restoer alloc ptr
				# skip exn arg
				# skip temp2 = r30
				# skip temp = r31
	lwz	r29, 28(SP)	# restore exn arg
	lwz	r31, 32(SP)	# restore target address and
	mtlr	r31		#	move to LR
	lwz	SP, 4(r3)	# restore SP from saved regs at end
	blr			

			
.context_restore:
        cal     r3,-1(r0)
        bl      .abort{PR}	# context_restore not implemented XXX
	cror	r31, r31, r31
		
