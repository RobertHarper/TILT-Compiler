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

	.globl	gc_raw
	.globl	float_alloc_raw
	.globl	int_alloc_raw
	.globl	ptr_alloc_raw
	.extern	.float_alloc{PR}
	.extern	.int_alloc{PR}
	.extern	.ptr_alloc{PR}
        .extern .abort{PR}
        .extern .gc_handler{PR}
	
	.csect  general_code{PR}
	.align 3
	# our local storage begins at 
	#   24 + 4 * num of args to gc =
	#   24 + 4 * 5 = 44
	# request size comes in at heap limit
gc_raw:
	mflr	r0
        stw     r0, 8(SP)      # save ret address
        subi    SP, SP, 640
	stw	r26, 44(SP)	# heap limit = req_size
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

 # now save all the fps
	stfd	fp0, 320(SP)
	stfd	fp1, 328(SP)
	stfd	fp2, 336(SP)
	stfd	fp3, 344(SP)
	stfd	fp4, 352(SP)
	stfd	fp5, 360(SP)
	stfd	fp6, 368(SP)
	stfd	fp7, 376(SP)
	stfd	fp8, 384(SP)
	stfd	fp9, 392(SP)
	stfd	fp10, 400(SP)
	stfd	fp11, 408(SP)
	stfd	fp12, 416(SP)
	stfd	fp13, 424(SP)
	stfd	fp14, 432(SP)
	stfd	fp15, 440(SP)
	stfd	fp16, 448(SP)
	stfd	fp17, 456(SP)
	stfd	fp18, 464(SP)
	stfd	fp19, 472(SP)
	stfd	fp20, 480(SP)
	stfd	fp21, 488(SP)
	stfd	fp22, 496(SP)
	stfd	fp23, 504(SP)
	stfd	fp24, 512(SP)
	stfd	fp25, 520(SP)
	stfd	fp26, 528(SP)
	stfd	fp27, 536(SP)
	stfd	fp28, 544(SP)
	stfd	fp29, 552(SP)
	stfd	fp30, 560(SP)
	stfd	fp31, 568(SP)

	addi	r3, SP, 56	# saved int regs
	addi	r4, SP, 640	# sp of caller
	lwz	r5, 648(SP)	# our ret_add
	addi	r5, r5, 4       # since the call to use is cross-toc
		                # the label for the return is 4 more
	lwz	r6, 44(SP)	# req_size
	lil	r7, 0		# not a majorGC
	bl	.gc_handler{PR}
	cror	r31, r31, r31

 # restore all the fps
	lfd	fp0, 320(SP)
	lfd	fp1, 328(SP)
	lfd	fp2, 336(SP)
	lfd	fp3, 344(SP)
	lfd	fp4, 352(SP)
	lfd	fp5, 360(SP)
	lfd	fp6, 368(SP)
	lfd	fp7, 376(SP)
	lfd	fp8, 384(SP)
	lfd	fp9, 392(SP)
	lfd	fp10, 400(SP)
	lfd	fp11, 408(SP)
	lfd	fp12, 416(SP)
	lfd	fp13, 424(SP)
	lfd	fp14, 432(SP)
	lfd	fp15, 440(SP)
	lfd	fp16, 448(SP)
	lfd	fp17, 456(SP)
	lfd	fp18, 464(SP)
	lfd	fp19, 472(SP)
	lfd	fp20, 480(SP)
	lfd	fp21, 488(SP)
	lfd	fp22, 496(SP)
	lfd	fp23, 504(SP)
	lfd	fp24, 512(SP)
	lfd	fp25, 520(SP)
	lfd	fp26, 528(SP)
	lfd	fp27, 536(SP)
	lfd	fp28, 544(SP)
	lfd	fp29, 552(SP)
	lfd	fp30, 560(SP)
	lfd	fp31, 568(SP)

 # restore registers to stack, starting at 56
	lwz	r0, 56(SP)
				# we do not need to or want to restore $r1 = SP
				# for it contains the sp of the caller
	lwz	RTOC, 64(SP)
	lwz	r3, 68(SP)
	lwz	r4, 72(SP)
	lwz	r5, 76(SP)
	lwz	r6, 80(SP)
	lwz	r7, 84(SP)
	lwz	r8, 88(SP)
	lwz	r9, 92(SP)
	lwz	r10, 96(SP)
	lwz	r11, 100(SP)
	lwz	r12, 104(SP)
	lwz	r13, 108(SP)
	lwz	r14, 112(SP)
	lwz	r15, 116(SP)
	lwz	r16, 120(SP)
	lwz	r17, 124(SP)
	lwz	r18, 128(SP)
	lwz	r19, 132(SP)
	lwz	r20, 136(SP)
	lwz	r21, 140(SP)
	lwz	r22, 144(SP)
	lwz	r23, 148(SP)
	lwz	r24, 152(SP)
	lwz	r25, 156(SP)
	lwz	r26, 160(SP)			
	lwz	r27, 164(SP)
	lwz	r28, 168(SP)
	lwz	r29, 172(SP)
	lwz	r30, 176(SP)
	lwz	r31, 180(SP)

        addi    SP, SP, 640
        lwz     r0, 8(SP)      # restore ret address
	mtlr	r0
	blr	
	

float_alloc_raw:
        mflr    r0
        stw     r0, 8(SP)      # save ret address
        subi    SP, SP, 640
	stw	r31, 600(SP)	# at = request size
	stw	r22, 604(SP)	# optional profile tag = alloc_limit

 # save registers to stack, starting at 56
        stw     r0, 56(SP)
        addi    r0, SP, 640     # we need to save sp of the caller of GC
        stw     r0,  60(SP)
        stw     RTOC, 64(SP)
        stw     r3, 68(SP)
        stw     r4, 72(SP)
        stw     r5, 76(SP)
        stw     r6, 80(SP)
        stw     r7, 84(SP)
        stw     r8, 88(SP)
        stw     r9, 92(SP)
        stw     r10, 96(SP)
        stw     r11, 100(SP)
        stw     r12, 104(SP)
        stw     r13, 108(SP)
        stw     r14, 112(SP)
        stw     r15, 116(SP)
        stw     r16, 120(SP)
        stw     r17, 124(SP)
        stw     r18, 128(SP)
        stw     r19, 132(SP)
        stw     r20, 136(SP)
        stw     r21, 140(SP)
        stw     r22, 144(SP)
        stw     r23, 148(SP)
        stw     r24, 152(SP)
        stw     r25, 156(SP)
        stw     r26, 160(SP)
        stw     r27, 164(SP)
        stw     r28, 168(SP)
        stw     r29, 172(SP)
        stw     r30, 176(SP)
        stw     r31, 180(SP)

 # now save all the fps
        stfd    fp0, 320(SP)
        stfd    fp1, 328(SP)
        stfd    fp2, 336(SP)
        stfd    fp3, 344(SP)
        stfd    fp4, 352(SP)
        stfd    fp5, 360(SP)
        stfd    fp6, 368(SP)
        stfd    fp7, 376(SP)
        stfd    fp8, 384(SP)
        stfd    fp9, 392(SP)
        stfd    fp10, 400(SP)
        stfd    fp11, 408(SP)
        stfd    fp12, 416(SP)
        stfd    fp13, 424(SP)
        stfd    fp14, 432(SP)
        stfd    fp15, 440(SP)
        stfd    fp16, 448(SP)
        stfd    fp17, 456(SP)
        stfd    fp18, 464(SP)
        stfd    fp19, 472(SP)
        stfd    fp20, 480(SP)
        stfd    fp21, 488(SP)
        stfd    fp22, 496(SP)
        stfd    fp23, 504(SP)
        stfd    fp24, 512(SP)
        stfd    fp25, 520(SP)
        stfd    fp26, 528(SP)
        stfd    fp27, 536(SP)
        stfd    fp28, 544(SP)
        stfd    fp29, 552(SP)
        stfd    fp30, 560(SP)
        stfd    fp31, 568(SP)

	cal	r3, 56(SP)	# pass addres of saved regs
	cal	r4, 640(SP)	# pass sp of caller of float_alloc
	mflr	r5		# self return-address
	addi	r5, r5, 4       # since the call to use is cross-toc
		                # the label for the return is 4 more
	
	lwz	r6, 600(SP)	# request size
	fmr	fp1, fp0	# $fp0 is ML first arg and $fp1 is the C first arg ;  note 2 GP "reserved"
	lwz	r9, 604(SP)
	bl	.float_alloc{PR}	# go to C code
	cror	r31, r31, r31		#	 with stylized no-op for loader to patch
	stw	r3, 576(SP)	# save result on stack
	
 # restore all the fps
        lfd     fp0, 320(SP)
        lfd     fp1, 328(SP)
        lfd     fp2, 336(SP)
        lfd     fp3, 344(SP)
        lfd     fp4, 352(SP)
        lfd     fp5, 360(SP)
        lfd     fp6, 368(SP)
        lfd     fp7, 376(SP)
        lfd     fp8, 384(SP)
        lfd     fp9, 392(SP)
        lfd     fp10, 400(SP)
        lfd     fp11, 408(SP)
        lfd     fp12, 416(SP)
        lfd     fp13, 424(SP)
        lfd     fp14, 432(SP)
        lfd     fp15, 440(SP)
        lfd     fp16, 448(SP)
        lfd     fp17, 456(SP)
        lfd     fp18, 464(SP)
        lfd     fp19, 472(SP)
        lfd     fp20, 480(SP)
        lfd     fp21, 488(SP)
        lfd     fp22, 496(SP)
        lfd     fp23, 504(SP)
        lfd     fp24, 512(SP)
        lfd     fp25, 520(SP)
        lfd     fp26, 528(SP)
        lfd     fp27, 536(SP)
        lfd     fp28, 544(SP)
        lfd     fp29, 552(SP)
        lfd     fp30, 560(SP)
        lfd     fp31, 568(SP)

 # restore registers to stack, starting at 56
        lwz     r0, 56(SP)
                                # we do not need to or want to restore $r1 = SP
                                # for it contains the sp of the caller
        lwz     RTOC, 64(SP)
        lwz     r3, 68(SP)
        lwz     r4, 72(SP)
        lwz     r5, 76(SP)
        lwz     r6, 80(SP)
        lwz     r7, 84(SP)
        lwz     r8, 88(SP)
        lwz     r9, 92(SP)
        lwz     r10, 96(SP)
        lwz     r11, 100(SP)
        lwz     r12, 104(SP)
        lwz     r13, 108(SP)
        lwz     r14, 112(SP)
        lwz     r15, 116(SP)
        lwz     r16, 120(SP)
        lwz     r17, 124(SP)
        lwz     r18, 128(SP)
        lwz     r19, 132(SP)
        lwz     r20, 136(SP)
        lwz     r21, 140(SP)
        lwz     r22, 144(SP)
        lwz     r23, 148(SP)
        lwz     r24, 152(SP)
        lwz     r25, 156(SP)
        lwz     r26, 160(SP)
        lwz     r27, 164(SP)
        lwz     r28, 168(SP)
        lwz     r29, 172(SP)
        lwz     r30, 176(SP)
        lwz     r31, 180(SP)

	lwz	r31, 576(SP)	# get return and return in temp
        addi    SP, SP, 640
        lwz     r0, 8(SP)      # restore ret address
        mtlr    r0
        blr
	
int_alloc_raw:
        mflr    r0
        stw     r0, 8(SP)      # save ret address
        subi    SP, SP, 640
	stw	r31, 600(SP)	# at = request size
 #	stw	???, 604(SP)	# non-existent profile tag 
	stw	r30, 608(SP)	# at2 = value to fill array
	
 # save registers to stack, starting at 56
        stw     r0, 56(SP)
        addi    r0, SP, 640     # we need to save sp of the caller of GC
        stw     r0,  60(SP)
        stw     RTOC, 64(SP)
        stw     r3, 68(SP)
        stw     r4, 72(SP)
        stw     r5, 76(SP)
        stw     r6, 80(SP)
        stw     r7, 84(SP)
        stw     r8, 88(SP)
        stw     r9, 92(SP)
        stw     r10, 96(SP)
        stw     r11, 100(SP)
        stw     r12, 104(SP)
        stw     r13, 108(SP)
        stw     r14, 112(SP)
        stw     r15, 116(SP)
        stw     r16, 120(SP)
        stw     r17, 124(SP)
        stw     r18, 128(SP)
        stw     r19, 132(SP)
        stw     r20, 136(SP)
        stw     r21, 140(SP)
        stw     r22, 144(SP)
        stw     r23, 148(SP)
        stw     r24, 152(SP)
        stw     r25, 156(SP)
        stw     r26, 160(SP)
        stw     r27, 164(SP)
        stw     r28, 168(SP)
        stw     r29, 172(SP)
        stw     r30, 176(SP)
        stw     r31, 180(SP)

 # now save all the fps
        stfd    fp0, 320(SP)
        stfd    fp1, 328(SP)
        stfd    fp2, 336(SP)
        stfd    fp3, 344(SP)
        stfd    fp4, 352(SP)
        stfd    fp5, 360(SP)
        stfd    fp6, 368(SP)
        stfd    fp7, 376(SP)
        stfd    fp8, 384(SP)
        stfd    fp9, 392(SP)
        stfd    fp10, 400(SP)
        stfd    fp11, 408(SP)
        stfd    fp12, 416(SP)
        stfd    fp13, 424(SP)
        stfd    fp14, 432(SP)
        stfd    fp15, 440(SP)
        stfd    fp16, 448(SP)
        stfd    fp17, 456(SP)
        stfd    fp18, 464(SP)
        stfd    fp19, 472(SP)
        stfd    fp20, 480(SP)
        stfd    fp21, 488(SP)
        stfd    fp22, 496(SP)
        stfd    fp23, 504(SP)
        stfd    fp24, 512(SP)
        stfd    fp25, 520(SP)
        stfd    fp26, 528(SP)
        stfd    fp27, 536(SP)
        stfd    fp28, 544(SP)
        stfd    fp29, 552(SP)
        stfd    fp30, 560(SP)
        stfd    fp31, 568(SP)

	cal	r3, 56(SP)	# pass addres of saved regs
	cal	r4, 640(SP)	# pass sp of caller of int_alloc
	mflr	r5		# self return-address
	addi	r5, r5, 4       # since the call to use is cross-toc
		                # the label for the return is 4 more
	lwz	r6, 600(SP)	# request size
	lwz	r7, 608(SP)	# value to fill array with
	lil	r8, 0
	bl	.int_alloc{PR}	# go to C code
	cror	r31, r31, r31		#	 with stylized no-op for loader to patch
	stw	r3, 576(SP)	# save result on stack
	
 # restore all the fps
        lfd     fp0, 320(SP)
        lfd     fp1, 328(SP)
        lfd     fp2, 336(SP)
        lfd     fp3, 344(SP)
        lfd     fp4, 352(SP)
        lfd     fp5, 360(SP)
        lfd     fp6, 368(SP)
        lfd     fp7, 376(SP)
        lfd     fp8, 384(SP)
        lfd     fp9, 392(SP)
        lfd     fp10, 400(SP)
        lfd     fp11, 408(SP)
        lfd     fp12, 416(SP)
        lfd     fp13, 424(SP)
        lfd     fp14, 432(SP)
        lfd     fp15, 440(SP)
        lfd     fp16, 448(SP)
        lfd     fp17, 456(SP)
        lfd     fp18, 464(SP)
        lfd     fp19, 472(SP)
        lfd     fp20, 480(SP)
        lfd     fp21, 488(SP)
        lfd     fp22, 496(SP)
        lfd     fp23, 504(SP)
        lfd     fp24, 512(SP)
        lfd     fp25, 520(SP)
        lfd     fp26, 528(SP)
        lfd     fp27, 536(SP)
        lfd     fp28, 544(SP)
        lfd     fp29, 552(SP)
        lfd     fp30, 560(SP)
        lfd     fp31, 568(SP)

 # restore registers to stack, starting at 56
        lwz     r0, 56(SP)
                                # we do not need to or want to restore $r1 = SP
                                # for it contains the sp of the caller
        lwz     RTOC, 64(SP)
        lwz     r3, 68(SP)
        lwz     r4, 72(SP)
        lwz     r5, 76(SP)
        lwz     r6, 80(SP)
        lwz     r7, 84(SP)
        lwz     r8, 88(SP)
        lwz     r9, 92(SP)
        lwz     r10, 96(SP)
        lwz     r11, 100(SP)
        lwz     r12, 104(SP)
        lwz     r13, 108(SP)
        lwz     r14, 112(SP)
        lwz     r15, 116(SP)
        lwz     r16, 120(SP)
        lwz     r17, 124(SP)
        lwz     r18, 128(SP)
        lwz     r19, 132(SP)
        lwz     r20, 136(SP)
        lwz     r21, 140(SP)
        lwz     r22, 144(SP)
        lwz     r23, 148(SP)
        lwz     r24, 152(SP)
        lwz     r25, 156(SP)
        lwz     r26, 160(SP)
        lwz     r27, 164(SP)
        lwz     r28, 168(SP)
        lwz     r29, 172(SP)
        lwz     r30, 176(SP)
        lwz     r31, 180(SP)

	lwz	r31, 576(SP)	# get return and return in temp
        addi    SP, SP, 640
        lwz     r0, 8(SP)      # restore ret address
        mtlr    r0
        blr


	
ptr_alloc_raw:
        mflr    r0
        stw     r0, 8(SP)      # save ret address
        subi    SP, SP, 640
	stw	r31, 600(SP)	# at = request size
 #	stw	???, 604(SP)	# non-existent profile tag 
	stw	r30, 608(SP)	# at2 = value to fill array
	
 # save registers to stack, starting at 56
        stw     r0, 56(SP)
        addi    r0, SP, 640     # we need to save sp of the caller of GC
        stw     r0,  60(SP)
        stw     RTOC, 64(SP)
        stw     r3, 68(SP)
        stw     r4, 72(SP)
        stw     r5, 76(SP)
        stw     r6, 80(SP)
        stw     r7, 84(SP)
        stw     r8, 88(SP)
        stw     r9, 92(SP)
        stw     r10, 96(SP)
        stw     r11, 100(SP)
        stw     r12, 104(SP)
        stw     r13, 108(SP)
        stw     r14, 112(SP)
        stw     r15, 116(SP)
        stw     r16, 120(SP)
        stw     r17, 124(SP)
        stw     r18, 128(SP)
        stw     r19, 132(SP)
        stw     r20, 136(SP)
        stw     r21, 140(SP)
        stw     r22, 144(SP)
        stw     r23, 148(SP)
        stw     r24, 152(SP)
        stw     r25, 156(SP)
        stw     r26, 160(SP)
        stw     r27, 164(SP)
        stw     r28, 168(SP)
        stw     r29, 172(SP)
        stw     r30, 176(SP)
        stw     r31, 180(SP)

 # now save all the fps
        stfd    fp0, 320(SP)
        stfd    fp1, 328(SP)
        stfd    fp2, 336(SP)
        stfd    fp3, 344(SP)
        stfd    fp4, 352(SP)
        stfd    fp5, 360(SP)
        stfd    fp6, 368(SP)
        stfd    fp7, 376(SP)
        stfd    fp8, 384(SP)
        stfd    fp9, 392(SP)
        stfd    fp10, 400(SP)
        stfd    fp11, 408(SP)
        stfd    fp12, 416(SP)
        stfd    fp13, 424(SP)
        stfd    fp14, 432(SP)
        stfd    fp15, 440(SP)
        stfd    fp16, 448(SP)
        stfd    fp17, 456(SP)
        stfd    fp18, 464(SP)
        stfd    fp19, 472(SP)
        stfd    fp20, 480(SP)
        stfd    fp21, 488(SP)
        stfd    fp22, 496(SP)
        stfd    fp23, 504(SP)
        stfd    fp24, 512(SP)
        stfd    fp25, 520(SP)
        stfd    fp26, 528(SP)
        stfd    fp27, 536(SP)
        stfd    fp28, 544(SP)
        stfd    fp29, 552(SP)
        stfd    fp30, 560(SP)
        stfd    fp31, 568(SP)

	cal	r3, 56(SP)	# pass addres of saved regs
	cal	r4, 640(SP)	# pass sp of caller of ptr_alloc
	mflr	r5		# self return-address
	addi	r5, r5, 4       # since the call to use is cross-toc
		                # the label for the return is 4 more
	lwz	r6, 600(SP)	# request size
	lwz	r7, 608(SP)	# value to fill array with
	lil	r8, 0
	bl	.ptr_alloc{PR}	# go to C code
	cror	r31, r31, r31		#	 with stylized no-op for loader to patch
	stw	r3, 576(SP)	# save result on stack
	
 # restore all the fps
        lfd     fp0, 320(SP)
        lfd     fp1, 328(SP)
        lfd     fp2, 336(SP)
        lfd     fp3, 344(SP)
        lfd     fp4, 352(SP)
        lfd     fp5, 360(SP)
        lfd     fp6, 368(SP)
        lfd     fp7, 376(SP)
        lfd     fp8, 384(SP)
        lfd     fp9, 392(SP)
        lfd     fp10, 400(SP)
        lfd     fp11, 408(SP)
        lfd     fp12, 416(SP)
        lfd     fp13, 424(SP)
        lfd     fp14, 432(SP)
        lfd     fp15, 440(SP)
        lfd     fp16, 448(SP)
        lfd     fp17, 456(SP)
        lfd     fp18, 464(SP)
        lfd     fp19, 472(SP)
        lfd     fp20, 480(SP)
        lfd     fp21, 488(SP)
        lfd     fp22, 496(SP)
        lfd     fp23, 504(SP)
        lfd     fp24, 512(SP)
        lfd     fp25, 520(SP)
        lfd     fp26, 528(SP)
        lfd     fp27, 536(SP)
        lfd     fp28, 544(SP)
        lfd     fp29, 552(SP)
        lfd     fp30, 560(SP)
        lfd     fp31, 568(SP)

 # restore registers to stack, starting at 56
        lwz     r0, 56(SP)
                                # we do not need to or want to restore $r1 = SP
                                # for it contains the sp of the caller
        lwz     RTOC, 64(SP)
        lwz     r3, 68(SP)
        lwz     r4, 72(SP)
        lwz     r5, 76(SP)
        lwz     r6, 80(SP)
        lwz     r7, 84(SP)
        lwz     r8, 88(SP)
        lwz     r9, 92(SP)
        lwz     r10, 96(SP)
        lwz     r11, 100(SP)
        lwz     r12, 104(SP)
        lwz     r13, 108(SP)
        lwz     r14, 112(SP)
        lwz     r15, 116(SP)
        lwz     r16, 120(SP)
        lwz     r17, 124(SP)
        lwz     r18, 128(SP)
        lwz     r19, 132(SP)
        lwz     r20, 136(SP)
        lwz     r21, 140(SP)
        lwz     r22, 144(SP)
        lwz     r23, 148(SP)
        lwz     r24, 152(SP)
        lwz     r25, 156(SP)
        lwz     r26, 160(SP)
        lwz     r27, 164(SP)
        lwz     r28, 168(SP)
        lwz     r29, 172(SP)
        lwz     r30, 176(SP)
        lwz     r31, 180(SP)

	lwz	r31, 576(SP)	# get return and return in temp
        addi    SP, SP, 640
        lwz     r0, 8(SP)      # restore ret address
        mtlr    r0
        blr

	
