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

	.globl	.ml_input
	.globl	ml_input_gcentry
	.globl	.ml_lookahead
	.globl	ml_lookahead_gcentry	

	
	.extern gc_raw
	.extern .mla_input
	.extern .mla_lookahead
	.extern cur_alloc_limit
	.extern cur_alloc_pointer	

.toc
T.cur_alloc_limit:      .tc     cur_alloc_limit{TC}, cur_alloc_limit
T.cur_alloc_pointer:    .tc     cur_alloc_pointer{TC}, cur_alloc_pointer

		
	.csect  general_code{PR}
	.align 3
	# 31 is temp
	# 26 is heap limit
	# 28 is heap ptr
	# 3 and 4 are arguments: descriptor, size
.ml_input:
        mfspr   r31, LR		# get return address
        stw     r31, 8(SP)	# and save on stack
        subi	SP, SP, 32	# allocate a frame
	addi	r31, r28, 36    # overhead for string
	addi    r31, r31, r4    # variable portion
	cmpl	0, 0, r26, r31  # compare 
        bc      0, 0, ml_input_gcret
ml_input_dogc:
	addi	r26, r4, 36     # load req sz into hlimit
        bl      gc_raw
        cror    r31,r31,r31
ml_input_gcret:	
        l       r31, T.cur_alloc_pointer(RTOC)
        la      r31, 0(r31)	# now r31 has adddress of cur_alloc_pointer
        lwz     r28, 0(r31)
        l       r31, T.cur_alloc_limit(RTOC)
        la      r31, 0(r31)	# now r31 has adddress of cur_alloc_limit
        lwz     r26, 0(r31)
	bl	.mla_input
	cror	r31,r31,r31
        addi    SP, SP, 32
        lwz     r31, 8(SP)	# restore ret add to link reg
        mtspr   LR, r31
        blr

        .csect  general_data{RW}
ml_input_gcentry:
	# -------- hand-crafted GC entry for input(see stack.c)
        # -------- label,sizes,reg
	.long ml_input_gcret
#ifdef GCTABLE_HASENTRYID
	.long 99998
#define ml_input_gcentrysize 6
	.long 6  # computed from (ml_input_gcentrysize + (8<<10) + (0<<19) + (0<<28))
#else
#define ml_input_gcentrysize 5
	.long 5  # computed from (ml_input_gcentrysize + (8<<10) + (0<<19) + (0<<28))
#endif
	.long 0xffffffff # represents all callee-save; should be save to use all callee save for now ;
	.long 0xffffffff # represents all callee-save; should be save to use all callee save for now ; 
		# stacktrace    no traceable in this frame
	.long 0
		# bytedata      no data

	


	.csect  general_code{PR}
	.align 3
	# 31 is temp
	# 26 is heap limit
	# 28 is heap ptr
	# 3 and 4 are arguments: descriptor, size
.ml_lookahead:
        mfspr   r31, LR		# get return address
        stw     r31, 8(SP)	# and save on stack
        subi	SP, SP, 32	# allocate a frame
	addi	r31, r28, 36    # room to store a 0 or 1 char string
	cmpl	0, 0, r26, r31  # compare 
        bc      0, 0, ml_lookahead_gcret
ml_lookahead_dogc:
	lil	r26, 36         # load req sz into hlimit
        bl      gc_raw
        cror    r31,r31,r31
ml_lookahead_gcret:	
        l       r31, T.cur_alloc_pointer(RTOC)
        la      r31, 0(r31)	# now r31 has adddress of cur_alloc_pointer
        lwz     r28, 0(r31)
        l       r31, T.cur_alloc_limit(RTOC)
        la      r31, 0(r31)	# now r31 has adddress of cur_alloc_limit
        lwz     r26, 0(r31)
	bl	.mla_lookahead
	cror	r31,r31,r31
        addi    SP, SP, 32
        lwz     r31, 8(SP)	# restore ret add to link reg
        mtspr   LR, r31
        blr

        .csect  general_data{RW}
ml_lookahead_gcentry:
	# -------- hand-crafted GC entry for lookahead(see stack.c)
        # -------- label,sizes,reg
	.long ml_lookahead_gcret
#ifdef GCTABLE_HASENTRYID
	.long 99998
#define ml_lookahead_gcentrysize 6
	.long 6  # computed from (ml_lookahead_gcentrysize + (8<<10) + (0<<19) + (0<<28))
#else
#define ml_lookahead_gcentrysize 5
	.long 5  # computed from (ml_lookahead_gcentrysize + (8<<10) + (0<<19) + (0<<28))
#endif
	.long 0xffffffff # represents all callee-save; should be save to use all callee save for now ;
	.long 0xffffffff # represents all callee-save; should be save to use all callee save for now ; 
		# stacktrace    no traceable in this frame
	.long 0
		# bytedata      no data
	
