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

	.toc
        .extern snapshots_saved_ra_add
	.extern exnstub_maxsp
	.extern exn_codeptr_table
T.snapshots_saved_ra_add:  .tc     snapshots_saved_ra_add{TC}, snapshots_saved_ra_add
T.exnstub_maxsp:	   .tc	   exnstub_maxsp{TC}, exnstub_maxsp
T.exn_codeptr_table:	   .tc	   exn_codeptr_table{TC}, exn_codeptr_table
		
	.globl	stack_stub_00
	.globl	stack_stub_01
	.globl	stack_stub_02
	.globl	stack_stub_03
	.globl	stack_stub_04
	.globl	stack_stub_05
	.globl	stack_stub_06
	.globl	stack_stub_07
	.globl	stack_stub_08
	.globl	stack_stub_09
	.globl	stack_stub_10
	.globl	stack_stub_11
	.globl	stack_stub_12
	.globl	stack_stub_13
	.globl	stack_stub_14
	.globl	stack_stub_15
	.globl	stack_stub_16
	.globl	stack_stub_17
	.globl	stack_stub_18
	.globl	stack_stub_19
	.globl	stack_stub_20
	.globl	stack_stub_21
	.globl	stack_stub_22
	.globl	stack_stub_23
	.globl	stack_stub_24
	.globl	stack_stub_25
	.globl	stack_stub_26
	.globl	stack_stub_27
	.globl	stack_stub_28
	.globl	stack_stub_29
	.globl	stack_stub_30
	.globl	stack_stub_31
	.globl	stack_stub_32
	.globl	stack_stub_33
	.globl	stack_stub_34
	.globl	stack_stub_35
	.globl	stack_stub_36
	.globl	stack_stub_37
	.globl	stack_stub_38
	.globl	stack_stub_39
	.globl	stack_stub_40
	.globl	stack_stub_41
	.globl	stack_stub_42
	.globl	stack_stub_43
	.globl	stack_stub_44
	.globl	stack_stub_45
	.globl	stack_stub_46
	.globl	stack_stub_47
	.globl	stack_stub_48
	.globl	stack_stub_49
	.globl	stack_stub_50
	.globl	stack_stub_51
	.globl	stack_stub_52
	.globl	stack_stub_53
	.globl	stack_stub_54
	.globl	stack_stub_55
	.globl	stack_stub_56
	.globl	stack_stub_57
	.globl	stack_stub_58
	.globl	stack_stub_59
	.globl	stack_stub_60
	.globl	stack_stub_61
	.globl	stack_stub_62
	.globl	stack_stub_63
	.globl	stack_stub_64
	.globl	stack_stub_65
	.globl	stack_stub_66
	.globl	stack_stub_67
	.globl	stack_stub_68
	.globl	stack_stub_69
	.globl	stack_stub_70
	.globl	stack_stub_71
	.globl	stack_stub_72
	.globl	stack_stub_73
	.globl	stack_stub_74
	.globl	stack_stub_75
	.globl	stack_stub_76
	.globl	stack_stub_77
	.globl	stack_stub_78
	.globl	stack_stub_79
	.globl	stack_stub_80
	.globl	stack_stub_81
	.globl	stack_stub_82
	.globl	stack_stub_83
	.globl	stack_stub_84
	.globl	stack_stub_85
	.globl	stack_stub_86
	.globl	stack_stub_87
	.globl	stack_stub_88
	.globl	stack_stub_89
	.globl	stack_stub_90
	.globl	stack_stub_91
	.globl	stack_stub_92
	.globl	stack_stub_93
	.globl	stack_stub_94
	.globl	stack_stub_95
	.globl	stack_stub_96
	.globl	stack_stub_97
	.globl	stack_stub_98
	.globl	stack_stub_99
	.globl	exn_stub_00
	.globl	exn_stub_01
	.globl	exn_stub_02
	.globl	exn_stub_03
	.globl	exn_stub_04
	.globl	exn_stub_05
	.globl	exn_stub_06
	.globl	exn_stub_07
	.globl	exn_stub_08
	.globl	exn_stub_09
	.globl	exn_stub_10
	.globl	exn_stub_11
	.globl	exn_stub_12
	.globl	exn_stub_13
	.globl	exn_stub_14
	.globl	exn_stub_15
	.globl	exn_stub_16
	.globl	exn_stub_17
	.globl	exn_stub_18
	.globl	exn_stub_19
	.globl	exn_stub_20
	.globl	exn_stub_21
	.globl	exn_stub_22
	.globl	exn_stub_23
	.globl	exn_stub_24
	.globl	exn_stub_25
	.globl	exn_stub_26
	.globl	exn_stub_27
	.globl	exn_stub_28
	.globl	exn_stub_29
	.globl	exn_stub_30
	.globl	exn_stub_31
	.globl	exn_stub_32
	.globl	exn_stub_33
	.globl	exn_stub_34
	.globl	exn_stub_35
	.globl	exn_stub_36
	.globl	exn_stub_37
	.globl	exn_stub_38
	.globl	exn_stub_39
	.globl	exn_stub_40
	.globl	exn_stub_41
	.globl	exn_stub_42
	.globl	exn_stub_43
	.globl	exn_stub_44
	.globl	exn_stub_45
	.globl	exn_stub_46
	.globl	exn_stub_47
	.globl	exn_stub_48
	.globl	exn_stub_49
	.globl	exn_stub_50
	.globl	exn_stub_51
	.globl	exn_stub_52
	.globl	exn_stub_53
	.globl	exn_stub_54
	.globl	exn_stub_55
	.globl	exn_stub_56
	.globl	exn_stub_57
	.globl	exn_stub_58
	.globl	exn_stub_59
	.globl	exn_stub_60
	.globl	exn_stub_61
	.globl	exn_stub_62
	.globl	exn_stub_63
	.globl	exn_stub_64
	.globl	exn_stub_65
	.globl	exn_stub_66
	.globl	exn_stub_67
	.globl	exn_stub_68
	.globl	exn_stub_69
	.globl	exn_stub_70
	.globl	exn_stub_71
	.globl	exn_stub_72
	.globl	exn_stub_73
	.globl	exn_stub_74
	.globl	exn_stub_75
	.globl	exn_stub_76
	.globl	exn_stub_77
	.globl	exn_stub_78
	.globl	exn_stub_79
	.globl	exn_stub_80
	.globl	exn_stub_81
	.globl	exn_stub_82
	.globl	exn_stub_83
	.globl	exn_stub_84
	.globl	exn_stub_85
	.globl	exn_stub_86
	.globl	exn_stub_87
	.globl	exn_stub_88
	.globl	exn_stub_89
	.globl	exn_stub_90
	.globl	exn_stub_91
	.globl	exn_stub_92
	.globl	exn_stub_93
	.globl	exn_stub_94
	.globl	exn_stub_95
	.globl	exn_stub_96
	.globl	exn_stub_97
	.globl	exn_stub_98
	.globl	exn_stub_99

	.csect  general_code{PR}
	.align 3			
stack_stub_internal:
	# temp register contains the index of the snapshot we want
	l	r0, T.snapshots_saved_ra_add(RTOC)
	mulli	r31, r31, 4	# Pointers are 4 bytes in size
	add	r31, r31, r0
	lwz	r31, 0(r31)	# we now have pointer to return address
	lwz	r0, 0(r31)	# we have the retun address now
	mtlr	r0		# get ready to go there
	lil	r0, 0
	st	r0, 0(r31)	# clear return address to show we've consumed it
	blr			# jump to retrieved address

exn_stub_internal:		
	# temp register contains the index of the exn_stub that was called
	lwz	SP, 4(r27)
	st	r31, -4(SP)	# save temp register
	l	r31, T.exnstub_maxsp(RTOC)
	lwz	r0, 0(r31)	# we now have previous exnstub_maxsp
	cmp	0, 0, r0, SP
	bgt	exn_stub_internal_skip
	mr	r0, SP
exn_stub_internal_skip:	
	st	r0, 0(r31)	# save original or new exnstub_maxsp
	lwz	r31, -4(SP)
	lwz	r0, T.exn_codeptr_table(RTOC)
	mulli	r31, r31, 4	# value_t are 4 bytes in size
	add	r31, r31, r0
	lwz	r31, 0(r31)	# we have code pointer to jump to now
	stw	r31, 0(r27)
	mtlr	r31
 #	mr	r31, r1		# not sure this should be here ;  see alpha version
	blr
	
stack_stub_00:
        lil	r31, 0
        b       stack_stub_internal
			
stack_stub_01:
        lil	r31, 1
	b	stack_stub_internal
			
stack_stub_02:
        lil	r31, 2
	b	stack_stub_internal
			
stack_stub_03:
        lil	r31, 3
	b	stack_stub_internal
			
stack_stub_04:
        lil	r31, 4
	b	stack_stub_internal
			
stack_stub_05:
        lil	r31, 5
	b	stack_stub_internal
			
stack_stub_06:
        lil	r31, 6
	b	stack_stub_internal
			
stack_stub_07:
        lil	r31, 7
	b	stack_stub_internal
			
stack_stub_08:
        lil	r31, 8
	b	stack_stub_internal
			
stack_stub_09:
        lil	r31, 9
	b	stack_stub_internal
			
stack_stub_10:
        lil	r31, 10
	b	stack_stub_internal
			
stack_stub_11:
        lil	r31, 11
	b	stack_stub_internal
			
stack_stub_12:
        lil	r31, 12
	b	stack_stub_internal
			
stack_stub_13:
        lil	r31, 13
	b	stack_stub_internal
			
stack_stub_14:
        lil	r31, 14
	b	stack_stub_internal
			
stack_stub_15:
        lil	r31, 15
	b	stack_stub_internal
			
stack_stub_16:
        lil	r31, 16
	b	stack_stub_internal
			
stack_stub_17:
        lil	r31, 17
	b	stack_stub_internal
			
stack_stub_18:
        lil	r31, 18
	b	stack_stub_internal
			
stack_stub_19:
        lil	r31, 19
	b	stack_stub_internal
			
stack_stub_20:
        lil	r31, 20
	b	stack_stub_internal
			
stack_stub_21:
        lil	r31, 21
	b	stack_stub_internal
			
stack_stub_22:
        lil	r31, 22
	b	stack_stub_internal
			
stack_stub_23:
        lil	r31, 23
	b	stack_stub_internal
			
stack_stub_24:
        lil	r31, 24
	b	stack_stub_internal
			
stack_stub_25:
        lil	r31, 25
	b	stack_stub_internal
			
stack_stub_26:
        lil	r31, 26
	b	stack_stub_internal
			
stack_stub_27:
        lil	r31, 27
	b	stack_stub_internal
			
stack_stub_28:
        lil	r31, 28
	b	stack_stub_internal
			
stack_stub_29:
        lil	r31, 29
	b	stack_stub_internal
			
stack_stub_30:
        lil	r31, 30
	b	stack_stub_internal
			
stack_stub_31:
        lil	r31, 31
	b	stack_stub_internal
			
stack_stub_32:
        lil	r31, 32
	b	stack_stub_internal
			
stack_stub_33:
        lil	r31, 33
	b	stack_stub_internal
			
stack_stub_34:
        lil	r31, 34
	b	stack_stub_internal
			
stack_stub_35:
        lil	r31, 35
	b	stack_stub_internal
			
stack_stub_36:
        lil	r31, 36
	b	stack_stub_internal
			
stack_stub_37:
        lil	r31, 37
	b	stack_stub_internal
			
stack_stub_38:
        lil	r31, 38
	b	stack_stub_internal
			
stack_stub_39:
        lil	r31, 39
	b	stack_stub_internal
			
stack_stub_40:
        lil	r31, 40
	b	stack_stub_internal
			
stack_stub_41:
        lil	r31, 41
	b	stack_stub_internal
			
stack_stub_42:
        lil	r31, 42
	b	stack_stub_internal
			
stack_stub_43:
        lil	r31, 43
	b	stack_stub_internal
			
stack_stub_44:
        lil	r31, 44
	b	stack_stub_internal
			
stack_stub_45:
        lil	r31, 45
	b	stack_stub_internal
			
stack_stub_46:
        lil	r31, 46
	b	stack_stub_internal
			
stack_stub_47:
        lil	r31, 47
	b	stack_stub_internal
			
stack_stub_48:
        lil	r31, 48
	b	stack_stub_internal
			
stack_stub_49:
        lil	r31, 49
	b	stack_stub_internal
			
stack_stub_50:
        lil	r31, 50
	b	stack_stub_internal
			
stack_stub_51:
        lil	r31, 51
	b	stack_stub_internal
			
stack_stub_52:
        lil	r31, 52
	b	stack_stub_internal
			
stack_stub_53:
        lil	r31, 53
	b	stack_stub_internal
			
stack_stub_54:
        lil	r31, 54
	b	stack_stub_internal
			
stack_stub_55:
        lil	r31, 55
	b	stack_stub_internal
			
stack_stub_56:
        lil	r31, 56
	b	stack_stub_internal
			
stack_stub_57:
        lil	r31, 57
	b	stack_stub_internal
			
stack_stub_58:
        lil	r31, 58
	b	stack_stub_internal
			
stack_stub_59:
        lil	r31, 59
	b	stack_stub_internal
			
stack_stub_60:
        lil	r31, 60 
	b	stack_stub_internal
			
stack_stub_61:
        lil	r31, 61
	b	stack_stub_internal
			
stack_stub_62:
        lil	r31, 62
	b	stack_stub_internal
			
stack_stub_63:
        lil	r31, 63
	b	stack_stub_internal
			
stack_stub_64:
        lil	r31, 64
	b	stack_stub_internal
			
stack_stub_65:
        lil	r31, 65
	b	stack_stub_internal
			
stack_stub_66:
        lil	r31, 66
	b	stack_stub_internal
			
stack_stub_67:
        lil	r31, 67
	b	stack_stub_internal
			
stack_stub_68:
        lil	r31, 68
	b	stack_stub_internal
			
stack_stub_69:
        lil	r31, 69
	b	stack_stub_internal
			
stack_stub_70:
        lil	r31, 70
	b	stack_stub_internal
			
stack_stub_71:
        lil	r31, 71
	b	stack_stub_internal
			
stack_stub_72:
        lil	r31, 72
	b	stack_stub_internal
			
stack_stub_73:
        lil	r31, 73
	b	stack_stub_internal
			
stack_stub_74:
        lil	r31, 74
	b	stack_stub_internal
			
stack_stub_75:
        lil	r31, 75
	b	stack_stub_internal
			
stack_stub_76:
        lil	r31, 76
	b	stack_stub_internal
			
stack_stub_77:
        lil	r31, 77
	b	stack_stub_internal
			
stack_stub_78:
        lil	r31, 78
	b	stack_stub_internal
			
stack_stub_79:
        lil	r31, 79
	b	stack_stub_internal
			
stack_stub_80:
        lil	r31, 80 
	b	stack_stub_internal
			
stack_stub_81:
        lil	r31, 81
	b	stack_stub_internal
			
stack_stub_82:
        lil	r31, 82
	b	stack_stub_internal
			
stack_stub_83:
        lil	r31, 83
	b	stack_stub_internal
			
stack_stub_84:
        lil	r31, 84
	b	stack_stub_internal
			
stack_stub_85:
        lil	r31, 85
	b	stack_stub_internal
			
stack_stub_86:
        lil	r31, 86
	b	stack_stub_internal
			
stack_stub_87:
        lil	r31, 87
	b	stack_stub_internal
			
stack_stub_88:
        lil	r31, 88
	b	stack_stub_internal
			
stack_stub_89:
        lil	r31, 89
	b	stack_stub_internal
			
stack_stub_90:
        lil	r31, 90 
	b	stack_stub_internal
			
stack_stub_91:
        lil	r31, 91 
	b	stack_stub_internal
			
stack_stub_92:
        lil	r31, 92
	b	stack_stub_internal
			
stack_stub_93:
        lil	r31, 93
	b	stack_stub_internal
			
stack_stub_94:
        lil	r31, 94
	b	stack_stub_internal
			
stack_stub_95:
        lil	r31, 95
	b	stack_stub_internal
			
stack_stub_96:
        lil	r31, 96
	b	stack_stub_internal
			
stack_stub_97:
        lil	r31, 97
	b	stack_stub_internal
			
stack_stub_98:
        lil	r31, 98
	b	stack_stub_internal
			
stack_stub_99:
        lil	r31, 99
	b	stack_stub_internal
	

	
exn_stub_00:
        lil	r31, 0
        b       exn_stub_internal
			
exn_stub_01:
        lil	r31, 1
	b	exn_stub_internal
			
exn_stub_02:
        lil	r31, 2
	b	exn_stub_internal
			
exn_stub_03:
        lil	r31, 3
	b	exn_stub_internal
			
exn_stub_04:
        lil	r31, 4
	b	exn_stub_internal
			
exn_stub_05:
        lil	r31, 5
	b	exn_stub_internal
			
exn_stub_06:
        lil	r31, 6
	b	exn_stub_internal
			
exn_stub_07:
        lil	r31, 7
	b	exn_stub_internal
			
exn_stub_08:
        lil	r31, 8
	b	exn_stub_internal
			
exn_stub_09:
        lil	r31, 9
	b	exn_stub_internal
			
exn_stub_10:
        lil	r31, 10
	b	exn_stub_internal
			
exn_stub_11:
        lil	r31, 11
	b	exn_stub_internal
			
exn_stub_12:
        lil	r31, 12
	b	exn_stub_internal
			
exn_stub_13:
        lil	r31, 13
	b	exn_stub_internal
			
exn_stub_14:
        lil	r31, 14
	b	exn_stub_internal
			
exn_stub_15:
        lil	r31, 15
	b	exn_stub_internal
			
exn_stub_16:
        lil	r31, 16
	b	exn_stub_internal
			
exn_stub_17:
        lil	r31, 17
	b	exn_stub_internal
			
exn_stub_18:
        lil	r31, 18
	b	exn_stub_internal
			
exn_stub_19:
        lil	r31, 19
	b	exn_stub_internal
			
exn_stub_20:
        lil	r31, 20
	b	exn_stub_internal
			
exn_stub_21:
        lil	r31, 21
	b	exn_stub_internal
			
exn_stub_22:
        lil	r31, 22
	b	exn_stub_internal
			
exn_stub_23:
        lil	r31, 23
	b	exn_stub_internal
			
exn_stub_24:
        lil	r31, 24
	b	exn_stub_internal
			
exn_stub_25:
        lil	r31, 25
	b	exn_stub_internal
			
exn_stub_26:
        lil	r31, 26
	b	exn_stub_internal
			
exn_stub_27:
        lil	r31, 27
	b	exn_stub_internal
			
exn_stub_28:
        lil	r31, 28
	b	exn_stub_internal
			
exn_stub_29:
        lil	r31, 29
	b	exn_stub_internal
			
exn_stub_30:
        lil	r31, 30
	b	exn_stub_internal
			
exn_stub_31:
        lil	r31, 31
	b	exn_stub_internal
			
exn_stub_32:
        lil	r31, 32
	b	exn_stub_internal
			
exn_stub_33:
        lil	r31, 33
	b	exn_stub_internal
			
exn_stub_34:
        lil	r31, 34
	b	exn_stub_internal
			
exn_stub_35:
        lil	r31, 35
	b	exn_stub_internal
			
exn_stub_36:
        lil	r31, 36
	b	exn_stub_internal
			
exn_stub_37:
        lil	r31, 37
	b	exn_stub_internal
			
exn_stub_38:
        lil	r31, 38
	b	exn_stub_internal
			
exn_stub_39:
        lil	r31, 39
	b	exn_stub_internal
			
exn_stub_40:
        lil	r31, 40
	b	exn_stub_internal
			
exn_stub_41:
        lil	r31, 41
	b	exn_stub_internal
			
exn_stub_42:
        lil	r31, 42
	b	exn_stub_internal
			
exn_stub_43:
        lil	r31, 43
	b	exn_stub_internal
			
exn_stub_44:
        lil	r31, 44
	b	exn_stub_internal
			
exn_stub_45:
        lil	r31, 45
	b	exn_stub_internal
			
exn_stub_46:
        lil	r31, 46
	b	exn_stub_internal
			
exn_stub_47:
        lil	r31, 47
	b	exn_stub_internal
			
exn_stub_48:
        lil	r31, 48
	b	exn_stub_internal
			
exn_stub_49:
        lil	r31, 49
	b	exn_stub_internal
			
exn_stub_50:
        lil	r31, 50
	b	exn_stub_internal
			
exn_stub_51:
        lil	r31, 51
	b	exn_stub_internal
			
exn_stub_52:
        lil	r31, 52
	b	exn_stub_internal
			
exn_stub_53:
        lil	r31, 53
	b	exn_stub_internal
			
exn_stub_54:
        lil	r31, 54
	b	exn_stub_internal
			
exn_stub_55:
        lil	r31, 55
	b	exn_stub_internal
			
exn_stub_56:
        lil	r31, 56
	b	exn_stub_internal
			
exn_stub_57:
        lil	r31, 57
	b	exn_stub_internal
			
exn_stub_58:
        lil	r31, 58
	b	exn_stub_internal
			
exn_stub_59:
        lil	r31, 59
	b	exn_stub_internal
			
exn_stub_60:
        lil	r31, 60 
	b	exn_stub_internal
			
exn_stub_61:
        lil	r31, 61
	b	exn_stub_internal
			
exn_stub_62:
        lil	r31, 62
	b	exn_stub_internal
			
exn_stub_63:
        lil	r31, 63
	b	exn_stub_internal
			
exn_stub_64:
        lil	r31, 64
	b	exn_stub_internal
			
exn_stub_65:
        lil	r31, 65
	b	exn_stub_internal
			
exn_stub_66:
        lil	r31, 66
	b	exn_stub_internal
			
exn_stub_67:
        lil	r31, 67
	b	exn_stub_internal
			
exn_stub_68:
        lil	r31, 68
	b	exn_stub_internal
			
exn_stub_69:
        lil	r31, 69
	b	exn_stub_internal
			
exn_stub_70:
        lil	r31, 70
	b	exn_stub_internal
			
exn_stub_71:
        lil	r31, 71
	b	exn_stub_internal
			
exn_stub_72:
        lil	r31, 72
	b	exn_stub_internal
			
exn_stub_73:
        lil	r31, 73
	b	exn_stub_internal
			
exn_stub_74:
        lil	r31, 74
	b	exn_stub_internal
			
exn_stub_75:
        lil	r31, 75
	b	exn_stub_internal
			
exn_stub_76:
        lil	r31, 76
	b	exn_stub_internal
			
exn_stub_77:
        lil	r31, 77
	b	exn_stub_internal
			
exn_stub_78:
        lil	r31, 78
	b	exn_stub_internal
			
exn_stub_79:
        lil	r31, 79
	b	exn_stub_internal
			
exn_stub_80:
        lil	r31, 80 
	b	exn_stub_internal
			
exn_stub_81:
        lil	r31, 81
	b	exn_stub_internal
			
exn_stub_82:
        lil	r31, 82
	b	exn_stub_internal
			
exn_stub_83:
        lil	r31, 83
	b	exn_stub_internal
			
exn_stub_84:
        lil	r31, 84
	b	exn_stub_internal
			
exn_stub_85:
        lil	r31, 85
	b	exn_stub_internal
			
exn_stub_86:
        lil	r31, 86
	b	exn_stub_internal
			
exn_stub_87:
        lil	r31, 87
	b	exn_stub_internal
			
exn_stub_88:
        lil	r31, 88
	b	exn_stub_internal
			
exn_stub_89:
        lil	r31, 89
	b	exn_stub_internal
			
exn_stub_90:
        lil	r31, 90 
	b	exn_stub_internal
			
exn_stub_91:
        lil	r31, 91 
	b	exn_stub_internal
			
exn_stub_92:
        lil	r31, 92
	b	exn_stub_internal
			
exn_stub_93:
        lil	r31, 93
	b	exn_stub_internal
			
exn_stub_94:
        lil	r31, 94
	b	exn_stub_internal
			
exn_stub_95:
        lil	r31, 95
	b	exn_stub_internal
			
exn_stub_96:
        lil	r31, 96
	b	exn_stub_internal
			
exn_stub_97:
        lil	r31, 97
	b	exn_stub_internal
			
exn_stub_98:
        lil	r31, 98
	b	exn_stub_internal
			
exn_stub_99:
        lil	r31, 99
	b	exn_stub_internal
	