	.section	".rodata"
		! gcinfo
	.globl Info_unit_GCTABLE_BEGIN_VAL
Info_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl Info_unit_CODE_END_VAL
	.globl Info_unit_CODE_BEGIN_VAL
Info_unit_CODE_BEGIN_VAL:
	.text
 	.align 8
	.global Info_anonfun_code_22985
 ! arguments : [$22987,$8] [$22988,$9] [$14745,$10] 
 ! results    : [$27691,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_anonfun_code_22985:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27706
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_27706:
	st	%r15, [%sp+92]
code_27697:
funtop_27676:
	sethi	%hi(baseTime_19070), %r8
	or	%r8, %lo(baseTime_19070), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! making closure call 
	sethi	%hi(PLUS_17338), %r8
	or	%r8, %lo(PLUS_17338), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_27702:
	! done making tail call
code_27704:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_22985,(.-Info_anonfun_code_22985)

	.section	".rodata"
	.text
 	.align 8
	.global Info_timeFromString_code_22990
 ! arguments : [$22992,$8] [$22993,$9] [$14737,$10] 
 ! results    : [$27675,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_timeFromString_code_22990:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27721
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_27721:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
code_27707:
funtop_27648:
	sethi	%hi(anonfun_14744), %r8
	or	%r8, %lo(anonfun_14744), %r10
	! making closure call 
	sethi	%hi(_17337), %r8
	or	%r8, %lo(_17337), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_27720:
	st	%r8, [%sp+96]
code_27711:
	! done making normal call
	! making closure call 
	sethi	%hi(fromString_17342), %r8
	or	%r8, %lo(fromString_17342), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_27718:
	mov	%r8, %r10
code_27714:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_27715:
	! done making tail call
code_27717:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_timeFromString_code_22990,(.-Info_timeFromString_code_22990)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_27718
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_27720
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
 	.align 8
	.global Info_anonfun_code_22995
 ! arguments : [$22997,$8] [$22998,$9] [$14755,$10] 
 ! results    : [$27633,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
Info_anonfun_code_22995:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27741
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_27741:
	st	%r15, [%sp+92]
code_27722:
funtop_27588:
	cmp	%r10, 58
	or	%r0, 1, %r8
	be	cmpui_27723
	nop
code_27724:
	or	%r0, 0, %r8
cmpui_27723:
	cmp	%r8, 0
	bne	one_case_27597
	nop
zero_case_27596:
	ba	after_zeroone_27598
	or	%r0, 1, %r8
one_case_27597:
	or	%r0, 0, %r8
after_zeroone_27598:
	cmp	%r8, 0
	bne	one_case_27607
	nop
zero_case_27606:
	ba	after_zeroone_27608
	or	%r0, 0, %r8
one_case_27607:
	cmp	%r10, 36
	or	%r0, 1, %r8
	be	cmpui_27729
	nop
code_27730:
	or	%r0, 0, %r8
cmpui_27729:
	cmp	%r8, 0
	bne	one_case_27619
	nop
zero_case_27618:
	ba	after_zeroone_27620
	or	%r0, 1, %r8
one_case_27619:
	or	%r0, 0, %r8
after_zeroone_27620:
after_zeroone_27608:
	cmp	%r8, 0
	bne	one_case_27629
	nop
zero_case_27628:
	ba	after_zeroone_27630
	or	%r0, 0, %r8
one_case_27629:
	cmp	%r10, 10
	or	%r0, 1, %r8
	be	cmpui_27735
	nop
code_27736:
	or	%r0, 0, %r8
cmpui_27735:
	cmp	%r8, 0
	bne	one_case_27641
	nop
zero_case_27640:
	ba	after_zeroone_27642
	or	%r0, 1, %r8
one_case_27641:
	or	%r0, 0, %r8
after_zeroone_27642:
after_zeroone_27630:
code_27740:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_22995,(.-Info_anonfun_code_22995)

	.section	".rodata"
	.text
 	.align 8
	.global Info_output_record_code_23005
 ! arguments : [$23007,$8] [$23008,$9] [$19176,$10] [$19177,$11] 
 ! results    : [$27587,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_output_record_code_23005:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27761
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_27761:
	st	%r15, [%sp+92]
	mov	%r10, %r13
	st	%r11, [%sp+104]
code_27742:
funtop_27540:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(output_17453), %r8
	or	%r8, %lo(output_17453), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r12, %r15
	mov	%r13, %r11
code_27760:
code_27745:
	! done making normal call
	or	%r0, 58, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_27757:
code_27748:
	! done making normal call
	! making closure call 
	sethi	%hi(output_17453), %r8
	or	%r8, %lo(output_17453), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_27758:
code_27751:
	! done making normal call
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+96], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_27754:
	! done making tail call
code_27756:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_output_record_code_23005,(.-Info_output_record_code_23005)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_27757
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x001c0000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_27758
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_27760
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x001c0000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.text
 	.align 8
	.global Info_output_ue_binding_code_23014
 ! arguments : [$23016,$8] [$23017,$9] [$19215,$10] [$19216,$11] 
 ! results    : [$27522,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_output_ue_binding_code_23014:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27805
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_27805:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_27762:
funtop_27441:
	sethi	%hi(anonfun_14754), %r8
	or	%r8, %lo(anonfun_14754), %r10
	! making closure call 
	sethi	%hi(_17350), %r8
	or	%r8, %lo(_17350), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_27802:
	st	%r8, [%sp+96]
code_27766:
	! done making normal call
	! making closure call 
	sethi	%hi(explode_17444), %r8
	or	%r8, %lo(explode_17444), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_27795:
	mov	%r8, %r10
code_27769:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_27796:
code_27770:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_27472
	nop
zero_case_27471:
	! making closure call 
	sethi	%hi(toString_17482), %r8
	or	%r8, %lo(toString_17482), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_27804:
	mov	%r8, %r11
code_27774:
	! done making normal call
	sethi	%hi(string_23757), %r8
	or	%r8, %lo(string_23757), %r10
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_27797:
	mov	%r8, %r10
code_27778:
	! done making normal call
	sethi	%hi(string_23767), %r8
	or	%r8, %lo(string_23767), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_27798:
	st	%r8, [%sp+96]
code_27782:
	! done making normal call
	sethi	%hi(string_23437), %r8
	or	%r8, %lo(string_23437), %r10
	! making closure call 
	sethi	%hi(_20644), %r8
	or	%r8, %lo(_20644), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_27799:
	mov	%r8, %r9
code_27786:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_27787:
	! done making tail call
	ba	after_zeroone_27473 ! delay slot empty
	nop
one_case_27472:
	! making closure call 
	sethi	%hi(_19238), %r8
	or	%r8, %lo(_19238), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_27803:
	mov	%r8, %r11
code_27791:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_27792:
	! done making tail call
after_zeroone_27473:
code_27794:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_output_ue_binding_code_23014,(.-Info_output_ue_binding_code_23014)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_27795
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00750000
		! worddata
	.word 0x00000002
	.long Crc_STR_c_INT
		! -------- label,sizes,reg
	.long code_27796
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00740000
		! worddata
	.word 0x00000002
	.long Crc_STR_c_INT
		! -------- label,sizes,reg
	.long code_27797
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_27798
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_27799
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_27802
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00740000
		! worddata
	.word 0x00000002
	.long Crc_STR_c_INT
		! -------- label,sizes,reg
	.long code_27803
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00440000
		! -------- label,sizes,reg
	.long code_27804
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global Info_write_code_23000
 ! arguments : [$23002,$8] [$23003,$9] [$19172,$10] [$19173,$11] 
 ! results    : [$27440,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_write_code_23000:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27958
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_27958:
	st	%r15, [%sp+92]
code_27806:
funtop_27019:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27807
	nop
code_27808:
	call	GCFromML ! delay slot empty
	nop
needgc_27807:
	ld	[%r11], %r9
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_23479), %r8
	or	%r8, %lo(string_23479), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+132]
	add	%r4, 12, %r4
	! done allocating 2 record
	ld	[%r11+16], %r17
	st	%r17, [%sp+100]
	ld	[%r11+12], %r17
	st	%r17, [%sp+128]
	ld	[%r11+20], %r17
	st	%r17, [%sp+124]
	ld	[%r11+4], %r17
	st	%r17, [%sp+120]
	ld	[%r11+8], %r17
	st	%r17, [%sp+116]
	! making closure call 
	sethi	%hi(openOut_17449), %r8
	or	%r8, %lo(openOut_17449), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_27954:
	st	%r8, [%sp+112]
code_27813:
	! done making normal call
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27814
	nop
code_27815:
	call	GCFromML ! delay slot empty
	nop
needgc_27814:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 17, %r10
	sethi	%hi(TextIO_STR_c_INT), %r8
	or	%r8, %lo(TextIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_27819
	nop
code_27820:
	or	%r0, 0, %r8
cmpui_27819:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	st	%r10, [%r4]
	sethi	%hi(TextIO_STR_c_INT), %r8
	or	%r8, %lo(TextIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_27823
	nop
code_27824:
	or	%r0, 0, %r8
cmpui_27823:
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	or	%r0, 10, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Info_output_record_code_23005), %r8
	or	%r8, %lo(Info_output_record_code_23005), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Info_output_ue_binding_code_23014), %r8
	or	%r8, %lo(Info_output_ue_binding_code_23014), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+108]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 36, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27930:
code_27829:
	! done making normal call
	sethi	%hi(string_23446), %r8
	or	%r8, %lo(string_23446), %r11
	! making closure call 
	sethi	%hi(output_17453), %r8
	or	%r8, %lo(output_17453), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27931:
code_27833:
	! done making normal call
	or	%r0, 10, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27932:
code_27836:
	! done making normal call
	sethi	%hi(string_23454), %r8
	or	%r8, %lo(string_23454), %r10
	sethi	%hi(string_23456), %r8
	or	%r8, %lo(string_23456), %r11
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_27933:
code_27839:
	! done making normal call
	or	%r0, 36, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27934:
code_27842:
	! done making normal call
	sethi	%hi(string_23474), %r8
	or	%r8, %lo(string_23474), %r11
	! making closure call 
	sethi	%hi(output_17453), %r8
	or	%r8, %lo(output_17453), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27935:
code_27846:
	! done making normal call
	or	%r0, 10, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27936:
code_27849:
	! done making normal call
	! making closure call 
	sethi	%hi(_17507), %r8
	or	%r8, %lo(_17507), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_27937:
	st	%r8, [%sp+104]
code_27852:
	! done making normal call
	sethi	%hi(baseTime_19070), %r8
	or	%r8, %lo(baseTime_19070), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! making closure call 
	sethi	%hi(MINUS_17327), %r8
	or	%r8, %lo(MINUS_17327), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_27938:
	mov	%r8, %r10
code_27857:
	! done making normal call
	! making closure call 
	sethi	%hi(toString_17326), %r8
	or	%r8, %lo(toString_17326), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_27939:
	mov	%r8, %r9
code_27860:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27861
	nop
code_27862:
	call	GCFromML ! delay slot empty
	nop
needgc_27861:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_23491), %r8
	or	%r8, %lo(string_23491), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+100]
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(baseTime_19070), %r8
	or	%r8, %lo(baseTime_19070), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! making closure call 
	sethi	%hi(MINUS_17327), %r8
	or	%r8, %lo(MINUS_17327), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_27955:
	mov	%r8, %r10
code_27869:
	! done making normal call
	! making closure call 
	sethi	%hi(toString_17326), %r8
	or	%r8, %lo(toString_17326), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_27940:
	mov	%r8, %r9
code_27872:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27873
	nop
code_27874:
	call	GCFromML ! delay slot empty
	nop
needgc_27873:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_23503), %r8
	or	%r8, %lo(string_23503), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 12, %r4
	! done allocating 2 record
	! making closure call 
	sethi	%hi(toString_17517), %r8
	or	%r8, %lo(toString_17517), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_27956:
	mov	%r8, %r9
code_27879:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27880
	nop
code_27881:
	call	GCFromML ! delay slot empty
	nop
needgc_27880:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_23515), %r8
	or	%r8, %lo(string_23515), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+132], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_27957:
code_27884:
	! done making normal call
	or	%r0, 36, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27941:
code_27887:
	! done making normal call
	sethi	%hi(string_23523), %r8
	or	%r8, %lo(string_23523), %r11
	! making closure call 
	sethi	%hi(output_17453), %r8
	or	%r8, %lo(output_17453), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27942:
code_27891:
	! done making normal call
	or	%r0, 10, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27943:
code_27894:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_appi_17577), %r8
	or	%r8, %lo(strbindvar_r_appi_17577), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_27944:
	mov	%r8, %r12
code_27897:
	! done making normal call
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_27349), %r8
	or	%r8, %lo(record_27349), %r10
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r9
code_27945:
	mov	%r8, %r9
code_27903:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_27946:
code_27904:
	! done making normal call
	or	%r0, 36, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27947:
code_27907:
	! done making normal call
	sethi	%hi(string_23531), %r8
	or	%r8, %lo(string_23531), %r11
	! making closure call 
	sethi	%hi(output_17453), %r8
	or	%r8, %lo(output_17453), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27948:
code_27911:
	! done making normal call
	or	%r0, 10, %r11
	! making closure call 
	sethi	%hi(output1_17456), %r8
	or	%r8, %lo(output1_17456), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_27949:
code_27914:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_appi_17577), %r8
	or	%r8, %lo(strbindvar_r_appi_17577), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_27950:
	mov	%r8, %r12
code_27917:
	! done making normal call
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_27415), %r8
	or	%r8, %lo(record_27415), %r10
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r9
code_27951:
	mov	%r8, %r9
code_27923:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_27952:
code_27924:
	! done making normal call
	! making closure call 
	sethi	%hi(_19300), %r8
	or	%r8, %lo(_19300), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 144, %sp
code_27927:
	! done making tail call
code_27929:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Info_write_code_23000,(.-Info_write_code_23000)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_27930
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_27807
	.word 0xb8004807
	.word 0x00000c00
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_27814
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f040000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27931
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27932
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27933
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27934
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27935
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27936
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27937
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f440000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27938
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f500000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27939
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f500000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_27861
	.word 0xb800480d
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f500000
	.word 0x00000005
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27940
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f540000
	.word 0x00000004
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_27873
	.word 0xb800480d
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f540000
	.word 0x00000004
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_27880
	.word 0xb800480d
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f550000
	.word 0x00000004
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27941
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27942
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27943
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27944
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27945
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27946
	.word 0xb800480b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27947
	.word 0xb800480b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27948
	.word 0xb800480b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27949
	.word 0xb800480b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27950
	.word 0xb800480b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0f000000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27951
	.word 0xb800480b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0f000000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27952
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03000000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_27954
	.word 0xb800480b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7c040000
	.word 0x00000005
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27955
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f540000
	.word 0x00000004
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27956
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f550000
	.word 0x00000004
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_27957
	.word 0xb800480d
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f400000
	.word 0x00000000
		! worddata
	.word 0x00000012
	.long TextIO_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.text
 	.align 8
	.global Info_anonfun_code_23021
 ! arguments : [$23023,$8] [$23024,$9] [$19303,$10] [$19304,$11] 
 ! results    : [$27014,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Info_anonfun_code_23021:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27967
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_27967:
	st	%r15, [%sp+92]
code_27959:
funtop_26997:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27960
	nop
code_27961:
	call	GCFromML ! delay slot empty
	nop
needgc_27960:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(exn_stamp_17676), %r8
	ld	[%r8+%lo(exn_stamp_17676)], %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(string_27013), %r8
	or	%r8, %lo(string_27013), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
code_27966:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_23021,(.-Info_anonfun_code_23021)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_27960
	.word 0xb8003006
	.word 0xbffc3c00
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global Info_splitPRIME_code_23026
 ! arguments : [$23028,$8] [$23029,$9] [$19333,$10] [$19334,$11] 
 ! results    : [$26922,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_splitPRIME_code_23026:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_27989
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_27989:
	st	%r15, [%sp+92]
	mov	%r10, %r12
	mov	%r11, %r10
code_27968:
funtop_26905:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27969
	nop
code_27970:
	call	GCFromML ! delay slot empty
	nop
needgc_27969:
sumarm_26917:
	cmp	%r12, 0
	bne	sumarm_26918
	nop
code_27972:
	ba	after_sum_26914
	or	%r0, 0, %r8
sumarm_26918:
	ld	[%r12], %r9
	ld	[%r12+4], %r17
	st	%r17, [%sp+96]
	cmp	%r9, 58
	or	%r0, 1, %r8
	be	cmpui_27974
	nop
code_27975:
	or	%r0, 0, %r8
cmpui_27974:
	cmp	%r8, 0
	bne	one_case_26953
	nop
zero_case_26952:
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	ld	[%sp+96], %r12
	ba	funtop_26905
	mov	%r8, %r10
code_27977:
	! done making self tail call
	ba	after_zeroone_26954
	or	%r0, 0, %r8
one_case_26953:
	! making closure call 
	sethi	%hi(_17749), %r8
	or	%r8, %lo(_17749), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_27988:
	mov	%r8, %r9
code_27981:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27982
	nop
code_27983:
	call	GCFromML ! delay slot empty
	nop
needgc_27982:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_26954:
	ba	after_sum_26914 ! delay slot empty
	nop
sumarm_26923:
after_sum_26914:
code_27987:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_splitPRIME_code_23026,(.-Info_splitPRIME_code_23026)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_27969
	.word 0xb8003806
	.word 0x00001400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_27982
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_27988
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global Info_addLines_code_23031
 ! arguments : [$23033,$8] [$23034,$9] [$19681,$10] [$19682,$11] [$19683,$12] 
 ! results    : [$26488,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_addLines_code_23031:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28144
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_28144:
	st	%r15, [%sp+92]
	st	%r10, [%sp+148]
	st	%r12, [%sp+128]
code_27990:
funtop_26472:
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_27991
	nop
code_27992:
	call	GCFromML ! delay slot empty
	nop
needgc_27991:
sumarm_26484:
	cmp	%r11, 0
	bne	sumarm_26485
	nop
code_27994:
	ba	after_sum_26481
	ld	[%sp+128], %r8
sumarm_26485:
	ld	[%r11], %r17
	st	%r17, [%sp+124]
	ld	[%r11+4], %r17
	st	%r17, [%sp+144]
	ld	[%sp+128], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+140]
	! start making constructor call
	sethi	%hi(type_21052), %r8
	or	%r8, %lo(type_21052), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r17
	st	%r17, [%sp+120]
	! start making constructor call
	sethi	%hi(type_21052), %r8
	or	%r8, %lo(type_21052), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r11
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_28140:
	st	%r8, [%sp+116]
code_28002:
	! done making constructor call
	! start making constructor call
	sethi	%hi(list_TYC), %r8
	or	%r8, %lo(list_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_28127:
	mov	%r8, %r9
code_28007:
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28008
	nop
code_28009:
	call	GCFromML ! delay slot empty
	nop
needgc_28008:
	! done making constructor call
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	ld	[%sp+116], %r17
	st	%r17, [%r4+12]
	st	%r9, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	ld	[%sp+120], %r17
	ld	[%r17], %r10
	ld	[%sp+120], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_28136:
	st	%r8, [%sp+96]
code_28011:
	! done making constructor call
	ld	[%sp+128], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+136]
	ld	[%sp+148], %r17
	addcc	%r17, 1, %r17
	st	%r17, [%sp+132]
sumarm_26574:
	ld	[%sp+124], %r17
	ld	[%r17], %r8
	cmp	%r8, 0
	bne	sumarm_26575
	nop
code_28012:
	ld	[%sp+124], %r17
	ld	[%r17+4], %r8
	ld	[%r8], %r17
	st	%r17, [%sp+128]
	ld	[%r8+4], %r17
	st	%r17, [%sp+116]
	! making closure call 
	sethi	%hi(_17981), %r8
	or	%r8, %lo(_17981), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+136], %r10
	jmpl	%r12, %r15
	ld	[%sp+140], %r11
code_28128:
code_28015:
	! done making normal call
sumarm_26603:
	cmp	%r8, 0
	bne	sumarm_26604
	nop
code_28016:
	sethi	%hi(emptySection_20700), %r8
	or	%r8, %lo(emptySection_20700), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ba	after_sum_26600
	ld	[%r8], %r8
sumarm_26604:
	ba	after_sum_26600 ! delay slot empty
	nop
sumarm_26611:
after_sum_26600:
	ld	[%r8], %r17
	st	%r17, [%sp+120]
	ld	[%r8+4], %r17
	st	%r17, [%sp+124]
	! making closure call 
	sethi	%hi(_18023), %r8
	or	%r8, %lo(_18023), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+120], %r10
	jmpl	%r12, %r15
	ld	[%sp+128], %r11
code_28125:
	mov	%r8, %r10
code_28023:
	! done making normal call
	! making closure call 
	sethi	%hi(_18018), %r8
	or	%r8, %lo(_18018), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28129:
code_28026:
	! done making normal call
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28027
	nop
code_28028:
	call	GCFromML ! delay slot empty
	nop
needgc_28027:
	cmp	%r8, 0
	bne	one_case_26645
	nop
zero_case_26644:
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	ld	[%sp+148], %r17
	st	%r17, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+116]
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(Help_STR_c_INT), %r8
	or	%r8, %lo(Help_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label StringMap_STR
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r11
	! start making constructor call
	sethi	%hi(type_20690), %r8
	or	%r8, %lo(type_20690), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_28142:
	st	%r8, [%sp+100]
code_28035:
	! done making constructor call
	! making closure call 
	sethi	%hi(_18036), %r8
	or	%r8, %lo(_18036), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+120], %r10
	ld	[%sp+128], %r11
	jmpl	%r13, %r15
	ld	[%sp+116], %r12
code_28130:
code_28038:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28039
	nop
code_28040:
	call	GCFromML ! delay slot empty
	nop
needgc_28039:
	ba	after_zeroone_26646
	mov	%r8, %r9
one_case_26645:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+148], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r17
	st	%r17, [%sp+116]
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(string_24266), %r8
	or	%r8, %lo(string_24266), %r10
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r11
code_28138:
	mov	%r8, %r11
code_28046:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_28131:
	st	%r8, [%sp+116]
code_28048:
	! done making normal call
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28049
	nop
code_28050:
	call	GCFromML ! delay slot empty
	nop
needgc_28049:
	! start making constructor call
	sethi	%hi(type_21052), %r8
	or	%r8, %lo(type_21052), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r8
	! allocating 4-record
	or	%r0, 3105, %r9
	st	%r9, [%r4]
	or	%r0, 5, %r9
	st	%r9, [%r4+4]
	or	%r0, 2, %r9
	st	%r9, [%r4+8]
	or	%r0, 2, %r9
	st	%r9, [%r4+12]
	sethi	%hi(string_TYC), %r9
	or	%r9, %lo(string_TYC), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	st	%r9, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	ld	[%r8], %r10
	jmpl	%r10, %r15
	ld	[%r8+4], %r8
code_28137:
	st	%r8, [%sp+104]
code_28056:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28057
	nop
code_28058:
	call	GCFromML ! delay slot empty
	nop
needgc_28057:
	! done making constructor call
	ld	[%sp+116], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
	mov	%r8, %r9
after_zeroone_26646:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 529, %r12
	sethi	%hi(type_18162), %r10
	or	%r10, %lo(type_18162), %r11
	ld	[%r2+812], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_28063
	nop
code_28064:
	or	%r0, 0, %r10
cmpui_28063:
	sll	%r10, 8, %r10
	add	%r10, %r0, %r10
	or	%r10, %r12, %r12
	st	%r12, [%r4]
	sethi	%hi(type_18162), %r10
	or	%r10, %lo(type_18162), %r11
	ld	[%r2+812], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_28067
	nop
code_28068:
	or	%r0, 0, %r10
cmpui_28067:
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+116]
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(Help_STR_c_INT), %r8
	or	%r8, %lo(Help_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label StringMap_STR
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r11
	! start making constructor call
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_28126:
	st	%r8, [%sp+108]
code_28073:
	! done making constructor call
	! making closure call 
	sethi	%hi(_18066), %r8
	or	%r8, %lo(_18066), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+136], %r10
	ld	[%sp+140], %r11
	jmpl	%r13, %r15
	ld	[%sp+116], %r12
code_28132:
	mov	%r8, %r10
code_28076:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28077
	nop
code_28078:
	call	GCFromML ! delay slot empty
	nop
needgc_28077:
	! allocating 2-record
	or	%r0, 273, %r9
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_28080
	nop
code_28081:
	or	%r0, 0, %r8
cmpui_28080:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	ld	[%sp+140], %r17
	st	%r17, [%r4+4]
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_28082
	nop
code_28083:
	or	%r0, 0, %r8
cmpui_28082:
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_26571 ! delay slot empty
	nop
sumarm_26575:
	ld	[%sp+124], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+120]
	! making closure call 
	sethi	%hi(_17981), %r8
	or	%r8, %lo(_17981), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+136], %r10
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_28141:
	mov	%r8, %r10
code_28087:
	! done making normal call
	! making closure call 
	sethi	%hi(_18080), %r8
	or	%r8, %lo(_18080), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28133:
code_28090:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28091
	nop
code_28092:
	call	GCFromML ! delay slot empty
	nop
needgc_28091:
	cmp	%r8, 0
	bne	one_case_26824
	nop
zero_case_26823:
	sethi	%hi(Help_STR_c_INT), %r8
	or	%r8, %lo(Help_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label StringMap_STR
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r11
	! start making constructor call
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_28143:
	st	%r8, [%sp+112]
code_28099:
	! done making constructor call
	sethi	%hi(emptySection_20700), %r8
	or	%r8, %lo(emptySection_20700), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	sethi	%hi(_18066), %r8
	or	%r8, %lo(_18066), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+136], %r10
	jmpl	%r13, %r15
	ld	[%sp+120], %r11
code_28134:
	mov	%r8, %r10
code_28104:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28105
	nop
code_28106:
	call	GCFromML ! delay slot empty
	nop
needgc_28105:
	! allocating 2-record
	or	%r0, 273, %r9
	ld	[%sp+112], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_28108
	nop
code_28109:
	or	%r0, 0, %r8
cmpui_28108:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_28110
	nop
code_28111:
	or	%r0, 0, %r8
cmpui_28110:
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_26825 ! delay slot empty
	nop
one_case_26824:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+148], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r17
	st	%r17, [%sp+116]
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(string_24316), %r8
	or	%r8, %lo(string_24316), %r10
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_28139:
	mov	%r8, %r11
code_28116:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_28135:
code_28118:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_zeroone_26825:
	ba	after_sum_26571 ! delay slot empty
	nop
sumarm_26797:
after_sum_26571:
	! making direct call 
	ld	[%sp+132], %r16
	st	%r16, [%sp+148]
	ld	[%sp+144], %r11
	ba	funtop_26472
	st	%r8, [%sp+128]
code_28121:
	! done making self tail call
	ba	after_sum_26481
	or	%r0, 0, %r8
sumarm_26489:
after_sum_26481:
code_28124:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size Info_addLines_code_23031,(.-Info_addLines_code_23031)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28125
	.word 0xb800500b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x74010000
	.word 0x00000171
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28126
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04010000
	.word 0x00000170
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_27991
	.word 0xb8005007
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_28127
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000141
		! -------- label,sizes,reg
	.long needgc_28008
	.word 0xb8005007
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000141
		! -------- label,sizes,reg
	.long code_28128
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04010000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28129
	.word 0xb800500b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x74010000
	.word 0x00000171
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_28027
	.word 0xb800500b
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x74010000
	.word 0x00000171
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28130
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40050000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_28039
	.word 0xb800500b
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x40050000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_28131
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40010000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_28049
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44010000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_28057
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44010000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28132
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x00000140
		! -------- label,sizes,reg
	.long needgc_28077
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x00000140
		! worddata
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_28133
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
	.word 0x00000130
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_28091
	.word 0xb8005009
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
	.word 0x00000130
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28134
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x11000000
	.word 0x00000100
		! -------- label,sizes,reg
	.long needgc_28105
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x11000000
	.word 0x00000100
		! worddata
	.word 0x00000000
	.word 0x00000070
		! -------- label,sizes,reg
	.long code_28135
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000100
		! -------- label,sizes,reg
	.long code_28136
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x00000141
		! -------- label,sizes,reg
	.long code_28137
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44010000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28138
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44010000
	.word 0x00000171
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28139
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
	.word 0x00000100
		! -------- label,sizes,reg
	.long code_28140
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50000000
	.word 0x00000141
		! -------- label,sizes,reg
	.long code_28141
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
	.word 0x00000130
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28142
	.word 0xb800500b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x74010000
	.word 0x00000171
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_28143
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
	.word 0x00000130
		! worddata
	.word 0x00000000
	.word 0x00000060
	.text
 	.align 8
	.global Info_anonfun_code_23048
 ! arguments : [$23050,$8] [$23051,$9] [$19798,$10] [$19799,$11] 
 ! results    : [$26456,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_anonfun_code_23048:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28171
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_28171:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+112]
	mov	%r11, %r10
code_28145:
funtop_26394:
	! Proj_c at label type_18385_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+104]
	! Proj_c at label var_poly_c_15247_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+100]
	ld	[%r9], %r17
	st	%r17, [%sp+108]
	ld	[%r9+4], %r9
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28170:
	mov	%r8, %r9
code_28146:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28147
	nop
code_28148:
	call	GCFromML ! delay slot empty
	nop
needgc_28147:
sumarm_26422:
	cmp	%r9, 0
	bne	sumarm_26423
	nop
code_28150:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r17
	st	%r17, [%sp+104]
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(string_24423), %r8
	or	%r8, %lo(string_24423), %r10
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r11
code_28168:
	mov	%r8, %r11
code_28154:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r10
code_28169:
code_28156:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_26419
	or	%r0, 0, %r8
sumarm_26423:
	ld	[%sp+104], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	ble	dynamic_box_26470
	nop
code_28159:
	cmp	%r8, 255
	ble	dynamic_nobox_26471
	nop
code_28160:
	ld	[%r8], %r8
	cmp	%r8, 12
	be	dynamic_box_26470
	nop
code_28161:
	cmp	%r8, 4
	be	dynamic_box_26470
	nop
code_28162:
	cmp	%r8, 8
	be	dynamic_box_26470
	nop
dynamic_nobox_26471:
	ba	projsum_single_after_26467
	mov	%r9, %r8
dynamic_box_26470:
	ld	[%r9], %r8
projsum_single_after_26467:
	ba	after_sum_26419 ! delay slot empty
	nop
sumarm_26457:
after_sum_26419:
code_28167:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Info_anonfun_code_23048,(.-Info_anonfun_code_23048)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_28147
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28168
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_28169
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_28170
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
 	.align 8
	.global Info_convert_inner_code_23041
 ! arguments : [$23043,$8] [$23044,$9] [$19795,$10] [$19796,$11] 
 ! results    : [$26389,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_convert_inner_code_23041:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28183
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_28183:
	st	%r15, [%sp+92]
	st	%r10, [%sp+104]
	mov	%r11, %r18
code_28172:
funtop_26346:
	! Proj_c at label type_18385_INT
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	! Proj_c at label type_18360_INT
	ld	[%r8+4], %r12
	! Proj_c at label type_18358_INT
	ld	[%r8+8], %r10
	! Proj_c at label var_poly_c_15247_INT
	ld	[%r8+12], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	mov	%r10, %r9
	mov	%r12, %r10
	jmpl	%r13, %r15
	mov	%r18, %r12
code_28182:
	mov	%r8, %r9
code_28175:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28176
	nop
code_28177:
	call	GCFromML ! delay slot empty
	nop
needgc_28176:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Info_anonfun_code_23048), %r8
	or	%r8, %lo(Info_anonfun_code_23048), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_28181:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_convert_inner_code_23041,(.-Info_convert_inner_code_23041)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_28176
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_28182
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
 	.align 8
	.global Info_convert_r_code_23036
 ! arguments : [$23038,$8] [$15247,$9] [$23039,$10] [$15248,$11] 
 ! results    : [$26340,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_convert_r_code_23036:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28199
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_28199:
	st	%r15, [%sp+92]
	st	%r9, [%sp+108]
code_28184:
funtop_26307:
	! Proj_c at label 'a_TYV
	ld	[%sp+108], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+104]
	! Proj_c at label 'b_TYV
	ld	[%sp+108], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+100]
	! start making constructor call
	sethi	%hi(option_TYC), %r8
	or	%r8, %lo(option_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+100], %r9
code_28198:
	st	%r8, [%sp+96]
code_28187:
	! done making constructor call
	! start making constructor call
	sethi	%hi(option_sum_INT), %r8
	or	%r8, %lo(option_sum_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+100], %r9
code_28197:
	mov	%r8, %r9
code_28190:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28191
	nop
code_28192:
	call	GCFromML ! delay slot empty
	nop
needgc_28191:
	! done making constructor call
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Info_convert_inner_code_23041), %r8
	or	%r8, %lo(Info_convert_inner_code_23041), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_28196:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_convert_r_code_23036,(.-Info_convert_r_code_23036)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28197
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long needgc_28191
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_28198
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
 	.align 8
	.global Info_folder_code_23065
 ! arguments : [$23067,$8] [$23068,$9] [$19915,$10] [$19916,$11] [$19917,$12] 
 ! results    : [$26306,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_folder_code_23065:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28211
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_28211:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
	st	%r12, [%sp+96]
code_28200:
funtop_26279:
	ld	[%r11], %r10
	ld	[%r11+4], %r11
	! making closure call 
	sethi	%hi(crc_19912), %r8
	or	%r8, %lo(crc_19912), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28210:
	mov	%r8, %r12
code_28203:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_insert_18448), %r8
	or	%r8, %lo(strbindvar_r_insert_18448), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+100], %r11
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_28206:
	! done making tail call
code_28208:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_folder_code_23065,(.-Info_folder_code_23065)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28210
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00070000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.text
 	.align 8
	.global Info_anonfun_code_23070
 ! arguments : [$23072,$8] [$23073,$9] [$19823,$10] [$19824,$11] 
 ! results    : [$19824,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
Info_anonfun_code_23070:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28215
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28215:
	st	%r15, [%sp+92]
	mov	%r11, %r8
code_28212:
funtop_26276:
code_28214:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_23070,(.-Info_anonfun_code_23070)

	.section	".rodata"
	.text
 	.align 8
	.global Info_loop_code_23080
 ! arguments : [$23082,$8] [$23083,$9] [$19375,$10] [$19376,$11] 
 ! results    : [$26265,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_loop_code_23080:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28339
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_28339:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
	st	%r10, [%sp+112]
	st	%r11, [%sp+108]
code_28216:
funtop_25918:
	ld	[%sp+104], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+96]
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 0
	bne	one_case_25927
	nop
zero_case_25926:
	ba	after_zeroone_25928
	or	%r0, 0, %r8
one_case_25927:
	ld	[%sp+112], %r17
	cmp	%r17, 172
	or	%r0, 1, %r8
	be	cmpui_28219
	nop
code_28220:
	or	%r0, 0, %r8
cmpui_28219:
after_zeroone_25928:
	cmp	%r8, 0
	bne	one_case_25939
	nop
zero_case_25938:
	ba	after_zeroone_25940
	or	%r0, 256, %r8
one_case_25939:
	sethi	%hi(string_24006), %r8
	or	%r8, %lo(string_24006), %r10
	! making closure call 
	sethi	%hi(print), %r8
	or	%r8, %lo(print), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28333:
code_28226:
	! done making normal call
	! making external call
	call	save_regs_MLtoC
	or	%r0, 256, %r8
	call	Breakpoint ! delay slot empty
	nop
code_28319:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_28227:
	! done making external call
after_zeroone_25940:
	! making closure call 
	sethi	%hi(_19384), %r8
	or	%r8, %lo(_19384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_28318:
	st	%r8, [%sp+100]
code_28230:
	! done making normal call
	! making closure call 
	sethi	%hi(toString_17839), %r8
	or	%r8, %lo(toString_17839), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_28320:
	mov	%r8, %r10
code_28233:
	! done making normal call
	sethi	%hi(string_24043), %r8
	or	%r8, %lo(string_24043), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28321:
	st	%r8, [%sp+96]
code_28237:
	! done making normal call
	! making closure call 
	sethi	%hi(toString_17482), %r8
	or	%r8, %lo(toString_17482), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_28322:
	mov	%r8, %r11
code_28240:
	! done making normal call
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_28323:
	mov	%r8, %r10
code_28243:
	! done making normal call
	sethi	%hi(string_24047), %r8
	or	%r8, %lo(string_24047), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28324:
	mov	%r8, %r10
code_28247:
	! done making normal call
	! making closure call 
	sethi	%hi(print), %r8
	or	%r8, %lo(print), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28325:
code_28250:
	! done making normal call
	sethi	%hi(string_24048), %r8
	or	%r8, %lo(string_24048), %r11
	! making closure call 
	sethi	%hi(PLUSEstring_INT), %r8
	or	%r8, %lo(PLUSEstring_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_28326:
code_28254:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_26040
	nop
zero_case_26039:
	or	%r0, 0, %r11
	! making closure call 
	sethi	%hi(sub_17861), %r8
	or	%r8, %lo(sub_17861), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_28337:
code_28258:
	! done making normal call
	cmp	%r8, 36
	or	%r0, 1, %r8
	be	cmpui_28259
	nop
code_28260:
	or	%r0, 0, %r8
cmpui_28259:
	cmp	%r8, 0
	bne	one_case_26060
	nop
zero_case_26059:
	! making closure call 
	sethi	%hi(size), %r8
	or	%r8, %lo(size), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_28338:
code_28264:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28265
	nop
code_28266:
	call	GCFromML ! delay slot empty
	nop
needgc_28265:
	subcc	%r8, 1, %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r12
	add	%r4, 8, %r4
	! done allocating 1 record
	or	%r0, 0, %r11
	! making closure call 
	sethi	%hi(extract_17866), %r8
	or	%r8, %lo(extract_17866), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_28331:
	mov	%r8, %r10
code_28270:
	! done making normal call
	! making closure call 
	sethi	%hi(explode_17444), %r8
	or	%r8, %lo(explode_17444), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28327:
	mov	%r8, %r10
code_28273:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call 
	sethi	%hi(splitPRIME_14970), %r8
	or	%r8, %lo(splitPRIME_14970), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28328:
code_28275:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28276
	nop
code_28277:
	call	GCFromML ! delay slot empty
	nop
needgc_28276:
sumarm_26124:
	cmp	%r8, 0
	bne	sumarm_26125
	nop
code_28279:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r10
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(string_24086), %r8
	or	%r8, %lo(string_24086), %r11
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28329:
code_28282:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28283
	nop
code_28284:
	call	GCFromML ! delay slot empty
	nop
needgc_28283:
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
	ba	after_sum_26121
	mov	%r8, %r9
sumarm_26125:
	ld	[%r8], %r10
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	! making closure call 
	sethi	%hi(implode_17807), %r8
	or	%r8, %lo(implode_17807), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28336:
	st	%r8, [%sp+96]
code_28290:
	! done making normal call
	! making closure call 
	sethi	%hi(implode_17807), %r8
	or	%r8, %lo(implode_17807), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_28330:
	mov	%r8, %r9
code_28293:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28294
	nop
code_28295:
	call	GCFromML ! delay slot empty
	nop
needgc_28294:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_26121
	mov	%r8, %r9
sumarm_26149:
after_sum_26121:
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_26061
	mov	%r8, %r10
one_case_26060:
	! making closure call 
	sethi	%hi(size), %r8
	or	%r8, %lo(size), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_28335:
code_28301:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28302
	nop
code_28303:
	call	GCFromML ! delay slot empty
	nop
needgc_28302:
	subcc	%r8, 2, %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r12
	add	%r4, 8, %r4
	! done allocating 1 record
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(extract_17866), %r8
	or	%r8, %lo(extract_17866), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_28332:
	mov	%r8, %r9
code_28307:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28308
	nop
code_28309:
	call	GCFromML ! delay slot empty
	nop
needgc_28308:
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	or	%r0, 1, %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	mov	%r8, %r10
after_zeroone_26061:
	ld	[%sp+112], %r17
	addcc	%r17, 1, %r9
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	st	%r9, [%sp+112]
	ba	funtop_25918
	st	%r8, [%sp+108]
code_28311:
	! done making self tail call
	ba	after_zeroone_26041
	or	%r0, 0, %r8
one_case_26040:
	! making closure call 
	sethi	%hi(_17901), %r8
	or	%r8, %lo(_17901), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+108], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_28315:
	! done making tail call
after_zeroone_26041:
code_28317:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Info_loop_code_23080,(.-Info_loop_code_23080)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28318
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_28319
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00530000
		! worddata
	.word 0x0000000e
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_28320
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28321
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28322
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_28323
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28324
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28325
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28326
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long needgc_28265
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28327
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_28328
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long needgc_28276
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_28329
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long needgc_28283
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_28330
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long needgc_28294
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long needgc_28302
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long needgc_28308
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_28331
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_28332
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_28333
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00530000
		! worddata
	.word 0x0000000e
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_28335
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28336
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28337
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_28338
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
 	.align 8
	.global Info_anonfun_code_23099
 ! arguments : [$23101,$8] [$23102,$9] [$15230,$10] 
 ! results    : [$25882,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_anonfun_code_23099:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28361
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_28361:
	st	%r15, [%sp+92]
	st	%r10, [%sp+108]
code_28340:
funtop_25825:
	! Proj_c at label reify_22804_INT
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	! Proj_c at label var_poly_c_15224_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	ld	[%r9], %r17
	st	%r17, [%sp+104]
	ld	[%r9+4], %r10
	! making closure call 
	sethi	%hi(_18023), %r8
	or	%r8, %lo(_18023), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r11
code_28359:
code_28343:
	! done making normal call
sumarm_25857:
	cmp	%r8, 0
	bne	sumarm_25858
	nop
code_28344:
	sethi	%hi(string_24412), %r8
	or	%r8, %lo(string_24412), %r10
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r11
code_28357:
	mov	%r8, %r11
code_28348:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28358:
code_28350:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_25854
	or	%r0, 0, %r8
sumarm_25858:
	ld	[%r8], %r10
	ld	[%r8+4], %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_28353:
	! done making tail call
	ba	after_sum_25854 ! delay slot empty
	nop
sumarm_25883:
after_sum_25854:
code_28356:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_anonfun_code_23099,(.-Info_anonfun_code_23099)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28357
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_28358
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_28359
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
 	.align 8
	.global Info_getStatus_inner_code_23094
 ! arguments : [$23096,$8] [$23097,$9] [$15228,$10] 
 ! results    : [$25820,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
Info_getStatus_inner_code_23094:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28373
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_28373:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r11
	mov	%r10, %r12
code_28362:
funtop_25793:
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28363
	nop
code_28364:
	call	GCFromML ! delay slot empty
	nop
needgc_28363:
	! Proj_c at label reify_22804_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	! Proj_c at label var_poly_c_15224_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r9
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 273, %r9
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_28366
	nop
code_28367:
	or	%r0, 0, %r8
cmpui_28366:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	st	%r12, [%r4+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_28368
	nop
code_28369:
	or	%r0, 0, %r8
cmpui_28368:
	st	%r11, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Info_anonfun_code_23099), %r8
	or	%r8, %lo(Info_anonfun_code_23099), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_28372:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_getStatus_inner_code_23094,(.-Info_getStatus_inner_code_23094)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_28363
	.word 0xb8003808
	.word 0xbffc3000
	.word 0xbffc2800
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000004
	.word 0x00000060
	.text
 	.align 8
	.global Info_getStatus_r_code_23089
 ! arguments : [$23091,$8] [$15224,$9] [$23092,$10] [$15225,$11] 
 ! results    : [$25785,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
Info_getStatus_r_code_23089:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28385
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_28385:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r12
code_28374:
funtop_25772:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28375
	nop
code_28376:
	call	GCFromML ! delay slot empty
	nop
needgc_28375:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 537, %r9
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_28378
	nop
code_28379:
	or	%r0, 0, %r8
cmpui_28378:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	sethi	%hi(Info_getStatus_inner_code_23094), %r8
	or	%r8, %lo(Info_getStatus_inner_code_23094), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_28381
	nop
code_28382:
	or	%r0, 0, %r8
cmpui_28381:
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_28384:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Info_getStatus_r_code_23089,(.-Info_getStatus_r_code_23089)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_28375
	.word 0xb8003808
	.word 0xbffc2200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
	.text
 	.align 8
	.global Info_read_code_23075
 ! arguments : [$23077,$8] [$23078,$9] [$15201,$10] 
 ! results    : [$25575,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_read_code_23075:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28728
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_28728:
	st	%r15, [%sp+92]
	st	%r10, [%sp+108]
code_28386:
funtop_24947:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28387
	nop
code_28388:
	call	GCFromML ! delay slot empty
	nop
needgc_28387:
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r11
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(exn_handler_24950), %r8
	or	%r8, %lo(exn_handler_24950), %r10
	ld	[%r2+816], %r8
	sub	%sp, %r8, %r9
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	st	%r11, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r8, %r1
	! making closure call 
	sethi	%hi(fileSize_17684), %r8
	or	%r8, %lo(fileSize_17684), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_28726:
	mov	%r8, %r10
code_28394:
	! done making normal call
	! making closure call 
	sethi	%hi(toString_17681), %r8
	or	%r8, %lo(toString_17681), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28680:
	mov	%r8, %r11
code_28397:
	! done making normal call
	sethi	%hi(string_23883), %r8
	or	%r8, %lo(string_23883), %r10
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28681:
	mov	%r8, %r10
code_28401:
	! done making normal call
	sethi	%hi(string_23904), %r8
	or	%r8, %lo(string_23904), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28682:
	mov	%r8, %r10
code_28405:
	! done making normal call
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r11
code_28683:
	mov	%r8, %r10
code_28408:
	! done making normal call
	sethi	%hi(string_23906), %r8
	or	%r8, %lo(string_23906), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28684:
	mov	%r8, %r10
code_28412:
	! done making normal call
	! making closure call 
	sethi	%hi(print), %r8
	or	%r8, %lo(print), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28685:
code_28415:
	! done making normal call
	! making closure call 
	sethi	%hi(fileSize_17684), %r8
	or	%r8, %lo(fileSize_17684), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_28686:
	mov	%r8, %r9
code_28418:
	! done making normal call
	sethi	%hi(8190), %r8
	or	%r8, %lo(8190), %r8
	cmp	%r9, %r8
	or	%r0, 1, %r8
	bge	cmpsi_28419
	nop
code_28420:
	or	%r0, 0, %r8
cmpsi_28419:
	cmp	%r8, 0
	bne	one_case_25046
	nop
zero_case_25045:
	ba	after_zeroone_25047
	or	%r0, 256, %r8
one_case_25046:
	sethi	%hi(string_23922), %r8
	or	%r8, %lo(string_23922), %r10
	! making closure call 
	sethi	%hi(print), %r8
	or	%r8, %lo(print), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28727:
code_28426:
	! done making normal call
after_zeroone_25047:
	! making closure call 
	sethi	%hi(openIn_17816), %r8
	or	%r8, %lo(openIn_17816), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_28677:
	st	%r8, [%sp+100]
code_28429:
	! done making normal call
	sethi	%hi(string_23988), %r8
	or	%r8, %lo(string_23988), %r11
	! making closure call 
	sethi	%hi(PLUSEstring_INT), %r8
	or	%r8, %lo(PLUSEstring_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r10
code_28687:
code_28433:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28434
	nop
code_28435:
	call	GCFromML ! delay slot empty
	nop
needgc_28434:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 529, %r11
	sethi	%hi(TextIO_STR_c_INT), %r9
	or	%r9, %lo(TextIO_STR_c_INT), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+8], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_28439
	nop
code_28440:
	or	%r0, 0, %r9
cmpui_28439:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r11, %r11
	st	%r11, [%r4]
	sethi	%hi(TextIO_STR_c_INT), %r9
	or	%r9, %lo(TextIO_STR_c_INT), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+8], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_28443
	nop
code_28444:
	or	%r0, 0, %r9
cmpui_28443:
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Info_loop_code_23080), %r8
	or	%r8, %lo(Info_loop_code_23080), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 1, %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28678:
	st	%r8, [%sp+96]
code_28446:
	! done making normal call
	! making closure call 
	sethi	%hi(_19458), %r8
	or	%r8, %lo(_19458), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_28688:
code_28449:
	! done making normal call
	or	%r0, 1, %r10
	sethi	%hi(funarg_3_18210), %r8
	or	%r8, %lo(funarg_3_18210), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	sethi	%hi(addLines_15174), %r8
	or	%r8, %lo(addLines_15174), %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+96], %r11
code_28689:
code_28453:
	! done making normal call
	ld	[%r8+4], %r17
	st	%r17, [%sp+120]
	sethi	%hi(string_23474), %r8
	or	%r8, %lo(string_23474), %r11
	! making closure call 
	sethi	%hi(_17981), %r8
	or	%r8, %lo(_17981), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_28690:
code_28457:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28458
	nop
code_28459:
	call	GCFromML ! delay slot empty
	nop
needgc_28458:
sumarm_25163:
	cmp	%r8, 0
	bne	sumarm_25164
	nop
code_28461:
	sethi	%hi(string_24397), %r8
	or	%r8, %lo(string_24397), %r10
	sethi	%hi(string_23474), %r8
	or	%r8, %lo(string_23474), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28691:
	mov	%r8, %r11
code_28466:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28692:
code_28468:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28469
	nop
code_28470:
	call	GCFromML ! delay slot empty
	nop
needgc_28469:
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_25160
	or	%r0, 0, %r8
sumarm_25164:
	ba	after_sum_25160 ! delay slot empty
	nop
sumarm_25190:
after_sum_25160:
	ld	[%r8], %r8
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 537, %r11
	sethi	%hi(type_18162), %r9
	or	%r9, %lo(type_18162), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_28477
	nop
code_28478:
	or	%r0, 0, %r9
cmpui_28477:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r11, %r11
	st	%r11, [%r4]
	sethi	%hi(Info_getStatus_r_code_23089), %r9
	or	%r9, %lo(Info_getStatus_r_code_23089), %r9
	st	%r9, [%r4+4]
	sethi	%hi(type_18162), %r9
	or	%r9, %lo(type_18162), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	st	%r9, [%r4+8]
	sethi	%hi(type_18162), %r9
	or	%r9, %lo(type_18162), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_28484
	nop
code_28485:
	or	%r0, 0, %r9
cmpui_28484:
	st	%r8, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_28679:
	mov	%r8, %r9
code_28488:
	! done making normal call
	sethi	%hi(anonfun_15277), %r8
	or	%r8, %lo(anonfun_15277), %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28693:
	mov	%r8, %r9
code_28490:
	! done making normal call
	sethi	%hi(string_23479), %r8
	or	%r8, %lo(string_23479), %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28694:
	st	%r8, [%sp+116]
code_28492:
	! done making normal call
	sethi	%hi(time_TYC), %r8
	or	%r8, %lo(time_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_28695:
	st	%r8, [%sp+96]
code_28495:
	! done making normal call
	sethi	%hi(time_19858), %r8
	or	%r8, %lo(time_19858), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_28696:
	mov	%r8, %r9
code_28498:
	! done making normal call
	sethi	%hi(string_23491), %r8
	or	%r8, %lo(string_23491), %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28697:
	st	%r8, [%sp+112]
code_28500:
	! done making normal call
	sethi	%hi(time_19858), %r8
	or	%r8, %lo(time_19858), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_28698:
	mov	%r8, %r9
code_28503:
	! done making normal call
	sethi	%hi(string_23503), %r8
	or	%r8, %lo(string_23503), %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28699:
	st	%r8, [%sp+108]
code_28505:
	! done making normal call
	sethi	%hi(bool_TYC), %r8
	or	%r8, %lo(bool_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_28700:
	mov	%r8, %r12
code_28508:
	! done making normal call
	sethi	%hi(bool_19885), %r8
	or	%r8, %lo(bool_19885), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call 
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_28701:
	mov	%r8, %r9
code_28511:
	! done making normal call
	sethi	%hi(string_23515), %r8
	or	%r8, %lo(string_23515), %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28702:
	st	%r8, [%sp+104]
code_28513:
	! done making normal call
	sethi	%hi(string_23523), %r8
	or	%r8, %lo(string_23523), %r11
	! making closure call 
	sethi	%hi(_17981), %r8
	or	%r8, %lo(_17981), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_28703:
code_28517:
	! done making normal call
sumarm_25339:
	cmp	%r8, 0
	bne	sumarm_25340
	nop
code_28518:
	sethi	%hi(string_24397), %r8
	or	%r8, %lo(string_24397), %r10
	sethi	%hi(string_23523), %r8
	or	%r8, %lo(string_23523), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28704:
	mov	%r8, %r11
code_28523:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28705:
code_28525:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_25336
	or	%r0, 0, %r8
sumarm_25340:
	ba	after_sum_25336 ! delay slot empty
	nop
sumarm_25366:
after_sum_25336:
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	sethi	%hi(folder_15321), %r8
	or	%r8, %lo(folder_15321), %r10
	! making closure call 
	sethi	%hi(_18468), %r8
	or	%r8, %lo(_18468), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28675:
	mov	%r8, %r12
code_28532:
	! done making normal call
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_20005), %r8
	or	%r8, %lo(type_20005), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_28706:
code_28539:
	! done making normal call
	sethi	%hi(UnitEnvironment_STR_c_INT), %r9
	or	%r9, %lo(UnitEnvironment_STR_c_INT), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_28542
	nop
code_28543:
	or	%r0, 0, %r9
cmpui_28542:
	cmp	%r9, 0
	be	else_case_25413
	nop
code_28544:
	sethi	%hi(strbindvar_r_empty_18470), %r9
	or	%r9, %lo(strbindvar_r_empty_18470), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ba	after_ite_25414
	ld	[%r9], %r12
else_case_25413:
	sethi	%hi(strbindvar_r_empty_18470), %r9
	ld	[%r9+%lo(strbindvar_r_empty_18470)], %r12
after_ite_25414:
	! making closure call 
	ld	[%r8], %r11
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	mov	%r10, %r8
	jmpl	%r11, %r15
	mov	%r12, %r10
code_28673:
	mov	%r8, %r12
code_28549:
	! done making normal call
	sethi	%hi(type_18162), %r8
	or	%r8, %lo(type_18162), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_28707:
	mov	%r8, %r9
code_28556:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_28708:
	st	%r8, [%sp+100]
code_28557:
	! done making normal call
	sethi	%hi(string_23531), %r8
	or	%r8, %lo(string_23531), %r11
	! making closure call 
	sethi	%hi(_17981), %r8
	or	%r8, %lo(_17981), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_28709:
code_28561:
	! done making normal call
sumarm_25464:
	cmp	%r8, 0
	bne	sumarm_25465
	nop
code_28562:
	sethi	%hi(string_24397), %r8
	or	%r8, %lo(string_24397), %r10
	sethi	%hi(string_23531), %r8
	or	%r8, %lo(string_23531), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28710:
	mov	%r8, %r11
code_28567:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call 
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28711:
code_28569:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_25461
	or	%r0, 0, %r8
sumarm_25465:
	ba	after_sum_25461 ! delay slot empty
	nop
sumarm_25491:
after_sum_25461:
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	sethi	%hi(folder_15321), %r8
	or	%r8, %lo(folder_15321), %r10
	! making closure call 
	sethi	%hi(_18468), %r8
	or	%r8, %lo(_18468), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28676:
	mov	%r8, %r12
code_28576:
	! done making normal call
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_20005), %r8
	or	%r8, %lo(type_20005), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_28712:
code_28583:
	! done making normal call
	sethi	%hi(UnitEnvironment_STR_c_INT), %r9
	or	%r9, %lo(UnitEnvironment_STR_c_INT), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_28586
	nop
code_28587:
	or	%r0, 0, %r9
cmpui_28586:
	cmp	%r9, 0
	be	else_case_25538
	nop
code_28588:
	sethi	%hi(strbindvar_r_empty_18470), %r9
	or	%r9, %lo(strbindvar_r_empty_18470), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ba	after_ite_25539
	ld	[%r9], %r12
else_case_25538:
	sethi	%hi(strbindvar_r_empty_18470), %r9
	ld	[%r9+%lo(strbindvar_r_empty_18470)], %r12
after_ite_25539:
	! making closure call 
	ld	[%r8], %r11
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	mov	%r10, %r8
	jmpl	%r11, %r15
	mov	%r12, %r10
code_28674:
	mov	%r8, %r12
code_28593:
	! done making normal call
	sethi	%hi(type_18162), %r8
	or	%r8, %lo(type_18162), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_28713:
	mov	%r8, %r9
code_28600:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_28714:
	mov	%r8, %r9
code_28601:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28602
	nop
code_28603:
	call	GCFromML ! delay slot empty
	nop
needgc_28602:
	! allocating 6-record
	sethi	%hi(14641), %r8
	or	%r8, %lo(14641), %r8
	sethi	%hi(UnitEnvironment_STR_c_INT), %r10
	or	%r10, %lo(UnitEnvironment_STR_c_INT), %r11
	ld	[%r2+812], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_28607
	nop
code_28608:
	or	%r0, 0, %r10
cmpui_28607:
	sll	%r10, 9, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	sethi	%hi(UnitEnvironment_STR_c_INT), %r10
	or	%r10, %lo(UnitEnvironment_STR_c_INT), %r11
	ld	[%r2+812], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_28611
	nop
code_28612:
	or	%r0, 0, %r10
cmpui_28611:
	sll	%r10, 10, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	st	%r8, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	sethi	%hi(UnitEnvironment_STR_c_INT), %r8
	or	%r8, %lo(UnitEnvironment_STR_c_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_28615
	nop
code_28616:
	or	%r0, 0, %r8
cmpui_28615:
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	sethi	%hi(UnitEnvironment_STR_c_INT), %r8
	or	%r8, %lo(UnitEnvironment_STR_c_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_28619
	nop
code_28620:
	or	%r0, 0, %r8
cmpui_28619:
	st	%r9, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	ld	[%sp+112], %r17
	st	%r17, [%r4+20]
	ld	[%sp+104], %r17
	st	%r17, [%r4+24]
	add	%r4, 4, %r8
	add	%r4, 28, %r4
	! done allocating 6 record
	ba	exn_handler_after_24951
	ld	[%r1+12], %r1
exn_handler_24950:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r17
	st	%r17, [%sp+108]
	mov	%r15, %r10
	ld	[%r10], %r9
exnarm_25605:
	sethi	%hi(exn_stamp_17676), %r8
	ld	[%r8+%lo(exn_stamp_17676)], %r8
	cmp	%r9, %r8
	bne	exnarm_25608
	nop
code_28625:
	ld	[%r10+4], %r8
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
sumarm_25625:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	ble	nomatch_sum_25623
	nop
code_28626:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	sethi	%hi(string_24581), %r8
	or	%r8, %lo(string_24581), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r10
code_28715:
	st	%r8, [%sp+96]
code_28630:
	! done making normal call
	! making closure call 
	sethi	%hi(toString_17839), %r8
	or	%r8, %lo(toString_17839), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_28716:
	mov	%r8, %r11
code_28633:
	! done making normal call
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_28717:
	mov	%r8, %r10
code_28636:
	! done making normal call
	sethi	%hi(string_23904), %r8
	or	%r8, %lo(string_23904), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28718:
	mov	%r8, %r10
code_28640:
	! done making normal call
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_28719:
	st	%r8, [%sp+96]
code_28643:
	! done making normal call
	sethi	%hi(string_23437), %r8
	or	%r8, %lo(string_23437), %r10
	! making closure call 
	sethi	%hi(_20748), %r8
	or	%r8, %lo(_20748), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28720:
	mov	%r8, %r9
code_28647:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_28648:
	! done making tail call
	ba	after_sum_25622 ! delay slot empty
	nop
sumarm_25626:
nomatch_sum_25623:
	mov	%r10, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_25622:
	ba	afterPLUSexncase_25604 ! delay slot empty
	nop
exnarm_25608:
	sethi	%hi(exn_stamp_17676), %r8
	ld	[%r8+%lo(exn_stamp_17676)], %r8
	cmp	%r9, %r8
	bne	exnarm_25709
	nop
code_28653:
	ld	[%r10+4], %r9
	ld	[%r9], %r8
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
sumarm_25726:
	cmp	%r8, 0
	bne	sumarm_25727
	nop
code_28654:
	sethi	%hi(string_24584), %r8
	or	%r8, %lo(string_24584), %r11
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r10
code_28722:
	mov	%r8, %r10
code_28658:
	! done making normal call
	! making closure call 
	sethi	%hi(HAT), %r8
	or	%r8, %lo(HAT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r11
code_28723:
	st	%r8, [%sp+96]
code_28661:
	! done making normal call
	sethi	%hi(string_23437), %r8
	or	%r8, %lo(string_23437), %r10
	! making closure call 
	sethi	%hi(_20748), %r8
	or	%r8, %lo(_20748), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_28724:
	mov	%r8, %r9
code_28665:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_28666:
	! done making tail call
	ba	after_sum_25723 ! delay slot empty
	nop
sumarm_25727:
nomatch_sum_25724:
	mov	%r10, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_25723:
	ba	afterPLUSexncase_25604 ! delay slot empty
	nop
exnarm_25709:
	mov	%r10, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
afterPLUSexncase_25604:
exn_handler_after_24951:
code_28672:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Info_read_code_23075,(.-Info_read_code_23075)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28673
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35530000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28674
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055f0000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28675
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35530000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28676
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055f0000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28677
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28678
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x0000000e
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_28679
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30040000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long needgc_28387
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28680
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28681
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28682
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28683
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28684
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28685
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28686
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28687
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x0000000e
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_28434
	.word 0xb8004008
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x0000000e
	.long TextIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_28688
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_28689
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_28690
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30000000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long needgc_28458
	.word 0xb8004008
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30000000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28691
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30000000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28692
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30000000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long needgc_28469
	.word 0xb8004008
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30000000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28693
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30040000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28694
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30040000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28695
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x34040000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28696
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x34050000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28697
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x34050000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28698
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35040000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28699
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35040000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28700
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35400000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28701
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35400000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28702
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35400000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28703
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35500000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28704
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35500000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28705
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35500000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28706
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35530000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28707
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35530000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28708
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35500000
		! worddata
	.word 0x00000002
	.long type_18165
		! -------- label,sizes,reg
	.long code_28709
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055c0000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28710
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055c0000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28711
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055c0000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28712
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055f0000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28713
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055f0000
		! worddata
	.word 0x00000002
	.long type_18162
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28714
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x055c0000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_28602
	.word 0xb800400a
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x055c0000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28715
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_28716
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
		! -------- label,sizes,reg
	.long code_28717
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_28718
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_28719
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_28720
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_28722
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_28723
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_28724
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_28726
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_28727
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
 	.align 8
	.global Info_eqTime_code_23117
 ! arguments : [$23119,$8] [$23120,$9] [$20193,$10] [$20194,$11] 
 ! results    : [$24946,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_eqTime_code_23117:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28740
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28740:
	st	%r15, [%sp+92]
code_28729:
funtop_24923:
	! making closure call 
	sethi	%hi(compare_18563), %r8
	or	%r8, %lo(compare_18563), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_28739:
	mov	%r8, %r10
code_28732:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call 
	sethi	%hi(PLUSEorder_INT), %r8
	or	%r8, %lo(PLUSEorder_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_28735:
	! done making tail call
code_28737:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_eqTime_code_23117,(.-Info_eqTime_code_23117)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28739
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global Info_eqBool_code_23122
 ! arguments : [$23124,$8] [$23125,$9] [$20206,$10] [$20207,$11] 
 ! results    : [$24922,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_eqBool_code_23122:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28748
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28748:
	st	%r15, [%sp+92]
code_28741:
funtop_24910:
	! making closure call 
	sethi	%hi(PLUSEbool_INT), %r8
	or	%r8, %lo(PLUSEbool_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_28744:
	! done making tail call
code_28746:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_eqBool_code_23122,(.-Info_eqBool_code_23122)

	.section	".rodata"
	.text
 	.align 8
	.global Info_eqUe_code_23127
 ! arguments : [$23129,$8] [$23130,$9] [$20214,$10] [$20215,$11] 
 ! results    : [$24909,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_eqUe_code_23127:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28756
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28756:
	st	%r15, [%sp+92]
code_28749:
funtop_24897:
	! making closure call 
	sethi	%hi(strbindvar_r_equal_18582), %r8
	or	%r8, %lo(strbindvar_r_equal_18582), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_28752:
	! done making tail call
code_28754:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_eqUe_code_23127,(.-Info_eqUe_code_23127)

	.section	".rodata"
	.text
 	.align 8
	.global Info_anonfun_code_23132
 ! arguments : [$23134,$8] [$23135,$9] [$20258,$10] [$20259,$11] [$20260,$12] [$20261,$13] [$20262,$18] [$20263,$19] 
 ! results    : [$20262,$8] 
 ! destroys   :  $19 $13 $12 $11 $10 $9 $8
 ! modifies   :  $19 $13 $12 $11 $10 $9 $8
Info_anonfun_code_23132:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28760
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28760:
	st	%r15, [%sp+92]
	mov	%r18, %r8
code_28757:
funtop_24894:
code_28759:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_23132,(.-Info_anonfun_code_23132)

	.section	".rodata"
	.text
 	.align 8
	.global Info_anonfun_code_23137
 ! arguments : [$23139,$8] [$23140,$9] [$20295,$10] [$20296,$11] [$20297,$12] [$20298,$13] [$20299,$18] [$20300,$19] 
 ! results    : [$20298,$8] 
 ! destroys   :  $19 $18 $12 $11 $10 $9 $8
 ! modifies   :  $19 $18 $12 $11 $10 $9 $8
Info_anonfun_code_23137:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28764
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28764:
	st	%r15, [%sp+92]
	mov	%r13, %r8
code_28761:
funtop_24891:
code_28763:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_23137,(.-Info_anonfun_code_23137)

	.section	".rodata"
	.text
 	.align 8
	.global Info_anonfun_code_23142
 ! arguments : [$23144,$8] [$23145,$9] [$20332,$10] [$20333,$11] [$20334,$12] [$20335,$13] [$20336,$18] [$20337,$19] 
 ! results    : [$20337,$8] 
 ! destroys   :  $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $18 $13 $12 $11 $10 $9 $8
Info_anonfun_code_23142:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28768
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28768:
	st	%r15, [%sp+92]
	mov	%r19, %r8
code_28765:
funtop_24888:
code_28767:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_23142,(.-Info_anonfun_code_23142)

	.section	".rodata"
	.text
 	.align 8
	.global Info_anonfun_code_23147
 ! arguments : [$23149,$8] [$23150,$9] [$20369,$10] [$20370,$11] [$20371,$12] [$20372,$13] [$20373,$18] [$20374,$19] 
 ! results    : [$20370,$8] 
 ! destroys   :  $19 $18 $13 $12 $10 $9 $8
 ! modifies   :  $19 $18 $13 $12 $10 $9 $8
Info_anonfun_code_23147:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28772
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28772:
	st	%r15, [%sp+92]
	mov	%r11, %r8
code_28769:
funtop_24885:
code_28771:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_23147,(.-Info_anonfun_code_23147)

	.section	".rodata"
	.text
 	.align 8
	.global Info_anonfun_code_23152
 ! arguments : [$23154,$8] [$23155,$9] [$20406,$10] [$20407,$11] [$20408,$12] [$20409,$13] [$20410,$18] [$20411,$19] 
 ! results    : [$20408,$8] 
 ! destroys   :  $19 $18 $13 $11 $10 $9 $8
 ! modifies   :  $19 $18 $13 $11 $10 $9 $8
Info_anonfun_code_23152:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28776
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_28776:
	st	%r15, [%sp+92]
	mov	%r12, %r8
code_28773:
funtop_24882:
code_28775:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Info_anonfun_code_23152,(.-Info_anonfun_code_23152)

	.section	".rodata"
	.text
 	.align 8
	.global Info_equal_code_23157
 ! arguments : [$23159,$8] [$23160,$9] [$20222,$10] [$20223,$11] 
 ! results    : [$24848,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_equal_code_23157:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_28833
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_28833:
	st	%r15, [%sp+92]
code_28777:
funtop_24659:
	ld	[%r10], %r17
	st	%r17, [%sp+144]
	ld	[%r10+4], %r17
	st	%r17, [%sp+140]
	ld	[%r10+8], %r17
	st	%r17, [%sp+136]
	ld	[%r10+12], %r17
	st	%r17, [%sp+132]
	ld	[%r10+16], %r17
	st	%r17, [%sp+128]
	ld	[%r10+20], %r17
	st	%r17, [%sp+124]
	ld	[%r11], %r17
	st	%r17, [%sp+120]
	ld	[%r11+4], %r17
	st	%r17, [%sp+116]
	ld	[%r11+8], %r17
	st	%r17, [%sp+112]
	ld	[%r11+12], %r17
	st	%r17, [%sp+108]
	ld	[%r11+16], %r17
	st	%r17, [%sp+104]
	ld	[%r11+20], %r17
	st	%r17, [%sp+100]
	! making closure call 
	sethi	%hi(anonfun_15434), %r8
	or	%r8, %lo(anonfun_15434), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+144], %r10
	ld	[%sp+140], %r11
	ld	[%sp+136], %r12
	ld	[%sp+132], %r13
	ld	[%sp+128], %r18
	jmpl	%r20, %r15
	ld	[%sp+124], %r19
code_28828:
	st	%r8, [%sp+96]
code_28779:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_15434), %r8
	or	%r8, %lo(anonfun_15434), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+120], %r10
	ld	[%sp+116], %r11
	ld	[%sp+112], %r12
	ld	[%sp+108], %r13
	ld	[%sp+104], %r18
	jmpl	%r20, %r15
	ld	[%sp+100], %r19
code_28818:
	mov	%r8, %r11
code_28781:
	! done making normal call
	! making closure call 
	sethi	%hi(eqTime_15396), %r8
	or	%r8, %lo(eqTime_15396), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_28819:
code_28783:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_24721
	nop
zero_case_24720:
	ba	after_zeroone_24722
	or	%r0, 0, %r8
one_case_24721:
	! making closure call 
	sethi	%hi(anonfun_15445), %r8
	or	%r8, %lo(anonfun_15445), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+144], %r10
	ld	[%sp+140], %r11
	ld	[%sp+136], %r12
	ld	[%sp+132], %r13
	ld	[%sp+128], %r18
	jmpl	%r20, %r15
	ld	[%sp+124], %r19
code_28829:
	st	%r8, [%sp+96]
code_28787:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_15445), %r8
	or	%r8, %lo(anonfun_15445), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+120], %r10
	ld	[%sp+116], %r11
	ld	[%sp+112], %r12
	ld	[%sp+108], %r13
	ld	[%sp+104], %r18
	jmpl	%r20, %r15
	ld	[%sp+100], %r19
code_28820:
	mov	%r8, %r11
code_28789:
	! done making normal call
	! making closure call 
	sethi	%hi(eqTime_15396), %r8
	or	%r8, %lo(eqTime_15396), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_28821:
code_28791:
	! done making normal call
after_zeroone_24722:
	cmp	%r8, 0
	bne	one_case_24762
	nop
zero_case_24761:
	ba	after_zeroone_24763
	or	%r0, 0, %r8
one_case_24762:
	! making closure call 
	sethi	%hi(anonfun_15456), %r8
	or	%r8, %lo(anonfun_15456), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+144], %r10
	ld	[%sp+140], %r11
	ld	[%sp+136], %r12
	ld	[%sp+132], %r13
	ld	[%sp+128], %r18
	jmpl	%r20, %r15
	ld	[%sp+124], %r19
code_28830:
	st	%r8, [%sp+96]
code_28795:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_15456), %r8
	or	%r8, %lo(anonfun_15456), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+120], %r10
	ld	[%sp+116], %r11
	ld	[%sp+112], %r12
	ld	[%sp+108], %r13
	ld	[%sp+104], %r18
	jmpl	%r20, %r15
	ld	[%sp+100], %r19
code_28822:
	mov	%r8, %r11
code_28797:
	! done making normal call
	! making closure call 
	sethi	%hi(eqBool_15401), %r8
	or	%r8, %lo(eqBool_15401), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_28823:
code_28799:
	! done making normal call
after_zeroone_24763:
	cmp	%r8, 0
	bne	one_case_24803
	nop
zero_case_24802:
	ba	after_zeroone_24804
	or	%r0, 0, %r8
one_case_24803:
	! making closure call 
	sethi	%hi(anonfun_15466), %r8
	or	%r8, %lo(anonfun_15466), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+144], %r10
	ld	[%sp+140], %r11
	ld	[%sp+136], %r12
	ld	[%sp+132], %r13
	ld	[%sp+128], %r18
	jmpl	%r20, %r15
	ld	[%sp+124], %r19
code_28831:
	st	%r8, [%sp+96]
code_28803:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_15466), %r8
	or	%r8, %lo(anonfun_15466), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+120], %r10
	ld	[%sp+116], %r11
	ld	[%sp+112], %r12
	ld	[%sp+108], %r13
	ld	[%sp+104], %r18
	jmpl	%r20, %r15
	ld	[%sp+100], %r19
code_28824:
	mov	%r8, %r11
code_28805:
	! done making normal call
	! making closure call 
	sethi	%hi(eqUe_15408), %r8
	or	%r8, %lo(eqUe_15408), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_28825:
code_28807:
	! done making normal call
after_zeroone_24804:
	cmp	%r8, 0
	bne	one_case_24844
	nop
zero_case_24843:
	ba	after_zeroone_24845
	or	%r0, 0, %r8
one_case_24844:
	! making closure call 
	sethi	%hi(anonfun_15476), %r8
	or	%r8, %lo(anonfun_15476), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+144], %r10
	ld	[%sp+140], %r11
	ld	[%sp+136], %r12
	ld	[%sp+132], %r13
	ld	[%sp+128], %r18
	jmpl	%r20, %r15
	ld	[%sp+124], %r19
code_28832:
	st	%r8, [%sp+96]
code_28811:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_15476), %r8
	or	%r8, %lo(anonfun_15476), %r9
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+120], %r10
	ld	[%sp+116], %r11
	ld	[%sp+112], %r12
	ld	[%sp+108], %r13
	ld	[%sp+104], %r18
	jmpl	%r20, %r15
	ld	[%sp+100], %r19
code_28826:
	mov	%r8, %r11
code_28813:
	! done making normal call
	! making closure call 
	sethi	%hi(eqUe_15408), %r8
	or	%r8, %lo(eqUe_15408), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_28827:
code_28815:
	! done making normal call
after_zeroone_24845:
code_28817:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size Info_equal_code_23157,(.-Info_equal_code_23157)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_28818
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f550000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28819
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28820
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f550000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28821
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28822
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f550000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28823
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28824
	.word 0xb8005011
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f570000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28825
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28826
	.word 0xb8005009
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
	.word 0x00000000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28827
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_28828
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28829
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28830
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28831
	.word 0xb800500f
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f540000
	.word 0x000001f5
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
		! -------- label,sizes,reg
	.long code_28832
	.word 0xb800500b
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x1f540000
	.word 0x00000000
		! worddata
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.word 0x00000006
	.long UnitEnvironment_STR_c_INT
	.text
 	.align 8
	.global Info_main
 ! arguments : 
 ! results    : [$24658,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Info_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 176, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_29888
	mov	%sp, %fp
	add	%sp, 176, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 176, %sp
code_29888:
	st	%r15, [%sp+92]
code_28834:
funtop_23165:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_29884:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_28835:
	sethi	%hi(UnitEnvironment_STR_c_INT), %r8
	or	%r8, %lo(UnitEnvironment_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label ue_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_17266+-4), %r8
	st	%r9, [%r8+%lo(type_17266+-4)]
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_28842
	nop
code_28843:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_28842:
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r12
	ld	[%r2+812], %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(Time_STR_c_INT), %r8
	or	%r8, %lo(Time_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label time_TYC
	ld	[%r8+4], %r17
	st	%r17, [%sp+160]
	! allocating 8-record
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28856
	nop
code_28857:
	call	GCFromML ! delay slot empty
	nop
needgc_28856:
	sethi	%hi(64577), %r8
	or	%r8, %lo(64577), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 6, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+20]
	ld	[%sp+160], %r17
	st	%r17, [%r4+24]
	ld	[%sp+160], %r17
	st	%r17, [%r4+28]
	sethi	%hi(bool_TYC), %r8
	or	%r8, %lo(bool_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+32]
	add	%r4, 4, %r8
	add	%r4, 36, %r4
	! done allocating 8 record
	! allocating 1-record
	! done allocating 1 record
	or	%r0, 111, %r10
	sethi	%hi(Info_STR_c_INT+-4), %r9
	st	%r10, [%r9+%lo(Info_STR_c_INT+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_28871
	nop
code_28872:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_28871:
	sethi	%hi(Info_STR_c_INT), %r9
	or	%r9, %lo(Info_STR_c_INT), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Date_STR_c_INT), %r8
	or	%r8, %lo(Date_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label date_TYC
	ld	[%r8+24], %r17
	st	%r17, [%sp+156]
	! allocating 2-record
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28885
	nop
code_28886:
	call	GCFromML ! delay slot empty
	nop
needgc_28885:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(time_TYC), %r8
	or	%r8, %lo(time_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+4]
	sethi	%hi(time_TYC), %r8
	or	%r8, %lo(time_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+152]
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(TextIO_STR_c_INT), %r8
	or	%r8, %lo(TextIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label vector_TYC
	ld	[%r8], %r9
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	st	%r9, [%r4+16]
	add	%r4, 4, %r17
	st	%r17, [%sp+148]
	add	%r4, 20, %r4
	! done allocating 4 record
	sethi	%hi(TextIO_STR_c_INT), %r8
	or	%r8, %lo(TextIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label outstream_TYC
	ld	[%r8+12], %r17
	st	%r17, [%sp+144]
	sethi	%hi(TextIO_STR_c_INT), %r8
	or	%r8, %lo(TextIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label instream_TYC
	ld	[%r8+8], %r17
	st	%r17, [%sp+140]
	sethi	%hi(Help_STR_c_INT), %r8
	or	%r8, %lo(Help_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label StringMap_STR
	ld	[%r8], %r8
	or	%r0, 111, %r10
	sethi	%hi(type_21052+-4), %r9
	st	%r10, [%r9+%lo(type_21052+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_28904
	nop
code_28905:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_28904:
	sethi	%hi(type_21052), %r9
	or	%r9, %lo(type_21052), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(type_21052), %r8
	or	%r8, %lo(type_21052), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r17
	st	%r17, [%sp+112]
	! allocating 4-record
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28918
	nop
code_28919:
	call	GCFromML ! delay slot empty
	nop
needgc_28918:
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r17
	st	%r17, [%sp+108]
	add	%r4, 20, %r4
	! done allocating 4 record
	! start making constructor call
	ld	[%sp+112], %r17
	ld	[%r17], %r10
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+108], %r9
code_29885:
code_28923:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_18162+-4), %r9
	st	%r10, [%r9+%lo(type_18162+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_28928
	nop
code_28929:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_28928:
	sethi	%hi(type_18162), %r9
	or	%r9, %lo(type_18162), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! start making constructor call
	sethi	%hi(list_TYC), %r8
	or	%r8, %lo(list_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_29853:
code_28944:
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28945
	nop
code_28946:
	call	GCFromML ! delay slot empty
	nop
needgc_28945:
	! done making constructor call
	! allocating 4-record
	or	%r0, 3105, %r9
	st	%r9, [%r4]
	or	%r0, 5, %r9
	st	%r9, [%r4+4]
	or	%r0, 2, %r9
	st	%r9, [%r4+8]
	sethi	%hi(type_18162), %r9
	or	%r9, %lo(type_18162), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	st	%r9, [%r4+12]
	st	%r8, [%r4+16]
	add	%r4, 4, %r17
	st	%r17, [%sp+104]
	add	%r4, 20, %r4
	! done allocating 4 record
	! start making constructor call
	ld	[%sp+112], %r17
	ld	[%r17], %r10
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+104], %r9
code_29886:
code_28950:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_18165+-4), %r9
	st	%r10, [%r9+%lo(type_18165+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_28955
	nop
code_28956:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_28955:
	sethi	%hi(type_18165), %r9
	or	%r9, %lo(type_18165), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	or	%r0, 111, %r9
	sethi	%hi(type_17978+-4), %r8
	st	%r9, [%r8+%lo(type_17978+-4)]
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r12
	ld	[%r2+812], %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	add	%r12, %r11, %r8
	ld	[%sp+104], %r17
	st	%r17, [%r8]
	or	%r0, 111, %r9
	sethi	%hi(type_20690+-4), %r8
	st	%r9, [%r8+%lo(type_20690+-4)]
	sethi	%hi(type_20690), %r8
	or	%r8, %lo(type_20690), %r12
	ld	[%r2+812], %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	add	%r12, %r11, %r8
	ld	[%sp+108], %r17
	st	%r17, [%r8]
	! allocating 2-record
	add	%r4, 64, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_28987
	nop
code_28988:
	call	GCFromML ! delay slot empty
	nop
needgc_28987:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+4]
	sethi	%hi(time_TYC), %r8
	or	%r8, %lo(time_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+136]
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+4]
	sethi	%hi(bool_TYC), %r8
	or	%r8, %lo(bool_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+132]
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+4]
	sethi	%hi(Crc_STR_c_INT), %r8
	or	%r8, %lo(Crc_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+128]
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+124]
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	or	%r0, 6, %r8
	st	%r8, [%r4+4]
	sethi	%hi(type_18162), %r8
	or	%r8, %lo(type_18162), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 111, %r10
	sethi	%hi(type_20005+-4), %r9
	st	%r10, [%r9+%lo(type_20005+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29012
	nop
code_29013:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29012:
	sethi	%hi(type_20005), %r9
	or	%r9, %lo(type_20005), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 8-record
	add	%r4, 76, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_29024
	nop
code_29025:
	call	GCFromML ! delay slot empty
	nop
needgc_29024:
	sethi	%hi(64577), %r8
	or	%r8, %lo(64577), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 6, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	sethi	%hi(type_17266), %r8
	or	%r8, %lo(type_17266), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+20]
	sethi	%hi(time_TYC), %r8
	or	%r8, %lo(time_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+24]
	sethi	%hi(time_TYC), %r8
	or	%r8, %lo(time_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+28]
	sethi	%hi(bool_TYC), %r8
	or	%r8, %lo(bool_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+32]
	add	%r4, 4, %r17
	st	%r17, [%sp+120]
	add	%r4, 36, %r4
	! done allocating 8 record
	sethi	%hi(Util_STR_r_INT), %r8
	or	%r8, %lo(Util_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r17
	st	%r17, [%sp+116]
	sethi	%hi(Date_STR_r_INT), %r8
	or	%r8, %lo(Date_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+52], %r9
	sethi	%hi(Time_STR_r_INT), %r8
	or	%r8, %lo(Time_STR_r_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r10
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	! allocating 7-record
	sethi	%hi(16953), %r10
	or	%r10, %lo(16953), %r10
	st	%r10, [%r4]
	or	%r0, 0, %r10
	st	%r10, [%r4+4]
	or	%r0, 4, %r10
	st	%r10, [%r4+8]
	or	%r0, 1, %r10
	st	%r10, [%r4+12]
	or	%r0, 0, %r10
	st	%r10, [%r4+16]
	or	%r0, 2000, %r10
	st	%r10, [%r4+20]
	or	%r0, 0, %r10
	st	%r10, [%r4+24]
	st	%r8, [%r4+28]
	add	%r4, 4, %r10
	add	%r4, 32, %r4
	! done allocating 7 record
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_29887:
	st	%r8, [%sp+104]
code_29045:
	! done making normal call
	sethi	%hi(Date_STR_r_INT), %r8
	or	%r8, %lo(Date_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+64], %r12
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+156], %r9
	jmpl	%r13, %r15
	ld	[%sp+160], %r10
code_29877:
	mov	%r8, %r9
code_29050:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_29878:
code_29051:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(baseTime_19070+-4), %r9
	st	%r10, [%r9+%lo(baseTime_19070+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29056
	nop
code_29057:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29056:
	sethi	%hi(baseTime_19070), %r9
	or	%r9, %lo(baseTime_19070), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Time_STR_r_INT), %r8
	or	%r8, %lo(Time_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+80], %r8
	or	%r0, 111, %r10
	sethi	%hi(toString_17326+-4), %r9
	st	%r10, [%r9+%lo(toString_17326+-4)]
	sethi	%hi(toString_17326), %r9
	or	%r9, %lo(toString_17326), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Time_STR_r_INT), %r8
	or	%r8, %lo(Time_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+52], %r8
	or	%r0, 111, %r10
	sethi	%hi(MINUS_17327+-4), %r9
	st	%r10, [%r9+%lo(MINUS_17327+-4)]
	sethi	%hi(MINUS_17327), %r9
	or	%r9, %lo(MINUS_17327), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Option_STR_r_INT), %r8
	or	%r8, %lo(Option_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+28], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+152], %r9
code_29854:
code_29094:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_17337+-4), %r9
	st	%r10, [%r9+%lo(_17337+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 72, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29099
	nop
code_29100:
	sub	%r4, 72, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29099:
	sethi	%hi(_17337), %r9
	or	%r9, %lo(_17337), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Time_STR_r_INT), %r8
	or	%r8, %lo(Time_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+48], %r8
	or	%r0, 111, %r10
	sethi	%hi(PLUS_17338+-4), %r9
	st	%r10, [%r9+%lo(PLUS_17338+-4)]
	sethi	%hi(PLUS_17338), %r9
	or	%r9, %lo(PLUS_17338), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_14744), %r8
	or	%r8, %lo(anonfun_14744), %r8
	! done allocating 1 closures
	sethi	%hi(Time_STR_r_INT), %r8
	or	%r8, %lo(Time_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+84], %r8
	or	%r0, 111, %r10
	sethi	%hi(fromString_17342+-4), %r9
	st	%r10, [%r9+%lo(fromString_17342+-4)]
	sethi	%hi(fromString_17342), %r9
	or	%r9, %lo(fromString_17342), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(timeFromString_14736), %r8
	or	%r8, %lo(timeFromString_14736), %r8
	! done allocating 1 closures
	sethi	%hi(TextIO_STR_r_INT), %r8
	or	%r8, %lo(TextIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+20], %r8
	or	%r0, 111, %r10
	sethi	%hi(openOut_17449+-4), %r9
	st	%r10, [%r9+%lo(openOut_17449+-4)]
	sethi	%hi(openOut_17449), %r9
	or	%r9, %lo(openOut_17449), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(TextIO_STR_r_INT), %r8
	or	%r8, %lo(TextIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r8
	or	%r0, 111, %r10
	sethi	%hi(output_17453+-4), %r9
	st	%r10, [%r9+%lo(output_17453+-4)]
	sethi	%hi(output_17453), %r9
	or	%r9, %lo(output_17453), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(TextIO_STR_r_INT), %r8
	or	%r8, %lo(TextIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+36], %r8
	or	%r0, 111, %r10
	sethi	%hi(output1_17456+-4), %r9
	st	%r10, [%r9+%lo(output1_17456+-4)]
	sethi	%hi(output1_17456), %r9
	or	%r9, %lo(output1_17456), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(List_STR_r_INT), %r8
	or	%r8, %lo(List_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+12], %r10
	or	%r0, 0, %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%r10], %r12
	ld	[%r10+4], %r8
	jmpl	%r12, %r15
	ld	[%r10+8], %r10
code_29855:
code_29175:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_17350+-4), %r9
	st	%r10, [%r9+%lo(_17350+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29180
	nop
code_29181:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29180:
	sethi	%hi(_17350), %r9
	or	%r9, %lo(_17350), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_14754), %r8
	or	%r8, %lo(anonfun_14754), %r8
	! done allocating 1 closures
	sethi	%hi(String_STR_r_INT), %r8
	or	%r8, %lo(String_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+36], %r8
	or	%r0, 111, %r10
	sethi	%hi(explode_17444+-4), %r9
	st	%r10, [%r9+%lo(explode_17444+-4)]
	sethi	%hi(explode_17444), %r9
	or	%r9, %lo(explode_17444), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(record_23237), %r8
	or	%r8, %lo(record_23237), %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_29856:
code_29206:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_20644+-4), %r9
	st	%r10, [%r9+%lo(_20644+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29211
	nop
code_29212:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29211:
	sethi	%hi(_20644), %r9
	or	%r9, %lo(_20644), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(String_STR_r_INT), %r8
	or	%r8, %lo(String_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+44], %r8
	or	%r0, 111, %r10
	sethi	%hi(toString_17482+-4), %r9
	st	%r10, [%r9+%lo(toString_17482+-4)]
	sethi	%hi(toString_17482), %r9
	or	%r9, %lo(toString_17482), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Crc_STR_r_INT), %r8
	or	%r8, %lo(Crc_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r12
	sethi	%hi(Crc_STR_c_INT), %r8
	or	%r8, %lo(Crc_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r11
	ld	[%r2+812], %r8
	add	%r11, %r8, %r8
	ld	[%r8], %r11
	ld	[%r11], %r13
	ld	[%r11+4], %r8
	jmpl	%r13, %r15
	ld	[%r11+8], %r11
code_29857:
code_29243:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_19238+-4), %r9
	st	%r10, [%r9+%lo(_19238+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29248
	nop
code_29249:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29248:
	sethi	%hi(_19238), %r9
	or	%r9, %lo(_19238), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(app_r_INT), %r8
	or	%r8, %lo(app_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+148], %r9
code_29858:
code_29262:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_17507+-4), %r9
	st	%r10, [%r9+%lo(_17507+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29267
	nop
code_29268:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29267:
	sethi	%hi(_17507), %r9
	or	%r9, %lo(_17507), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Bool_STR_r_INT), %r8
	or	%r8, %lo(Bool_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r8
	or	%r0, 111, %r10
	sethi	%hi(toString_17517+-4), %r9
	st	%r10, [%r9+%lo(toString_17517+-4)]
	sethi	%hi(toString_17517), %r9
	or	%r9, %lo(toString_17517), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(UnitEnvironment_STR_r_INT), %r8
	or	%r8, %lo(UnitEnvironment_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r8
	or	%r0, 111, %r10
	sethi	%hi(strbindvar_r_appi_17577+-4), %r9
	st	%r10, [%r9+%lo(strbindvar_r_appi_17577+-4)]
	sethi	%hi(strbindvar_r_appi_17577), %r9
	or	%r9, %lo(strbindvar_r_appi_17577), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(TextIO_STR_r_INT), %r8
	or	%r8, %lo(TextIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+44], %r12
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_23838), %r8
	or	%r8, %lo(record_23838), %r10
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+144], %r9
code_29859:
code_29308:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_19300+-4), %r9
	st	%r10, [%r9+%lo(_19300+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29313
	nop
code_29314:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29313:
	sethi	%hi(_19300), %r9
	or	%r9, %lo(_19300), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(write_14802), %r8
	or	%r8, %lo(write_14802), %r8
	! done allocating 1 closures
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	sethi	%hi(exn_stamp_17676), %r8
	st	%r10, [%r8+%lo(exn_stamp_17676)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_14949), %r8
	or	%r8, %lo(anonfun_14949), %r8
	! done allocating 1 closures
	sethi	%hi(Help_STR_r_INT), %r8
	or	%r8, %lo(Help_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+40], %r17
	st	%r17, [%sp+112]
	sethi	%hi(Position_STR_r_INT), %r8
	or	%r8, %lo(Position_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+16], %r8
	or	%r0, 111, %r10
	sethi	%hi(toString_17681+-4), %r9
	st	%r10, [%r9+%lo(toString_17681+-4)]
	sethi	%hi(toString_17681), %r9
	or	%r9, %lo(toString_17681), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(OS_STR_r_INT), %r8
	or	%r8, %lo(OS_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	ld	[%r8+52], %r8
	or	%r0, 111, %r10
	sethi	%hi(fileSize_17684+-4), %r9
	st	%r10, [%r9+%lo(fileSize_17684+-4)]
	sethi	%hi(fileSize_17684), %r9
	or	%r9, %lo(fileSize_17684), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	or	%r0, 0, %r13
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(rev_r_INT), %r8
	or	%r8, %lo(rev_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_29860:
code_29357:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_17749+-4), %r9
	st	%r10, [%r9+%lo(_17749+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29362
	nop
code_29363:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29362:
	sethi	%hi(_17749), %r9
	or	%r9, %lo(_17749), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(splitPRIME_14970), %r8
	or	%r8, %lo(splitPRIME_14970), %r8
	! done allocating 1 closures
	sethi	%hi(TextIO_STR_r_INT), %r8
	or	%r8, %lo(TextIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+12], %r8
	or	%r0, 111, %r10
	sethi	%hi(openIn_17816+-4), %r9
	st	%r10, [%r9+%lo(openIn_17816+-4)]
	sethi	%hi(openIn_17816), %r9
	or	%r9, %lo(openIn_17816), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(TextIO_STR_r_INT), %r8
	or	%r8, %lo(TextIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+4], %r12
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+140], %r9
code_29861:
code_29393:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_19384+-4), %r9
	st	%r10, [%r9+%lo(_19384+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 60, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29398
	nop
code_29399:
	sub	%r4, 60, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29398:
	sethi	%hi(_19384), %r9
	or	%r9, %lo(_19384), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Int_STR_r_INT), %r8
	or	%r8, %lo(Int_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+16], %r8
	or	%r0, 111, %r10
	sethi	%hi(toString_17839+-4), %r9
	st	%r10, [%r9+%lo(toString_17839+-4)]
	sethi	%hi(toString_17839), %r9
	or	%r9, %lo(toString_17839), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(String_STR_r_INT), %r8
	or	%r8, %lo(String_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r8
	or	%r0, 111, %r10
	sethi	%hi(sub_17861+-4), %r9
	st	%r10, [%r9+%lo(sub_17861+-4)]
	sethi	%hi(sub_17861), %r9
	or	%r9, %lo(sub_17861), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(String_STR_r_INT), %r8
	or	%r8, %lo(String_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	or	%r0, 111, %r10
	sethi	%hi(extract_17866+-4), %r9
	st	%r10, [%r9+%lo(extract_17866+-4)]
	sethi	%hi(extract_17866), %r9
	or	%r9, %lo(extract_17866), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(String_STR_r_INT), %r8
	or	%r8, %lo(String_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r8
	or	%r0, 111, %r10
	sethi	%hi(implode_17807+-4), %r9
	st	%r10, [%r9+%lo(implode_17807+-4)]
	sethi	%hi(implode_17807), %r9
	or	%r9, %lo(implode_17807), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(record_23210), %r8
	or	%r8, %lo(record_23210), %r13
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(rev_r_INT), %r8
	or	%r8, %lo(rev_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_29862:
code_29461:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_17901+-4), %r9
	st	%r10, [%r9+%lo(_17901+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29466
	nop
code_29467:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29466:
	sethi	%hi(_17901), %r9
	or	%r9, %lo(_17901), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(TextIO_STR_r_INT), %r8
	or	%r8, %lo(TextIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r12
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_24124), %r8
	or	%r8, %lo(record_24124), %r10
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+140], %r9
code_29863:
code_29483:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_19458+-4), %r9
	st	%r10, [%r9+%lo(_19458+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29488
	nop
code_29489:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29488:
	sethi	%hi(_19458), %r9
	or	%r9, %lo(_19458), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	ld	[%sp+112], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+108]
	ld	[%sp+112], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+104]
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_29864:
code_29502:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_17981+-4), %r9
	st	%r10, [%r9+%lo(_17981+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29507
	nop
code_29508:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29507:
	sethi	%hi(_17981), %r9
	or	%r9, %lo(_17981), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(Help_STR_c_INT), %r8
	or	%r8, %lo(Help_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label StringMap_STR
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r11
	! start making constructor call
	sethi	%hi(type_20690), %r8
	or	%r8, %lo(type_20690), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_29865:
	st	%r8, [%sp+96]
code_29523:
	! done making constructor call
	sethi	%hi(type_20690), %r8
	or	%r8, %lo(type_20690), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_29879:
	mov	%r8, %r10
code_29526:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_29527
	nop
code_29528:
	call	GCFromML ! delay slot empty
	nop
needgc_29527:
	! allocating 2-record
	or	%r0, 529, %r9
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_29530
	nop
code_29531:
	or	%r0, 0, %r8
cmpui_29530:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_29532
	nop
code_29533:
	or	%r0, 0, %r8
cmpui_29532:
	st	%r10, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(emptySection_20700+-4), %r9
	st	%r10, [%r9+%lo(emptySection_20700+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29538
	nop
code_29539:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29538:
	sethi	%hi(emptySection_20700), %r9
	or	%r9, %lo(emptySection_20700), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(type_20690), %r8
	or	%r8, %lo(type_20690), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(isSome_r_INT), %r8
	or	%r8, %lo(isSome_r_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r12
	ld	[%r10+4], %r8
	jmpl	%r12, %r15
	ld	[%r10+8], %r10
code_29866:
code_29554:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_18018+-4), %r9
	st	%r10, [%r9+%lo(_18018+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29559
	nop
code_29560:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29559:
	sethi	%hi(_18018), %r9
	or	%r9, %lo(_18018), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(type_20690), %r8
	or	%r8, %lo(type_20690), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_29867:
code_29573:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_18023+-4), %r9
	st	%r10, [%r9+%lo(_18023+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29578
	nop
code_29579:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29578:
	sethi	%hi(_18023), %r9
	or	%r9, %lo(_18023), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	ld	[%sp+112], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+104]
	sethi	%hi(type_20690), %r8
	or	%r8, %lo(type_20690), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_29868:
code_29592:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_18036+-4), %r9
	st	%r10, [%r9+%lo(_18036+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29597
	nop
code_29598:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29597:
	sethi	%hi(_18036), %r9
	or	%r9, %lo(_18036), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_29869:
code_29611:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_18066+-4), %r9
	st	%r10, [%r9+%lo(_18066+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29616
	nop
code_29617:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29616:
	sethi	%hi(_18066), %r9
	or	%r9, %lo(_18066), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r13
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(isSome_r_INT), %r8
	or	%r8, %lo(isSome_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_29870:
code_29632:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_18080+-4), %r9
	st	%r10, [%r9+%lo(_18080+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29637
	nop
code_29638:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29637:
	sethi	%hi(_18080), %r9
	or	%r9, %lo(_18080), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(addLines_15174), %r8
	or	%r8, %lo(addLines_15174), %r8
	! done allocating 1 closures
	sethi	%hi(Help_STR_c_INT), %r8
	or	%r8, %lo(Help_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label StringMap_STR
	ld	[%r8], %r8
	! Proj_c at label map_TYC
	ld	[%r8+4], %r11
	! start making constructor call
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_29871:
	st	%r8, [%sp+100]
code_29654:
	! done making constructor call
	sethi	%hi(type_17978), %r8
	or	%r8, %lo(type_17978), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_29880:
	mov	%r8, %r10
code_29657:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_29658
	nop
code_29659:
	call	GCFromML ! delay slot empty
	nop
needgc_29658:
	! allocating 2-record
	or	%r0, 273, %r9
	ld	[%sp+100], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_29661
	nop
code_29662:
	or	%r0, 0, %r8
cmpui_29661:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	sethi	%hi(string_24334), %r8
	or	%r8, %lo(string_24334), %r8
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_29664
	nop
code_29665:
	or	%r0, 0, %r8
cmpui_29664:
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(funarg_3_18210+-4), %r9
	st	%r10, [%r9+%lo(funarg_3_18210+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29670
	nop
code_29671:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29670:
	sethi	%hi(funarg_3_18210), %r9
	or	%r9, %lo(funarg_3_18210), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(convert_r_15270), %r8
	or	%r8, %lo(convert_r_15270), %r8
	! done allocating 1 closures
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(convert_r_15270), %r8
	or	%r8, %lo(convert_r_15270), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+136], %r9
code_29872:
	mov	%r8, %r9
code_29684:
	! done making normal call
	sethi	%hi(string_24448), %r8
	or	%r8, %lo(string_24448), %r10
	sethi	%hi(timeFromString_14736), %r8
	or	%r8, %lo(timeFromString_14736), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_29881:
code_29687:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(time_19858+-4), %r9
	st	%r10, [%r9+%lo(time_19858+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29692
	nop
code_29693:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29692:
	sethi	%hi(time_19858), %r9
	or	%r9, %lo(time_19858), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(convert_r_15270), %r8
	or	%r8, %lo(convert_r_15270), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+132], %r9
code_29873:
	mov	%r8, %r13
code_29705:
	! done making normal call
	sethi	%hi(Bool_STR_r_INT), %r8
	or	%r8, %lo(Bool_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r11
	sethi	%hi(string_24477), %r8
	or	%r8, %lo(string_24477), %r10
	! making closure call 
	ld	[%r13], %r12
	ld	[%r13+4], %r8
	jmpl	%r12, %r15
	ld	[%r13+8], %r9
code_29882:
code_29709:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(bool_19885+-4), %r9
	st	%r10, [%r9+%lo(bool_19885+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29714
	nop
code_29715:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29714:
	sethi	%hi(bool_19885), %r9
	or	%r9, %lo(bool_19885), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(convert_r_15270), %r8
	or	%r8, %lo(convert_r_15270), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+128], %r9
code_29874:
	mov	%r8, %r13
code_29727:
	! done making normal call
	sethi	%hi(Crc_STR_r_INT), %r8
	or	%r8, %lo(Crc_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r11
	sethi	%hi(string_24506), %r8
	or	%r8, %lo(string_24506), %r10
	! making closure call 
	ld	[%r13], %r12
	ld	[%r13+4], %r8
	jmpl	%r12, %r15
	ld	[%r13+8], %r9
code_29883:
code_29731:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(crc_19912+-4), %r9
	st	%r10, [%r9+%lo(crc_19912+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29736
	nop
code_29737:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29736:
	sethi	%hi(crc_19912), %r9
	or	%r9, %lo(crc_19912), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(UnitEnvironment_STR_r_INT), %r8
	or	%r8, %lo(UnitEnvironment_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_insert_18448+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_insert_18448+-4)]
	sethi	%hi(strbindvar_r_insert_18448), %r8
	or	%r8, %lo(strbindvar_r_insert_18448), %r12
	ld	[%r2+812], %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(folder_15321), %r8
	or	%r8, %lo(folder_15321), %r8
	! done allocating 1 closures
	ld	[%sp+112], %r17
	ld	[%r17+72], %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+124], %r9
code_29875:
code_29761:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_18468+-4), %r9
	st	%r10, [%r9+%lo(_18468+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_29766
	nop
code_29767:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29766:
	sethi	%hi(_18468), %r9
	or	%r9, %lo(_18468), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	st	%r8, [%r9]
	sethi	%hi(UnitEnvironment_STR_r_INT), %r8
	or	%r8, %lo(UnitEnvironment_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	sethi	%hi(UnitEnvironment_STR_c_INT), %r9
	or	%r9, %lo(UnitEnvironment_STR_c_INT), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_29782
	nop
code_29783:
	or	%r0, 0, %r9
cmpui_29782:
	cmp	%r9, 0
	be	else_case_24562
	nop
code_29784:
	or	%r0, 111, %r10
	sethi	%hi(strbindvar_r_empty_18470+-4), %r9
	st	%r10, [%r9+%lo(strbindvar_r_empty_18470+-4)]
	sethi	%hi(strbindvar_r_empty_18470), %r9
	or	%r9, %lo(strbindvar_r_empty_18470), %r13
	ld	[%r2+812], %r12
	ld	[%r2+800], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+800]
	add	%r13, %r12, %r9
	ba	after_ite_24563
	st	%r8, [%r9]
else_case_24562:
	or	%r0, 9, %r10
	sethi	%hi(strbindvar_r_empty_18470+-4), %r9
	st	%r10, [%r9+%lo(strbindvar_r_empty_18470+-4)]
	or	%r0, 23, %r10
	sethi	%hi(strbindvar_r_empty_18470+4), %r9
	st	%r10, [%r9+%lo(strbindvar_r_empty_18470+4)]
	sethi	%hi(strbindvar_r_empty_18470), %r9
	st	%r8, [%r9+%lo(strbindvar_r_empty_18470)]
after_ite_24563:
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_15277), %r8
	or	%r8, %lo(anonfun_15277), %r8
	! done allocating 1 closures
	or	%r0, 256, %r11
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+120], %r9
code_29876:
	mov	%r8, %r13
code_29800:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_20748+-4), %r8
	st	%r9, [%r8+%lo(_20748+-4)]
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 36, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_29805
	nop
code_29806:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_29805:
	sethi	%hi(_20748), %r8
	or	%r8, %lo(_20748), %r12
	ld	[%r2+812], %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(read_15200), %r8
	or	%r8, %lo(read_15200), %r8
	! done allocating 1 closures
	sethi	%hi(Time_STR_r_INT), %r8
	or	%r8, %lo(Time_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+56], %r13
	or	%r0, 111, %r9
	sethi	%hi(compare_18563+-4), %r8
	st	%r9, [%r8+%lo(compare_18563+-4)]
	sethi	%hi(compare_18563), %r8
	or	%r8, %lo(compare_18563), %r12
	ld	[%r2+812], %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(eqTime_15396), %r8
	or	%r8, %lo(eqTime_15396), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(eqBool_15401), %r8
	or	%r8, %lo(eqBool_15401), %r8
	! done allocating 1 closures
	sethi	%hi(UnitEnvironment_STR_r_INT), %r8
	or	%r8, %lo(UnitEnvironment_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_equal_18582+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_equal_18582+-4)]
	sethi	%hi(strbindvar_r_equal_18582), %r8
	or	%r8, %lo(strbindvar_r_equal_18582), %r12
	ld	[%r2+812], %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(eqUe_15408), %r8
	or	%r8, %lo(eqUe_15408), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_15434), %r8
	or	%r8, %lo(anonfun_15434), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_15445), %r8
	or	%r8, %lo(anonfun_15445), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_15456), %r8
	or	%r8, %lo(anonfun_15456), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_15466), %r8
	or	%r8, %lo(anonfun_15466), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_15476), %r8
	or	%r8, %lo(anonfun_15476), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(equal_15411), %r8
	or	%r8, %lo(equal_15411), %r8
	! done allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	or	%r0, 256, %r8
code_29852:
	ld	[%sp+92], %r15
	retl
	add	%sp, 176, %sp
	.size Info_main,(.-Info_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_29853
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00015540
		! -------- label,sizes,reg
	.long code_29854
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long code_29855
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long code_29856
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long code_29857
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long code_29858
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000155
		! -------- label,sizes,reg
	.long code_29859
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_29860
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_29861
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_29862
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_29863
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29864
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29865
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29866
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29867
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29868
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29869
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29870
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29871
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29872
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_29873
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_29874
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_29875
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x14000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_29876
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_28842
	.word 0xb8005807
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_28856
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long afterMutateCheck_28871
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_28885
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00014000
		! -------- label,sizes,reg
	.long afterMutateCheck_28904
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00015540
		! -------- label,sizes,reg
	.long needgc_28918
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00015540
		! -------- label,sizes,reg
	.long afterMutateCheck_28928
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00015540
		! -------- label,sizes,reg
	.long needgc_28945
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00015540
		! -------- label,sizes,reg
	.long afterMutateCheck_28955
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00015540
		! -------- label,sizes,reg
	.long needgc_28987
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x00015540
		! -------- label,sizes,reg
	.long afterMutateCheck_29012
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x00015555
		! -------- label,sizes,reg
	.long needgc_29024
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x00015555
		! -------- label,sizes,reg
	.long code_29877
	.word 0xb8005809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54300000
	.word 0x00001555
		! worddata
	.word 0x0000001e
	.long Date_STR_c_INT
		! -------- label,sizes,reg
	.long code_29878
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00001555
		! -------- label,sizes,reg
	.long afterMutateCheck_29056
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00001555
		! -------- label,sizes,reg
	.long afterMutateCheck_29099
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long afterMutateCheck_29180
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long afterMutateCheck_29211
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long afterMutateCheck_29248
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000555
		! -------- label,sizes,reg
	.long afterMutateCheck_29267
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000155
		! -------- label,sizes,reg
	.long afterMutateCheck_29313
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_29362
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_29398
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_29466
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_29488
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_29507
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29879
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55510000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_29527
	.word 0xb8005809
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x55510000
	.word 0x00000015
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long afterMutateCheck_29538
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_29559
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_29578
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_29597
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_29616
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_29637
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29880
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55040000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_29658
	.word 0xb8005809
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x55040000
	.word 0x00000015
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long afterMutateCheck_29670
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_29881
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000005
		! -------- label,sizes,reg
	.long afterMutateCheck_29692
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_29882
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000001
		! -------- label,sizes,reg
	.long afterMutateCheck_29714
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_29883
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_29736
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_29766
	.word 0xb8005807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x14000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_29805
	.word 0xb8005807
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_29884
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_29885
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00015540
		! -------- label,sizes,reg
	.long code_29886
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00015540
		! -------- label,sizes,reg
	.long code_29887
	.word 0xb8005807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00015555
	.text
Info_unit_CODE_END_VAL:
	.section	".rodata"
		! endgcinfo with filler for alignment
	.globl Info_unit_GCTABLE_END_VAL
Info_unit_GCTABLE_END_VAL:
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl Info_unit_GLOBALS_BEGIN_VAL
Info_unit_GLOBALS_BEGIN_VAL:
		! Global
	.word 0x00000037
type_17266:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_23210:
	.word 0x00000008
		! Global
	.word 0x00000037
	.globl Info_STR_c_INT
Info_STR_c_INT:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000011
record_23237:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x00000037
type_21052:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_18162:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_18165:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_17978:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_20690:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_20005:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000042
string_23437:
		! string size = 8
	.ascii "info.sml"
.align 4
	.word 0x00000042
string_23446:
		! string size = 8
	.ascii "InfoFile"
.align 4
	.word 0x0000003a
string_23454:
		! string size = 7
	.ascii "version"
.align 4
	.word 0x0000000a
string_23456:
		! string size = 1
	.ascii "1"
.align 4
	.word 0x0000008a
string_23474:
		! string size = 17
	.ascii "ContextFileStatus"
.align 4
	.word 0x00000022
string_23479:
		! string size = 4
	.ascii "unit"
.align 4
	.word 0x0000005a
string_23491:
		! string size = 11
	.ascii "lastWritten"
.align 4
	.word 0x0000005a
string_23503:
		! string size = 11
	.ascii "lastChecked"
.align 4
	.word 0x0000005a
string_23515:
		! string size = 11
	.ascii "constrained"
.align 4
	.word 0x0000003a
string_23523:
		! string size = 7
	.ascii "Imports"
.align 4
	.word 0x0000003a
string_23531:
		! string size = 7
	.ascii "Exports"
.align 4
		! Global
	.word 0x00000037
baseTime_19070:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
toString_17326:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
MINUS_17327:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_17337:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
PLUS_17338:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_14744:
	.long Info_anonfun_code_22985
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
fromString_17342:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
timeFromString_14736:
	.long Info_timeFromString_code_22990
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
openOut_17449:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
output_17453:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
output1_17456:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_17350:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_14754:
	.long Info_anonfun_code_22995
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
explode_17444:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_20644:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000009a
string_23757:
		! string size = 19
	.ascii "invalid unit name \""
.align 4
		! Global
	.word 0x00000037
toString_17482:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000000a
string_23767:
		! string size = 1
	.ascii "\""
.align 4
		! Global
	.word 0x00000037
_19238:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_17507:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
toString_17517:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_appi_17577:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000011
record_23838:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x00000037
_19300:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
write_14802:
	.long Info_write_code_23000
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000027
exn_stamp_17676:
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_14949:
	.long Info_anonfun_code_23021
	.word 0x00000100
	.word 0x00000100
	.word 0x00000062
string_23883:
		! string size = 12
	.ascii "XXX Reading "
.align 4
		! Global
	.word 0x00000037
toString_17681:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
fileSize_17684:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000000a
string_23904:
		! string size = 1
	.ascii " "
.align 4
	.word 0x0000000a
string_23906:
		! string size = 1
	.ascii "\n"
.align 4
	.word 0x0000007a
string_23922:
		! string size = 15
	.ascii "XXX Large File\n"
.align 4
		! Global
	.word 0x00000037
_17749:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
splitPRIME_14970:
	.long Info_splitPRIME_code_23026
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
openIn_17816:
	.word 0x00000102
	.word 0x00000102
	.word 0x000000fa
string_23988:
		! string size = 31
	.ascii "Sparc/TM/any/sparc.sig.sml.info"
.align 4
	.word 0x0000008a
string_24006:
		! string size = 17
	.ascii "About to fail...\n"
.align 4
		! Global
	.word 0x00000037
_19384:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
toString_17839:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000001a
string_24043:
		! string size = 3
	.ascii " <<"
.align 4
	.word 0x0000001a
string_24047:
		! string size = 3
	.ascii ">>\n"
.align 4
	.word 0x00000002
string_24048:
		! string size = 0
	! .ascii "" (zero length string)
		! Global
	.word 0x00000037
sub_17861:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
extract_17866:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000082
string_24086:
		! string size = 16
	.ascii "malformed record"
.align 4
		! Global
	.word 0x00000037
implode_17807:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_17901:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000011
record_24124:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x00000037
_19458:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_17981:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
emptySection_20700:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_18018:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_18023:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_18036:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000008a
string_24266:
		! string size = 17
	.ascii "duplicate record "
.align 4
		! Global
	.word 0x00000037
_18066:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_18080:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000092
string_24316:
		! string size = 18
	.ascii "duplicate section "
.align 4
		! static record tag
	.word 0x00000619
addLines_15174:
	.long Info_addLines_code_23031
	.word 0x00000100
	.word 0x00000100
	.word 0x00000062
string_24334:
		! string size = 12
	.ascii "DummySection"
.align 4
		! Global
	.word 0x00000037
funarg_3_18210:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000082
string_24397:
		! string size = 16
	.ascii "missing section "
.align 4
	.word 0x00000072
string_24412:
		! string size = 14
	.ascii "missing entry "
.align 4
	.word 0x00000052
string_24423:
		! string size = 10
	.ascii "malformed "
.align 4
		! static record tag
	.word 0x00000619
convert_r_15270:
	.long Info_convert_r_code_23036
	.word 0x00000100
	.word 0x00000100
	.word 0x00000052
string_24448:
		! string size = 10
	.ascii "time value"
.align 4
		! Global
	.word 0x00000037
time_19858:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000003a
string_24477:
		! string size = 7
	.ascii "boolean"
.align 4
		! Global
	.word 0x00000037
bool_19885:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000001a
string_24506:
		! string size = 3
	.ascii "CRC"
.align 4
		! Global
	.word 0x00000037
crc_19912:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_insert_18448:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
folder_15321:
	.long Info_folder_code_23065
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
_18468:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_empty_18470:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_15277:
	.long Info_anonfun_code_23070
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
_20748:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000000a
string_24581:
		! string size = 1
	.ascii ":"
.align 4
	.word 0x00000012
string_24584:
		! string size = 2
	.ascii ": "
.align 4
		! static record tag
	.word 0x00000619
read_15200:
	.long Info_read_code_23075
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
compare_18563:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
eqTime_15396:
	.long Info_eqTime_code_23117
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
eqBool_15401:
	.long Info_eqBool_code_23122
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
strbindvar_r_equal_18582:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
eqUe_15408:
	.long Info_eqUe_code_23127
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_15434:
	.long Info_anonfun_code_23132
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_15445:
	.long Info_anonfun_code_23137
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_15456:
	.long Info_anonfun_code_23142
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_15466:
	.long Info_anonfun_code_23147
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_15476:
	.long Info_anonfun_code_23152
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
equal_15411:
	.long Info_equal_code_23157
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000719
record_24656:
	.long read_15200
	.long write_14802
	.long equal_15411
		! Global
	.word 0x0000006f
	.globl Info_STR_r_INT
Info_STR_r_INT:
	.long record_24656
	.long record_24656
	.word 0x0000002a
string_27013:
		! string size = 5
	.ascii "Error"
.align 4
		! static record tag
	.word 0x00000011
record_27349:
	.word 0x00000005
	.word 0x00000000
		! static record tag
	.word 0x00000011
record_27415:
	.word 0x00000005
	.word 0x00000000
		! Module closure
	.word 0x00000619
	.globl Info_unit_closure
Info_unit_closure:
	.long Info_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl Info_unit
Info_unit:
	.long Info_unit_closure
	.long Info_unit_closure
	.globl Info_unit_GLOBALS_END_VAL
Info_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.long 0 !filler

	.data
	.align 8
	.globl Info_unit_TRACE_GLOBALS_BEGIN_VAL
Info_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long strbindvar_r_equal_18582
	.long compare_18563
	.long _20748
	.long strbindvar_r_empty_18470
	.long _18468
	.long strbindvar_r_insert_18448
	.long crc_19912
	.long bool_19885
	.long time_19858
	.long funarg_3_18210
	.long _18080
	.long _18066
	.long _18036
	.long _18023
	.long _18018
	.long emptySection_20700
	.long _17981
	.long _19458
	.long _17901
	.long implode_17807
	.long extract_17866
	.long sub_17861
	.long toString_17839
	.long _19384
	.long openIn_17816
	.long _17749
	.long fileSize_17684
	.long toString_17681
	.long _19300
	.long strbindvar_r_appi_17577
	.long toString_17517
	.long _17507
	.long _19238
	.long toString_17482
	.long _20644
	.long explode_17444
	.long _17350
	.long output1_17456
	.long output_17453
	.long openOut_17449
	.long fromString_17342
	.long PLUS_17338
	.long _17337
	.long MINUS_17327
	.long toString_17326
	.long baseTime_19070
	.long type_20005
	.long type_20690
	.long type_17978
	.long type_18165
	.long type_18162
	.long type_21052
	.long Info_STR_c_INT
	.long type_17266
	.globl Info_unit_TRACE_GLOBALS_END_VAL
Info_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
