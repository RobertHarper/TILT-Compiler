	.section	".rodata"
	.text
	.align 8
	.proc 07
localDivFromML:
	call	DivFromML
	nop
	call	abort
	nop
	.size localDivFromML,(.-localDivFromML)
	.align 8
	.proc 07
localOverflowFromML:
	call	OverflowFromML
	nop
	call	abort
	nop
	.size localOverflowFromML,(.-localOverflowFromML)
	.section	".rodata"
		! gcinfo
	.globl SplaySetFn_unit_GCTABLE_BEGIN_VAL
SplaySetFn_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl SplaySetFn_unit_CODE_END_VAL
	.globl SplaySetFn_unit_CODE_BEGIN_VAL
SplaySetFn_unit_CODE_BEGIN_VAL:
	.text
	.align 8
	.global SplaySetFn_functor_var_c_code_410128
 ! arguments : [$410130,$8] [$393937,$9] 
 ! results    : [$417726,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
SplaySetFn_functor_var_c_code_410128:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417744
	nop
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_417744:
	st	%r15, [%sp+92]
	mov	%r9, %r10
code_417736:
funtop_417705:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417737
	nop
code_417738:
	call	GCFromML ! delay slot empty
	nop
needgc_417737:
	! allocating 1-record
	! done allocating 1 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	sethi	%hi(record_417711), %r8
	or	%r8, %lo(record_417711), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	st	%r10, [%r4+12]
	sethi	%hi(record_417711), %r8
	or	%r8, %lo(record_417711), %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
code_417743:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size SplaySetFn_functor_var_c_code_410128,(.-SplaySetFn_functor_var_c_code_410128)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_417737
	.word 0x00180007
	.word 0x00170000
	.word 0xbffc3c00
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_left_inner_code_410138
 ! arguments : [$410140,$8] [$410141,$9] [$406305,$10] [$406306,$11] 
 ! results    : [$417658,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
SplaySetFn_left_inner_code_410138:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417755
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_417755:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r12
code_417745:
funtop_417630:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417746
	nop
code_417747:
	call	GCFromML ! delay slot empty
	nop
needgc_417746:
	! Proj_c at label reify_408135_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+104]
	! Proj_c at label reify_408100_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label reify_408097_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	! Proj_c at label type_401691_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
sumarm_417654:
	cmp	%r10, 0
	bne	sumarm_417655
	nop
code_417749:
	ba	after_sum_417651
	mov	%r11, %r8
sumarm_417655:
	! allocating 2-record
	st	%r12, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	ld	[%r10], %r8
	! making direct call 
	mov	%r8, %r10
	ba	funtop_417630
	mov	%r9, %r11
code_417751:
	! done making self tail call
	ba	after_sum_417651
	or	%r0, 0, %r8
sumarm_417659:
after_sum_417651:
code_417754:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_left_inner_code_410138,(.-SplaySetFn_left_inner_code_410138)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_417746
	.word 0x001c000b
	.word 0x00170000
	.word 0xbffc2000
	.word 0xbffc2c00
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000002
	.word 0x00000060
	.word 0x00000003
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_left_r_code_410133
 ! arguments : [$410135,$8] [$401674,$9] [$410136,$10] [$401675,$11] 
 ! results    : [$417625,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_left_r_code_410133:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417791
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_417791:
	st	%r15, [%sp+92]
	st	%r9, [%sp+112]
code_417756:
funtop_417549:
	! start making constructor call
	sethi	%hi(type_400387), %r8
	or	%r8, %lo(type_400387), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+112], %r9
code_417790:
	st	%r8, [%sp+108]
code_417759:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_401683), %r8
	or	%r8, %lo(type_401683), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! start making constructor call
	sethi	%hi(type_400387), %r8
	or	%r8, %lo(type_400387), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+112], %r9
code_417785:
	mov	%r8, %r9
code_417764:
	! done making constructor call
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_417786:
	st	%r8, [%sp+104]
code_417765:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_407688), %r8
	or	%r8, %lo(type_407688), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+112], %r9
code_417787:
	st	%r8, [%sp+100]
code_417768:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_401683), %r8
	or	%r8, %lo(type_401683), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+100], %r9
code_417788:
	st	%r8, [%sp+96]
code_417771:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_400387), %r8
	or	%r8, %lo(type_400387), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+112], %r9
code_417789:
	mov	%r8, %r9
code_417774:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417775
	nop
code_417776:
	call	GCFromML ! delay slot empty
	nop
needgc_417775:
	! done making constructor call
	or	%r0, 17, %r10
	ld	[%sp+100], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_417778
	nop
code_417779:
	or	%r0, 0, %r8
cmpui_417778:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_417780
	nop
code_417781:
	or	%r0, 0, %r8
cmpui_417780:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 537, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_left_inner_code_410138), %r8
	or	%r8, %lo(SplaySetFn_left_inner_code_410138), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_417784:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_left_r_code_410133,(.-SplaySetFn_left_r_code_410133)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_417785
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01410000
		! -------- label,sizes,reg
	.long code_417786
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
		! -------- label,sizes,reg
	.long code_417787
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01500000
		! -------- label,sizes,reg
	.long code_417788
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
		! -------- label,sizes,reg
	.long code_417789
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long needgc_417775
	.word 0x00200007
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_417790
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.text
	.align 8
	.global SplaySetFn_vars_eq_0_code_410158
 ! arguments : [$410160,$8] [$410161,$9] [$405607,$10] [$405608,$11] 
 ! results    : [$417509,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
SplaySetFn_vars_eq_0_code_410158:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417808
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_417808:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_417792:
funtop_417480:
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label type_400225_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
sumarm_417493:
	cmp	%r10, 0
	bne	sumarm_417494
	nop
sumarm_417501:
	cmp	%r11, 0
	bne	sumarm_417502
	nop
code_417794:
	ba	after_sum_417498
	or	%r0, 1, %r8
sumarm_417502:
nomatch_sum_417499:
	or	%r0, 0, %r8
after_sum_417498:
	ba	after_sum_417490 ! delay slot empty
	nop
sumarm_417494:
sumarm_417517:
	or	%r0, 255, %r8
	cmp	%r11, %r8
	bleu	nomatch_sum_417515
	nop
code_417797:
	ld	[%r10], %r9
	ld	[%r11], %r8
	cmp	%r9, %r8
	or	%r0, 1, %r8
	be	cmpui_417798
	nop
code_417799:
	or	%r0, 0, %r8
cmpui_417798:
	cmp	%r8, 0
	bne,pn	%icc,one_case_417533
	nop
zero_case_417532:
	ba	after_zeroone_417534
	or	%r0, 0, %r8
one_case_417533:
	ld	[%r10+4], %r9
	ld	[%r11+4], %r8
	cmp	%r9, %r8
	or	%r0, 1, %r8
	be	cmpui_417802
	nop
code_417803:
	or	%r0, 0, %r8
cmpui_417802:
after_zeroone_417534:
	ba	after_sum_417514 ! delay slot empty
	nop
sumarm_417518:
nomatch_sum_417515:
	or	%r0, 0, %r8
after_sum_417514:
	ba	after_sum_417490 ! delay slot empty
	nop
sumarm_417510:
after_sum_417490:
code_417807:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_vars_eq_0_code_410158,(.-SplaySetFn_vars_eq_0_code_410158)

	.section	".rodata"
	.text
	.align 8
	.global SplaySetFn_singleton_code_410167
 ! arguments : [$410169,$8] [$410170,$9] [$394047,$10] 
 ! results    : [$417475,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_singleton_code_410167:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417821
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_417821:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r11
code_417809:
funtop_417434:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417810
	nop
code_417811:
	call	GCFromML ! delay slot empty
	nop
needgc_417810:
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	ld	[%r11], %r9
	ld	[%r11+4], %r8
	ld	[%r11+8], %r16
	st	%r16, [%sp+104]
	! allocating 3-record
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+100], %r8
code_417820:
code_417814:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417815
	nop
code_417816:
	call	GCFromML ! delay slot empty
	nop
needgc_417815:
	! allocating 2-record
	ld	[%sp+104], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	or	%r0, 1, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
code_417819:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_singleton_code_410167,(.-SplaySetFn_singleton_code_410167)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_417810
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000800
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000003
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_417815
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_417820
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410191
 ! arguments : [$410193,$8] [$410194,$9] [$409462,$10] 
 ! results    : [$417433,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410191:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417827
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_417827:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_417822:
funtop_417418:
	ld	[%r8], %r9
	ld	[%r8+4], %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_417823:
	! done making tail call
code_417825:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410191,(.-SplaySetFn_anonfun_code_410191)

	.section	".rodata"
	.text
	.align 8
	.global SplaySetFn_insert_code_410184
 ! arguments : [$410186,$8] [$410187,$9] [$405646,$10] [$405647,$11] 
 ! results    : [$417282,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_insert_code_410184:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417858
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_417858:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+124]
code_417828:
funtop_417176:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417829
	nop
code_417830:
	call	GCFromML ! delay slot empty
	nop
needgc_417829:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+104]
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+112]
	ld	[%r9], %r10
	ld	[%r9+4], %r16
	st	%r16, [%sp+128]
	ld	[%r9+8], %r16
	st	%r16, [%sp+132]
	ld	[%r9+12], %r16
	st	%r16, [%sp+136]
	ld	[%r9+16], %r16
	st	%r16, [%sp+140]
	ld	[%r11], %r16
	st	%r16, [%sp+116]
	ld	[%r11+4], %r16
	st	%r16, [%sp+120]
	! allocating 1 closures
	or	%r0, 273, %r9
	ld	[%sp+112], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_417832
	nop
code_417833:
	or	%r0, 0, %r8
cmpui_417832:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410191), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410191), %r8
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_400218), %r8
	or	%r8, %lo(type_400218), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+112], %r9
code_417856:
	mov	%r8, %r10
code_417839:
	! done making normal call
	! making closure call
	ld	[%sp+136], %r17
	ld	[%r17], %r12
	ld	[%sp+136], %r17
	ld	[%r17+4], %r8
	ld	[%sp+136], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_417857:
code_417840:
	! done making normal call
	add	%r4, 144, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417841
	nop
code_417842:
	call	GCFromML ! delay slot empty
	nop
needgc_417841:
	ld	[%r8], %r11
	ld	[%r8+4], %r9
sumarm_417259:
	cmp	%r9, 0
	bne	sumarm_417260
	nop
code_417844:
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+8]
	ld	[%sp+124], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	ld	[%sp+140], %r17
	st	%r17, [%r4]
	or	%r0, 1, %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_417256 ! delay slot empty
	nop
sumarm_417260:
	ld	[%r9+8], %r8
	ld	[%r9], %r10
	ld	[%r9+4], %r9
sumarm_417321:
	cmp	%r11, 0
	bne	sumarm_417322
	nop
code_417846:
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+124], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	ld	[%sp+140], %r17
	st	%r17, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_417318 ! delay slot empty
	nop
sumarm_417322:
	cmp	%r11, 1
	bne	sumarm_417344
	nop
code_417848:
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	st	%r10, [%r4+4]
	st	%r8, [%r4+8]
	ld	[%sp+124], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	ld	[%sp+116], %r17
	addcc	%r17, 1, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_417849:
	! allocating 2-record
	ld	[%sp+140], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_417318 ! delay slot empty
	nop
sumarm_417344:
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	st	%r10, [%r4+4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+8]
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+124], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	ld	[%sp+116], %r17
	addcc	%r17, 1, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_417851:
	! allocating 2-record
	ld	[%sp+140], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_417318 ! delay slot empty
	nop
sumarm_417381:
after_sum_417318:
	ba	after_sum_417256 ! delay slot empty
	nop
sumarm_417283:
after_sum_417256:
code_417855:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_insert_code_410184,(.-SplaySetFn_insert_code_410184)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_417856
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xf1550000
	.word 0x00000013
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000004
	.word 0x00000060
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_417829
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000a00
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0010000
	.word 0x00000000
		! worddata
	.word 0x00000004
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_417857
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc1450000
	.word 0x00000003
		! worddata
	.word 0x00000004
	.word 0x00000060
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_417841
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc1450000
	.word 0x00000003
		! worddata
	.word 0x00000004
	.word 0x00000060
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_add_code_410217
 ! arguments : [$410219,$8] [$410220,$9] [$405722,$10] [$405723,$11] 
 ! results    : [$417128,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_add_code_410217:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417881
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_417881:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r11, [%sp+132]
code_417859:
funtop_417088:
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+128]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r8
	ld	[%r9], %r16
	st	%r16, [%sp+120]
	ld	[%r9+4], %r16
	st	%r16, [%sp+124]
	ld	[%r9+8], %r16
	st	%r16, [%sp+116]
	ld	[%r9+12], %r9
sumarm_417118:
	cmp	%r10, 0
	bne	sumarm_417119
	nop
code_417860:
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+132], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 144, %sp
code_417861:
	! done making tail call
	ba	after_sum_417115 ! delay slot empty
	nop
sumarm_417119:
	ld	[%r10], %r9
	ld	[%r10+4], %r16
	st	%r16, [%sp+112]
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_417880:
code_417864:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417865
	nop
code_417866:
	call	GCFromML ! delay slot empty
	nop
needgc_417865:
	! allocating 2-record
	ld	[%sp+116], %r17
	st	%r17, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	! making closure call
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_417879:
code_417868:
	! done making normal call
	ld	[%r8], %r16
	st	%r16, [%sp+112]
	ld	[%r8+4], %r10
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+128], %r8
code_417878:
code_417870:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417871
	nop
code_417872:
	call	GCFromML ! delay slot empty
	nop
needgc_417871:
	! allocating 2-record
	ld	[%sp+124], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_417115 ! delay slot empty
	nop
sumarm_417129:
after_sum_417115:
code_417876:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_add_code_410217,(.-SplaySetFn_add_code_410217)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_417865
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x10150000
	.word 0x0000000d
		! worddata
	.word 0x00000005
	.word 0x00000060
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_417878
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_417871
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_417879
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_417880
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10150000
	.word 0x0000000d
		! worddata
	.word 0x00000005
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_addList_code_410240
 ! arguments : [$410242,$8] [$410243,$9] [$405736,$10] [$405737,$11] 
 ! results    : [$417003,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_addList_code_410240:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417912
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_417912:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r11, [%sp+140]
code_417882:
funtop_416960:
	! Proj_c at label reify_407806_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_405804_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+132]
	! Proj_c at label type_400488_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+136]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+20], %r16
	st	%r16, [%sp+100]
	ld	[%r9], %r16
	st	%r16, [%sp+120]
	ld	[%r9+4], %r16
	st	%r16, [%sp+124]
	ld	[%r9+8], %r16
	st	%r16, [%sp+128]
	ld	[%r9+12], %r8
	ld	[%r9+16], %r16
	st	%r16, [%sp+116]
sumarm_416999:
	ld	[%sp+140], %r17
	cmp	%r17, 0
	bne	sumarm_417000
	nop
code_417883:
	ba	after_sum_416996
	mov	%r10, %r8
sumarm_417000:
nomatch_sum_416997:
sumarm_417009:
	cmp	%r10, 0
	bne	sumarm_417010
	nop
code_417885:
	ba	after_sum_417006
	st	%r8, [%sp+112]
sumarm_417010:
	ld	[%r10], %r9
	ld	[%r10+4], %r16
	st	%r16, [%sp+112]
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_417911:
code_417888:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417889
	nop
code_417890:
	call	GCFromML ! delay slot empty
	nop
needgc_417889:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_417006
	st	%r8, [%sp+112]
sumarm_417014:
after_sum_417006:
	! making closure call
	ld	[%sp+116], %r17
	ld	[%r17], %r11
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_417906:
	mov	%r8, %r9
code_417893:
	! done making normal call
	ld	[%sp+112], %r17
	ld	[%r17], %r10
	ld	[%sp+112], %r17
	ld	[%r17+4], %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_417907:
	mov	%r8, %r12
code_417894:
	! done making normal call
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+132], %r9
	jmpl	%r13, %r15
	ld	[%sp+136], %r10
code_417908:
	mov	%r8, %r9
code_417897:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+140], %r10
code_417909:
code_417898:
	! done making normal call
	ld	[%r8], %r16
	st	%r16, [%sp+112]
	ld	[%r8+4], %r10
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+108], %r8
code_417910:
code_417900:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417901
	nop
code_417902:
	call	GCFromML ! delay slot empty
	nop
needgc_417901:
	! allocating 2-record
	ld	[%sp+124], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_sum_416996:
code_417905:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_addList_code_410240,(.-SplaySetFn_addList_code_410240)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_417906
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01450000
	.word 0x000000d4
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_417889
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x14550000
	.word 0x000000d4
		! worddata
	.word 0x00000001
	.word 0x00000060
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_417907
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
	.word 0x000000d4
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_417908
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
	.word 0x000000c0
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_417909
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00440000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_417910
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_417901
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_417911
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x14550000
	.word 0x000000d4
		! worddata
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410274
 ! arguments : [$410276,$8] [$410277,$9] [$409469,$10] 
 ! results    : [$416959,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410274:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417918
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_417918:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_417913:
funtop_416944:
	ld	[%r8], %r9
	ld	[%r8+4], %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_417914:
	! done making tail call
code_417916:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410274,(.-SplaySetFn_anonfun_code_410274)

	.section	".rodata"
	.text
	.align 8
	.global SplaySetFn_delete_code_410267
 ! arguments : [$410269,$8] [$410270,$9] [$405809,$10] [$405810,$11] 
 ! results    : [$416778,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_delete_code_410267:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417972
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_417972:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
	mov	%r11, %r12
code_417919:
funtop_416738:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417920
	nop
code_417921:
	call	GCFromML ! delay slot empty
	nop
needgc_417920:
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r9
	ld	[%r8], %r11
	ld	[%r8+4], %r16
	st	%r16, [%sp+128]
	ld	[%r8+8], %r16
	st	%r16, [%sp+140]
	ld	[%r8+12], %r16
	st	%r16, [%sp+132]
	ld	[%r8+16], %r16
	st	%r16, [%sp+136]
sumarm_416770:
	cmp	%r10, 0
	bne	sumarm_416771
	nop
code_417923:
	sethi	%hi(mk_400942), %r8
	or	%r8, %lo(mk_400942), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_416767
	or	%r0, 0, %r8
sumarm_416771:
	ld	[%r10], %r16
	st	%r16, [%sp+120]
	ld	[%r10+4], %r16
	st	%r16, [%sp+124]
	! allocating 1 closures
	or	%r0, 273, %r10
	cmp	%r9, 3
	or	%r0, 1, %r8
	bgu	cmpui_417928
	nop
code_417929:
	or	%r0, 0, %r8
cmpui_417928:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 2-record
	st	%r10, [%r4]
	st	%r11, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410274), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410274), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_400218), %r8
	or	%r8, %lo(type_400218), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_417965:
	st	%r8, [%sp+116]
code_417935:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+104], %r8
	jmpl	%r11, %r15
	ld	[%sp+120], %r9
code_417966:
	mov	%r8, %r11
code_417937:
	! done making normal call
	! making closure call
	ld	[%sp+132], %r17
	ld	[%r17], %r12
	ld	[%sp+132], %r17
	ld	[%r17+4], %r8
	ld	[%sp+132], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_417967:
	mov	%r8, %r9
code_417938:
	! done making normal call
	ld	[%r9], %r8
	ld	[%r9+4], %r11
sumarm_416843:
	cmp	%r8, 0
	bne	sumarm_416844
	nop
sumarm_416856:
	or	%r0, 255, %r8
	cmp	%r11, %r8
	bleu	nomatch_sum_416854
	nop
code_417940:
	ld	[%r11], %r10
	ld	[%r11+4], %r11
intarm_416887:
	ld	[%sp+124], %r17
	cmp	%r17, 1
	bne	intarm_416888
	nop
code_417941:
	ba	after_intcase_416886
	ld	[%sp+128], %r8
intarm_416888:
	! making closure call
	ld	[%sp+136], %r17
	ld	[%r17], %r12
	ld	[%sp+136], %r17
	ld	[%r17+4], %r8
	ld	[%sp+136], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_417969:
	mov	%r8, %r10
code_417943:
	! done making normal call
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+112], %r8
code_417968:
	mov	%r8, %r9
code_417945:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417946
	nop
code_417947:
	call	GCFromML ! delay slot empty
	nop
needgc_417946:
	ld	[%sp+124], %r17
	subcc	%r17, 1, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_417949:
	! allocating 2-record
	ld	[%sp+140], %r17
	st	%r17, [%r4]
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_intcase_416886:
	ba	after_sum_416853 ! delay slot empty
	nop
sumarm_416857:
nomatch_sum_416854:
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	ld	[%sp+104], %r8
	jmpl	%r12, %r15
	ld	[%sp+120], %r9
code_417971:
code_417952:
	! done making normal call
	sethi	%hi(mk_400942), %r8
	or	%r8, %lo(mk_400942), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_416853:
	ba	after_sum_416840 ! delay slot empty
	nop
sumarm_416844:
nomatch_sum_416841:
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	ld	[%sp+104], %r8
	jmpl	%r12, %r15
	ld	[%sp+120], %r9
code_417970:
code_417958:
	! done making normal call
	sethi	%hi(mk_400942), %r8
	or	%r8, %lo(mk_400942), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_416840:
	ba	after_sum_416767 ! delay slot empty
	nop
sumarm_416779:
after_sum_416767:
code_417964:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_delete_code_410267,(.-SplaySetFn_delete_code_410267)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_417965
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x11540000
	.word 0x00000017
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_417920
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000100
	.word 0x00001400
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000000
		! worddata
	.word 0x00000004
	.word 0x00000060
	.word 0x00000005
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_417966
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15540000
	.word 0x00000017
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_417967
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x11540000
	.word 0x00000013
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_417968
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_417946
	.word 0x00240008
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_417969
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_417970
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_417971
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410309
 ! arguments : [$410311,$8] [$410312,$9] [$409486,$10] 
 ! results    : [$416737,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410309:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_417978
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_417978:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_417973:
funtop_416722:
	ld	[%r8], %r9
	ld	[%r8+4], %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_417974:
	! done making tail call
code_417976:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410309,(.-SplaySetFn_anonfun_code_410309)

	.section	".rodata"
	.text
	.align 8
	.global SplaySetFn_member_code_410302
 ! arguments : [$410304,$8] [$410305,$9] [$405935,$10] [$405936,$11] 
 ! results    : [$416633,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_member_code_410302:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418010
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418010:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r12
	mov	%r11, %r13
code_417979:
funtop_416605:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_417980
	nop
code_417981:
	call	GCFromML ! delay slot empty
	nop
needgc_417980:
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r9
	ld	[%r12], %r11
	ld	[%r12+4], %r16
	st	%r16, [%sp+116]
sumarm_416628:
	cmp	%r10, 0
	bne	sumarm_416629
	nop
code_417983:
	ba	after_sum_416625
	or	%r0, 0, %r8
sumarm_416629:
	ld	[%r10], %r16
	st	%r16, [%sp+112]
	! allocating 1 closures
	or	%r0, 273, %r10
	cmp	%r9, 3
	or	%r0, 1, %r8
	bgu	cmpui_417985
	nop
code_417986:
	or	%r0, 0, %r8
cmpui_417985:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 2-record
	st	%r10, [%r4]
	st	%r11, [%r4+4]
	st	%r13, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410309), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410309), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_400218), %r8
	or	%r8, %lo(type_400218), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_418005:
	st	%r8, [%sp+108]
code_417992:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+100], %r8
	jmpl	%r11, %r15
	ld	[%sp+112], %r9
code_418006:
	mov	%r8, %r11
code_417994:
	! done making normal call
	! making closure call
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r10
code_418007:
	mov	%r8, %r9
code_417995:
	! done making normal call
	ld	[%r9], %r8
	ld	[%r9+4], %r11
sumarm_416696:
	cmp	%r8, 0
	bne	sumarm_416697
	nop
code_417996:
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	ld	[%sp+100], %r8
	jmpl	%r12, %r15
	ld	[%sp+112], %r9
code_418008:
code_417998:
	! done making normal call
	ba	after_sum_416693
	or	%r0, 1, %r8
sumarm_416697:
nomatch_sum_416694:
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	ld	[%sp+100], %r8
	jmpl	%r12, %r15
	ld	[%sp+112], %r9
code_418009:
code_418001:
	! done making normal call
	or	%r0, 0, %r8
after_sum_416693:
	ba	after_sum_416625 ! delay slot empty
	nop
sumarm_416634:
after_sum_416625:
code_418004:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_member_code_410302,(.-SplaySetFn_member_code_410302)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418005
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05140000
		! -------- label,sizes,reg
	.long needgc_417980
	.word 0x0020000b
	.word 0x00170000
	.word 0x00001000
	.word 0x00002400
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000003
	.word 0x00000060
	.word 0x00000004
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418006
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! -------- label,sizes,reg
	.long code_418007
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01140000
		! -------- label,sizes,reg
	.long code_418008
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418009
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_isEmpty_code_410329
 ! arguments : [$410331,$8] [$410332,$9] [$394417,$10] 
 ! results    : [$416602,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
SplaySetFn_isEmpty_code_410329:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418016
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418016:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418011:
funtop_416588:
sumarm_416597:
	cmp	%r10, 0
	bne	sumarm_416598
	nop
code_418012:
	ba	after_sum_416594
	or	%r0, 1, %r8
sumarm_416598:
nomatch_sum_416595:
	or	%r0, 0, %r8
after_sum_416594:
code_418015:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_isEmpty_code_410329,(.-SplaySetFn_isEmpty_code_410329)

	.section	".rodata"
	.text
	.align 8
	.global SplaySetFn_mbr_code_410343
 ! arguments : [$410345,$8] [$410346,$9] [$394426,$10] 
 ! results    : [$416522,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_mbr_code_410343:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418031
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418031:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+116]
	mov	%r10, %r12
code_418017:
funtop_416492:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	ld	[%sp+116], %r17
	ld	[%r17], %r9
	ld	[%sp+116], %r17
	ld	[%r17+4], %r10
sumarm_416517:
	cmp	%r12, 0
	bne	sumarm_416518
	nop
code_418018:
	ba	after_sum_416514
	or	%r0, 0, %r8
sumarm_416518:
	ld	[%r12+8], %r11
	ld	[%r12], %r16
	st	%r16, [%sp+108]
	ld	[%r12+4], %r16
	st	%r16, [%sp+112]
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_418030:
code_418020:
	! done making normal call
sumarm_416568:
	cmp	%r8, 1
	bne	sumarm_416569
	nop
code_418021:
	! making direct call 
	ba	funtop_416492
	ld	[%sp+112], %r12
code_418022:
	! done making self tail call
	ba	after_sum_416565
	or	%r0, 0, %r8
sumarm_416569:
	cmp	%r8, 2
	bne	sumarm_416578
	nop
code_418024:
	! making direct call 
	ba	funtop_416492
	ld	[%sp+108], %r12
code_418025:
	! done making self tail call
	ba	after_sum_416565
	or	%r0, 0, %r8
sumarm_416578:
nomatch_sum_416566:
	or	%r0, 1, %r8
after_sum_416565:
	ba	after_sum_416514 ! delay slot empty
	nop
sumarm_416523:
after_sum_416514:
code_418029:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_mbr_code_410343,(.-SplaySetFn_mbr_code_410343)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418030
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x07d10000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000068
	.text
	.align 8
	.global SplaySetFn_member_code_410336
 ! arguments : [$410338,$8] [$410339,$9] [$405993,$10] [$405994,$11] 
 ! results    : [$416491,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_member_code_410336:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418043
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418043:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r13
	mov	%r10, %r18
	mov	%r11, %r10
code_418032:
funtop_416452:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418033
	nop
code_418034:
	call	GCFromML ! delay slot empty
	nop
needgc_418033:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r9
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r12
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 273, %r9
	cmp	%r12, 3
	or	%r0, 1, %r8
	bgu	cmpui_418036
	nop
code_418037:
	or	%r0, 0, %r8
cmpui_418036:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r13, [%r4+4]
	st	%r18, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_mbr_code_410343), %r8
	or	%r8, %lo(SplaySetFn_mbr_code_410343), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_418039:
	! done making tail call
code_418041:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_member_code_410336,(.-SplaySetFn_member_code_410336)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418033
	.word 0x001c000b
	.word 0x00170000
	.word 0x00002000
	.word 0x00040400
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.word 0x00000003
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_isIn_code_410368
 ! arguments : [$410370,$8] [$410371,$9] [$394460,$10] 
 ! results    : [$416293,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_isIn_code_410368:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418077
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418077:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
	mov	%r10, %r8
code_418044:
funtop_416263:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+104]
	ld	[%sp+120], %r17
	ld	[%r17], %r9
	ld	[%sp+120], %r17
	ld	[%r17+4], %r11
sumarm_416288:
	cmp	%r8, 0
	bne	sumarm_416289
	nop
code_418045:
	ba	after_sum_416285
	or	%r0, 1, %r8
sumarm_416289:
	ld	[%r8+8], %r10
	ld	[%r8], %r16
	st	%r16, [%sp+116]
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
sumarm_416337:
	ld	[%sp+112], %r17
	cmp	%r17, 0
	bne	sumarm_416338
	nop
sumarm_416350:
	ld	[%sp+116], %r17
	cmp	%r17, 0
	bne	sumarm_416351
	nop
code_418048:
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418049:
	! done making tail call
	ba	after_sum_416347 ! delay slot empty
	nop
sumarm_416351:
nomatch_sum_416348:
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_418074:
code_418051:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_416372
	nop
zero_case_416371:
	ba	after_zeroone_416373
	or	%r0, 0, %r8
one_case_416372:
	! making direct call 
	ba	funtop_416263
	ld	[%sp+116], %r8
code_418054:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_416373:
after_sum_416347:
	ba	after_sum_416334 ! delay slot empty
	nop
sumarm_416338:
nomatch_sum_416335:
sumarm_416394:
	ld	[%sp+116], %r17
	cmp	%r17, 0
	bne	sumarm_416395
	nop
code_418056:
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_418073:
code_418057:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_416407
	nop
zero_case_416406:
	ba	after_zeroone_416408
	or	%r0, 0, %r8
one_case_416407:
	! making direct call 
	ba	funtop_416263
	ld	[%sp+112], %r8
code_418060:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_416408:
	ba	after_sum_416391 ! delay slot empty
	nop
sumarm_416395:
nomatch_sum_416392:
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_418075:
code_418062:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_416429
	nop
zero_case_416428:
	ba	after_zeroone_416430
	or	%r0, 0, %r8
one_case_416429:
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+120], %r9
	call	SplaySetFn_isIn_code_410368
	ld	[%sp+116], %r10
code_418076:
code_418065:
	! done making normal call
after_zeroone_416430:
	cmp	%r8, 0
	bne,pn	%icc,one_case_416441
	nop
zero_case_416440:
	ba	after_zeroone_416442
	or	%r0, 0, %r8
one_case_416441:
	! making direct call 
	ba	funtop_416263
	ld	[%sp+112], %r8
code_418068:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_416442:
after_sum_416391:
after_sum_416334:
	ba	after_sum_416285 ! delay slot empty
	nop
sumarm_416294:
after_sum_416285:
code_418071:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_isIn_code_410368,(.-SplaySetFn_isIn_code_410368)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418073
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x13410000
		! worddata
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418074
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x1c410000
		! worddata
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418075
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x1f410000
		! worddata
	.word 0x00000000
	.word 0x0000006c
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418076
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x13410000
		! worddata
	.word 0x00000000
	.word 0x0000006c
	.text
	.align 8
	.global SplaySetFn_treeIn_code_410362
 ! arguments : [$410364,$8] [$410365,$9] [$406059,$10] [$406060,$11] 
 ! results    : [$416262,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_treeIn_code_410362:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418089
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418089:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r13
	mov	%r11, %r18
code_418078:
funtop_416223:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418079
	nop
code_418080:
	call	GCFromML ! delay slot empty
	nop
needgc_418079:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r11
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r12, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 273, %r9
	cmp	%r12, 3
	or	%r0, 1, %r8
	bgu	cmpui_418082
	nop
code_418083:
	or	%r0, 0, %r8
cmpui_418082:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r13, [%r4+4]
	st	%r18, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_isIn_code_410368), %r8
	or	%r8, %lo(SplaySetFn_isIn_code_410368), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_418085:
	! done making tail call
code_418087:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_treeIn_code_410362,(.-SplaySetFn_treeIn_code_410362)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418079
	.word 0x001c000b
	.word 0x00170000
	.word 0x00002000
	.word 0x00040400
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_equal_code_410387
 ! arguments : [$410389,$8] [$410390,$9] [$406173,$10] [$406174,$11] 
 ! results    : [$416165,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_equal_code_410387:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418112
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418112:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+112]
code_418090:
funtop_416136:
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
sumarm_416149:
	cmp	%r10, 0
	bne	sumarm_416150
	nop
sumarm_416157:
	cmp	%r11, 0
	bne	sumarm_416158
	nop
code_418092:
	ba	after_sum_416154
	or	%r0, 1, %r8
sumarm_416158:
nomatch_sum_416155:
	or	%r0, 0, %r8
after_sum_416154:
	ba	after_sum_416146 ! delay slot empty
	nop
sumarm_416150:
	ld	[%r10], %r12
	ld	[%r10+4], %r9
sumarm_416178:
	or	%r0, 255, %r8
	cmp	%r11, %r8
	bleu	nomatch_sum_416176
	nop
code_418095:
	ld	[%r11], %r16
	st	%r16, [%sp+108]
	ld	[%r11+4], %r8
	cmp	%r9, %r8
	or	%r0, 1, %r8
	be	cmpui_418096
	nop
code_418097:
	or	%r0, 0, %r8
cmpui_418096:
	cmp	%r8, 0
	bne,pn	%icc,one_case_416193
	nop
zero_case_416192:
	ba	after_zeroone_416194
	or	%r0, 0, %r8
one_case_416193:
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+100], %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_418111:
	st	%r8, [%sp+104]
code_418101:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+100], %r8
	jmpl	%r11, %r15
	ld	[%sp+108], %r9
code_418109:
	mov	%r8, %r11
code_418103:
	! done making normal call
	! making closure call
	ld	[%sp+112], %r17
	ld	[%r17], %r12
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418104:
	! done making tail call
after_zeroone_416194:
	ba	after_sum_416175 ! delay slot empty
	nop
sumarm_416179:
nomatch_sum_416176:
	or	%r0, 0, %r8
after_sum_416175:
	ba	after_sum_416146 ! delay slot empty
	nop
sumarm_416166:
after_sum_416146:
code_418108:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_equal_code_410387,(.-SplaySetFn_equal_code_410387)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418109
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01340000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418111
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01440000
	.text
	.align 8
	.global SplaySetFn_isSubset_code_410398
 ! arguments : [$410400,$8] [$410401,$9] [$406190,$10] [$406191,$11] 
 ! results    : [$416078,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_isSubset_code_410398:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418133
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418133:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+112]
code_418113:
funtop_416060:
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
sumarm_416073:
	cmp	%r10, 0
	bne	sumarm_416074
	nop
code_418114:
	ba	after_sum_416070
	or	%r0, 1, %r8
sumarm_416074:
	ld	[%r10], %r12
	ld	[%r10+4], %r9
sumarm_416091:
	or	%r0, 255, %r8
	cmp	%r11, %r8
	bleu	nomatch_sum_416089
	nop
code_418116:
	ld	[%r11], %r16
	st	%r16, [%sp+108]
	ld	[%r11+4], %r8
	cmp	%r9, %r8
	or	%r0, 1, %r8
	ble	cmpsi_418117
	nop
code_418118:
	or	%r0, 0, %r8
cmpsi_418117:
	cmp	%r8, 0
	bne,pn	%icc,one_case_416106
	nop
zero_case_416105:
	ba	after_zeroone_416107
	or	%r0, 0, %r8
one_case_416106:
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+100], %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_418132:
	st	%r8, [%sp+104]
code_418122:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+100], %r8
	jmpl	%r11, %r15
	ld	[%sp+108], %r9
code_418130:
	mov	%r8, %r11
code_418124:
	! done making normal call
	! making closure call
	ld	[%sp+112], %r17
	ld	[%r17], %r12
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418125:
	! done making tail call
after_zeroone_416107:
	ba	after_sum_416088 ! delay slot empty
	nop
sumarm_416092:
nomatch_sum_416089:
	or	%r0, 0, %r8
after_sum_416088:
	ba	after_sum_416070 ! delay slot empty
	nop
sumarm_416079:
after_sum_416070:
code_418129:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_isSubset_code_410398,(.-SplaySetFn_isSubset_code_410398)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418130
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01340000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418132
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01440000
	.text
	.align 8
	.global SplaySetFn_cmp_code_410409
 ! arguments : [$410411,$8] [$410412,$9] [$406332,$10] [$406333,$11] 
 ! results    : [$415965,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_cmp_code_410409:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418171
	nop
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_418171:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+144]
	st	%r11, [%sp+148]
code_418134:
funtop_415730:
	! Proj_c at label reify_408257_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+112]
	! Proj_c at label reify_408149_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_406223_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+20], %r16
	st	%r16, [%sp+116]
	ld	[%sp+144], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+136]
	ld	[%sp+144], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+140]
	ld	[%sp+144], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+128]
	ld	[%sp+144], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+132]
sumarm_415767:
	or	%r0, 255, %r8
	cmp	%r10, %r8
	bleu	nomatch_sum_415765
	nop
code_418135:
	ld	[%r10], %r16
	st	%r16, [%sp+120]
	ld	[%r10+4], %r11
sumarm_415805:
	or	%r0, 255, %r8
	ld	[%sp+120], %r17
	cmp	%r17, %r8
	bleu	nomatch_sum_415803
	nop
code_418136:
	ld	[%sp+120], %r17
	ld	[%r17+4], %r10
	! making closure call
	ld	[%sp+140], %r17
	ld	[%r17], %r12
	ld	[%sp+140], %r17
	ld	[%r17+4], %r8
	ld	[%sp+140], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_418168:
code_418137:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418138
	nop
code_418139:
	call	GCFromML ! delay slot empty
	nop
needgc_418138:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_415802 ! delay slot empty
	nop
sumarm_415806:
nomatch_sum_415803:
	ld	[%sp+132], %r8
after_sum_415802:
	ba	after_sum_415764
	st	%r8, [%sp+124]
sumarm_415768:
nomatch_sum_415765:
	ld	[%sp+132], %r16
	st	%r16, [%sp+124]
after_sum_415764:
sumarm_415851:
	or	%r0, 255, %r8
	ld	[%sp+148], %r17
	cmp	%r17, %r8
	bleu	nomatch_sum_415849
	nop
code_418143:
	ld	[%sp+148], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+120]
	ld	[%sp+148], %r17
	ld	[%r17+4], %r11
sumarm_415889:
	or	%r0, 255, %r8
	ld	[%sp+120], %r17
	cmp	%r17, %r8
	bleu	nomatch_sum_415887
	nop
code_418144:
	ld	[%sp+120], %r17
	ld	[%r17+4], %r10
	! making closure call
	ld	[%sp+140], %r17
	ld	[%r17], %r12
	ld	[%sp+140], %r17
	ld	[%r17+4], %r8
	ld	[%sp+140], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_418169:
code_418145:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418146
	nop
code_418147:
	call	GCFromML ! delay slot empty
	nop
needgc_418146:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_415886 ! delay slot empty
	nop
sumarm_415890:
nomatch_sum_415887:
	ld	[%sp+132], %r8
after_sum_415886:
	ba	after_sum_415848
	mov	%r8, %r10
sumarm_415852:
nomatch_sum_415849:
	ld	[%sp+132], %r10
after_sum_415848:
	ld	[%sp+124], %r17
	ld	[%r17], %r9
	ld	[%sp+124], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+124]
	ld	[%r10], %r8
	ld	[%r10+4], %r16
	st	%r16, [%sp+120]
sumarm_415944:
	cmp	%r9, 0
	bne	sumarm_415945
	nop
sumarm_415957:
	cmp	%r8, 0
	bne	sumarm_415958
	nop
code_418152:
	ba	after_sum_415954
	or	%r0, 0, %r8
sumarm_415958:
nomatch_sum_415955:
	or	%r0, 2, %r8
after_sum_415954:
	ba	after_sum_415941 ! delay slot empty
	nop
sumarm_415945:
nomatch_sum_415942:
sumarm_415976:
	cmp	%r8, 0
	bne	sumarm_415977
	nop
code_418155:
	ba	after_sum_415973
	or	%r0, 1, %r8
sumarm_415977:
	ld	[%r8+8], %r11
sumarm_416011:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	bleu	nomatch_sum_416009
	nop
code_418157:
	ld	[%r9+8], %r10
	! making closure call
	ld	[%sp+136], %r17
	ld	[%r17], %r12
	ld	[%sp+136], %r17
	ld	[%r17+4], %r8
	ld	[%sp+136], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_418170:
code_418158:
	! done making normal call
sumarm_416044:
	cmp	%r8, 0
	bne	sumarm_416045
	nop
code_418159:
	! making direct call 
	ld	[%sp+124], %r10
	ld	[%sp+120], %r16
	ba	funtop_415730
	st	%r16, [%sp+148]
code_418160:
	! done making self tail call
	ba	after_sum_416041
	or	%r0, 0, %r8
sumarm_416045:
nomatch_sum_416042:
after_sum_416041:
	ba	after_sum_416008 ! delay slot empty
	nop
sumarm_416012:
nomatch_sum_416009:
	sethi	%hi(record_411651), %r8
	or	%r8, %lo(record_411651), %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_416008:
	ba	after_sum_415973 ! delay slot empty
	nop
sumarm_415982:
after_sum_415973:
after_sum_415941:
code_418167:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size SplaySetFn_cmp_code_410409,(.-SplaySetFn_cmp_code_410409)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418168
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35550000
	.word 0x00000d54
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_418138
	.word 0x0028000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x35550000
	.word 0x00000d54
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418169
	.word 0x0028000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x75550000
	.word 0x00000110
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_418146
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x75550000
	.word 0x00000110
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418170
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xf1010000
	.word 0x00000100
		! worddata
	.word 0x00000000
	.word 0x00000070
	.word 0x00000000
	.word 0x00000070
	.text
	.align 8
	.global SplaySetFn_compare_code_410434
 ! arguments : [$410436,$8] [$410437,$9] [$406327,$10] [$406328,$11] 
 ! results    : [$415661,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_compare_code_410434:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418198
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418198:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418172:
funtop_415623:
	! Proj_c at label type_406223_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	ld	[%r9], %r16
	st	%r16, [%sp+116]
	ld	[%r9+4], %r16
	st	%r16, [%sp+120]
	ld	[%r9+8], %r16
	st	%r16, [%sp+124]
sumarm_415645:
	cmp	%r10, 0
	bne	sumarm_415646
	nop
sumarm_415653:
	cmp	%r11, 0
	bne	sumarm_415654
	nop
code_418174:
	ba	after_sum_415650
	or	%r0, 0, %r8
sumarm_415654:
nomatch_sum_415651:
	or	%r0, 2, %r8
after_sum_415650:
	ba	after_sum_415642 ! delay slot empty
	nop
sumarm_415646:
nomatch_sum_415643:
sumarm_415667:
	cmp	%r11, 0
	bne	sumarm_415668
	nop
code_418177:
	ba	after_sum_415664
	or	%r0, 1, %r8
sumarm_415668:
	ld	[%r11], %r16
	st	%r16, [%sp+112]
sumarm_415683:
	or	%r0, 255, %r8
	cmp	%r10, %r8
	bleu	nomatch_sum_415681
	nop
code_418179:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418193:
	mov	%r8, %r10
code_418181:
	! done making normal call
	! making closure call
	ld	[%sp+124], %r17
	ld	[%r17], %r12
	ld	[%sp+124], %r17
	ld	[%r17+4], %r8
	ld	[%sp+124], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_418194:
	st	%r8, [%sp+108]
code_418182:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+104], %r8
	jmpl	%r11, %r15
	ld	[%sp+112], %r9
code_418195:
	mov	%r8, %r10
code_418184:
	! done making normal call
	! making closure call
	ld	[%sp+124], %r17
	ld	[%r17], %r12
	ld	[%sp+124], %r17
	ld	[%r17+4], %r8
	ld	[%sp+124], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_418196:
	mov	%r8, %r11
code_418185:
	! done making normal call
	! making closure call
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418186:
	! done making tail call
	ba	after_sum_415680 ! delay slot empty
	nop
sumarm_415684:
nomatch_sum_415681:
	sethi	%hi(record_411651), %r8
	or	%r8, %lo(record_411651), %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_415680:
	ba	after_sum_415664 ! delay slot empty
	nop
sumarm_415673:
after_sum_415664:
after_sum_415642:
code_418192:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_compare_code_410434,(.-SplaySetFn_compare_code_410434)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418193
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x75140000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418194
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x75140000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418195
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x74d40000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418196
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04c40000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_numItems_code_410451
 ! arguments : [$410453,$8] [$410454,$9] [$394787,$10] 
 ! results    : [$415616,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
SplaySetFn_numItems_code_410451:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418205
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418205:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418199:
funtop_415602:
sumarm_415611:
	cmp	%r10, 0
	bne	sumarm_415612
	nop
code_418200:
	ba	after_sum_415608
	or	%r0, 0, %r8
sumarm_415612:
	ba	after_sum_415608
	ld	[%r10+4], %r8
sumarm_415617:
after_sum_415608:
code_418204:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_numItems_code_410451,(.-SplaySetFn_numItems_code_410451)

	.section	".rodata"
	.text
	.align 8
	.global SplaySetFn_apply_inner_code_410458
 ! arguments : [$410460,$8] [$410461,$9] [$406636,$10] [$406637,$11] 
 ! results    : [$415544,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_apply_inner_code_410458:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418218
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418218:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
code_418206:
funtop_415516:
	! Proj_c at label reify_407806_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+104]
sumarm_415540:
	cmp	%r10, 0
	bne	sumarm_415541
	nop
code_418207:
	ba	after_sum_415537
	mov	%r11, %r8
sumarm_415541:
	ld	[%r10+8], %r16
	st	%r16, [%sp+112]
	ld	[%r10], %r16
	st	%r16, [%sp+116]
	ld	[%r10+4], %r10
	! making direct call 
	ld	[%sp+96], %r8
	call	SplaySetFn_apply_inner_code_410458
	ld	[%sp+120], %r9
code_418217:
code_418209:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418210
	nop
code_418211:
	call	GCFromML ! delay slot empty
	nop
needgc_418210:
	! allocating 2-record
	ld	[%sp+120], %r17
	st	%r17, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	ld	[%sp+116], %r10
	ba	funtop_415516
	mov	%r8, %r11
code_418213:
	! done making self tail call
	ba	after_sum_415537
	or	%r0, 0, %r8
sumarm_415545:
after_sum_415537:
code_418216:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_apply_inner_code_410458,(.-SplaySetFn_apply_inner_code_410458)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418210
	.word 0x0020000d
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x0f550000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x0000006c
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418217
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0f550000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x0000006c
	.text
	.align 8
	.global SplaySetFn_listItems_code_410473
 ! arguments : [$410475,$8] [$410476,$9] [$394793,$10] 
 ! results    : [$415495,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_listItems_code_410473:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418230
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418230:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418219:
funtop_415471:
	! Proj_c at label reify_407806_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	ld	[%r9], %r16
	st	%r16, [%sp+112]
	ld	[%r9+4], %r16
	st	%r16, [%sp+108]
sumarm_415491:
	cmp	%r10, 0
	bne	sumarm_415492
	nop
code_418220:
	ba	after_sum_415488
	ld	[%sp+108], %r8
sumarm_415492:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418229:
	mov	%r8, %r10
code_418223:
	! done making normal call
	! making closure call
	ld	[%sp+112], %r17
	ld	[%r17], %r12
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418224:
	! done making tail call
	ba	after_sum_415488 ! delay slot empty
	nop
sumarm_415496:
after_sum_415488:
code_418227:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_listItems_code_410473,(.-SplaySetFn_listItems_code_410473)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418229
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01d40000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410495
 ! arguments : [$410497,$8] [$410498,$9] [$409525,$10] 
 ! results    : [$415470,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410495:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418236
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418236:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_418231:
funtop_415455:
	ld	[%r8], %r9
	ld	[%r8+4], %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_418232:
	! done making tail call
code_418234:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410495,(.-SplaySetFn_anonfun_code_410495)

	.section	".rodata"
	.text
	.align 8
	.global SplaySetFn_split_code_410488
 ! arguments : [$410490,$8] [$410491,$9] [$406692,$10] [$406693,$11] 
 ! results    : [$415344,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_split_code_410488:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418273
	nop
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_418273:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r12
	st	%r11, [%sp+136]
code_418237:
funtop_415251:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418238
	nop
code_418239:
	call	GCFromML ! delay slot empty
	nop
needgc_418238:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_402297_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+140]
	! Proj_c at label type_402264_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+20], %r16
	st	%r16, [%sp+112]
	ld	[%r9], %r10
	ld	[%r9+4], %r16
	st	%r16, [%sp+128]
	ld	[%r9+8], %r16
	st	%r16, [%sp+120]
	ld	[%r9+12], %r16
	st	%r16, [%sp+124]
	ld	[%r9+16], %r16
	st	%r16, [%sp+116]
	ld	[%r9+20], %r16
	st	%r16, [%sp+132]
	ld	[%r9+24], %r16
	st	%r16, [%sp+144]
	! allocating 1 closures
	or	%r0, 273, %r9
	ld	[%sp+112], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418241
	nop
code_418242:
	or	%r0, 0, %r8
cmpui_418241:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410495), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410495), %r8
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_400218), %r8
	or	%r8, %lo(type_400218), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+112], %r9
code_418271:
	mov	%r8, %r10
code_418248:
	! done making normal call
	! making closure call
	ld	[%sp+124], %r17
	ld	[%r17], %r12
	ld	[%sp+124], %r17
	ld	[%r17+4], %r8
	ld	[%sp+124], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+136], %r11
code_418272:
code_418249:
	! done making normal call
	add	%r4, 88, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418250
	nop
code_418251:
	call	GCFromML ! delay slot empty
	nop
needgc_418250:
	ld	[%r8], %r9
	ld	[%r8+4], %r8
sumarm_415340:
	cmp	%r8, 0
	bne	sumarm_415341
	nop
code_418253:
	ba	after_sum_415337
	ld	[%sp+144], %r8
sumarm_415341:
	ld	[%r8+8], %r12
	ld	[%r8], %r11
	ld	[%r8+4], %r10
sumarm_415383:
	cmp	%r9, 0
	bne	sumarm_415384
	nop
code_418255:
	ld	[%sp+140], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	bleu,pn	%icc,dynamic_box_415397
	nop
code_418256:
	cmp	%r8, 255
	bleu,pn	%icc,dynamic_nobox_415398
	nop
code_418257:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_415397
	nop
code_418258:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_415397
	nop
code_418259:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_415397
	nop
dynamic_nobox_415398:
	ba	xinject_sum_dyn_after_415394
	mov	%r12, %r8
dynamic_box_415397:
	or	%r0, 9, %r9
	ld	[%sp+112], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418262
	nop
code_418263:
	or	%r0, 0, %r8
cmpui_418262:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1-record
	st	%r9, [%r4]
	st	%r12, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
xinject_sum_dyn_after_415394:
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	ba	after_sum_415380 ! delay slot empty
	nop
sumarm_415384:
	cmp	%r9, 1
	bne	sumarm_415411
	nop
code_418265:
	! allocating 3-record
	ld	[%sp+120], %r17
	st	%r17, [%r4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+4]
	st	%r10, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	ba	after_sum_415380 ! delay slot empty
	nop
sumarm_415411:
	! allocating 3-record
	ld	[%sp+120], %r17
	st	%r17, [%r4]
	st	%r11, [%r4+4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	ba	after_sum_415380 ! delay slot empty
	nop
sumarm_415433:
after_sum_415380:
	ba	after_sum_415337 ! delay slot empty
	nop
sumarm_415345:
after_sum_415337:
code_418270:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size SplaySetFn_split_code_410488,(.-SplaySetFn_split_code_410488)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418271
	.word 0x0028000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x4d550000
	.word 0x00000173
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_418238
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000200
	.word 0x00001000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000030
		! worddata
	.word 0x00000001
	.word 0x00000060
	.word 0x00000006
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418272
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0d540000
	.word 0x00000143
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_418250
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0d540000
	.word 0x00000143
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_inter_code_410529
 ! arguments : [$410531,$8] [$410532,$9] [$406773,$10] [$406774,$11] 
 ! results    : [$415072,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_inter_code_410529:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418311
	nop
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_418311:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+148]
	mov	%r10, %r12
code_418274:
funtop_415028:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_402297_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+120]
	! Proj_c at label type_402264_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+20], %r16
	st	%r16, [%sp+100]
	ld	[%sp+148], %r17
	ld	[%r17], %r9
	ld	[%sp+148], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+140]
	ld	[%sp+148], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+144]
	ld	[%sp+148], %r17
	ld	[%r17+12], %r8
	ld	[%sp+148], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+136]
sumarm_415068:
	cmp	%r12, 0
	bne	sumarm_415069
	nop
code_418275:
	ba	after_sum_415065 ! delay slot empty
	nop
sumarm_415069:
nomatch_sum_415066:
sumarm_415083:
	cmp	%r11, 0
	bne	sumarm_415084
	nop
code_418277:
	ba	after_sum_415080 ! delay slot empty
	nop
sumarm_415084:
	ld	[%r11+8], %r10
	ld	[%r11], %r16
	st	%r16, [%sp+116]
	ld	[%r11+4], %r16
	st	%r16, [%sp+132]
	! making closure call
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	mov	%r12, %r11
code_418310:
code_418279:
	! done making normal call
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	ld	[%r8+8], %r16
	st	%r16, [%sp+128]
sumarm_415145:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	bleu	nomatch_sum_415143
	nop
code_418280:
	ld	[%sp+120], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	bleu,pn	%icc,dynamic_box_415160
	nop
code_418281:
	cmp	%r8, 255
	bleu,pn	%icc,dynamic_nobox_415161
	nop
code_418282:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_415160
	nop
code_418283:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_415160
	nop
code_418284:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_415160
	nop
dynamic_nobox_415161:
	ba	projsum_single_after_415157
	st	%r9, [%sp+124]
dynamic_box_415160:
	ld	[%r9], %r16
	st	%r16, [%sp+124]
projsum_single_after_415157:
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+148], %r9
	call	SplaySetFn_inter_code_410529
	ld	[%sp+116], %r11
code_418309:
code_418287:
	! done making normal call
	ld	[%r8], %r16
	st	%r16, [%sp+116]
	ld	[%r8+4], %r16
	st	%r16, [%sp+120]
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+148], %r9
	ld	[%sp+128], %r10
	call	SplaySetFn_inter_code_410529
	ld	[%sp+132], %r11
code_418305:
code_418288:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418289
	nop
code_418290:
	call	GCFromML ! delay slot empty
	nop
needgc_418289:
	ld	[%r8], %r9
	ld	[%r8+4], %r8
	! allocating 3-record
	ld	[%sp+136], %r17
	st	%r17, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+124], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	ld	[%sp+120], %r17
	addcc	%r17, %r8, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_418292:
	addcc	%r8, 1, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_418293:
	! allocating 2-record
	ld	[%sp+144], %r17
	st	%r17, [%r4]
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_415142 ! delay slot empty
	nop
sumarm_415146:
nomatch_sum_415143:
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+148], %r9
	call	SplaySetFn_inter_code_410529
	ld	[%sp+116], %r11
code_418308:
code_418295:
	! done making normal call
	ld	[%r8], %r16
	st	%r16, [%sp+120]
	ld	[%r8+4], %r16
	st	%r16, [%sp+124]
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+148], %r9
	ld	[%sp+128], %r10
	call	SplaySetFn_inter_code_410529
	ld	[%sp+132], %r11
code_418306:
code_418296:
	! done making normal call
	ld	[%r8], %r11
	ld	[%r8+4], %r16
	st	%r16, [%sp+116]
	! making closure call
	ld	[%sp+140], %r17
	ld	[%r17], %r12
	ld	[%sp+140], %r17
	ld	[%r17+4], %r8
	ld	[%sp+140], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_418307:
	mov	%r8, %r9
code_418297:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418298
	nop
code_418299:
	call	GCFromML ! delay slot empty
	nop
needgc_418298:
	ld	[%sp+116], %r16
	ld	[%sp+124], %r17
	addcc	%r17, %r16, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_418301:
	! allocating 2-record
	ld	[%sp+144], %r17
	st	%r17, [%r4]
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_sum_415142:
	ba	after_sum_415080 ! delay slot empty
	nop
sumarm_415088:
after_sum_415080:
after_sum_415065:
code_418304:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size SplaySetFn_inter_code_410529,(.-SplaySetFn_inter_code_410529)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418305
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xcc140000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_418289
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xcc140000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418306
	.word 0x0028000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x31100000
	.word 0x00000040
		! worddata
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418307
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_418298
	.word 0x0028000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000070
		! -------- label,sizes,reg
	.long code_418308
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01110000
	.word 0x0000044f
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418309
	.word 0x0028000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0150000
	.word 0x0000040f
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418310
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x1d550000
	.word 0x0000044c
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000068
	.text
	.align 8
	.global SplaySetFn_intersection_code_410556
 ! arguments : [$410558,$8] [$410559,$9] [$406768,$10] [$406769,$11] 
 ! results    : [$414947,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_intersection_code_410556:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418340
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418340:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418312:
funtop_414918:
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+132]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	ld	[%r9], %r16
	st	%r16, [%sp+120]
	ld	[%r9+4], %r16
	st	%r16, [%sp+124]
	ld	[%r9+8], %r16
	st	%r16, [%sp+128]
sumarm_414943:
	cmp	%r10, 0
	bne	sumarm_414944
	nop
code_418313:
	ba	after_sum_414940
	ld	[%sp+124], %r8
sumarm_414944:
nomatch_sum_414941:
sumarm_414953:
	cmp	%r11, 0
	bne	sumarm_414954
	nop
code_418315:
	ba	after_sum_414950
	ld	[%sp+124], %r8
sumarm_414954:
	ld	[%r11], %r16
	st	%r16, [%sp+116]
sumarm_414968:
	or	%r0, 255, %r8
	cmp	%r10, %r8
	bleu	nomatch_sum_414966
	nop
code_418317:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418336:
	st	%r8, [%sp+112]
code_418319:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+104], %r8
	jmpl	%r11, %r15
	ld	[%sp+116], %r9
code_418337:
	mov	%r8, %r11
code_418321:
	! done making normal call
	! making closure call
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_418338:
code_418322:
	! done making normal call
	ld	[%r8], %r10
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
	ld	[%sp+112], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_415005
	nop
zero_case_415004:
	ba	after_zeroone_415006
	ld	[%sp+124], %r8
one_case_415005:
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+132], %r8
code_418339:
code_418326:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418327
	nop
code_418328:
	call	GCFromML ! delay slot empty
	nop
needgc_418327:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_415006:
	ba	after_sum_414965 ! delay slot empty
	nop
sumarm_414969:
nomatch_sum_414966:
	sethi	%hi(record_411651), %r8
	or	%r8, %lo(record_411651), %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_414965:
	ba	after_sum_414950 ! delay slot empty
	nop
sumarm_414958:
after_sum_414950:
after_sum_414940:
code_418335:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_intersection_code_410556,(.-SplaySetFn_intersection_code_410556)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418336
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd4540000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418337
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd3540000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418338
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0440000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_418327
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418339
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_cnt_inner_code_410575
 ! arguments : [$410577,$8] [$410578,$9] [$406875,$10] [$406876,$11] 
 ! results    : [$414876,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_cnt_inner_code_410575:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418351
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418351:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+108]
	mov	%r10, %r9
code_418341:
funtop_414854:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+100]
sumarm_414872:
	cmp	%r9, 0
	bne	sumarm_414873
	nop
code_418342:
	ba	after_sum_414869
	mov	%r11, %r8
sumarm_414873:
	ld	[%r9+4], %r10
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	addcc	%r11, 1, %r11
	bvs,pn	%icc,localOverflowFromML
	nop
code_418344:
	! making direct call 
	ld	[%sp+96], %r8
	call	SplaySetFn_cnt_inner_code_410575
	ld	[%sp+108], %r9
code_418350:
code_418345:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r9
	ba	funtop_414854
	mov	%r8, %r11
code_418346:
	! done making self tail call
	ba	after_sum_414869
	or	%r0, 0, %r8
sumarm_414877:
after_sum_414869:
code_418349:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_cnt_inner_code_410575,(.-SplaySetFn_cnt_inner_code_410575)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418350
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00750000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_diff_code_410584
 ! arguments : [$410586,$8] [$410587,$9] [$406923,$10] [$406924,$11] 
 ! results    : [$414739,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_diff_code_410584:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418377
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418377:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+136]
	st	%r10, [%sp+112]
	mov	%r11, %r12
code_418352:
funtop_414701:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	ld	[%sp+136], %r17
	ld	[%r17], %r11
	ld	[%sp+136], %r17
	ld	[%r17+4], %r9
	ld	[%sp+136], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+132]
	ld	[%sp+136], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+128]
	ld	[%sp+136], %r17
	ld	[%r17+16], %r8
sumarm_414735:
	ld	[%sp+112], %r17
	cmp	%r17, 0
	bne	sumarm_414736
	nop
code_418353:
	ba	after_sum_414732 ! delay slot empty
	nop
sumarm_414736:
nomatch_sum_414733:
sumarm_414750:
	cmp	%r12, 0
	bne	sumarm_414751
	nop
code_418355:
	or	%r0, 0, %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_418372:
code_418356:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418357
	nop
code_418358:
	call	GCFromML ! delay slot empty
	nop
needgc_418357:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_414747 ! delay slot empty
	nop
sumarm_414751:
	ld	[%r12+8], %r10
	ld	[%r12+4], %r16
	st	%r16, [%sp+124]
	ld	[%r12], %r16
	st	%r16, [%sp+116]
	! making closure call
	ld	[%r11], %r12
	ld	[%r11+4], %r8
	ld	[%r11+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r11
code_418376:
code_418361:
	! done making normal call
	ld	[%r8+4], %r10
	ld	[%r8+8], %r16
	st	%r16, [%sp+112]
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+136], %r9
	call	SplaySetFn_diff_code_410584
	ld	[%sp+116], %r11
code_418373:
code_418362:
	! done making normal call
	ld	[%r8], %r16
	st	%r16, [%sp+116]
	ld	[%r8+4], %r16
	st	%r16, [%sp+120]
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+136], %r9
	ld	[%sp+112], %r10
	call	SplaySetFn_diff_code_410584
	ld	[%sp+124], %r11
code_418374:
code_418363:
	! done making normal call
	ld	[%r8], %r11
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
	! making closure call
	ld	[%sp+132], %r17
	ld	[%r17], %r12
	ld	[%sp+132], %r17
	ld	[%r17+4], %r8
	ld	[%sp+132], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_418375:
	mov	%r8, %r9
code_418364:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418365
	nop
code_418366:
	call	GCFromML ! delay slot empty
	nop
needgc_418365:
	ld	[%sp+112], %r16
	ld	[%sp+120], %r17
	addcc	%r17, %r16, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_418368:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_414747 ! delay slot empty
	nop
sumarm_414768:
after_sum_414747:
after_sum_414732:
code_418371:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_diff_code_410584,(.-SplaySetFn_diff_code_410584)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418372
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03010000
	.word 0x00000000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_418357
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03010000
	.word 0x00000000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418373
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc3510000
	.word 0x00000014
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418374
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c500000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418375
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_418365
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418376
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xcc510000
	.word 0x00000014
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000068
	.text
	.align 8
	.global SplaySetFn_difference_code_410607
 ! arguments : [$410609,$8] [$410610,$9] [$406918,$10] [$406919,$11] 
 ! results    : [$414620,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_difference_code_410607:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418406
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418406:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418378:
funtop_414591:
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+132]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	ld	[%r9], %r16
	st	%r16, [%sp+120]
	ld	[%r9+4], %r16
	st	%r16, [%sp+124]
	ld	[%r9+8], %r16
	st	%r16, [%sp+128]
sumarm_414616:
	cmp	%r10, 0
	bne	sumarm_414617
	nop
code_418379:
	ba	after_sum_414613
	ld	[%sp+124], %r8
sumarm_414617:
nomatch_sum_414614:
sumarm_414626:
	cmp	%r11, 0
	bne	sumarm_414627
	nop
code_418381:
	ba	after_sum_414623 ! delay slot empty
	nop
sumarm_414627:
	ld	[%r11], %r16
	st	%r16, [%sp+116]
sumarm_414641:
	or	%r0, 255, %r8
	cmp	%r10, %r8
	bleu	nomatch_sum_414639
	nop
code_418383:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418402:
	st	%r8, [%sp+112]
code_418385:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+104], %r8
	jmpl	%r11, %r15
	ld	[%sp+116], %r9
code_418403:
	mov	%r8, %r11
code_418387:
	! done making normal call
	! making closure call
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_418404:
code_418388:
	! done making normal call
	ld	[%r8], %r10
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
	ld	[%sp+112], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_414678
	nop
zero_case_414677:
	ba	after_zeroone_414679
	ld	[%sp+124], %r8
one_case_414678:
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+132], %r8
code_418405:
code_418392:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418393
	nop
code_418394:
	call	GCFromML ! delay slot empty
	nop
needgc_418393:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_414679:
	ba	after_sum_414638 ! delay slot empty
	nop
sumarm_414642:
nomatch_sum_414639:
	sethi	%hi(record_411651), %r8
	or	%r8, %lo(record_411651), %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_414638:
	ba	after_sum_414623
	mov	%r8, %r10
sumarm_414631:
after_sum_414623:
	mov	%r10, %r8
after_sum_414613:
code_418401:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_difference_code_410607,(.-SplaySetFn_difference_code_410607)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418402
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd4540000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418403
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd3540000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418404
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0440000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_418393
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418405
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_uni_code_410626
 ! arguments : [$410628,$8] [$410629,$9] [$407030,$10] [$407031,$11] 
 ! results    : [$414467,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_uni_code_410626:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418436
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418436:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+136]
	st	%r10, [%sp+112]
	st	%r11, [%sp+108]
code_418407:
funtop_414418:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	ld	[%sp+136], %r17
	ld	[%r17], %r10
	ld	[%sp+136], %r17
	ld	[%r17+4], %r9
	ld	[%sp+136], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+132]
	ld	[%sp+136], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+128]
sumarm_414450:
	ld	[%sp+112], %r17
	cmp	%r17, 0
	bne	sumarm_414451
	nop
code_418408:
	or	%r0, 0, %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r10
code_418431:
code_418409:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418410
	nop
code_418411:
	call	GCFromML ! delay slot empty
	nop
needgc_418410:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_414447 ! delay slot empty
	nop
sumarm_414451:
nomatch_sum_414448:
sumarm_414478:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	bne	sumarm_414479
	nop
code_418414:
	or	%r0, 0, %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_418432:
code_418415:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418416
	nop
code_418417:
	call	GCFromML ! delay slot empty
	nop
needgc_418416:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_414475 ! delay slot empty
	nop
sumarm_414479:
	ld	[%sp+108], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+120]
	ld	[%sp+108], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+124]
	ld	[%sp+108], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! making closure call
	ld	[%r10], %r12
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	ld	[%sp+120], %r10
	jmpl	%r12, %r15
	ld	[%sp+112], %r11
code_418435:
code_418420:
	! done making normal call
	ld	[%r8+4], %r10
	ld	[%r8+8], %r16
	st	%r16, [%sp+116]
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+136], %r9
	call	SplaySetFn_uni_code_410626
	ld	[%sp+108], %r11
code_418433:
code_418421:
	! done making normal call
	ld	[%r8], %r16
	st	%r16, [%sp+112]
	ld	[%r8+4], %r16
	st	%r16, [%sp+108]
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+136], %r9
	ld	[%sp+116], %r10
	call	SplaySetFn_uni_code_410626
	ld	[%sp+124], %r11
code_418434:
code_418422:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418423
	nop
code_418424:
	call	GCFromML ! delay slot empty
	nop
needgc_418423:
	ld	[%r8], %r9
	ld	[%r8+4], %r8
	! allocating 3-record
	ld	[%sp+132], %r17
	st	%r17, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+120], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	ld	[%sp+108], %r17
	addcc	%r17, %r8, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_418426:
	addcc	%r8, 1, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_418427:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_414475 ! delay slot empty
	nop
sumarm_414496:
after_sum_414475:
after_sum_414447:
code_418430:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_uni_code_410626,(.-SplaySetFn_uni_code_410626)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418431
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00c10000
	.word 0x00000000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_418410
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00c10000
	.word 0x00000000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418432
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03010000
	.word 0x00000000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_418416
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03010000
	.word 0x00000000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418433
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xfc150000
	.word 0x00000010
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418434
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x33140000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_418423
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x33140000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418435
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xf0d50000
	.word 0x00000010
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
	.word 0x00000000
	.word 0x00000068
	.text
	.align 8
	.global SplaySetFn_union_code_410647
 ! arguments : [$410649,$8] [$410650,$9] [$407025,$10] [$407026,$11] 
 ! results    : [$414344,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_union_code_410647:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418463
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418463:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418437:
funtop_414317:
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+124]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	ld	[%r9], %r16
	st	%r16, [%sp+128]
	ld	[%r9+4], %r16
	st	%r16, [%sp+120]
sumarm_414340:
	cmp	%r10, 0
	bne	sumarm_414341
	nop
code_418438:
	ba	after_sum_414337
	mov	%r11, %r8
sumarm_414341:
nomatch_sum_414338:
sumarm_414350:
	cmp	%r11, 0
	bne	sumarm_414351
	nop
code_418440:
	ba	after_sum_414347 ! delay slot empty
	nop
sumarm_414351:
	ld	[%r11], %r16
	st	%r16, [%sp+116]
sumarm_414365:
	or	%r0, 255, %r8
	cmp	%r10, %r8
	bleu	nomatch_sum_414363
	nop
code_418442:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418459:
	st	%r8, [%sp+112]
code_418444:
	! done making normal call
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	ld	[%sp+104], %r8
	jmpl	%r11, %r15
	ld	[%sp+116], %r9
code_418460:
	mov	%r8, %r11
code_418446:
	! done making normal call
	! making closure call
	ld	[%sp+128], %r17
	ld	[%r17], %r12
	ld	[%sp+128], %r17
	ld	[%r17+4], %r8
	ld	[%sp+128], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_418461:
code_418447:
	! done making normal call
	ld	[%r8], %r10
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+124], %r8
code_418462:
code_418449:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418450
	nop
code_418451:
	call	GCFromML ! delay slot empty
	nop
needgc_418450:
	! allocating 2-record
	ld	[%sp+120], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_414362 ! delay slot empty
	nop
sumarm_414366:
nomatch_sum_414363:
	sethi	%hi(record_411651), %r8
	or	%r8, %lo(record_411651), %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_414362:
	ba	after_sum_414347
	mov	%r8, %r10
sumarm_414355:
after_sum_414347:
	mov	%r10, %r8
after_sum_414337:
code_418458:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_union_code_410647,(.-SplaySetFn_union_code_410647)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418459
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44140000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_418460
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x43140000
	.word 0x00000001
		! worddata
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418461
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418462
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_418450
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_mapf_code_410670
 ! arguments : [$410672,$8] [$410673,$9] [$407122,$10] [$407123,$11] 
 ! results    : [$414258,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_mapf_code_410670:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418477
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418477:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+132]
	mov	%r11, %r9
code_418464:
funtop_414226:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+104]
	ld	[%sp+132], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+124]
	ld	[%sp+132], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+128]
sumarm_414254:
	cmp	%r9, 0
	bne	sumarm_414255
	nop
code_418465:
	ba	after_sum_414251
	mov	%r10, %r8
sumarm_414255:
	ld	[%r9+8], %r16
	st	%r16, [%sp+120]
	ld	[%r9], %r11
	ld	[%r9+4], %r16
	st	%r16, [%sp+116]
	! making direct call 
	ld	[%sp+96], %r8
	call	SplaySetFn_mapf_code_410670
	ld	[%sp+132], %r9
code_418476:
	st	%r8, [%sp+112]
code_418467:
	! done making normal call
	! making closure call
	ld	[%sp+128], %r17
	ld	[%r17], %r11
	ld	[%sp+128], %r17
	ld	[%r17+4], %r8
	ld	[%sp+128], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_418474:
	mov	%r8, %r11
code_418468:
	! done making normal call
	! making closure call
	ld	[%sp+124], %r17
	ld	[%r17], %r12
	ld	[%sp+124], %r17
	ld	[%r17+4], %r8
	ld	[%sp+124], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_418475:
code_418469:
	! done making normal call
	! making direct call 
	mov	%r8, %r10
	ba	funtop_414226
	ld	[%sp+116], %r9
code_418470:
	! done making self tail call
	ba	after_sum_414251
	or	%r0, 0, %r8
sumarm_414259:
after_sum_414251:
code_418473:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_mapf_code_410670,(.-SplaySetFn_mapf_code_410670)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418474
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x4f550000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418475
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c450000
	.word 0x00000004
		! worddata
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418476
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7c550000
	.word 0x00000005
		! worddata
	.word 0x00000000
	.word 0x0000006c
	.word 0x00000000
	.word 0x00000068
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410687
 ! arguments : [$410689,$8] [$410690,$9] [$395370,$10] 
 ! results    : [$414205,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410687:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418489
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418489:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418478:
funtop_414184:
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+100]
	ld	[%r9], %r16
	st	%r16, [%sp+112]
	ld	[%r9+4], %r16
	st	%r16, [%sp+108]
sumarm_414201:
	cmp	%r10, 0
	bne	sumarm_414202
	nop
code_418479:
	ba	after_sum_414198
	ld	[%sp+108], %r8
sumarm_414202:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418488:
	mov	%r8, %r11
code_418482:
	! done making normal call
	! making closure call
	ld	[%sp+112], %r17
	ld	[%r17], %r12
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418483:
	! done making tail call
	ba	after_sum_414198 ! delay slot empty
	nop
sumarm_414206:
after_sum_414198:
code_418486:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_anonfun_code_410687,(.-SplaySetFn_anonfun_code_410687)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418488
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01d40000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_map_code_410664
 ! arguments : [$410666,$8] [$410667,$9] [$395368,$10] 
 ! results    : [$414179,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_map_code_410664:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418504
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418504:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_418490:
funtop_414107:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r16
	st	%r16, [%sp+116]
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+120]
	! Proj_c at label type_400233_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_400230_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+96]
	! Proj_c at label type_400221_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+104]
	ld	[%r9], %r16
	st	%r16, [%sp+108]
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+104], %r9
	jmpl	%r13, %r15
	ld	[%sp+104], %r10
code_418503:
	mov	%r8, %r9
code_418493:
	! done making normal call
	add	%r4, 88, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418494
	nop
code_418495:
	call	GCFromML ! delay slot empty
	nop
needgc_418494:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	ld	[%sp+104], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_mapf_code_410670), %r8
	or	%r8, %lo(SplaySetFn_mapf_code_410670), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 273, %r9
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418498
	nop
code_418499:
	or	%r0, 0, %r8
cmpui_418498:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r11, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410687), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410687), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418502:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_map_code_410664,(.-SplaySetFn_map_code_410664)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418494
	.word 0x00200009
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x155d0000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418503
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x155d0000
		! worddata
	.word 0x00000000
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_filt_code_410713
 ! arguments : [$410715,$8] [$410716,$9] [$407316,$10] [$407317,$11] 
 ! results    : [$414042,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_filt_code_410713:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418521
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418521:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+124]
	mov	%r10, %r9
code_418505:
funtop_414013:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	ld	[%sp+124], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+120]
	ld	[%sp+124], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+116]
sumarm_414038:
	cmp	%r9, 0
	bne	sumarm_414039
	nop
code_418506:
	ba	after_sum_414035
	mov	%r11, %r8
sumarm_414039:
	ld	[%r9+8], %r16
	st	%r16, [%sp+112]
	ld	[%r9], %r10
	ld	[%r9+4], %r16
	st	%r16, [%sp+108]
	! making direct call 
	ld	[%sp+96], %r8
	call	SplaySetFn_filt_code_410713
	ld	[%sp+124], %r9
code_418520:
	mov	%r8, %r11
code_418508:
	! done making normal call
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+124], %r9
	call	SplaySetFn_filt_code_410713
	ld	[%sp+108], %r10
code_418517:
	st	%r8, [%sp+108]
code_418509:
	! done making normal call
	! making closure call
	ld	[%sp+116], %r17
	ld	[%r17], %r11
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_418518:
code_418510:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_414095
	nop
zero_case_414094:
	ba	after_zeroone_414096
	ld	[%sp+108], %r8
one_case_414095:
	! making closure call
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+108], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418513:
	! done making tail call
after_zeroone_414096:
	ba	after_sum_414035 ! delay slot empty
	nop
sumarm_414043:
after_sum_414035:
code_418516:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_filt_code_410713,(.-SplaySetFn_filt_code_410713)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418517
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x17040000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418518
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x13440000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_418520
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x57d50000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410728
 ! arguments : [$410730,$8] [$410731,$9] [$395552,$10] 
 ! results    : [$413965,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410728:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418541
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418541:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418522:
funtop_413934:
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_407689_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+116]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	ld	[%r9], %r16
	st	%r16, [%sp+120]
	ld	[%r9+4], %r16
	st	%r16, [%sp+124]
	ld	[%r9+8], %r16
	st	%r16, [%sp+128]
	ld	[%r9+12], %r16
	st	%r16, [%sp+112]
sumarm_413961:
	cmp	%r10, 0
	bne	sumarm_413962
	nop
code_418523:
	ba	after_sum_413958
	ld	[%sp+124], %r8
sumarm_413962:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418540:
	mov	%r8, %r10
code_418526:
	! done making normal call
	! making closure call
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r11
code_418538:
code_418527:
	! done making normal call
	ld	[%r8], %r16
	st	%r16, [%sp+112]
	ld	[%r8+4], %r10
	ld	[%sp+112], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_413994
	nop
zero_case_413993:
	ba	after_zeroone_413995
	ld	[%sp+124], %r8
one_case_413994:
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+116], %r8
code_418539:
code_418531:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418532
	nop
code_418533:
	call	GCFromML ! delay slot empty
	nop
needgc_418532:
	! allocating 2-record
	ld	[%sp+128], %r17
	st	%r17, [%r4]
	st	%r8, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_413995:
	ba	after_sum_413958 ! delay slot empty
	nop
sumarm_413966:
after_sum_413958:
code_418537:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_anonfun_code_410728,(.-SplaySetFn_anonfun_code_410728)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418538
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc4440000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long needgc_418532
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418539
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418540
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5540000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_filter_code_410707
 ! arguments : [$410709,$8] [$410710,$9] [$395550,$10] 
 ! results    : [$413929,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_filter_code_410707:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418558
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418558:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_418542:
funtop_413844:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r16
	st	%r16, [%sp+120]
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+124]
	! Proj_c at label type_407689_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+128]
	! Proj_c at label type_400233_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+116]
	! Proj_c at label type_400230_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+96]
	! Proj_c at label type_400221_INT
	ld	[%r8+20], %r16
	st	%r16, [%sp+108]
	ld	[%r9], %r16
	st	%r16, [%sp+112]
	ld	[%r9+4], %r16
	st	%r16, [%sp+104]
	ld	[%r9+8], %r16
	st	%r16, [%sp+100]
	ld	[%r9+12], %r16
	st	%r16, [%sp+132]
	sethi	%hi(type_400275), %r8
	or	%r8, %lo(type_400275), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+108], %r9
code_418557:
	mov	%r8, %r9
code_418547:
	! done making normal call
	add	%r4, 100, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418548
	nop
code_418549:
	call	GCFromML ! delay slot empty
	nop
needgc_418548:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_filt_code_410713), %r8
	or	%r8, %lo(SplaySetFn_filt_code_410713), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+8]
	ld	[%sp+116], %r17
	st	%r17, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	or	%r0, 2337, %r9
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418552
	nop
code_418553:
	or	%r0, 0, %r8
cmpui_418552:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 4-record
	st	%r9, [%r4]
	st	%r11, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+132], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410728), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410728), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418556:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_filter_code_410707,(.-SplaySetFn_filter_code_410707)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418548
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55710000
	.word 0x00000005
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418557
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55710000
	.word 0x00000005
		! worddata
	.word 0x00000000
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_ex_code_410765
 ! arguments : [$410767,$8] [$410768,$9] [$395625,$10] 
 ! results    : [$413775,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_ex_code_410765:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418574
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418574:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+116]
	mov	%r10, %r9
code_418559:
funtop_413749:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
sumarm_413770:
	cmp	%r9, 0
	bne	sumarm_413771
	nop
code_418560:
	ba	after_sum_413767
	or	%r0, 0, %r8
sumarm_413771:
	ld	[%r9+8], %r10
	ld	[%r9], %r16
	st	%r16, [%sp+108]
	ld	[%r9+4], %r16
	st	%r16, [%sp+112]
	! making closure call
	ld	[%sp+116], %r17
	ld	[%r17], %r11
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_418572:
code_418562:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_413818
	nop
zero_case_413817:
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+116], %r9
	call	SplaySetFn_ex_code_410765
	ld	[%sp+108], %r10
code_418573:
code_418564:
	! done making normal call
sumarm_413829:
	cmp	%r8, 0
	bne	sumarm_413830
	nop
code_418565:
	! making direct call 
	ba	funtop_413749
	ld	[%sp+112], %r9
code_418566:
	! done making self tail call
	ba	after_sum_413826
	or	%r0, 0, %r8
sumarm_413830:
nomatch_sum_413827:
	or	%r0, 1, %r8
after_sum_413826:
	ba	after_zeroone_413819 ! delay slot empty
	nop
one_case_413818:
	or	%r0, 1, %r8
after_zeroone_413819:
	ba	after_sum_413767 ! delay slot empty
	nop
sumarm_413776:
after_sum_413767:
code_418571:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_ex_code_410765,(.-SplaySetFn_ex_code_410765)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418572
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x07d10000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418573
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x07110000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410778
 ! arguments : [$410780,$8] [$410781,$9] [$395619,$10] 
 ! results    : [$413729,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410778:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418586
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418586:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+104]
code_418575:
funtop_413711:
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
sumarm_413724:
	cmp	%r10, 0
	bne	sumarm_413725
	nop
code_418576:
	ba	after_sum_413721
	or	%r0, 0, %r8
sumarm_413725:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+100], %r8
code_418585:
	mov	%r8, %r10
code_418579:
	! done making normal call
	! making closure call
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_418580:
	! done making tail call
	ba	after_sum_413721 ! delay slot empty
	nop
sumarm_413730:
after_sum_413721:
code_418583:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410778,(.-SplaySetFn_anonfun_code_410778)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418585
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
	.text
	.align 8
	.global SplaySetFn_exists_code_410759
 ! arguments : [$410761,$8] [$410762,$9] [$395617,$10] 
 ! results    : [$413706,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_exists_code_410759:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418601
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418601:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_418587:
funtop_413652:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400233_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400230_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_400221_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+96]
	sethi	%hi(type_400275), %r8
	or	%r8, %lo(type_400275), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+96], %r9
code_418600:
	mov	%r8, %r10
code_418592:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418593
	nop
code_418594:
	call	GCFromML ! delay slot empty
	nop
needgc_418593:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_ex_code_410765), %r8
	or	%r8, %lo(SplaySetFn_ex_code_410765), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410778), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410778), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418599:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_exists_code_410759,(.-SplaySetFn_exists_code_410759)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418593
	.word 0x00200007
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long code_418600
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
	.text
	.align 8
	.global SplaySetFn_ex_code_410800
 ! arguments : [$410802,$8] [$410803,$9] [$395660,$10] 
 ! results    : [$413564,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_ex_code_410800:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418628
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418628:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+128]
code_418602:
funtop_413529:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_402297_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+124]
	! Proj_c at label type_402264_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+104]
	ld	[%sp+128], %r17
	ld	[%r17], %r8
	ld	[%sp+128], %r17
	ld	[%r17+4], %r9
sumarm_413560:
	cmp	%r10, 0
	bne	sumarm_413561
	nop
code_418603:
	ba	after_sum_413557 ! delay slot empty
	nop
sumarm_413561:
	ld	[%r10+8], %r16
	st	%r16, [%sp+116]
	ld	[%r10], %r16
	st	%r16, [%sp+112]
	ld	[%r10+4], %r16
	st	%r16, [%sp+120]
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_418626:
code_418605:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418606
	nop
code_418607:
	call	GCFromML ! delay slot empty
	nop
needgc_418606:
	cmp	%r8, 0
	bne,pn	%icc,one_case_413607
	nop
zero_case_413606:
	! making direct call 
	ld	[%sp+96], %r8
	ld	[%sp+128], %r9
	call	SplaySetFn_ex_code_410800
	ld	[%sp+112], %r10
code_418627:
code_418610:
	! done making normal call
sumarm_413622:
	cmp	%r8, 0
	bne	sumarm_413623
	nop
code_418611:
	! making direct call 
	ba	funtop_413529
	ld	[%sp+120], %r10
code_418612:
	! done making self tail call
	ba	after_sum_413619
	or	%r0, 0, %r8
sumarm_413623:
nomatch_sum_413620:
after_sum_413619:
	ba	after_zeroone_413608 ! delay slot empty
	nop
one_case_413607:
	ld	[%sp+124], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	bleu,pn	%icc,dynamic_box_413645
	nop
code_418615:
	cmp	%r8, 255
	bleu,pn	%icc,dynamic_nobox_413646
	nop
code_418616:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_413645
	nop
code_418617:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_413645
	nop
code_418618:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_413645
	nop
dynamic_nobox_413646:
	ba	xinject_sum_dyn_after_413642
	ld	[%sp+116], %r8
dynamic_box_413645:
	or	%r0, 9, %r9
	ld	[%sp+104], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418621
	nop
code_418622:
	or	%r0, 0, %r8
cmpui_418621:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1-record
	st	%r9, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
xinject_sum_dyn_after_413642:
after_zeroone_413608:
	ba	after_sum_413557 ! delay slot empty
	nop
sumarm_413565:
after_sum_413557:
code_418625:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_ex_code_410800,(.-SplaySetFn_ex_code_410800)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418606
	.word 0x00240010
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x7f550000
	.word 0x00000001
		! worddata
	.word 0x00000000
	.word 0x0000006c
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x0000006c
	.word 0x80000000
	.long type_400275
		! -------- label,sizes,reg
	.long code_418626
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f550000
	.word 0x00000001
		! worddata
	.word 0x00000000
	.word 0x0000006c
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418627
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x30450000
	.word 0x00000001
		! worddata
	.word 0x00000000
	.word 0x0000006c
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410819
 ! arguments : [$410821,$8] [$410822,$9] [$395654,$10] 
 ! results    : [$413509,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410819:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418640
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418640:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418629:
funtop_413485:
	! Proj_c at label type_402264_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	ld	[%r9], %r8
	ld	[%r9+4], %r16
	st	%r16, [%sp+108]
sumarm_413505:
	cmp	%r10, 0
	bne	sumarm_413506
	nop
code_418630:
	ba	after_sum_413502 ! delay slot empty
	nop
sumarm_413506:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418639:
	mov	%r8, %r10
code_418633:
	! done making normal call
	! making closure call
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_418634:
	! done making tail call
	ba	after_sum_413502 ! delay slot empty
	nop
sumarm_413510:
after_sum_413502:
code_418637:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410819,(.-SplaySetFn_anonfun_code_410819)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418639
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
	.align 8
	.global SplaySetFn_find_code_410794
 ! arguments : [$410796,$8] [$410797,$9] [$395652,$10] 
 ! results    : [$413480,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_find_code_410794:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418659
	nop
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_418659:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+112]
	mov	%r10, %r12
code_418641:
funtop_413399:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+116]
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+120]
	! Proj_c at label type_402297_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+124]
	! Proj_c at label type_402264_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+20], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+24], %r16
	st	%r16, [%sp+128]
	sethi	%hi(type_400275), %r8
	or	%r8, %lo(type_400275), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+128], %r9
code_418658:
	mov	%r8, %r11
code_418646:
	! done making normal call
	add	%r4, 96, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418647
	nop
code_418648:
	call	GCFromML ! delay slot empty
	nop
needgc_418647:
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+8]
	ld	[%sp+124], %r17
	st	%r17, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	ld	[%sp+128], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	or	%r0, 529, %r9
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418650
	nop
code_418651:
	or	%r0, 0, %r8
cmpui_418650:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_ex_code_410800), %r8
	or	%r8, %lo(SplaySetFn_ex_code_410800), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 529, %r9
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418653
	nop
code_418654:
	or	%r0, 0, %r8
cmpui_418653:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410819), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410819), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418657:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size SplaySetFn_find_code_410794,(.-SplaySetFn_find_code_410794)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418647
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x57550000
	.word 0x00000001
		! worddata
	.word 0x00000004
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_418658
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x57550000
	.word 0x00000001
		! worddata
	.word 0x00000004
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_apply_code_410848
 ! arguments : [$410850,$8] [$410851,$9] [$395422,$10] 
 ! results    : [$413349,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_apply_code_410848:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418671
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418671:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+116]
	mov	%r10, %r9
code_418660:
funtop_413323:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
sumarm_413344:
	cmp	%r9, 0
	bne	sumarm_413345
	nop
code_418661:
	ba	after_sum_413341
	or	%r0, 256, %r8
sumarm_413345:
	ld	[%r9+8], %r16
	st	%r16, [%sp+112]
	ld	[%r9], %r10
	ld	[%r9+4], %r16
	st	%r16, [%sp+108]
	! making direct call 
	ld	[%sp+96], %r8
	call	SplaySetFn_apply_code_410848
	ld	[%sp+116], %r9
code_418670:
code_418663:
	! done making normal call
	! making closure call
	ld	[%sp+116], %r17
	ld	[%r17], %r11
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_418669:
code_418664:
	! done making normal call
	! making direct call 
	ba	funtop_413323
	ld	[%sp+108], %r9
code_418665:
	! done making self tail call
	ba	after_sum_413341
	or	%r0, 0, %r8
sumarm_413350:
after_sum_413341:
code_418668:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_apply_code_410848,(.-SplaySetFn_apply_code_410848)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418669
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04d10000
		! worddata
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_418670
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x07d50000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410861
 ! arguments : [$410863,$8] [$410864,$9] [$395416,$10] 
 ! results    : [$413303,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410861:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418683
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418683:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+104]
code_418672:
funtop_413285:
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
sumarm_413298:
	cmp	%r10, 0
	bne	sumarm_413299
	nop
code_418673:
	ba	after_sum_413295
	or	%r0, 256, %r8
sumarm_413299:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+100], %r8
code_418682:
	mov	%r8, %r10
code_418676:
	! done making normal call
	! making closure call
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_418677:
	! done making tail call
	ba	after_sum_413295 ! delay slot empty
	nop
sumarm_413304:
after_sum_413295:
code_418680:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410861,(.-SplaySetFn_anonfun_code_410861)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418682
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
	.text
	.align 8
	.global SplaySetFn_app_inner_code_410842
 ! arguments : [$410844,$8] [$410845,$9] [$395414,$10] 
 ! results    : [$413280,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_app_inner_code_410842:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418697
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418697:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_418684:
funtop_413228:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400233_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_400230_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_400221_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+96]
	sethi	%hi(record_411683), %r8
	or	%r8, %lo(record_411683), %r10
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+96], %r9
code_418696:
	mov	%r8, %r10
code_418688:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418689
	nop
code_418690:
	call	GCFromML ! delay slot empty
	nop
needgc_418689:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_apply_code_410848), %r8
	or	%r8, %lo(SplaySetFn_apply_code_410848), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410861), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410861), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418695:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_app_inner_code_410842,(.-SplaySetFn_app_inner_code_410842)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418689
	.word 0x00200007
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long code_418696
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
	.text
	.align 8
	.global SplaySetFn_apply_code_410887
 ! arguments : [$410889,$8] [$410890,$9] [$407277,$10] [$407278,$11] 
 ! results    : [$413175,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_apply_code_410887:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418709
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418709:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
	mov	%r10, %r9
code_418698:
funtop_413147:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_404496_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+104]
sumarm_413171:
	cmp	%r9, 0
	bne	sumarm_413172
	nop
code_418699:
	ba	after_sum_413168
	mov	%r11, %r8
sumarm_413172:
	ld	[%r9+8], %r16
	st	%r16, [%sp+116]
	ld	[%r9], %r10
	ld	[%r9+4], %r16
	st	%r16, [%sp+112]
	! making direct call 
	ld	[%sp+96], %r8
	call	SplaySetFn_apply_code_410887
	ld	[%sp+120], %r9
code_418708:
	mov	%r8, %r11
code_418701:
	! done making normal call
	! making closure call
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_418707:
code_418702:
	! done making normal call
	! making direct call 
	ld	[%sp+112], %r9
	ba	funtop_413147
	mov	%r8, %r11
code_418703:
	! done making self tail call
	ba	after_sum_413168
	or	%r0, 0, %r8
sumarm_413176:
after_sum_413168:
code_418706:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_apply_code_410887,(.-SplaySetFn_apply_code_410887)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418707
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x13450000
		! worddata
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418708
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x1f550000
		! worddata
	.word 0x00000000
	.word 0x0000006c
	.word 0x00000000
	.word 0x00000068
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410908
 ! arguments : [$410910,$8] [$410911,$9] [$395506,$10] 
 ! results    : [$413126,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410908:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418721
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418721:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418710:
funtop_413102:
	! Proj_c at label type_404496_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	ld	[%r9], %r16
	st	%r16, [%sp+112]
	ld	[%r9+4], %r16
	st	%r16, [%sp+108]
sumarm_413122:
	cmp	%r10, 0
	bne	sumarm_413123
	nop
code_418711:
	ba	after_sum_413119
	ld	[%sp+112], %r8
sumarm_413123:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418720:
	mov	%r8, %r10
code_418714:
	! done making normal call
	! making closure call
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418715:
	! done making tail call
	ba	after_sum_413119 ! delay slot empty
	nop
sumarm_413127:
after_sum_413119:
code_418718:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_anonfun_code_410908,(.-SplaySetFn_anonfun_code_410908)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418720
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03540000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410902
 ! arguments : [$410904,$8] [$410905,$9] [$395504,$10] 
 ! results    : [$413097,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410902:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418731
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418731:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r12
	mov	%r10, %r13
code_418722:
funtop_413067:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418723
	nop
code_418724:
	call	GCFromML ! delay slot empty
	nop
needgc_418723:
	! Proj_c at label type_404496_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r10
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 529, %r9
	cmp	%r11, 3
	or	%r0, 1, %r8
	bgu	cmpui_418726
	nop
code_418727:
	or	%r0, 0, %r8
cmpui_418726:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r13, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410908), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410908), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418730:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410902,(.-SplaySetFn_anonfun_code_410902)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418723
	.word 0x001c0009
	.word 0x00170000
	.word 0xbffc1000
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_foldl_inner_code_410882
 ! arguments : [$410884,$8] [$410885,$9] [$395502,$10] 
 ! results    : [$413066,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_foldl_inner_code_410882:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418744
	nop
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_418744:
	st	%r15, [%sp+92]
	mov	%r10, %r20
code_418732:
funtop_413004:
	add	%r4, 68, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418733
	nop
code_418734:
	call	GCFromML ! delay slot empty
	nop
needgc_418733:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r19
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r12
	! Proj_c at label type_407274_INT
	ld	[%r8+8], %r10
	! Proj_c at label type_404496_INT
	ld	[%r8+12], %r9
	! Proj_c at label type_400233_INT
	ld	[%r8+16], %r18
	! Proj_c at label type_400230_INT
	ld	[%r8+20], %r13
	! Proj_c at label type_400221_INT
	ld	[%r8+24], %r11
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r19, [%r4+4]
	st	%r12, [%r4+8]
	st	%r9, [%r4+12]
	st	%r11, [%r4+16]
	add	%r4, 4, %r11
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_apply_code_410887), %r8
	or	%r8, %lo(SplaySetFn_apply_code_410887), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r20, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r18, [%r4+8]
	st	%r13, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410902), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410902), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r11
	ld	[%r2+804], %r8
	add	%r11, %r8, %r8
	ld	[%r8], %r11
	ld	[%r11], %r13
	ld	[%r11+4], %r8
	jmpl	%r13, %r15
	ld	[%r11+8], %r11
code_418743:
code_418740:
	! done making normal call
code_418742:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size SplaySetFn_foldl_inner_code_410882,(.-SplaySetFn_foldl_inner_code_410882)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418733
	.word 0x00180007
	.word 0x00170000
	.word 0x00100100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418743
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_foldl_r_code_410877
 ! arguments : [$410879,$8] [$395726,$9] [$410880,$10] [$395727,$11] 
 ! results    : [$412998,$8] 
 ! destroys   :  $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $18 $13 $12 $11 $10 $9 $8
SplaySetFn_foldl_r_code_410877:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418753
	nop
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_418753:
	st	%r15, [%sp+92]
	mov	%r9, %r18
code_418745:
funtop_412963:
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418746
	nop
code_418747:
	call	GCFromML ! delay slot empty
	nop
needgc_418746:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r13
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r12
	! Proj_c at label type_400233_INT
	ld	[%r8+8], %r11
	! Proj_c at label type_400230_INT
	ld	[%r8+12], %r10
	! Proj_c at label type_400221_INT
	ld	[%r8+16], %r9
	! allocating 1-record
	! done allocating 1 record
	! allocating 1 closures
	! allocating 7-record
	sethi	%hi(32569), %r8
	or	%r8, %lo(32569), %r8
	st	%r8, [%r4]
	st	%r13, [%r4+4]
	st	%r12, [%r4+8]
	sethi	%hi(record_412983), %r8
	or	%r8, %lo(record_412983), %r8
	st	%r8, [%r4+12]
	st	%r18, [%r4+16]
	st	%r11, [%r4+20]
	st	%r10, [%r4+24]
	st	%r9, [%r4+28]
	add	%r4, 4, %r9
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_foldl_inner_code_410882), %r8
	or	%r8, %lo(SplaySetFn_foldl_inner_code_410882), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418752:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size SplaySetFn_foldl_r_code_410877,(.-SplaySetFn_foldl_r_code_410877)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418746
	.word 0x00180007
	.word 0x00170000
	.word 0xbffc0100
	.word 0xbff80000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_apply_code_410950
 ! arguments : [$410952,$8] [$410953,$9] [$407227,$10] [$407228,$11] 
 ! results    : [$412910,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_apply_code_410950:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418765
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418765:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
code_418754:
funtop_412882:
	! Proj_c at label reify_407796_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label reify_407771_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_404515_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400221_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+104]
sumarm_412906:
	cmp	%r10, 0
	bne	sumarm_412907
	nop
code_418755:
	ba	after_sum_412903
	mov	%r11, %r8
sumarm_412907:
	ld	[%r10+8], %r16
	st	%r16, [%sp+112]
	ld	[%r10], %r16
	st	%r16, [%sp+116]
	ld	[%r10+4], %r10
	! making direct call 
	ld	[%sp+96], %r8
	call	SplaySetFn_apply_code_410950
	ld	[%sp+120], %r9
code_418764:
	mov	%r8, %r11
code_418757:
	! done making normal call
	! making closure call
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_418763:
code_418758:
	! done making normal call
	! making direct call 
	ld	[%sp+116], %r10
	ba	funtop_412882
	mov	%r8, %r11
code_418759:
	! done making self tail call
	ba	after_sum_412903
	or	%r0, 0, %r8
sumarm_412911:
after_sum_412903:
code_418762:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_apply_code_410950,(.-SplaySetFn_apply_code_410950)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418763
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x1c450000
		! worddata
	.word 0x00000000
	.word 0x0000006c
		! -------- label,sizes,reg
	.long code_418764
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x1f550000
		! worddata
	.word 0x00000000
	.word 0x00000068
	.word 0x00000000
	.word 0x0000006c
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410971
 ! arguments : [$410973,$8] [$410974,$9] [$395455,$10] 
 ! results    : [$412861,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410971:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418777
	nop
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_418777:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_418766:
funtop_412837:
	! Proj_c at label type_404515_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	ld	[%r9], %r16
	st	%r16, [%sp+112]
	ld	[%r9+4], %r16
	st	%r16, [%sp+108]
sumarm_412857:
	cmp	%r10, 0
	bne	sumarm_412858
	nop
code_418767:
	ba	after_sum_412854
	ld	[%sp+112], %r8
sumarm_412858:
	ld	[%r10], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	jmpl	%r11, %r15
	ld	[%sp+104], %r8
code_418776:
	mov	%r8, %r10
code_418770:
	! done making normal call
	! making closure call
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_418771:
	! done making tail call
	ba	after_sum_412854 ! delay slot empty
	nop
sumarm_412862:
after_sum_412854:
code_418774:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size SplaySetFn_anonfun_code_410971,(.-SplaySetFn_anonfun_code_410971)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_418776
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03540000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global SplaySetFn_anonfun_code_410965
 ! arguments : [$410967,$8] [$410968,$9] [$395453,$10] 
 ! results    : [$412832,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
SplaySetFn_anonfun_code_410965:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418787
	nop
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_418787:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r12
	mov	%r10, %r13
code_418778:
funtop_412802:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418779
	nop
code_418780:
	call	GCFromML ! delay slot empty
	nop
needgc_418779:
	! Proj_c at label type_404515_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	! Proj_c at label type_400233_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r10
	! Proj_c at label type_400230_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 529, %r9
	cmp	%r11, 3
	or	%r0, 1, %r8
	bgu	cmpui_418782
	nop
code_418783:
	or	%r0, 0, %r8
cmpui_418782:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r13, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410971), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410971), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418786:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size SplaySetFn_anonfun_code_410965,(.-SplaySetFn_anonfun_code_410965)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418779
	.word 0x001c0009
	.word 0x00170000
	.word 0xbffc1000
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global SplaySetFn_foldr_inner_code_410945
 ! arguments : [$410947,$8] [$410948,$9] [$395451,$10] 
 ! results    : [$412801,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_foldr_inner_code_410945:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418800
	nop
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_418800:
	st	%r15, [%sp+92]
	mov	%r10, %r20
code_418788:
funtop_412739:
	add	%r4, 68, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418789
	nop
code_418790:
	call	GCFromML ! delay slot empty
	nop
needgc_418789:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r19
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r12
	! Proj_c at label type_407224_INT
	ld	[%r8+8], %r10
	! Proj_c at label type_404515_INT
	ld	[%r8+12], %r9
	! Proj_c at label type_400233_INT
	ld	[%r8+16], %r18
	! Proj_c at label type_400230_INT
	ld	[%r8+20], %r13
	! Proj_c at label type_400221_INT
	ld	[%r8+24], %r11
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r19, [%r4+4]
	st	%r12, [%r4+8]
	st	%r9, [%r4+12]
	st	%r11, [%r4+16]
	add	%r4, 4, %r11
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_apply_code_410950), %r8
	or	%r8, %lo(SplaySetFn_apply_code_410950), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r20, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r18, [%r4+8]
	st	%r13, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_anonfun_code_410965), %r8
	or	%r8, %lo(SplaySetFn_anonfun_code_410965), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r11
	ld	[%r2+804], %r8
	add	%r11, %r8, %r8
	ld	[%r8], %r11
	ld	[%r11], %r13
	ld	[%r11+4], %r8
	jmpl	%r13, %r15
	ld	[%r11+8], %r11
code_418799:
code_418796:
	! done making normal call
code_418798:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size SplaySetFn_foldr_inner_code_410945,(.-SplaySetFn_foldr_inner_code_410945)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418789
	.word 0x00180007
	.word 0x00170000
	.word 0x00100100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_418799
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_foldr_r_code_410940
 ! arguments : [$410942,$8] [$395743,$9] [$410943,$10] [$395744,$11] 
 ! results    : [$412733,$8] 
 ! destroys   :  $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $18 $13 $12 $11 $10 $9 $8
SplaySetFn_foldr_r_code_410940:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_418809
	nop
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_418809:
	st	%r15, [%sp+92]
	mov	%r9, %r18
code_418801:
funtop_412698:
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418802
	nop
code_418803:
	call	GCFromML ! delay slot empty
	nop
needgc_418802:
	! Proj_c at label reify_407796_INT
	ld	[%r8], %r13
	! Proj_c at label reify_407771_INT
	ld	[%r8+4], %r12
	! Proj_c at label type_400233_INT
	ld	[%r8+8], %r11
	! Proj_c at label type_400230_INT
	ld	[%r8+12], %r10
	! Proj_c at label type_400221_INT
	ld	[%r8+16], %r9
	! allocating 1-record
	! done allocating 1 record
	! allocating 1 closures
	! allocating 7-record
	sethi	%hi(32569), %r8
	or	%r8, %lo(32569), %r8
	st	%r8, [%r4]
	st	%r13, [%r4+4]
	st	%r12, [%r4+8]
	sethi	%hi(record_412718), %r8
	or	%r8, %lo(record_412718), %r8
	st	%r8, [%r4+12]
	st	%r18, [%r4+16]
	st	%r11, [%r4+20]
	st	%r10, [%r4+24]
	st	%r9, [%r4+28]
	add	%r4, 4, %r9
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_foldr_inner_code_410945), %r8
	or	%r8, %lo(SplaySetFn_foldr_inner_code_410945), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_418808:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size SplaySetFn_foldr_r_code_410940,(.-SplaySetFn_foldr_r_code_410940)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_418802
	.word 0x00180007
	.word 0x00170000
	.word 0xbffc0100
	.word 0xbff80000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global SplaySetFn_functor_var_r_code_410153
 ! arguments : [$410155,$8] [$400214,$9] [$410156,$10] [$393938,$11] 
 ! results    : [$412694,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_functor_var_r_code_410153:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 208, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_419039
	nop
	add	%sp, 208, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 208, %sp
code_419039:
	st	%r15, [%sp+92]
	st	%r9, [%sp+200]
	st	%r11, [%sp+96]
code_418810:
funtop_411691:
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_400232), %r8
	or	%r8, %lo(type_400232), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419033:
	st	%r8, [%sp+196]
code_418813:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418814
	nop
code_418815:
	call	GCFromML ! delay slot empty
	nop
needgc_418814:
	! done making constructor call
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+196], %r17
	st	%r17, [%r4+4]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_vars_eq_0_code_410158), %r8
	or	%r8, %lo(SplaySetFn_vars_eq_0_code_410158), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+192]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+188]
	! start making constructor call
	sethi	%hi(type_407688), %r8
	or	%r8, %lo(type_407688), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419034:
	st	%r8, [%sp+184]
code_418821:
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418822
	nop
code_418823:
	call	GCFromML ! delay slot empty
	nop
needgc_418822:
	! done making constructor call
	or	%r0, 25, %r16
	st	%r16, [%sp+180]
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418825
	nop
code_418826:
	or	%r0, 0, %r8
cmpui_418825:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	ld	[%sp+180], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+180]
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418827
	nop
code_418828:
	or	%r0, 0, %r8
cmpui_418827:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	ld	[%sp+180], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+180]
	ld	[%sp+200], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418829
	nop
code_418830:
	or	%r0, 0, %r8
cmpui_418829:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	ld	[%sp+180], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+180]
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+184], %r17
	st	%r17, [%r4+4]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+8]
	ld	[%sp+200], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 25, %r9
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418832
	nop
code_418833:
	or	%r0, 0, %r8
cmpui_418832:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 3-record
	st	%r9, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	ld	[%sp+180], %r17
	st	%r17, [%r4+8]
	or	%r0, 273, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_singleton_code_410167), %r8
	or	%r8, %lo(SplaySetFn_singleton_code_410167), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+108]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r10
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+200], %r9
	jmpl	%r13, %r15
	ld	[%sp+108], %r12
code_419020:
	st	%r8, [%sp+176]
code_418838:
	! done making normal call
	! start making constructor call
	sethi	%hi(type_400387), %r8
	or	%r8, %lo(type_400387), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419025:
	st	%r8, [%sp+172]
code_418841:
	! done making constructor call
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(openvar_r_splay_400466), %r8
	or	%r8, %lo(openvar_r_splay_400466), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+200], %r9
code_419026:
	st	%r8, [%sp+168]
code_418844:
	! done making normal call
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418845
	nop
code_418846:
	call	GCFromML ! delay slot empty
	nop
needgc_418845:
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	ld	[%sp+184], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r16
	st	%r16, [%sp+104]
	add	%r4, 20, %r4
	! done allocating 4 record
	! start making constructor call
	sethi	%hi(type_400387), %r8
	or	%r8, %lo(type_400387), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419035:
	st	%r8, [%sp+164]
code_418850:
	add	%r4, 120, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418851
	nop
code_418852:
	call	GCFromML ! delay slot empty
	nop
needgc_418851:
	! done making constructor call
	or	%r0, 17, %r16
	st	%r16, [%sp+96]
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418854
	nop
code_418855:
	or	%r0, 0, %r8
cmpui_418854:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	ld	[%sp+96], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+96]
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+184], %r17
	st	%r17, [%r4+12]
	ld	[%sp+200], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	or	%r0, 2345, %r10
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r9
	bgu	cmpui_418856
	nop
code_418857:
	or	%r0, 0, %r9
cmpui_418856:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r10, %r10
	! allocating 5-record
	st	%r10, [%r4]
	ld	[%sp+188], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r9
	st	%r9, [%r4+8]
	ld	[%sp+180], %r17
	st	%r17, [%r4+12]
	ld	[%sp+168], %r17
	st	%r17, [%r4+16]
	ld	[%sp+96], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(SplaySetFn_insert_code_410184), %r9
	or	%r9, %lo(SplaySetFn_insert_code_410184), %r9
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+160]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 17, %r16
	st	%r16, [%sp+100]
	ld	[%sp+196], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418859
	nop
code_418860:
	or	%r0, 0, %r8
cmpui_418859:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	ld	[%sp+100], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+100]
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+4]
	ld	[%sp+184], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	ld	[%sp+200], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 4-record
	or	%r0, 2337, %r8
	st	%r8, [%r4]
	ld	[%sp+160], %r17
	st	%r17, [%r4+4]
	or	%r0, 273, %r8
	st	%r8, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(SplaySetFn_add_code_410217), %r10
	or	%r10, %lo(SplaySetFn_add_code_410217), %r10
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	st	%r8, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+156]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! start making constructor call
	sethi	%hi(type_401683), %r8
	or	%r8, %lo(type_401683), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419021:
	st	%r8, [%sp+152]
code_418865:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418866
	nop
code_418867:
	call	GCFromML ! delay slot empty
	nop
needgc_418866:
	! done making constructor call
	! allocating 2-record
	sethi	%hi(record_411894+-4), %r8
	ld	[%sp+96], %r17
	st	%r17, [%r8+%lo(record_411894+-4)]
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+200], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(foldl_400905), %r8
	or	%r8, %lo(foldl_400905), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_419036:
	st	%r8, [%sp+96]
code_418872:
	! done making normal call
	! start making constructor call
	sethi	%hi(type_405802), %r8
	or	%r8, %lo(type_405802), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419027:
code_418875:
	add	%r4, 68, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418876
	nop
code_418877:
	call	GCFromML ! delay slot empty
	nop
needgc_418876:
	! done making constructor call
	! allocating 1 closures
	! allocating 6-record
	sethi	%hi(16177), %r9
	or	%r9, %lo(16177), %r9
	st	%r9, [%r4]
	ld	[%sp+152], %r17
	st	%r17, [%r4+4]
	ld	[%sp+184], %r17
	st	%r17, [%r4+8]
	st	%r8, [%r4+12]
	ld	[%sp+104], %r17
	st	%r17, [%r4+16]
	ld	[%sp+196], %r17
	st	%r17, [%r4+20]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+24]
	add	%r4, 4, %r8
	add	%r4, 28, %r4
	! done allocating 6 record
	! allocating 5-record
	sethi	%hi(6441), %r9
	or	%r9, %lo(6441), %r9
	st	%r9, [%r4]
	ld	[%sp+160], %r17
	st	%r17, [%r4+4]
	or	%r0, 273, %r9
	st	%r9, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411894), %r9
	or	%r9, %lo(record_411894), %r9
	st	%r9, [%r4+16]
	ld	[%sp+96], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(SplaySetFn_addList_code_410240), %r9
	or	%r9, %lo(SplaySetFn_addList_code_410240), %r9
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+148]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(openvar_r_join_401024), %r8
	or	%r8, %lo(openvar_r_join_401024), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+200], %r9
code_419037:
	st	%r8, [%sp+144]
code_418884:
	! done making normal call
	add	%r4, 232, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418885
	nop
code_418886:
	call	GCFromML ! delay slot empty
	nop
needgc_418885:
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+4]
	ld	[%sp+184], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	ld	[%sp+200], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	sethi	%hi(6441), %r8
	or	%r8, %lo(6441), %r8
	sethi	%hi(record_411696), %r10
	or	%r10, %lo(record_411696), %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_418890
	nop
code_418891:
	or	%r0, 0, %r10
cmpui_418890:
	sll	%r10, 9, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 5-record
	st	%r8, [%r4]
	ld	[%sp+188], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	or	%r0, 273, %r8
	st	%r8, [%r4+12]
	ld	[%sp+168], %r17
	st	%r17, [%r4+16]
	ld	[%sp+144], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(SplaySetFn_delete_code_410267), %r10
	or	%r10, %lo(SplaySetFn_delete_code_410267), %r10
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	st	%r8, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+140]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+4]
	ld	[%sp+196], %r17
	st	%r17, [%r4+8]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+12]
	ld	[%sp+200], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+188], %r17
	st	%r17, [%r4+4]
	ld	[%sp+168], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(SplaySetFn_member_code_410302), %r10
	or	%r10, %lo(SplaySetFn_member_code_410302), %r10
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	st	%r8, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+136]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_412017), %r8
	or	%r8, %lo(record_412017), %r16
	st	%r16, [%sp+132]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+200], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_member_code_410336), %r8
	or	%r8, %lo(SplaySetFn_member_code_410336), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+188], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+200], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_treeIn_code_410362), %r8
	or	%r8, %lo(SplaySetFn_treeIn_code_410362), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+196], %r17
	st	%r17, [%r4+4]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_equal_code_410387), %r8
	or	%r8, %lo(SplaySetFn_equal_code_410387), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+128]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+196], %r17
	st	%r17, [%r4+4]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_isSubset_code_410398), %r8
	or	%r8, %lo(SplaySetFn_isSubset_code_410398), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+124]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! start making constructor call
	sethi	%hi(type_401683), %r8
	or	%r8, %lo(type_401683), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! start making constructor call
	sethi	%hi(type_400387), %r8
	or	%r8, %lo(type_400387), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419022:
	mov	%r8, %r9
code_418906:
	! done making constructor call
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_419028:
	st	%r8, [%sp+112]
code_418907:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_401683), %r8
	or	%r8, %lo(type_401683), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+184], %r9
code_419029:
	st	%r8, [%sp+108]
code_418910:
	! done making constructor call
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(left_r_394664), %r8
	or	%r8, %lo(left_r_394664), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+200], %r9
code_419030:
	st	%r8, [%sp+100]
code_418912:
	! done making normal call
	or	%r0, 17, %r16
	st	%r16, [%sp+104]
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418913
	nop
code_418914:
	or	%r0, 0, %r8
cmpui_418913:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	ld	[%sp+104], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+104]
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418915
	nop
code_418916:
	or	%r0, 0, %r8
cmpui_418915:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	ld	[%sp+104], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+104]
	! allocating 2-record
	sethi	%hi(record_412120+-4), %r8
	ld	[%sp+104], %r17
	st	%r17, [%r8+%lo(record_412120+-4)]
	! done allocating 2 record
	! start making constructor call
	sethi	%hi(type_401683), %r8
	or	%r8, %lo(type_401683), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! start making constructor call
	sethi	%hi(type_400387), %r8
	or	%r8, %lo(type_400387), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419023:
	mov	%r8, %r9
code_418922:
	! done making constructor call
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_419031:
	mov	%r8, %r9
code_418923:
	add	%r4, 64, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418924
	nop
code_418925:
	call	GCFromML ! delay slot empty
	nop
needgc_418924:
	! done making constructor call
	! allocating 1 closures
	! allocating 6-record
	sethi	%hi(16177), %r8
	or	%r8, %lo(16177), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	ld	[%sp+164], %r17
	st	%r17, [%r4+12]
	ld	[%sp+184], %r17
	st	%r17, [%r4+16]
	ld	[%sp+108], %r17
	st	%r17, [%r4+20]
	ld	[%sp+200], %r17
	st	%r17, [%r4+24]
	add	%r4, 4, %r10
	add	%r4, 28, %r4
	! done allocating 6 record
	! allocating 4-record
	or	%r0, 2849, %r8
	st	%r8, [%r4]
	ld	[%sp+188], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_412120), %r8
	or	%r8, %lo(record_412120), %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_cmp_code_410409), %r8
	or	%r8, %lo(SplaySetFn_cmp_code_410409), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(left_r_394664), %r8
	or	%r8, %lo(left_r_394664), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+200], %r9
code_419038:
	mov	%r8, %r11
code_418930:
	! done making normal call
	add	%r4, 128, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418931
	nop
code_418932:
	call	GCFromML ! delay slot empty
	nop
needgc_418931:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+196], %r17
	st	%r17, [%r4+8]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 1305, %r9
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418935
	nop
code_418936:
	or	%r0, 0, %r8
cmpui_418935:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 3-record
	st	%r9, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_compare_code_410434), %r8
	or	%r8, %lo(SplaySetFn_compare_code_410434), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+120]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_412205), %r8
	or	%r8, %lo(record_412205), %r16
	st	%r16, [%sp+116]
	! done allocating 1 closures
	or	%r0, 17, %r10
	ld	[%sp+200], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418939
	nop
code_418940:
	or	%r0, 0, %r8
cmpui_418939:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	ld	[%sp+152], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418941
	nop
code_418942:
	or	%r0, 0, %r8
cmpui_418941:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+152], %r17
	st	%r17, [%r4+4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+8]
	ld	[%sp+164], %r17
	st	%r17, [%r4+12]
	ld	[%sp+200], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 537, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_apply_inner_code_410458), %r8
	or	%r8, %lo(SplaySetFn_apply_inner_code_410458), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+152], %r17
	st	%r17, [%r4+4]
	ld	[%sp+196], %r17
	st	%r17, [%r4+8]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 273, %r9
	ld	[%sp+152], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418945
	nop
code_418946:
	or	%r0, 0, %r8
cmpui_418945:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r11, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_listItems_code_410473), %r8
	or	%r8, %lo(SplaySetFn_listItems_code_410473), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! start making constructor call
	sethi	%hi(type_402262), %r8
	or	%r8, %lo(type_402262), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419024:
	st	%r8, [%sp+108]
code_418950:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_402295), %r8
	or	%r8, %lo(type_402295), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+200], %r9
code_419032:
	st	%r8, [%sp+104]
code_418953:
	add	%r4, 896, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	bleu	needgc_418954
	nop
code_418955:
	call	GCFromML ! delay slot empty
	nop
needgc_418954:
	! done making constructor call
	or	%r0, 25, %r11
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418957
	nop
code_418958:
	or	%r0, 0, %r8
cmpui_418957:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r11
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418959
	nop
code_418960:
	or	%r0, 0, %r8
cmpui_418959:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r11
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418961
	nop
code_418962:
	or	%r0, 0, %r8
cmpui_418961:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r11
	! allocating 3-record
	sethi	%hi(record_412287+-4), %r8
	st	%r11, [%r8+%lo(record_412287+-4)]
	! done allocating 3 record
	! allocating 1 closures
	! allocating 6-record
	sethi	%hi(16177), %r8
	or	%r8, %lo(16177), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+184], %r17
	st	%r17, [%r4+12]
	ld	[%sp+104], %r17
	st	%r17, [%r4+16]
	ld	[%sp+108], %r17
	st	%r17, [%r4+20]
	ld	[%sp+200], %r17
	st	%r17, [%r4+24]
	add	%r4, 4, %r10
	add	%r4, 28, %r4
	! done allocating 6 record
	sethi	%hi(18745), %r9
	or	%r9, %lo(18745), %r9
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418964
	nop
code_418965:
	or	%r0, 0, %r8
cmpui_418964:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418966
	nop
code_418967:
	or	%r0, 0, %r8
cmpui_418966:
	sll	%r8, 12, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 7-record
	st	%r9, [%r4]
	ld	[%sp+188], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	ld	[%sp+180], %r17
	st	%r17, [%r4+12]
	ld	[%sp+168], %r17
	st	%r17, [%r4+16]
	or	%r0, 0, %r8
	st	%r8, [%r4+20]
	st	%r11, [%r4+24]
	sethi	%hi(record_412287), %r8
	or	%r8, %lo(record_412287), %r8
	st	%r8, [%r4+28]
	add	%r4, 4, %r9
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_split_code_410488), %r8
	or	%r8, %lo(SplaySetFn_split_code_410488), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r18
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 17, %r13
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418970
	nop
code_418971:
	or	%r0, 0, %r8
cmpui_418970:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r13, %r13
	! allocating 2-record
	sethi	%hi(record_412330+-4), %r8
	st	%r13, [%r8+%lo(record_412330+-4)]
	! done allocating 2 record
	or	%r0, 25, %r9
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418973
	nop
code_418974:
	or	%r0, 0, %r8
cmpui_418973:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	ld	[%sp+184], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418975
	nop
code_418976:
	or	%r0, 0, %r8
cmpui_418975:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	ld	[%sp+200], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_418977
	nop
code_418978:
	or	%r0, 0, %r8
cmpui_418977:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1 closures
	! allocating 6-record
	sethi	%hi(16177), %r8
	or	%r8, %lo(16177), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+184], %r17
	st	%r17, [%r4+12]
	ld	[%sp+104], %r17
	st	%r17, [%r4+16]
	ld	[%sp+108], %r17
	st	%r17, [%r4+20]
	ld	[%sp+200], %r17
	st	%r17, [%r4+24]
	add	%r4, 4, %r10
	add	%r4, 28, %r4
	! done allocating 6 record
	! allocating 5-record
	or	%r0, 2857, %r8
	st	%r8, [%r4]
	st	%r18, [%r4+4]
	ld	[%sp+144], %r17
	st	%r17, [%r4+8]
	st	%r13, [%r4+12]
	sethi	%hi(record_412330), %r8
	or	%r8, %lo(record_412330), %r8
	st	%r8, [%r4+16]
	st	%r9, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_inter_code_410529), %r8
	or	%r8, %lo(SplaySetFn_inter_code_410529), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+4]
	ld	[%sp+184], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	or	%r0, 281, %r9
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_418983
	nop
code_418984:
	or	%r0, 0, %r8
cmpui_418983:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 3-record
	st	%r9, [%r4]
	st	%r11, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	or	%r0, 273, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_intersection_code_410556), %r8
	or	%r8, %lo(SplaySetFn_intersection_code_410556), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_cnt_inner_code_410575), %r8
	or	%r8, %lo(SplaySetFn_cnt_inner_code_410575), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r9
	st	%r9, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+184], %r17
	st	%r17, [%r4+12]
	ld	[%sp+200], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r11
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 5-record
	sethi	%hi(5929), %r9
	or	%r9, %lo(5929), %r9
	st	%r9, [%r4]
	st	%r18, [%r4+4]
	st	%r8, [%r4+8]
	ld	[%sp+144], %r17
	st	%r17, [%r4+12]
	st	%r13, [%r4+16]
	sethi	%hi(record_412330), %r9
	or	%r9, %lo(record_412330), %r9
	st	%r9, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(SplaySetFn_diff_code_410584), %r9
	or	%r9, %lo(SplaySetFn_diff_code_410584), %r9
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r9
	st	%r9, [%r4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+4]
	ld	[%sp+184], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r9
	or	%r9, %lo(record_411696), %r9
	st	%r9, [%r4+16]
	add	%r4, 4, %r11
	add	%r4, 20, %r4
	! done allocating 4 record
	or	%r0, 281, %r10
	sethi	%hi(record_411696), %r9
	or	%r9, %lo(record_411696), %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_418991
	nop
code_418992:
	or	%r0, 0, %r9
cmpui_418991:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r10, %r10
	! allocating 3-record
	st	%r10, [%r4]
	st	%r12, [%r4+4]
	or	%r0, 0, %r9
	st	%r9, [%r4+8]
	or	%r0, 273, %r9
	st	%r9, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(SplaySetFn_difference_code_410607), %r9
	or	%r9, %lo(SplaySetFn_difference_code_410607), %r9
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r9
	st	%r9, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+184], %r17
	st	%r17, [%r4+12]
	ld	[%sp+200], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 4-record
	or	%r0, 801, %r9
	st	%r9, [%r4]
	st	%r18, [%r4+4]
	st	%r8, [%r4+8]
	ld	[%sp+180], %r17
	st	%r17, [%r4+12]
	st	%r13, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_uni_code_410626), %r8
	or	%r8, %lo(SplaySetFn_uni_code_410626), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+4]
	ld	[%sp+184], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 273, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_union_code_410647), %r8
	or	%r8, %lo(SplaySetFn_union_code_410647), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r22
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	ld	[%sp+200], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	or	%r0, 273, %r9
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_418999
	nop
code_419000:
	or	%r0, 0, %r8
cmpui_418999:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	ld	[%sp+156], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_map_code_410664), %r8
	or	%r8, %lo(SplaySetFn_map_code_410664), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r21
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 6-record
	sethi	%hi(16177), %r8
	or	%r8, %lo(16177), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+184], %r17
	st	%r17, [%r4+12]
	ld	[%sp+196], %r17
	st	%r17, [%r4+16]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+20]
	ld	[%sp+200], %r17
	st	%r17, [%r4+24]
	add	%r4, 4, %r10
	add	%r4, 28, %r4
	! done allocating 6 record
	or	%r0, 2337, %r9
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_419004
	nop
code_419005:
	or	%r0, 0, %r8
cmpui_419004:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 4-record
	st	%r9, [%r4]
	ld	[%sp+160], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	or	%r0, 273, %r8
	st	%r8, [%r4+12]
	sethi	%hi(record_411894), %r8
	or	%r8, %lo(record_411894), %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_filter_code_410707), %r8
	or	%r8, %lo(SplaySetFn_filter_code_410707), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r20
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	ld	[%sp+200], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_exists_code_410759), %r8
	or	%r8, %lo(SplaySetFn_exists_code_410759), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 7-record
	sethi	%hi(32569), %r8
	or	%r8, %lo(32569), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	ld	[%sp+196], %r17
	st	%r17, [%r4+20]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+24]
	ld	[%sp+200], %r17
	st	%r17, [%r4+28]
	add	%r4, 4, %r9
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_find_code_410794), %r8
	or	%r8, %lo(SplaySetFn_find_code_410794), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 0, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	ld	[%sp+200], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_app_inner_code_410842), %r8
	or	%r8, %lo(SplaySetFn_app_inner_code_410842), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r8
	or	%r8, %lo(record_411696), %r8
	st	%r8, [%r4+16]
	ld	[%sp+200], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r12
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(SplaySetFn_foldl_r_code_410877), %r8
	or	%r8, %lo(SplaySetFn_foldl_r_code_410877), %r8
	st	%r8, [%r4+4]
	st	%r12, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r12
	or	%r12, %lo(7977), %r12
	st	%r12, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+8]
	ld	[%sp+196], %r17
	st	%r17, [%r4+12]
	sethi	%hi(record_411696), %r12
	or	%r12, %lo(record_411696), %r12
	st	%r12, [%r4+16]
	ld	[%sp+200], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r13
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r12
	st	%r12, [%r4]
	sethi	%hi(SplaySetFn_foldr_r_code_410940), %r12
	or	%r12, %lo(SplaySetFn_foldr_r_code_410940), %r12
	st	%r12, [%r4+4]
	st	%r13, [%r4+8]
	or	%r0, 256, %r12
	st	%r12, [%r4+12]
	add	%r4, 4, %r19
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1-record
	or	%r0, 265, %r12
	st	%r12, [%r4]
	ld	[%sp+192], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r13
	add	%r4, 8, %r4
	! done allocating 1 record
	! allocating 1-record
	or	%r0, 265, %r12
	st	%r12, [%r4]
	st	%r13, [%r4+4]
	add	%r4, 4, %r18
	add	%r4, 8, %r4
	! done allocating 1 record
	! allocating 1-record
	or	%r0, 265, %r12
	st	%r12, [%r4]
	ld	[%sp+188], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r13
	add	%r4, 8, %r4
	! done allocating 1 record
	! allocating 2-record
	or	%r0, 785, %r12
	st	%r12, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 23-record
	sethi	%hi(2146369465), %r10
	or	%r10, %lo(2146369465), %r10
	st	%r10, [%r4]
	st	%r18, [%r4+4]
	st	%r13, [%r4+8]
	or	%r0, 0, %r10
	st	%r10, [%r4+12]
	ld	[%sp+176], %r17
	st	%r17, [%r4+16]
	ld	[%sp+156], %r17
	st	%r17, [%r4+20]
	ld	[%sp+148], %r17
	st	%r17, [%r4+24]
	ld	[%sp+140], %r17
	st	%r17, [%r4+28]
	ld	[%sp+136], %r17
	st	%r17, [%r4+32]
	ld	[%sp+132], %r17
	st	%r17, [%r4+36]
	ld	[%sp+128], %r17
	st	%r17, [%r4+40]
	ld	[%sp+120], %r17
	st	%r17, [%r4+44]
	ld	[%sp+124], %r17
	st	%r17, [%r4+48]
	ld	[%sp+116], %r17
	st	%r17, [%r4+52]
	ld	[%sp+112], %r17
	st	%r17, [%r4+56]
	st	%r22, [%r4+60]
	ld	[%sp+100], %r17
	st	%r17, [%r4+64]
	ld	[%sp+96], %r17
	st	%r17, [%r4+68]
	st	%r21, [%r4+72]
	st	%r9, [%r4+76]
	st	%r8, [%r4+80]
	st	%r19, [%r4+84]
	st	%r20, [%r4+88]
	st	%r11, [%r4+92]
	add	%r4, 4, %r8
	add	%r4, 96, %r4
	! done allocating 23 record
code_419019:
	ld	[%sp+92], %r15
	retl
	add	%sp, 208, %sp
	.size SplaySetFn_functor_var_r_code_410153,(.-SplaySetFn_functor_var_r_code_410153)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_419020
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x50000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419021
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x51554000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419022
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40010000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419023
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x41450000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419024
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x51000000
	.word 0x51554551
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418814
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000000
	.word 0x00000014
		! -------- label,sizes,reg
	.long needgc_418822
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x50000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419025
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x51000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419026
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x51400000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418845
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.word 0x51500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418851
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x51540000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418866
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x51555000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419027
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x51555000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418876
	.word 0x00340009
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x51555000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418885
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x51555500
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419028
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419029
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x41000000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419030
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x41400000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419031
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x41440000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418924
	.word 0x00340009
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x41440000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418931
	.word 0x00340009
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40410000
	.word 0x51555551
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419032
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x51400000
	.word 0x51554551
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_418954
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x51500000
	.word 0x51554551
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419033
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000000
	.word 0x00000010
		! -------- label,sizes,reg
	.long code_419034
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x40000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419035
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x51500000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419036
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x51555000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419037
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x51555400
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_419038
	.word 0x00340009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40410000
	.word 0x51555551
	.word 0x00000015
	.text
	.align 8
	.global SplaySetFn_main
 ! arguments : 
 ! results    : [$411690,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
SplaySetFn_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bgu	code_419235
	nop
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_419235:
	st	%r15, [%sp+92]
code_419040:
funtop_411482:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_419234:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_419041:
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(PLUSO_order_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_order_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label order_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_400218+-4), %r8
	st	%r9, [%r8+%lo(type_400218+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 156, %r8
	cmp	%r8, %r9
	bleu	afterMutateCheck_419048
	nop
code_419049:
	sub	%r4, 156, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_419048:
	sethi	%hi(type_400218), %r8
	or	%r8, %lo(type_400218), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(SplayTree_STR_c_INT), %r8
	or	%r8, %lo(SplayTree_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label splay_TYC
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_400232+-4), %r8
	st	%r9, [%r8+%lo(type_400232+-4)]
	sethi	%hi(type_400232), %r8
	or	%r8, %lo(type_400232), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(PLUSO_bool_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label bool_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_400275+-4), %r8
	st	%r9, [%r8+%lo(type_400275+-4)]
	sethi	%hi(type_400275), %r8
	or	%r8, %lo(type_400275), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(PLUSO_bool_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	sethi	%hi(PLUSO_bool_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r9
	sethi	%hi(PLUSNbool_out_400314), %r8
	st	%r9, [%r8+%lo(PLUSNbool_out_400314)]
	sethi	%hi(SplayTree_STR_r_INT), %r8
	or	%r8, %lo(SplayTree_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	sethi	%hi(PLUSNsplay_in_400386), %r9
	st	%r10, [%r9+%lo(PLUSNsplay_in_400386)]
	sethi	%hi(SplayTree_STR_c_INT), %r9
	or	%r9, %lo(SplayTree_STR_c_INT), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	! Proj_c at label hiddenThinModule_INT
	ld	[%r9], %r18
	or	%r0, 111, %r10
	sethi	%hi(type_400387+-4), %r9
	st	%r10, [%r9+%lo(type_400387+-4)]
	sethi	%hi(type_400387), %r9
	or	%r9, %lo(type_400387), %r13
	ld	[%r2+804], %r12
	ld	[%r2+792], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+792]
	add	%r13, %r12, %r9
	st	%r18, [%r9]
	sethi	%hi(type_400387), %r9
	or	%r9, %lo(type_400387), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	! Proj_c at label splay_TYC
	ld	[%r9], %r18
	or	%r0, 111, %r10
	sethi	%hi(type_407688+-4), %r9
	st	%r10, [%r9+%lo(type_407688+-4)]
	sethi	%hi(type_407688), %r9
	or	%r9, %lo(type_407688), %r13
	ld	[%r2+804], %r12
	ld	[%r2+792], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+792]
	add	%r13, %r12, %r9
	st	%r18, [%r9]
	sethi	%hi(SplayTree_STR_r_INT), %r9
	or	%r9, %lo(SplayTree_STR_r_INT), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+16], %r18
	or	%r0, 111, %r10
	sethi	%hi(openvar_r_splay_400466+-4), %r9
	st	%r10, [%r9+%lo(openvar_r_splay_400466+-4)]
	sethi	%hi(openvar_r_splay_400466), %r9
	or	%r9, %lo(openvar_r_splay_400466), %r13
	ld	[%r2+804], %r12
	ld	[%r2+792], %r11
	mov	%r13, %r10
	mov	%r12, %r9
	st	%r10, [%r11]
	st	%r9, [%r11+4]
	add	%r13, %r12, %r9
	ld	[%r9], %r9
	st	%r9, [%r11+8]
	add	%r11, 12, %r9
	st	%r9, [%r2+792]
	add	%r13, %r12, %r9
	st	%r18, [%r9]
	ld	[%r8+4], %r9
	sethi	%hi(PLUSNsplay_out_400497), %r8
	st	%r9, [%r8+%lo(PLUSNsplay_out_400497)]
	sethi	%hi(PLUSO_order_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_order_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r9
	sethi	%hi(PLUSNorder_out_400580), %r8
	st	%r9, [%r8+%lo(PLUSNorder_out_400580)]
	sethi	%hi(PLUSO_list_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_list_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r9
	sethi	%hi(PLUSNlist_out_400846), %r8
	st	%r9, [%r8+%lo(PLUSNlist_out_400846)]
	sethi	%hi(List_STR_r_INT), %r8
	or	%r8, %lo(List_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(foldl_400905+-4), %r8
	st	%r9, [%r8+%lo(foldl_400905+-4)]
	sethi	%hi(foldl_400905), %r8
	or	%r8, %lo(foldl_400905), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(List_STR_c_INT), %r8
	or	%r8, %lo(List_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label list_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_405802+-4), %r8
	st	%r9, [%r8+%lo(type_405802+-4)]
	sethi	%hi(type_405802), %r8
	or	%r8, %lo(type_405802), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(LibBase_STR_r_INT), %r8
	or	%r8, %lo(LibBase_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_400942+-4), %r8
	st	%r9, [%r8+%lo(mk_400942+-4)]
	sethi	%hi(mk_400942), %r8
	or	%r8, %lo(mk_400942), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(SplayTree_STR_r_INT), %r8
	or	%r8, %lo(SplayTree_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(openvar_r_join_401024+-4), %r8
	st	%r9, [%r8+%lo(openvar_r_join_401024+-4)]
	sethi	%hi(openvar_r_join_401024), %r8
	or	%r8, %lo(openvar_r_join_401024), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(PLUSO_list_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_list_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label list_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_401683+-4), %r8
	st	%r9, [%r8+%lo(type_401683+-4)]
	sethi	%hi(type_401683), %r8
	or	%r8, %lo(type_401683), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(PLUSO_list_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_list_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r9
	sethi	%hi(PLUSNlist_in_401729), %r8
	st	%r9, [%r8+%lo(PLUSNlist_in_401729)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(left_r_394664), %r8
	or	%r8, %lo(left_r_394664), %r8
	! done allocating 1 closures
	sethi	%hi(PLUSO_order_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_order_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(PLUSO_option_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label option_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_402262+-4), %r8
	st	%r9, [%r8+%lo(type_402262+-4)]
	sethi	%hi(type_402262), %r8
	or	%r8, %lo(type_402262), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(PLUSO_option_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r9
	sethi	%hi(PLUSNoption_in_402293), %r8
	st	%r9, [%r8+%lo(PLUSNoption_in_402293)]
	sethi	%hi(PLUSO_option_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label option_sum_INT
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_402295+-4), %r8
	st	%r9, [%r8+%lo(type_402295+-4)]
	sethi	%hi(type_402295), %r8
	or	%r8, %lo(type_402295), %r12
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r12, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r12, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r12, %r11, %r8
	st	%r13, [%r8]
	sethi	%hi(PLUSO_option_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r9
	sethi	%hi(PLUSNoption_out_402673), %r8
	st	%r9, [%r8+%lo(PLUSNoption_out_402673)]
	! allocating 2-record
	! done allocating 2 record
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(functor_var_r_393936), %r8
	or	%r8, %lo(functor_var_r_393936), %r8
	! done allocating 1 closures
	or	%r0, 256, %r8
code_419233:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size SplaySetFn_main,(.-SplaySetFn_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_419048
	.word 0x00180007
	.word 0x00170000
	.word 0x80002000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_419234
	.word 0x00180007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
SplaySetFn_unit_CODE_END_VAL:
	.section	".rodata"
		! mark end of GC table
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl SplaySetFn_unit_GLOBALS_BEGIN_VAL
SplaySetFn_unit_GLOBALS_BEGIN_VAL:
		! static record tag
	.word 0x00000211
record_411489:
	.long SplaySetFn_functor_var_c_code_410128
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl SplaySetFn_FCT_c_INT
SplaySetFn_FCT_c_INT:
	.long record_411489
	.long record_411489
		! Global
	.word 0x00000037
type_400218:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_400232:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_400275:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNbool_out_400314:
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNsplay_in_400386:
	.word 0x00000102
		! Global
	.word 0x00000037
type_400387:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_407688:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
openvar_r_splay_400466:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNsplay_out_400497:
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNorder_out_400580:
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNlist_out_400846:
	.word 0x00000102
		! Global
	.word 0x00000037
foldl_400905:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_405802:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
mk_400942:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
openvar_r_join_401024:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_401683:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNlist_in_401729:
	.word 0x00000102
		! static record tag
	.word 0x00000619
left_r_394664:
	.long SplaySetFn_left_r_code_410133
	.word 0x00000100
	.word 0x00000100
	.word 0x0000002a
string_411649:
		! string size = 5
	.ascii "match"
.align 4
		! static record tag
	.word 0x00000619
record_411651:
	.word 0x00000102
	.word 0x00000100
	.long string_411649
		! Global
	.word 0x00000037
type_402262:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNoption_in_402293:
	.word 0x00000102
		! Global
	.word 0x00000037
type_402295:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNoption_out_402673:
	.word 0x00000102
		! static record tag
	.word 0x00000011
record_411683:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x0000006f
	.globl SplaySetFn_FCT_r_INT
SplaySetFn_FCT_r_INT:
	.long functor_var_r_393936
	.long functor_var_r_393936
		! static record tag
	.word 0x00000619
functor_var_r_393936:
	.long SplaySetFn_functor_var_r_code_410153
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000009
record_411696:
	.word 0x00000008
		! static record tag
	.word 0x00000037
record_411894:
	.word 0x00000000
	.word 0x00000000
		! static record tag
	.word 0x00000619
record_412017:
	.long SplaySetFn_isEmpty_code_410329
	.long record_411696
	.word 0x00000100
		! static record tag
	.word 0x00000037
record_412120:
	.word 0x00000000
	.word 0x00000000
		! static record tag
	.word 0x00000619
record_412205:
	.long SplaySetFn_numItems_code_410451
	.long record_411696
	.word 0x00000100
		! static record tag
	.word 0x00000047
record_412287:
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! static record tag
	.word 0x00000037
record_412330:
	.word 0x00000000
	.word 0x00000000
		! static record tag
	.word 0x00000009
record_412718:
	.word 0x00000009
		! static record tag
	.word 0x00000009
record_412983:
	.word 0x00000009
		! static record tag
	.word 0x00000009
record_417711:
	.word 0x00000008
		! Module closure
	.word 0x00000619
	.globl SplaySetFn_unit_closure
SplaySetFn_unit_closure:
	.long SplaySetFn_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl SplaySetFn_unit
SplaySetFn_unit:
	.long SplaySetFn_unit_closure
	.long SplaySetFn_unit_closure
	.globl SplaySetFn_unit_GLOBALS_END_VAL
SplaySetFn_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.long 0 !filler

	.data
	.align 8
	.globl SplaySetFn_unit_TRACE_GLOBALS_BEGIN_VAL
SplaySetFn_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long type_402295
	.long type_402262
	.long type_401683
	.long openvar_r_join_401024
	.long mk_400942
	.long type_405802
	.long foldl_400905
	.long openvar_r_splay_400466
	.long type_407688
	.long type_400387
	.long type_400275
	.long type_400232
	.long type_400218
	.globl SplaySetFn_unit_TRACE_GLOBALS_END_VAL
SplaySetFn_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
