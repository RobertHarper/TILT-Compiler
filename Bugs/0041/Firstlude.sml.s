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
	.globl Prelude_unit_GCTABLE_BEGIN_VAL
Prelude_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl Prelude_unit_CODE_END_VAL
	.globl Prelude_unit_CODE_BEGIN_VAL
Prelude_unit_CODE_BEGIN_VAL:
	.text
	.align 8
	.global Prelude__code_5034
 ! arguments : [$5036,$8] [$1360,$9] 
 ! results    : [$7764,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
Prelude__code_5034:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7774
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7774:
	st	%r15, [%sp+92]
code_7770:
funtop_7756:
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_7762), %r8
	or	%r8, %lo(record_7762), %r8
code_7773:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude__code_5034,(.-Prelude__code_5034)

	.section	".rodata"
	.text
	.align 8
	.global Prelude__code_5039
 ! arguments : [$5041,$8] [$1364,$9] 
 ! results    : [$1364,$8] 
 ! destroys   :  $8
 ! modifies   :  $8
Prelude__code_5039:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7778
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7778:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_7775:
funtop_7752:
code_7777:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude__code_5039,(.-Prelude__code_5039)

	.section	".rodata"
	.text
	.align 8
	.global Prelude__code_5044
 ! arguments : [$5046,$8] [$1367,$9] 
 ! results    : [$7745,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude__code_5044:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7788
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7788:
	st	%r15, [%sp+92]
code_7779:
funtop_7734:
	! start making constructor call
	sethi	%hi(record_6478), %r8
	or	%r8, %lo(record_6478), %r8
	ld	[%r8], %r10
	jmpl	%r10, %r15
	ld	[%r8+4], %r8
code_7787:
	mov	%r8, %r9
code_7781:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7782
	nop
code_7783:
	call	GCFromML ! delay slot empty
	nop
needgc_7782:
	! done making constructor call
	! allocating 5-record
	sethi	%hi(4137), %r8
	or	%r8, %lo(4137), %r8
	st	%r8, [%r4]
	or	%r0, 4, %r8
	st	%r8, [%r4+4]
	or	%r0, -1, %r8
	st	%r8, [%r4+8]
	or	%r0, 1, %r8
	st	%r8, [%r4+12]
	or	%r0, 2, %r8
	st	%r8, [%r4+16]
	st	%r9, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
code_7786:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude__code_5044,(.-Prelude__code_5044)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7782
	.word 0xb8003006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_7787
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude__code_5049
 ! arguments : [$5051,$8] [$1434,$9] 
 ! results    : [$7733,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
Prelude__code_5049:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7793
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7793:
	st	%r15, [%sp+92]
code_7789:
funtop_7725:
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_7731), %r8
	or	%r8, %lo(record_7731), %r8
code_7792:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude__code_5049,(.-Prelude__code_5049)

	.section	".rodata"
	.text
	.align 8
	.global Prelude__code_5054
 ! arguments : [$5056,$8] [$1438,$9] 
 ! results    : [$7720,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude__code_5054:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7803
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_7803:
	st	%r15, [%sp+92]
	st	%r9, [%sp+96]
code_7794:
funtop_7708:
	! start making constructor call
	sethi	%hi(record_6493), %r8
	or	%r8, %lo(record_6493), %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+96], %r9
code_7802:
	mov	%r8, %r9
code_7796:
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7797
	nop
code_7798:
	call	GCFromML ! delay slot empty
	nop
needgc_7797:
	! done making constructor call
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	st	%r9, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
code_7801:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Prelude__code_5054,(.-Prelude__code_5054)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7797
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_7802
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
	.align 8
	.global Prelude__code_5059
 ! arguments : [$5061,$8] [$1441,$9] 
 ! results    : [$7701,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude__code_5059:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7813
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7813:
	st	%r15, [%sp+92]
code_7804:
funtop_7690:
	! start making constructor call
	sethi	%hi(record_6497), %r8
	or	%r8, %lo(record_6497), %r8
	ld	[%r8], %r10
	jmpl	%r10, %r15
	ld	[%r8+4], %r8
code_7812:
	mov	%r8, %r9
code_7806:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7807
	nop
code_7808:
	call	GCFromML ! delay slot empty
	nop
needgc_7807:
	! done making constructor call
	! allocating 5-record
	sethi	%hi(4137), %r8
	or	%r8, %lo(4137), %r8
	st	%r8, [%r4]
	or	%r0, 4, %r8
	st	%r8, [%r4+4]
	or	%r0, -1, %r8
	st	%r8, [%r4+8]
	or	%r0, 1, %r8
	st	%r8, [%r4+12]
	or	%r0, 2, %r8
	st	%r8, [%r4+16]
	st	%r9, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
code_7811:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude__code_5059,(.-Prelude__code_5059)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7807
	.word 0xb8003006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_7812
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_vars_eq_0_code_5064
 ! arguments : [$5066,$8] [$5067,$9] [$2803,$10] [$2804,$11] 
 ! results    : [$7676,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_vars_eq_0_code_5064:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7823
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7823:
	st	%r15, [%sp+92]
code_7814:
funtop_7654:
	cmp	%r10, 0
	bne,pn	%icc,one_case_7661
	nop
zero_case_7660:
sumarm_7668:
	cmp	%r11, 0
	bne	sumarm_7669
	nop
code_7816:
	ba	after_sum_7665
	or	%r0, 1, %r8
sumarm_7669:
nomatch_sum_7666:
	or	%r0, 0, %r8
after_sum_7665:
	ba	after_zeroone_7662 ! delay slot empty
	nop
one_case_7661:
sumarm_7682:
	cmp	%r11, 1
	bne	sumarm_7683
	nop
code_7819:
	ba	after_sum_7679
	or	%r0, 1, %r8
sumarm_7683:
nomatch_sum_7680:
	or	%r0, 0, %r8
after_sum_7679:
after_zeroone_7662:
code_7822:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_vars_eq_0_code_5064,(.-Prelude_vars_eq_0_code_5064)

	.section	".rodata"
	.text
	.align 8
	.global Prelude_vars_eq_0_code_5074
 ! arguments : [$5076,$8] [$5077,$9] [$2809,$10] [$2810,$11] 
 ! results    : [$7615,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_vars_eq_0_code_5074:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7848
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_7848:
	st	%r15, [%sp+92]
	mov	%r9, %r13
code_7824:
funtop_7584:
	! Proj_c at label type_2209_INT
	ld	[%r8], %r9
	! Proj_c at label mpoly_var_c_1372_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
sumarm_7599:
	cmp	%r10, 0
	bne	sumarm_7600
	nop
sumarm_7607:
	cmp	%r11, 0
	bne	sumarm_7608
	nop
code_7826:
	ba	after_sum_7604
	or	%r0, 1, %r8
sumarm_7608:
nomatch_sum_7605:
	or	%r0, 0, %r8
after_sum_7604:
	ba	after_sum_7596 ! delay slot empty
	nop
sumarm_7600:
sumarm_7623:
	or	%r0, 255, %r8
	cmp	%r11, %r8
	ble	nomatch_sum_7621
	nop
code_7829:
	ld	[%r9+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_7634
	nop
code_7830:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_7635
	nop
code_7831:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_7634
	nop
code_7832:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_7634
	nop
code_7833:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_7634
	nop
dynamic_nobox_7635:
	ba	projsum_single_after_7631 ! delay slot empty
	nop
dynamic_box_7634:
	ld	[%r10], %r10
projsum_single_after_7631:
	ld	[%r9+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_7642
	nop
code_7836:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_7643
	nop
code_7837:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_7642
	nop
code_7838:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_7642
	nop
code_7839:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_7642
	nop
dynamic_nobox_7643:
	ba	projsum_single_after_7639 ! delay slot empty
	nop
dynamic_box_7642:
	ld	[%r11], %r11
projsum_single_after_7639:
	! making closure call 
	ld	[%r13], %r12
	ld	[%r13+4], %r8
	ld	[%r13+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_7842:
	! done making tail call
	ba	after_sum_7620 ! delay slot empty
	nop
sumarm_7624:
nomatch_sum_7621:
	or	%r0, 0, %r8
after_sum_7620:
	ba	after_sum_7596 ! delay slot empty
	nop
sumarm_7616:
after_sum_7596:
code_7846:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Prelude_vars_eq_0_code_5074,(.-Prelude_vars_eq_0_code_5074)

	.section	".rodata"
	.text
	.align 8
	.global Prelude_polyPLUSEoption_INT_r_code_5069
 ! arguments : [$5071,$8] [$1372,$9] [$5072,$10] [$1373,$11] 
 ! results    : [$7579,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
Prelude_polyPLUSEoption_INT_r_code_5069:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7856
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7856:
	st	%r15, [%sp+92]
	mov	%r9, %r12
code_7849:
funtop_7557:
	add	%r4, 52, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7850
	nop
code_7851:
	call	GCFromML ! delay slot empty
	nop
needgc_7850:
	! allocating 5-record
	sethi	%hi(4137), %r8
	or	%r8, %lo(4137), %r8
	st	%r8, [%r4]
	or	%r0, 4, %r8
	st	%r8, [%r4+4]
	or	%r0, 1, %r8
	st	%r8, [%r4+8]
	or	%r0, 1, %r8
	st	%r8, [%r4+12]
	or	%r0, 2, %r8
	st	%r8, [%r4+16]
	st	%r12, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	ld	[%r11], %r10
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_vars_eq_0_code_5074), %r8
	or	%r8, %lo(Prelude_vars_eq_0_code_5074), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_7855:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_polyPLUSEoption_INT_r_code_5069,(.-Prelude_polyPLUSEoption_INT_r_code_5069)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7850
	.word 0xb8003006
	.word 0xbffc3800
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_vars_eq_0_code_5085
 ! arguments : [$5087,$8] [$5088,$9] [$2819,$10] [$2820,$11] 
 ! results    : [$7526,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_vars_eq_0_code_5085:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7871
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7871:
	st	%r15, [%sp+92]
code_7857:
funtop_7501:
sumarm_7510:
	cmp	%r10, 0
	bne	sumarm_7511
	nop
sumarm_7518:
	cmp	%r11, 0
	bne	sumarm_7519
	nop
code_7859:
	ba	after_sum_7515
	or	%r0, 1, %r8
sumarm_7519:
nomatch_sum_7516:
	or	%r0, 0, %r8
after_sum_7515:
	ba	after_sum_7507 ! delay slot empty
	nop
sumarm_7511:
	cmp	%r10, 1
	bne	sumarm_7527
	nop
sumarm_7534:
	cmp	%r11, 1
	bne	sumarm_7535
	nop
code_7863:
	ba	after_sum_7531
	or	%r0, 1, %r8
sumarm_7535:
nomatch_sum_7532:
	or	%r0, 0, %r8
after_sum_7531:
	ba	after_sum_7507 ! delay slot empty
	nop
sumarm_7527:
sumarm_7549:
	cmp	%r11, 2
	bne	sumarm_7550
	nop
code_7866:
	ba	after_sum_7546
	or	%r0, 1, %r8
sumarm_7550:
nomatch_sum_7547:
	or	%r0, 0, %r8
after_sum_7546:
	ba	after_sum_7507 ! delay slot empty
	nop
sumarm_7542:
after_sum_7507:
code_7870:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_vars_eq_0_code_5085,(.-Prelude_vars_eq_0_code_5085)

	.section	".rodata"
	.text
	.align 8
	.global Prelude_vars_eq_0_code_5095
 ! arguments : [$5097,$8] [$5098,$9] [$2825,$10] [$2826,$11] 
 ! results    : [$7453,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_vars_eq_0_code_5095:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7887
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_7887:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+100]
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_7872:
funtop_7426:
sumarm_7437:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	bne	sumarm_7438
	nop
sumarm_7445:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	bne	sumarm_7446
	nop
code_7874:
	ba	after_sum_7442
	or	%r0, 1, %r8
sumarm_7446:
nomatch_sum_7443:
	or	%r0, 0, %r8
after_sum_7442:
	ba	after_sum_7434 ! delay slot empty
	nop
sumarm_7438:
sumarm_7461:
	or	%r0, 255, %r8
	ld	[%sp+104], %r17
	cmp	%r17, %r8
	ble	nomatch_sum_7459
	nop
code_7877:
	ld	[%sp+108], %r17
	ld	[%r17], %r10
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_7886:
code_7878:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_7482
	nop
zero_case_7481:
	ba	after_zeroone_7483
	or	%r0, 0, %r8
one_case_7482:
	ld	[%sp+108], %r17
	ld	[%r17+4], %r9
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	! making direct call 
	st	%r9, [%sp+108]
	ba	funtop_7426
	st	%r8, [%sp+104]
code_7881:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_7483:
	ba	after_sum_7458 ! delay slot empty
	nop
sumarm_7462:
nomatch_sum_7459:
	or	%r0, 0, %r8
after_sum_7458:
	ba	after_sum_7434 ! delay slot empty
	nop
sumarm_7454:
after_sum_7434:
code_7885:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Prelude_vars_eq_0_code_5095,(.-Prelude_vars_eq_0_code_5095)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_7886
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
	.align 8
	.global Prelude_polyPLUSElist_INT_r_code_5090
 ! arguments : [$5092,$8] [$1446,$9] [$5093,$10] [$1447,$11] 
 ! results    : [$7421,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
Prelude_polyPLUSElist_INT_r_code_5090:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7895
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7895:
	st	%r15, [%sp+92]
	mov	%r9, %r12
code_7888:
funtop_7412:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7889
	nop
code_7890:
	call	GCFromML ! delay slot empty
	nop
needgc_7889:
	ld	[%r11], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_vars_eq_0_code_5095), %r8
	or	%r8, %lo(Prelude_vars_eq_0_code_5095), %r8
	st	%r8, [%r4+4]
	st	%r12, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_7894:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_polyPLUSElist_INT_r_code_5090,(.-Prelude_polyPLUSElist_INT_r_code_5090)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7889
	.word 0xb8003006
	.word 0xbffc3800
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_vector_eq_loop_code_5120
 ! arguments : [$5122,$8] [$5123,$9] [$1503,$10] 
 ! results    : [$7409,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_vector_eq_loop_code_5120:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7914
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_7914:
	st	%r15, [%sp+92]
	st	%r8, [%sp+120]
	st	%r9, [%sp+116]
	st	%r10, [%sp+124]
code_7896:
funtop_7347:
	! Proj_c at label type_2417_INT
	ld	[%sp+120], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+112]
	! Proj_c at label var_poly_c_1490_INT
	ld	[%sp+120], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+96]
	ld	[%sp+116], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+108]
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+104]
	ld	[%sp+124], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	bcc	cmpui_7897
	nop
code_7898:
	or	%r0, 0, %r8
cmpui_7897:
	cmp	%r8, 0
	bne,pn	%icc,one_case_7370
	nop
zero_case_7369:
	! making direct call 
	sethi	%hi(polyVsub_INT), %r8
	ld	[%r8+%lo(polyVsub_INT)], %r11
	ld	[%sp+112], %r8
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_7913:
	st	%r8, [%sp+100]
code_7901:
	! done making normal call
	! making direct call 
	sethi	%hi(polyVsub_INT), %r8
	ld	[%r8+%lo(polyVsub_INT)], %r11
	ld	[%sp+112], %r8
	ld	[%sp+104], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_7911:
	mov	%r8, %r11
code_7903:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_7912:
code_7904:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_7395
	nop
zero_case_7394:
	ba	after_zeroone_7396
	or	%r0, 0, %r8
one_case_7395:
	ld	[%sp+124], %r17
	add	%r17, 1, %r8
	! making direct call 
	ba	funtop_7347
	st	%r8, [%sp+124]
code_7907:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_7396:
	ba	after_zeroone_7371 ! delay slot empty
	nop
one_case_7370:
	or	%r0, 1, %r8
after_zeroone_7371:
code_7910:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Prelude_vector_eq_loop_code_5120,(.-Prelude_vector_eq_loop_code_5120)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_7911
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x144d0000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_7912
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x14000000
		! -------- label,sizes,reg
	.long code_7913
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15510000
	.text
	.align 8
	.global Prelude_anonfun_code_5114
 ! arguments : [$5116,$8] [$5117,$9] [$2846,$10] [$2847,$11] 
 ! results    : [$7338,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_anonfun_code_5114:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7934
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_7934:
	st	%r15, [%sp+92]
	st	%r9, [%sp+108]
	st	%r10, [%sp+116]
	st	%r11, [%sp+112]
code_7915:
funtop_7290:
	! Proj_c at label type_2417_INT
	ld	[%r8], %r17
	st	%r17, [%sp+104]
	! Proj_c at label var_poly_c_1490_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	! making direct call 
	sethi	%hi(polyLen_INT), %r8
	ld	[%r8+%lo(polyLen_INT)], %r10
	ld	[%sp+104], %r8
	jmpl	%r10, %r15
	ld	[%sp+116], %r9
code_7932:
	st	%r8, [%sp+96]
code_7917:
	! done making normal call
	! making direct call 
	sethi	%hi(polyLen_INT), %r8
	ld	[%r8+%lo(polyLen_INT)], %r10
	ld	[%sp+104], %r8
	jmpl	%r10, %r15
	ld	[%sp+112], %r9
code_7931:
	mov	%r8, %r11
code_7919:
	! done making normal call
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7920
	nop
code_7921:
	call	GCFromML ! delay slot empty
	nop
needgc_7920:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 4-record
	or	%r0, 3361, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	ld	[%sp+116], %r17
	st	%r17, [%r4+12]
	ld	[%sp+112], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_vector_eq_loop_code_5120), %r8
	or	%r8, %lo(Prelude_vector_eq_loop_code_5120), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+96], %r17
	cmp	%r17, %r11
	or	%r0, 1, %r8
	be	cmpui_7924
	nop
code_7925:
	or	%r0, 0, %r8
cmpui_7924:
	cmp	%r8, 0
	bne,pn	%icc,one_case_7334
	nop
zero_case_7333:
	ba	after_zeroone_7335
	or	%r0, 0, %r8
one_case_7334:
	or	%r0, 0, %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_7928:
	! done making tail call
after_zeroone_7335:
code_7930:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Prelude_anonfun_code_5114,(.-Prelude_anonfun_code_5114)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_7931
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! -------- label,sizes,reg
	.long needgc_7920
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! -------- label,sizes,reg
	.long code_7932
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
	.text
	.align 8
	.global Prelude_vector_eq_inner_code_5109
 ! arguments : [$5111,$8] [$5112,$9] [$1494,$10] 
 ! results    : [$7285,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_vector_eq_inner_code_5109:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7942
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7942:
	st	%r15, [%sp+92]
	mov	%r10, %r11
code_7935:
funtop_7268:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7936
	nop
code_7937:
	call	GCFromML ! delay slot empty
	nop
needgc_7936:
	! Proj_c at label type_2417_INT
	ld	[%r8], %r10
	! Proj_c at label var_poly_c_1490_INT
	ld	[%r8+4], %r9
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_anonfun_code_5114), %r8
	or	%r8, %lo(Prelude_anonfun_code_5114), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_7941:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_vector_eq_inner_code_5109,(.-Prelude_vector_eq_inner_code_5109)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7936
	.word 0xb8003006
	.word 0xbffc3900
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_vector_eq_r_code_5104
 ! arguments : [$5106,$8] [$1490,$9] [$5107,$10] [$1491,$11] 
 ! results    : [$7262,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_vector_eq_r_code_5104:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7950
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7950:
	st	%r15, [%sp+92]
code_7943:
funtop_7250:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7944
	nop
code_7945:
	call	GCFromML ! delay slot empty
	nop
needgc_7944:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_vector_eq_inner_code_5109), %r8
	or	%r8, %lo(Prelude_vector_eq_inner_code_5109), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_7949:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_vector_eq_r_code_5104,(.-Prelude_vector_eq_r_code_5104)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7944
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_anonfun_code_5142
 ! arguments : [$5144,$8] [$5145,$9] [$2861,$10] [$2862,$11] 
 ! results    : [$7249,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_anonfun_code_5142:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7956
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7956:
	st	%r15, [%sp+92]
code_7951:
funtop_7244:
	cmp	%r10, %r11
	or	%r0, 1, %r8
	be	cmpui_7952
	nop
code_7953:
	or	%r0, 0, %r8
cmpui_7952:
code_7955:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_anonfun_code_5142,(.-Prelude_anonfun_code_5142)

	.section	".rodata"
	.text
	.align 8
	.global Prelude_anonfun_code_5147
 ! arguments : [$5149,$8] [$5150,$9] [$1558,$10] 
 ! results    : [$7239,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
Prelude_anonfun_code_5147:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7965
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_7965:
	st	%r15, [%sp+92]
code_7957:
funtop_7228:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7958
	nop
code_7959:
	call	GCFromML ! delay slot empty
	nop
needgc_7958:
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(exn_stamp_2499), %r8
	ld	[%r8+%lo(exn_stamp_2499)], %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	sethi	%hi(string_7238), %r8
	or	%r8, %lo(string_7238), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
code_7964:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_anonfun_code_5147,(.-Prelude_anonfun_code_5147)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7958
	.word 0xb8003006
	.word 0xbffc3c00
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_anonfun_code_5165
 ! arguments : [$5167,$8] [$5168,$9] [$1595,$10] 
 ! results    : [$7227,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_anonfun_code_5165:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7973
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_7973:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_7966:
funtop_7207:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_7972:
	mov	%r8, %r10
code_7967:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_7968:
	! done making tail call
code_7970:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Prelude_anonfun_code_5165,(.-Prelude_anonfun_code_5165)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_7972
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
	.align 8
	.global Prelude_o_inner_code_5157
 ! arguments : [$5159,$8] [$5160,$9] [$2894,$10] [$2895,$11] 
 ! results    : [$7206,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_o_inner_code_5157:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_7993
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_7993:
	st	%r15, [%sp+92]
	st	%r10, [%sp+116]
	mov	%r11, %r12
code_7974:
funtop_7143:
	! Proj_c at label type_2528_INT
	ld	[%r8], %r17
	st	%r17, [%sp+112]
	! Proj_c at label type_2527_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+108]
	! Proj_c at label type_2526_INT
	ld	[%r8+8], %r17
	st	%r17, [%sp+104]
	! Proj_c at label var_poly_c_1584_INT
	ld	[%r8+12], %r17
	st	%r17, [%sp+100]
	! making closure call 
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+112], %r9
	jmpl	%r13, %r15
	ld	[%sp+104], %r10
code_7991:
	st	%r8, [%sp+96]
code_7977:
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
	ld	[%sp+104], %r9
	ld	[%sp+108], %r10
	jmpl	%r13, %r15
	ld	[%sp+116], %r12
code_7990:
	mov	%r8, %r9
code_7980:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7981
	nop
code_7982:
	call	GCFromML ! delay slot empty
	nop
needgc_7981:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_anonfun_code_5165), %r8
	or	%r8, %lo(Prelude_anonfun_code_5165), %r8
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+112], %r9
	jmpl	%r13, %r15
	ld	[%sp+108], %r10
code_7992:
code_7987:
	! done making normal call
code_7989:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Prelude_o_inner_code_5157,(.-Prelude_o_inner_code_5157)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_7990
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01450000
		! -------- label,sizes,reg
	.long needgc_7981
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01450000
		! -------- label,sizes,reg
	.long code_7991
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! -------- label,sizes,reg
	.long code_7992
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_o_r_code_5152
 ! arguments : [$5154,$8] [$1584,$9] [$5155,$10] [$1585,$11] 
 ! results    : [$7137,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
Prelude_o_r_code_5152:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8001
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8001:
	st	%r15, [%sp+92]
	mov	%r9, %r12
code_7994:
funtop_7118:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_7995
	nop
code_7996:
	call	GCFromML ! delay slot empty
	nop
needgc_7995:
	! Proj_c at label 'a_TYV
	ld	[%r12], %r11
	! Proj_c at label 'b_TYV
	ld	[%r12+4], %r10
	! Proj_c at label 'c_TYV
	ld	[%r12+8], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	st	%r11, [%r4+12]
	st	%r12, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_o_inner_code_5157), %r8
	or	%r8, %lo(Prelude_o_inner_code_5157), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_8000:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_o_r_code_5152,(.-Prelude_o_r_code_5152)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_7995
	.word 0xb8003006
	.word 0xbffc3000
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_before_inner_code_5185
 ! arguments : [$5187,$8] [$5188,$9] [$2918,$10] [$2919,$11] 
 ! results    : [$2918,$8] 
 ! destroys   :  $11 $9 $8
 ! modifies   :  $11 $9 $8
Prelude_before_inner_code_5185:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8005
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_8005:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r8
code_8002:
funtop_7112:
code_8004:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Prelude_before_inner_code_5185,(.-Prelude_before_inner_code_5185)

	.section	".rodata"
	.text
	.align 8
	.global Prelude_before_r_code_5180
 ! arguments : [$5182,$8] [$1600,$9] [$5183,$10] [$1601,$11] 
 ! results    : [$7106,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_before_r_code_5180:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8013
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8013:
	st	%r15, [%sp+92]
code_8006:
funtop_7099:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8007
	nop
code_8008:
	call	GCFromML ! delay slot empty
	nop
needgc_8007:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_before_inner_code_5185), %r8
	or	%r8, %lo(Prelude_before_inner_code_5185), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_8012:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_before_r_code_5180,(.-Prelude_before_r_code_5180)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_8007
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_ignore_inner_code_5197
 ! arguments : [$5199,$8] [$5200,$9] [$1616,$10] 
 ! results    : [$7098,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
Prelude_ignore_inner_code_5197:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8017
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_8017:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_8014:
funtop_7092:
	or	%r0, 256, %r8
code_8016:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Prelude_ignore_inner_code_5197,(.-Prelude_ignore_inner_code_5197)

	.section	".rodata"
	.text
	.align 8
	.global Prelude_ignore_r_code_5192
 ! arguments : [$5194,$8] [$1612,$9] [$5195,$10] [$1613,$11] 
 ! results    : [$7091,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_ignore_r_code_5192:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8030
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8030:
	st	%r15, [%sp+92]
code_8018:
funtop_7064:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8019
	nop
code_8020:
	call	GCFromML ! delay slot empty
	nop
needgc_8019:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_ignore_inner_code_5197), %r8
	or	%r8, %lo(Prelude_ignore_inner_code_5197), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r18
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_7081), %r8
	or	%r8, %lo(record_7081), %r12
	! making closure call 
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	mov	%r12, %r10
	jmpl	%r13, %r15
	mov	%r18, %r12
code_8029:
code_8026:
	! done making normal call
code_8028:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_ignore_r_code_5192,(.-Prelude_ignore_r_code_5192)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_8019
	.word 0xb8003006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_8029
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_revappend_inner_code_5209
 ! arguments : [$5211,$8] [$5212,$9] [$2927,$10] [$2928,$11] 
 ! results    : [$7038,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
Prelude_revappend_inner_code_5209:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8045
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_8045:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r11, %r12
code_8031:
funtop_7024:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8032
	nop
code_8033:
	call	GCFromML ! delay slot empty
	nop
needgc_8032:
sumarm_7034:
	cmp	%r10, 0
	bne	sumarm_7035
	nop
code_8035:
	ba	after_sum_7031
	mov	%r12, %r8
sumarm_7035:
	ld	[%r10], %r11
	ld	[%r10+4], %r10
	! allocating 2-record
	or	%r0, 529, %r9
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_8037
	nop
code_8038:
	or	%r0, 0, %r8
cmpui_8037:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_8039
	nop
code_8040:
	or	%r0, 0, %r8
cmpui_8039:
	st	%r11, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	ba	funtop_7024
	mov	%r8, %r12
code_8041:
	! done making self tail call
	ba	after_sum_7031
	or	%r0, 0, %r8
sumarm_7039:
after_sum_7031:
code_8044:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Prelude_revappend_inner_code_5209,(.-Prelude_revappend_inner_code_5209)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_8032
	.word 0xb8003806
	.word 0xbffc3400
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
	.align 8
	.global Prelude_rev_inner_code_5216
 ! arguments : [$5218,$8] [$5219,$9] [$1625,$10] 
 ! results    : [$7023,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_rev_inner_code_5216:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8051
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8051:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_8046:
funtop_7010:
	ld	[%r8], %r9
	ld	[%r8+4], %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_8047:
	! done making tail call
code_8049:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_rev_inner_code_5216,(.-Prelude_rev_inner_code_5216)

	.section	".rodata"
	.text
	.align 8
	.global Prelude_rev_r_code_5204
 ! arguments : [$5206,$8] [$1621,$9] [$5207,$10] [$1622,$11] 
 ! results    : [$7004,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_rev_r_code_5204:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8060
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8060:
	st	%r15, [%sp+92]
code_8052:
funtop_6982:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8053
	nop
code_8054:
	call	GCFromML ! delay slot empty
	nop
needgc_8053:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_revappend_inner_code_5209), %r8
	or	%r8, %lo(Prelude_revappend_inner_code_5209), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Prelude_rev_inner_code_5216), %r8
	or	%r8, %lo(Prelude_rev_inner_code_5216), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_8059:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_rev_r_code_5204,(.-Prelude_rev_r_code_5204)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_8053
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_anonfun_code_5225
 ! arguments : [$5227,$8] [$5228,$9] [$2956,$10] [$2957,$11] 
 ! results    : [$6977,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Prelude_anonfun_code_5225:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8069
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8069:
	st	%r15, [%sp+92]
code_8061:
funtop_6959:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8062
	nop
code_8063:
	call	GCFromML ! delay slot empty
	nop
needgc_8062:
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
	sethi	%hi(exn_stamp_2622), %r8
	ld	[%r8+%lo(exn_stamp_2622)], %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(string_6976), %r8
	or	%r8, %lo(string_6976), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
code_8068:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_anonfun_code_5225,(.-Prelude_anonfun_code_5225)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_8062
	.word 0xb8003006
	.word 0xbffc3c00
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_anonfun_code_5230
 ! arguments : [$5232,$8] [$5233,$9] [$1666,$10] 
 ! results    : [$6954,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
Prelude_anonfun_code_5230:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8078
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8078:
	st	%r15, [%sp+92]
code_8070:
funtop_6940:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8071
	nop
code_8072:
	call	GCFromML ! delay slot empty
	nop
needgc_8071:
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(exn_stamp_2625), %r8
	ld	[%r8+%lo(exn_stamp_2625)], %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	sethi	%hi(string_6953), %r8
	or	%r8, %lo(string_6953), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
code_8077:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_anonfun_code_5230,(.-Prelude_anonfun_code_5230)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_8071
	.word 0xb8003006
	.word 0xbffc3c00
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Prelude_main
 ! arguments : 
 ! results    : [$6939,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Prelude_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_8400
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_8400:
	st	%r15, [%sp+92]
code_8079:
funtop_6460:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_8399:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_8080:
	! allocating 1-record
	! done allocating 1 record
	! allocating 5-record
	! done allocating 5 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 1-record
	! done allocating 1 record
	! allocating 5-record
	! done allocating 5 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(vars_eq_0_1338), %r8
	or	%r8, %lo(vars_eq_0_1338), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(polyPLUSEoption_INT_r_1371), %r8
	or	%r8, %lo(polyPLUSEoption_INT_r_1371), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(vars_eq_0_1408), %r8
	or	%r8, %lo(vars_eq_0_1408), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(polyPLUSElist_INT_r_1445), %r8
	or	%r8, %lo(polyPLUSElist_INT_r_1445), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(vector_eq_r_1520), %r8
	or	%r8, %lo(vector_eq_r_1520), %r8
	! done allocating 1 closures
	! allocating 1-record
	! done allocating 1 record
	or	%r0, 0, %r13
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(vector_eq_r_1520), %r8
	or	%r8, %lo(vector_eq_r_1520), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_8397:
	mov	%r8, %r9
code_8087:
	! done making normal call
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_1530), %r8
	or	%r8, %lo(anonfun_1530), %r8
	! done allocating 1 closures
	sethi	%hi(anonfun_1530), %r8
	or	%r8, %lo(anonfun_1530), %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_8398:
	mov	%r8, %r13
code_8090:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(PLUSEstring_INT+-4), %r8
	st	%r9, [%r8+%lo(PLUSEstring_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8095
	nop
code_8096:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8095:
	sethi	%hi(PLUSEstring_INT), %r8
	or	%r8, %lo(PLUSEstring_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8108
	nop
code_8109:
	call	GCFromML ! delay slot empty
	nop
needgc_8108:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6592), %r8
	or	%r8, %lo(string_6592), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Bind_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Bind_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8116
	nop
code_8117:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8116:
	sethi	%hi(Bind_r_INT), %r8
	or	%r8, %lo(Bind_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8129
	nop
code_8130:
	call	GCFromML ! delay slot empty
	nop
needgc_8129:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6615), %r8
	or	%r8, %lo(string_6615), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Chr_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Chr_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8137
	nop
code_8138:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8137:
	sethi	%hi(Chr_r_INT), %r8
	or	%r8, %lo(Chr_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8150
	nop
code_8151:
	call	GCFromML ! delay slot empty
	nop
needgc_8150:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6638), %r8
	or	%r8, %lo(string_6638), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Div_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Div_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8158
	nop
code_8159:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8158:
	sethi	%hi(Div_r_INT), %r8
	or	%r8, %lo(Div_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8171
	nop
code_8172:
	call	GCFromML ! delay slot empty
	nop
needgc_8171:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6664), %r8
	or	%r8, %lo(string_6664), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Domain_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Domain_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8179
	nop
code_8180:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8179:
	sethi	%hi(Domain_r_INT), %r8
	or	%r8, %lo(Domain_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8192
	nop
code_8193:
	call	GCFromML ! delay slot empty
	nop
needgc_8192:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6689), %r8
	or	%r8, %lo(string_6689), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Empty_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Empty_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8200
	nop
code_8201:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8200:
	sethi	%hi(Empty_r_INT), %r8
	or	%r8, %lo(Empty_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	sethi	%hi(exn_stamp_2499), %r8
	st	%r10, [%r8+%lo(exn_stamp_2499)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_1557), %r8
	or	%r8, %lo(anonfun_1557), %r8
	! done allocating 1 closures
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8215
	nop
code_8216:
	call	GCFromML ! delay slot empty
	nop
needgc_8215:
	or	%r0, 529, %r8
	st	%r8, [%r4]
	sethi	%hi(exn_stamp_2499), %r8
	ld	[%r8+%lo(exn_stamp_2499)], %r8
	st	%r8, [%r4+4]
	sethi	%hi(anonfun_1557), %r8
	or	%r8, %lo(anonfun_1557), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Fail_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Fail_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8224
	nop
code_8225:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8224:
	sethi	%hi(Fail_r_INT), %r8
	or	%r8, %lo(Fail_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8237
	nop
code_8238:
	call	GCFromML ! delay slot empty
	nop
needgc_8237:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6733), %r8
	or	%r8, %lo(string_6733), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Match_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Match_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8245
	nop
code_8246:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8245:
	sethi	%hi(Match_r_INT), %r8
	or	%r8, %lo(Match_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8258
	nop
code_8259:
	call	GCFromML ! delay slot empty
	nop
needgc_8258:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6759), %r8
	or	%r8, %lo(string_6759), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Option_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Option_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8266
	nop
code_8267:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8266:
	sethi	%hi(Option_r_INT), %r8
	or	%r8, %lo(Option_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8279
	nop
code_8280:
	call	GCFromML ! delay slot empty
	nop
needgc_8279:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6787), %r8
	or	%r8, %lo(string_6787), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Overflow_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Overflow_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8287
	nop
code_8288:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8287:
	sethi	%hi(Overflow_r_INT), %r8
	or	%r8, %lo(Overflow_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8300
	nop
code_8301:
	call	GCFromML ! delay slot empty
	nop
needgc_8300:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6811), %r8
	or	%r8, %lo(string_6811), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Size_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Size_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8308
	nop
code_8309:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8308:
	sethi	%hi(Size_r_INT), %r8
	or	%r8, %lo(Size_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8321
	nop
code_8322:
	call	GCFromML ! delay slot empty
	nop
needgc_8321:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6835), %r8
	or	%r8, %lo(string_6835), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Span_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Span_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8329
	nop
code_8330:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8329:
	sethi	%hi(Span_r_INT), %r8
	or	%r8, %lo(Span_r_INT), %r12
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
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8342
	nop
code_8343:
	call	GCFromML ! delay slot empty
	nop
needgc_8342:
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_6864), %r8
	or	%r8, %lo(string_6864), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(Subscript_r_INT+-4), %r8
	st	%r9, [%r8+%lo(Subscript_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8350
	nop
code_8351:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8350:
	sethi	%hi(Subscript_r_INT), %r8
	or	%r8, %lo(Subscript_r_INT), %r12
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
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(o_r_1599), %r8
	or	%r8, %lo(o_r_1599), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(before_r_1611), %r8
	or	%r8, %lo(before_r_1611), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(ignore_r_1620), %r8
	or	%r8, %lo(ignore_r_1620), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(rev_r_1653), %r8
	or	%r8, %lo(rev_r_1653), %r8
	! done allocating 1 closures
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	sethi	%hi(exn_stamp_2622), %r8
	st	%r10, [%r8+%lo(exn_stamp_2622)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_1659), %r8
	or	%r8, %lo(anonfun_1659), %r8
	! done allocating 1 closures
	! allocating 2-record
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_8369
	nop
code_8370:
	call	GCFromML ! delay slot empty
	nop
needgc_8369:
	or	%r0, 529, %r8
	st	%r8, [%r4]
	sethi	%hi(exn_stamp_2622), %r8
	ld	[%r8+%lo(exn_stamp_2622)], %r8
	st	%r8, [%r4+4]
	sethi	%hi(anonfun_1659), %r8
	or	%r8, %lo(anonfun_1659), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r10
	add	%r10, 1, %r8
	st	%r8, [%r9]
	sethi	%hi(exn_stamp_2625), %r8
	st	%r10, [%r8+%lo(exn_stamp_2625)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_1665), %r8
	or	%r8, %lo(anonfun_1665), %r8
	! done allocating 1 closures
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	sethi	%hi(exn_stamp_2625), %r8
	ld	[%r8+%lo(exn_stamp_2625)], %r8
	st	%r8, [%r4+4]
	sethi	%hi(anonfun_1665), %r8
	or	%r8, %lo(anonfun_1665), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(TiltExn_STR_r_INT+-4), %r8
	st	%r9, [%r8+%lo(TiltExn_STR_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_8383
	nop
code_8384:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_8383:
	sethi	%hi(TiltExn_STR_r_INT), %r8
	or	%r8, %lo(TiltExn_STR_r_INT), %r12
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
	or	%r0, 256, %r8
code_8396:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Prelude_main,(.-Prelude_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_8397
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_8398
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8095
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8108
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8116
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8129
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8137
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8150
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8158
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8171
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8179
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8192
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8200
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8215
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8224
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8237
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8245
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8258
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8266
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8279
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8287
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8300
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8308
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8321
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8329
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8342
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8350
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_8369
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_8383
	.word 0xb8003006
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_8399
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
Prelude_unit_CODE_END_VAL:
	.section	".rodata"
		! endgcinfo with filler for alignment
	.globl Prelude_unit_GCTABLE_END_VAL
Prelude_unit_GCTABLE_END_VAL:
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl Prelude_unit_GLOBALS_BEGIN_VAL
Prelude_unit_GLOBALS_BEGIN_VAL:
		! static record tag
	.word 0x00000009
record_6465:
	.word 0x00000008
		! Global
	.word 0x0000006f
	.globl bool_TYC
bool_TYC:
	.long record_6465
	.long record_6465
		! Global
	.word 0x0000006f
	.globl bool_sumarg_INT
bool_sumarg_INT:
	.globl order_sumarg_INT
order_sumarg_INT:
	.globl TiltVectorEq_STR_c_INT
TiltVectorEq_STR_c_INT:
	.globl Bind_c_INT
Bind_c_INT:
	.globl Chr_c_INT
Chr_c_INT:
	.globl Div_c_INT
Div_c_INT:
	.globl Domain_c_INT
Domain_c_INT:
	.globl Empty_c_INT
Empty_c_INT:
	.globl Fail_c_INT
Fail_c_INT:
	.globl Match_c_INT
Match_c_INT:
	.globl Option_c_INT
Option_c_INT:
	.globl Overflow_c_INT
Overflow_c_INT:
	.globl Size_c_INT
Size_c_INT:
	.globl Span_c_INT
Span_c_INT:
	.globl Subscript_c_INT
Subscript_c_INT:
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00001029
record_6470:
	.word 0x00000004
	.word 0xffffffff
	.word 0x00000002
	.word 0x00000002
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl bool_sum_INT
bool_sum_INT:
	.long record_6470
	.long record_6470
		! static record tag
	.word 0x00000211
record_6474:
	.long Prelude__code_5034
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl option_TYC
option_TYC:
	.long record_6474
	.long record_6474
		! static record tag
	.word 0x00000211
record_6478:
	.long Prelude__code_5039
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl option_sumarg_INT
option_sumarg_INT:
	.long record_6478
	.long record_6478
		! static record tag
	.word 0x00000211
record_6482:
	.long Prelude__code_5044
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl option_sum_INT
option_sum_INT:
	.long record_6482
	.long record_6482
		! static record tag
	.word 0x00000009
record_6484:
	.word 0x00000008
		! Global
	.word 0x0000006f
	.globl order_TYC
order_TYC:
	.long record_6484
	.long record_6484
		! static record tag
	.word 0x00001029
record_6489:
	.word 0x00000004
	.word 0xffffffff
	.word 0x00000003
	.word 0x00000003
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl order_sum_INT
order_sum_INT:
	.long record_6489
	.long record_6489
		! static record tag
	.word 0x00000211
record_6493:
	.long Prelude__code_5049
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl list_TYC
list_TYC:
	.long record_6493
	.long record_6493
		! static record tag
	.word 0x00000211
record_6497:
	.long Prelude__code_5054
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl list_sumarg_INT
list_sumarg_INT:
	.long record_6497
	.long record_6497
		! static record tag
	.word 0x00000211
record_6501:
	.long Prelude__code_5059
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl list_sum_INT
list_sum_INT:
	.long record_6501
	.long record_6501
		! static record tag
	.word 0x00000211
record_6504:
	.word 0x00000001
	.word 0x00000000
		! Global
	.word 0x0000006f
	.globl string_TYC
string_TYC:
	.long record_6504
	.long record_6504
		! static record tag
	.word 0x00000311
record_6508:
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl TiltExn_STR_c_INT
TiltExn_STR_c_INT:
	.long record_6508
	.long record_6508
		! Global
	.word 0x0000006f
	.globl PLUSEbool_INT
PLUSEbool_INT:
	.long vars_eq_0_1338
	.long vars_eq_0_1338
		! static record tag
	.word 0x00000619
vars_eq_0_1338:
	.long Prelude_vars_eq_0_code_5064
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl PLUSEoption_INT_r_INT
PLUSEoption_INT_r_INT:
	.long polyPLUSEoption_INT_r_1371
	.long polyPLUSEoption_INT_r_1371
		! static record tag
	.word 0x00000619
polyPLUSEoption_INT_r_1371:
	.long Prelude_polyPLUSEoption_INT_r_code_5069
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl PLUSEorder_INT
PLUSEorder_INT:
	.long vars_eq_0_1408
	.long vars_eq_0_1408
		! static record tag
	.word 0x00000619
vars_eq_0_1408:
	.long Prelude_vars_eq_0_code_5085
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl PLUSElist_INT_r_INT
PLUSElist_INT_r_INT:
	.long polyPLUSElist_INT_r_1445
	.long polyPLUSElist_INT_r_1445
		! static record tag
	.word 0x00000619
polyPLUSElist_INT_r_1445:
	.long Prelude_polyPLUSElist_INT_r_code_5090
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
vector_eq_r_1520:
	.long Prelude_vector_eq_r_code_5104
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000109
record_6556:
	.long vector_eq_r_1520
		! Global
	.word 0x0000006f
	.globl TiltVectorEq_STR_r_INT
TiltVectorEq_STR_r_INT:
	.long record_6556
	.long record_6556
		! static record tag
	.word 0x00000619
anonfun_1530:
	.long Prelude_anonfun_code_5142
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
	.globl PLUSEstring_INT
PLUSEstring_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000022
string_6592:
		! string size = 4
	.ascii "Bind"
.align 4
		! Global
	.word 0x00000037
	.globl Bind_r_INT
Bind_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000001a
string_6615:
		! string size = 3
	.ascii "Chr"
.align 4
		! Global
	.word 0x00000037
	.globl Chr_r_INT
Chr_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000001a
string_6638:
		! string size = 3
	.ascii "Div"
.align 4
		! Global
	.word 0x00000037
	.globl Div_r_INT
Div_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000032
string_6664:
		! string size = 6
	.ascii "Domain"
.align 4
		! Global
	.word 0x00000037
	.globl Domain_r_INT
Domain_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_6689:
		! string size = 5
	.ascii "Empty"
.align 4
		! Global
	.word 0x00000037
	.globl Empty_r_INT
Empty_r_INT:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
exn_stamp_2499:
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_1557:
	.long Prelude_anonfun_code_5147
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
	.globl Fail_r_INT
Fail_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_6733:
		! string size = 5
	.ascii "Match"
.align 4
		! Global
	.word 0x00000037
	.globl Match_r_INT
Match_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000032
string_6759:
		! string size = 6
	.ascii "Option"
.align 4
		! Global
	.word 0x00000037
	.globl Option_r_INT
Option_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000042
string_6787:
		! string size = 8
	.ascii "Overflow"
.align 4
		! Global
	.word 0x00000037
	.globl Overflow_r_INT
Overflow_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000022
string_6811:
		! string size = 4
	.ascii "Size"
.align 4
		! Global
	.word 0x00000037
	.globl Size_r_INT
Size_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000022
string_6835:
		! string size = 4
	.ascii "Span"
.align 4
		! Global
	.word 0x00000037
	.globl Span_r_INT
Span_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000004a
string_6864:
		! string size = 9
	.ascii "Subscript"
.align 4
		! Global
	.word 0x00000037
	.globl Subscript_r_INT
Subscript_r_INT:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x0000006f
	.globl o_r_INT
o_r_INT:
	.long o_r_1599
	.long o_r_1599
		! static record tag
	.word 0x00000619
o_r_1599:
	.long Prelude_o_r_code_5152
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl before_r_INT
before_r_INT:
	.long before_r_1611
	.long before_r_1611
		! static record tag
	.word 0x00000619
before_r_1611:
	.long Prelude_before_r_code_5180
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl ignore_r_INT
ignore_r_INT:
	.long ignore_r_1620
	.long ignore_r_1620
		! static record tag
	.word 0x00000619
ignore_r_1620:
	.long Prelude_ignore_r_code_5192
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl rev_r_INT
rev_r_INT:
	.long rev_r_1653
	.long rev_r_1653
		! static record tag
	.word 0x00000619
rev_r_1653:
	.long Prelude_rev_r_code_5204
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000027
exn_stamp_2622:
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_1659:
	.long Prelude_anonfun_code_5225
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000027
exn_stamp_2625:
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_1665:
	.long Prelude_anonfun_code_5230
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
	.globl TiltExn_STR_r_INT
TiltExn_STR_r_INT:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000003a
string_6953:
		! string size = 7
	.ascii "LibFail"
.align 4
	.word 0x00000032
string_6976:
		! string size = 6
	.ascii "SysErr"
.align 4
		! static record tag
	.word 0x00000011
record_7081:
	.word 0x00000005
	.word 0x00000000
	.word 0x00000022
string_7238:
		! string size = 4
	.ascii "Fail"
.align 4
		! static record tag
	.word 0x00000009
record_7731:
	.word 0x00000008
		! static record tag
	.word 0x00000009
record_7762:
	.word 0x00000008
		! Module closure
	.word 0x00000619
	.globl Prelude_unit_closure
Prelude_unit_closure:
	.long Prelude_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl Prelude_unit
Prelude_unit:
	.long Prelude_unit_closure
	.long Prelude_unit_closure
	.globl Prelude_unit_GLOBALS_END_VAL
Prelude_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.long 0 !filler

	.data
	.align 8
	.globl Prelude_unit_TRACE_GLOBALS_BEGIN_VAL
Prelude_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long TiltExn_STR_r_INT
	.long Subscript_r_INT
	.long Span_r_INT
	.long Size_r_INT
	.long Overflow_r_INT
	.long Option_r_INT
	.long Match_r_INT
	.long Fail_r_INT
	.long Empty_r_INT
	.long Domain_r_INT
	.long Div_r_INT
	.long Chr_r_INT
	.long Bind_r_INT
	.long PLUSEstring_INT
	.globl Prelude_unit_TRACE_GLOBALS_END_VAL
Prelude_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
