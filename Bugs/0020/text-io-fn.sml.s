	.section	".rodata"
		! gcinfo
	.globl TextIOFn_unit_GCTABLE_BEGIN_VAL
TextIOFn_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl TextIOFn_unit_CODE_END_VAL
	.globl TextIOFn_unit_CODE_BEGIN_VAL
TextIOFn_unit_CODE_BEGIN_VAL:
	.text
 	.align 8
	.global TextIOFn_functor_var_c_code_146295
 ! arguments : [$146297,$8] [$132592,$9] 
 ! results    : [$156874,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_functor_var_c_code_146295:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156929
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_156929:
	st	%r15, [%sp+92]
code_156890:
funtop_156744:
	! Proj_c at label PrimIO_STR
	ld	[%r9], %r8
	! Proj_c at label reader_TYC
	ld	[%r8+16], %r17
	st	%r17, [%sp+104]
	! Proj_c at label writer_TYC
	ld	[%r8+28], %r17
	st	%r17, [%sp+100]
	! Proj_c at label pos_TYC
	ld	[%r8+12], %r17
	st	%r17, [%sp+96]
	! start making constructor call
	sethi	%hi(option_TYC), %r8
	or	%r8, %lo(option_TYC), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+96], %r9
code_156928:
code_156893:
	add	%r4, 160, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_156894
	nop
code_156895:
	call	GCFromML ! delay slot empty
	nop
needgc_156894:
	! done making constructor call
	! allocating 1-record
	! done allocating 1 record
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_156768), %r8
	or	%r8, %lo(record_156768), %r8
	! Proj_c at label 1_~48
	ld	[%r8], %r11
	sethi	%hi(record_156768), %r8
	or	%r8, %lo(record_156768), %r8
	! Proj_c at label 2_~48
	ld	[%r8+4], %r10
	! allocating 1-record
	! done allocating 1 record
	! allocating 1-record
	! done allocating 1 record
	! allocating 1-record
	! done allocating 1 record
	! allocating 1-record
	! done allocating 1 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	! allocating 9-record
	sethi	%hi(130889), %r8
	or	%r8, %lo(130889), %r8
	st	%r8, [%r4]
	sethi	%hi(vector_132792), %r8
	or	%r8, %lo(vector_132792), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+4]
	sethi	%hi(elem_132805), %r8
	or	%r8, %lo(elem_132805), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	sethi	%hi(record_156780), %r8
	or	%r8, %lo(record_156780), %r8
	st	%r8, [%r4+20]
	sethi	%hi(record_156788), %r8
	or	%r8, %lo(record_156788), %r8
	st	%r8, [%r4+24]
	ld	[%sp+96], %r17
	st	%r17, [%r4+28]
	sethi	%hi(record_156784), %r8
	or	%r8, %lo(record_156784), %r8
	st	%r8, [%r4+32]
	sethi	%hi(record_156792), %r8
	or	%r8, %lo(record_156792), %r8
	st	%r8, [%r4+36]
	add	%r4, 4, %r9
	add	%r4, 40, %r4
	! done allocating 9 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 11-record
	sethi	%hi(524121), %r12
	or	%r12, %lo(524121), %r12
	st	%r12, [%r4]
	sethi	%hi(vector_132792), %r12
	or	%r12, %lo(vector_132792), %r13
	ld	[%r2+812], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	sethi	%hi(record_156768), %r12
	or	%r12, %lo(record_156768), %r12
	st	%r12, [%r4+20]
	st	%r11, [%r4+24]
	st	%r10, [%r4+28]
	sethi	%hi(record_156780), %r10
	or	%r10, %lo(record_156780), %r10
	st	%r10, [%r4+32]
	sethi	%hi(record_156784), %r10
	or	%r10, %lo(record_156784), %r10
	st	%r10, [%r4+36]
	sethi	%hi(record_156788), %r10
	or	%r10, %lo(record_156788), %r10
	st	%r10, [%r4+40]
	sethi	%hi(record_156792), %r10
	or	%r10, %lo(record_156792), %r10
	st	%r10, [%r4+44]
	add	%r4, 4, %r11
	add	%r4, 48, %r4
	! done allocating 11 record
	! allocating 6-record
	sethi	%hi(16177), %r10
	or	%r10, %lo(16177), %r10
	st	%r10, [%r4]
	st	%r8, [%r4+4]
	sethi	%hi(_134916), %r8
	or	%r8, %lo(_134916), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(strbindvar_c_134918), %r8
	or	%r8, %lo(strbindvar_c_134918), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	st	%r11, [%r4+16]
	sethi	%hi(record_156796), %r8
	or	%r8, %lo(record_156796), %r8
	st	%r8, [%r4+20]
	sethi	%hi(record_156799), %r8
	or	%r8, %lo(record_156799), %r8
	st	%r8, [%r4+24]
	add	%r4, 4, %r10
	add	%r4, 28, %r4
	! done allocating 6 record
	! allocating 6-record
	sethi	%hi(16177), %r8
	or	%r8, %lo(16177), %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	sethi	%hi(vector_132792), %r8
	or	%r8, %lo(vector_132792), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(elem_132805), %r8
	or	%r8, %lo(elem_132805), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(record_156796), %r8
	or	%r8, %lo(record_156796), %r8
	st	%r8, [%r4+16]
	sethi	%hi(record_156799), %r8
	or	%r8, %lo(record_156799), %r8
	st	%r8, [%r4+20]
	st	%r9, [%r4+24]
	add	%r4, 4, %r8
	add	%r4, 28, %r4
	! done allocating 6 record
code_156927:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_functor_var_c_code_146295,(.-TextIOFn_functor_var_c_code_146295)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_156894
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_156928
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146300
 ! arguments : [$146302,$8] [$146303,$9] [$140886,$10] [$140887,$11] 
 ! results    : [$156743,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
TextIOFn_anonfun_code_146300:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156935
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_156935:
	st	%r15, [%sp+92]
code_156930:
funtop_156738:
	cmp	%r10, %r11
	or	%r0, 1, %r8
	be	cmpui_156931
	nop
code_156932:
	or	%r0, 0, %r8
cmpui_156931:
code_156934:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146300,(.-TextIOFn_anonfun_code_146300)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146305
 ! arguments : [$146307,$8] [$146308,$9] [$133356,$10] 
 ! results    : [$156737,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
TextIOFn_anonfun_code_146305:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156942
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_156942:
	st	%r15, [%sp+92]
code_156936:
funtop_156731:
	sethi	%hi(mk_137126), %r8
	or	%r8, %lo(mk_137126), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
code_156941:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146305,(.-TextIOFn_anonfun_code_146305)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146310
 ! arguments : [$146312,$8] [$146313,$9] 
 ! results    : [$156730,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146310:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156946
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_156946:
	st	%r15, [%sp+92]
code_156943:
funtop_156727:
	or	%r0, 0, %r8
code_156945:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146310,(.-TextIOFn_anonfun_code_146310)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146315
 ! arguments : [$146317,$8] [$146318,$9] 
 ! results    : [$156726,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146315:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156950
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_156950:
	st	%r15, [%sp+92]
code_156947:
funtop_156723:
	or	%r0, 0, %r8
code_156949:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146315,(.-TextIOFn_anonfun_code_146315)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146320
 ! arguments : [$146322,$8] [$146323,$9] 
 ! results    : [$156722,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146320:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156954
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_156954:
	st	%r15, [%sp+92]
code_156951:
funtop_156719:
	or	%r0, 256, %r8
code_156953:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146320,(.-TextIOFn_anonfun_code_146320)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_isNL_code_146325
 ! arguments : [$146327,$8] [$146328,$9] [$133729,$10] 
 ! results    : [$156716,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
TextIOFn_isNL_code_146325:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156962
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_156962:
	st	%r15, [%sp+92]
code_156955:
funtop_156703:
	cmp	%r10, 10
	or	%r0, 1, %r8
	be	cmpui_156956
	nop
code_156957:
	or	%r0, 0, %r8
cmpui_156956:
	cmp	%r8, 0
	bne	one_case_156712
	nop
zero_case_156711:
	ba	after_zeroone_156713
	or	%r0, 0, %r8
one_case_156712:
	or	%r0, 1, %r8
after_zeroone_156713:
code_156961:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_isNL_code_146325,(.-TextIOFn_isNL_code_146325)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_copyVec_code_146330
 ! arguments : [$146332,$8] [$146333,$9] [$141727,$10] [$141728,$11] [$141729,$12] [$141730,$13] [$141731,$18] 
 ! results    : [$156702,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_copyVec_code_146330:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156973
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_156973:
	st	%r15, [%sp+92]
	mov	%r10, %r20
	mov	%r13, %r19
	mov	%r18, %r10
code_156963:
funtop_156675:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_156964
	nop
code_156965:
	call	GCFromML ! delay slot empty
	nop
needgc_156964:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r12, [%r4+4]
	add	%r4, 4, %r13
	add	%r4, 8, %r4
	! done allocating 1 record
	! making closure call 
	sethi	%hi(strbindvar_r_copyVec_138046), %r8
	or	%r8, %lo(strbindvar_r_copyVec_138046), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r21
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	mov	%r20, %r12
	jmpl	%r21, %r15
	mov	%r19, %r18
code_156972:
code_156969:
	! done making normal call
	or	%r0, 0, %r8
code_156971:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_copyVec_code_146330,(.-TextIOFn_copyVec_code_146330)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_156964
	.word 0xb8003006
	.word 0x00180000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_156972
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_cpy_code_146340
 ! arguments : [$146342,$8] [$146343,$9] [$141748,$10] [$141749,$11] [$141750,$12] 
 ! results    : [$156623,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_cpy_code_146340:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_156995
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_156995:
	st	%r15, [%sp+92]
	st	%r9, [%sp+108]
	st	%r10, [%sp+100]
	st	%r11, [%sp+116]
	st	%r12, [%sp+112]
code_156974:
funtop_156605:
	ld	[%sp+108], %r17
	ld	[%r17], %r8
	ld	[%sp+108], %r17
	ld	[%r17+4], %r10
	ld	[%sp+108], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+96]
	ld	[%sp+100], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	bl	cmpsi_156975
	nop
code_156976:
	or	%r0, 0, %r8
cmpsi_156975:
	cmp	%r8, 0
	bne	one_case_156620
	nop
zero_case_156619:
	ba	after_zeroone_156621
	ld	[%sp+112], %r8
one_case_156620:
	! making closure call 
	sethi	%hi(strbindvar_r_sub_136246), %r8
	or	%r8, %lo(strbindvar_r_sub_136246), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_156993:
	st	%r8, [%sp+104]
code_156981:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_update_136247), %r8
	or	%r8, %lo(strbindvar_r_update_136247), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+116], %r11
	jmpl	%r13, %r15
	ld	[%sp+104], %r12
code_156992:
code_156984:
	! done making normal call
	ld	[%sp+100], %r17
	addcc	%r17, 1, %r17
	st	%r17, [%sp+100]
	ld	[%sp+116], %r17
	addcc	%r17, 1, %r17
	st	%r17, [%sp+96]
	ld	[%sp+112], %r17
	cmp	%r17, 0
	bne	one_case_156655
	nop
zero_case_156654:
	! making closure call 
	sethi	%hi(isNL_133728), %r8
	or	%r8, %lo(isNL_133728), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_156994:
code_156987:
	! done making normal call
	ba	after_zeroone_156656 ! delay slot empty
	nop
one_case_156655:
	or	%r0, 1, %r8
after_zeroone_156656:
	! making direct call 
	ld	[%sp+100], %r16
	st	%r16, [%sp+100]
	ld	[%sp+96], %r16
	st	%r16, [%sp+116]
	ba	funtop_156605
	st	%r8, [%sp+112]
code_156989:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_156621:
code_156991:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_cpy_code_146340,(.-TextIOFn_cpy_code_146340)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_156992
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
		! -------- label,sizes,reg
	.long code_156993
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01410000
		! -------- label,sizes,reg
	.long code_156994
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
 	.align 8
	.global TextIOFn_copyVec_code_146335
 ! arguments : [$146337,$8] [$146338,$9] [$141742,$10] [$141743,$11] [$141744,$12] [$141745,$13] [$141746,$18] 
 ! results    : [$156604,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_copyVec_code_146335:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157005
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157005:
	st	%r15, [%sp+92]
	mov	%r10, %r19
	mov	%r11, %r10
	mov	%r18, %r11
code_156996:
funtop_156576:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_156997
	nop
code_156998:
	call	GCFromML ! delay slot empty
	nop
needgc_156997:
	addcc	%r10, %r12, %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r19, [%r4+8]
	st	%r13, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_cpy_code_146340), %r8
	or	%r8, %lo(TextIOFn_cpy_code_146340), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 0, %r12
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 96, %sp
code_157001:
	! done making tail call
code_157003:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_copyVec_code_146335,(.-TextIOFn_copyVec_code_146335)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_156997
	.word 0xb8003006
	.word 0x00082000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_lp_code_146367
 ! arguments : [$146369,$8] [$146370,$9] [$141880,$10] [$141881,$11] 
 ! results    : [$156573,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_lp_code_146367:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157019
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157019:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+100]
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_157006:
funtop_156525:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157007
	nop
code_157008:
	call	GCFromML ! delay slot empty
	nop
needgc_157007:
	ld	[%sp+100], %r17
	ld	[%r17], %r9
	ld	[%sp+100], %r17
	ld	[%r17+4], %r12
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	be	cmpui_157010
	nop
code_157011:
	or	%r0, 0, %r8
cmpui_157010:
	cmp	%r8, 0
	bne	one_case_156540
	nop
zero_case_156539:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r11
	add	%r4, 8, %r4
	! done allocating 1 record
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+108], %r10
code_157018:
code_157013:
	! done making normal call
	ld	[%sp+108], %r17
	addcc	%r17, %r8, %r9
	ld	[%sp+104], %r17
	subcc	%r17, %r8, %r8
	! making direct call 
	st	%r9, [%sp+108]
	ba	funtop_156525
	st	%r8, [%sp+104]
code_157014:
	! done making self tail call
	ba	after_zeroone_156541
	or	%r0, 0, %r8
one_case_156540:
	or	%r0, 256, %r8
after_zeroone_156541:
code_157017:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_lp_code_146367,(.-TextIOFn_lp_code_146367)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157007
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_157018
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146361
 ! arguments : [$146363,$8] [$146364,$9] [$141876,$10] [$141877,$11] [$141878,$12] 
 ! results    : [$156524,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146361:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157033
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157033:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r13
	mov	%r10, %r18
	mov	%r11, %r10
	mov	%r12, %r11
code_157020:
funtop_156497:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157021
	nop
code_157022:
	call	GCFromML ! delay slot empty
	nop
needgc_157021:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 273, %r9
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_157024
	nop
code_157025:
	or	%r0, 0, %r8
cmpui_157024:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	st	%r13, [%r4+4]
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_157026
	nop
code_157027:
	or	%r0, 0, %r8
cmpui_157026:
	st	%r18, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_lp_code_146367), %r8
	or	%r8, %lo(TextIOFn_lp_code_146367), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_157029:
	! done making tail call
code_157031:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_anonfun_code_146361,(.-TextIOFn_anonfun_code_146361)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157021
	.word 0xb8003808
	.word 0x00002000
	.word 0x00040000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
	.text
 	.align 8
	.global TextIOFn_iterate_inner_code_146356
 ! arguments : [$146358,$8] [$146359,$9] [$134013,$10] 
 ! results    : [$156492,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
TextIOFn_iterate_inner_code_146356:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157041
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157041:
	st	%r15, [%sp+92]
	mov	%r8, %r11
code_157034:
funtop_156483:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157035
	nop
code_157036:
	call	GCFromML ! delay slot empty
	nop
needgc_157035:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146361), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_146361), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_157040:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_iterate_inner_code_146356,(.-TextIOFn_iterate_inner_code_146356)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157035
	.word 0xb8003006
	.word 0xbffc3c00
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_iterate_r_code_146351
 ! arguments : [$146353,$8] [$134009,$9] [$146354,$10] [$134010,$11] 
 ! results    : [$156477,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
TextIOFn_iterate_r_code_146351:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157049
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157049:
	st	%r15, [%sp+92]
code_157042:
funtop_156470:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157043
	nop
code_157044:
	call	GCFromML ! delay slot empty
	nop
needgc_157043:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_iterate_inner_code_146356), %r8
	or	%r8, %lo(TextIOFn_iterate_inner_code_146356), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_157048:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_iterate_r_code_146351,(.-TextIOFn_iterate_r_code_146351)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157043
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146381
 ! arguments : [$146383,$8] [$146384,$9] [$141904,$10] [$141905,$11] [$141906,$12] 
 ! results    : [$156469,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146381:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157056
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157056:
	st	%r15, [%sp+92]
code_157050:
funtop_156463:
	sethi	%hi(mk_137126), %r8
	or	%r8, %lo(mk_137126), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
code_157055:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146381,(.-TextIOFn_anonfun_code_146381)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146386
 ! arguments : [$146388,$8] [$146389,$9] [$141947,$10] [$141948,$11] [$141949,$12] 
 ! results    : [$156462,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146386:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157063
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157063:
	st	%r15, [%sp+92]
code_157057:
funtop_156456:
	sethi	%hi(mk_137126), %r8
	or	%r8, %lo(mk_137126), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
code_157062:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146386,(.-TextIOFn_anonfun_code_146386)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146391
 ! arguments : [$146393,$8] [$146394,$9] 
 ! results    : [$156455,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146391:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157067
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157067:
	st	%r15, [%sp+92]
code_157064:
funtop_156452:
	or	%r0, 256, %r8
code_157066:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146391,(.-TextIOFn_anonfun_code_146391)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146396
 ! arguments : [$146398,$8] [$146399,$9] 
 ! results    : [$156451,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146396:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157071
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157071:
	st	%r15, [%sp+92]
code_157068:
funtop_156448:
	or	%r0, 256, %r8
code_157070:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146396,(.-TextIOFn_anonfun_code_146396)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146401
 ! arguments : [$146403,$8] [$146404,$9] 
 ! results    : [$156447,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146401:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157075
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157075:
	st	%r15, [%sp+92]
code_157072:
funtop_156444:
	or	%r0, 256, %r8
code_157074:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146401,(.-TextIOFn_anonfun_code_146401)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146406
 ! arguments : [$146408,$8] [$146409,$9] [$133743,$10] 
 ! results    : [$156443,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
TextIOFn_anonfun_code_146406:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157079
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157079:
	st	%r15, [%sp+92]
code_157076:
funtop_156440:
	or	%r0, 0, %r8
code_157078:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146406,(.-TextIOFn_anonfun_code_146406)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146411
 ! arguments : [$146413,$8] [$146414,$9] [$142224,$10] [$142225,$11] 
 ! results    : [$156439,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
TextIOFn_anonfun_code_146411:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157085
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157085:
	st	%r15, [%sp+92]
code_157080:
funtop_156434:
	cmp	%r10, %r11
	or	%r0, 1, %r8
	be	cmpui_157081
	nop
code_157082:
	or	%r0, 0, %r8
cmpui_157081:
code_157084:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146411,(.-TextIOFn_anonfun_code_146411)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146416
 ! arguments : [$146418,$8] [$146419,$9] 
 ! results    : [$156433,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146416:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157089
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157089:
	st	%r15, [%sp+92]
code_157086:
funtop_156430:
	or	%r0, 256, %r8
code_157088:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146416,(.-TextIOFn_anonfun_code_146416)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146421
 ! arguments : [$146423,$8] [$146424,$9] 
 ! results    : [$156429,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146421:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157093
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157093:
	st	%r15, [%sp+92]
code_157090:
funtop_156426:
	or	%r0, 256, %r8
code_157092:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146421,(.-TextIOFn_anonfun_code_146421)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146426
 ! arguments : [$146428,$8] [$146429,$9] 
 ! results    : [$156425,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146426:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157097
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157097:
	st	%r15, [%sp+92]
code_157094:
funtop_156422:
	or	%r0, 256, %r8
code_157096:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146426,(.-TextIOFn_anonfun_code_146426)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146431
 ! arguments : [$146433,$8] [$146434,$9] 
 ! results    : [$156421,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146431:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157101
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157101:
	st	%r15, [%sp+92]
code_157098:
funtop_156418:
	or	%r0, 256, %r8
code_157100:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146431,(.-TextIOFn_anonfun_code_146431)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146436
 ! arguments : [$146438,$8] [$146439,$9] 
 ! results    : [$156417,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
TextIOFn_anonfun_code_146436:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157105
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157105:
	st	%r15, [%sp+92]
code_157102:
funtop_156414:
	or	%r0, 256, %r8
code_157104:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146436,(.-TextIOFn_anonfun_code_146436)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_chunkSzOfIBuf_code_146446
 ! arguments : [$146448,$8] [$146449,$9] [$132845,$10] 
 ! results    : [$156413,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
TextIOFn_chunkSzOfIBuf_code_146446:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157115
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157115:
	st	%r15, [%sp+92]
code_157106:
funtop_156366:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157107
	nop
code_157108:
	call	GCFromML ! delay slot empty
	nop
needgc_157107:
sumarm_156374:
	ba	after_sum_156371
	ld	[%r10+12], %r8
sumarm_156375:
after_sum_156371:
sumarm_156387:
	ld	[%r8+4], %r8
sumarm_156399:
	ld	[%r8+36], %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_156396 ! delay slot empty
	nop
sumarm_156400:
after_sum_156396:
	ba	after_sum_156384 ! delay slot empty
	nop
sumarm_156388:
after_sum_156384:
	ld	[%r8], %r8
code_157114:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_chunkSzOfIBuf_code_146446,(.-TextIOFn_chunkSzOfIBuf_code_146446)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157107
	.word 0xb8003006
	.word 0xbffc3c00
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_extendStream_code_146451
 ! arguments : [$146453,$8] [$146454,$9] [$140935,$10] [$140936,$11] [$140937,$12] 
 ! results    : [$156365,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_extendStream_code_146451:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157184
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_157184:
	st	%r15, [%sp+92]
	st	%r10, [%sp+112]
	st	%r12, [%sp+108]
code_157116:
funtop_156186:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157117
	nop
code_157118:
	call	GCFromML ! delay slot empty
	nop
needgc_157117:
	ld	[%r9], %r17
	st	%r17, [%sp+96]
	ld	[%r9+4], %r17
	st	%r17, [%sp+128]
	ld	[%r9+8], %r17
	st	%r17, [%sp+104]
sumarm_156200:
	ld	[%sp+108], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+124]
	ld	[%sp+108], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+120]
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(exn_handler_156210), %r8
	or	%r8, %lo(exn_handler_156210), %r10
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
sumarm_156224:
	ld	[%sp+124], %r17
	ld	[%r17+12], %r10
	ld	[%sp+124], %r17
	ld	[%r17+8], %r9
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_156221 ! delay slot empty
	nop
sumarm_156225:
after_sum_156221:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+116]
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_157178:
	st	%r8, [%sp+100]
code_157123:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_157179:
	mov	%r8, %r10
code_157124:
	! done making normal call
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r11
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_157180:
	st	%r8, [%sp+96]
code_157125:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_157181:
code_157128:
	! done making normal call
	cmp	%r8, 0
	or	%r0, 1, %r8
	be	cmpui_157129
	nop
code_157130:
	or	%r0, 0, %r8
cmpui_157129:
	cmp	%r8, 0
	bne	one_case_156275
	nop
zero_case_156274:
	or	%r0, 1, %r10
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_ptr_alloc_156287
	nop
code_157132:
	mov	%r10, %r8
	call	save_regs_MLtoC
	ld	[%sp+128], %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_157182:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r9
code_157133:
	ba	array_ptr_aftert_156286 ! delay slot empty
	nop
array_ptr_alloc_156287:
	sll	%r10, 2, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 5, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157135
	nop
code_157136:
	call	GCFromML ! delay slot empty
	nop
needgc_157135:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r9
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_156293
	add	%r10, %r0, %r10
array_init_loopto_156294:
	add	%r9, %r10, %r8
	ld	[%sp+128], %r17
	st	%r17, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_156293:
	cmp	%r10, 0
	bge	array_init_loopto_156294
	nop
array_ptr_aftert_156286:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157141
	nop
code_157142:
	call	GCFromML ! delay slot empty
	nop
needgc_157141:
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	ld	[%sp+124], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 1-record
	or	%r0, 265, %r10
	st	%r10, [%r4]
	st	%r8, [%r4+4]
	add	%r4, 4, %r13
	add	%r4, 8, %r4
	! done allocating 1 record
	ld	[%r2+800], %r10
	ld	[%r2+804], %r11
	add	%r10, 24, %r10
	cmp	%r10, %r11
	ble	afterMutateCheck_157147
	nop
code_157148:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_157147:
	ld	[%r2+800], %r12
	ld	[%sp+120], %r11
	or	%r0, 0, %r10
	st	%r11, [%r12]
	st	%r10, [%r12+4]
	ld	[%sp+120], %r17
	ld	[%r17], %r10
	st	%r10, [%r12+8]
	add	%r12, 12, %r10
	st	%r10, [%r2+800]
	ld	[%sp+120], %r17
	st	%r13, [%r17]
	ld	[%r2+800], %r12
	ld	[%sp+116], %r11
	or	%r0, 0, %r10
	st	%r11, [%r12]
	st	%r10, [%r12+4]
	ld	[%sp+116], %r17
	ld	[%r17], %r10
	st	%r10, [%r12+8]
	add	%r12, 12, %r10
	st	%r10, [%r2+800]
	ld	[%sp+116], %r17
	st	%r9, [%r17]
	! allocating 1-record
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157162
	nop
code_157163:
	call	GCFromML ! delay slot empty
	nop
needgc_157162:
	or	%r0, 265, %r9
	st	%r9, [%r4]
	st	%r8, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_zeroone_156276 ! delay slot empty
	nop
one_case_156275:
	ld	[%sp+104], %r8
after_zeroone_156276:
	ba	exn_handler_after_156211
	ld	[%r1+12], %r1
exn_handler_156210:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r11
	ld	[%r8+4], %r17
	st	%r17, [%sp+124]
	mov	%r15, %r12
sumarm_156332:
	ld	[%sp+124], %r17
	ld	[%r17+4], %r8
sumarm_156344:
	ld	[%r8+24], %r10
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_157183:
code_157171:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_156341
	or	%r0, 0, %r8
sumarm_156345:
after_sum_156341:
	ba	after_sum_156329 ! delay slot empty
	nop
sumarm_156333:
after_sum_156329:
exn_handler_after_156211:
	ba	after_sum_156197 ! delay slot empty
	nop
sumarm_156201:
after_sum_156197:
code_157177:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size TextIOFn_extendStream_code_146451,(.-TextIOFn_extendStream_code_146451)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157178
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55510000
	.word 0x00000001
		! -------- label,sizes,reg
	.long needgc_157117
	.word 0xb8004807
	.word 0x00000a00
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157179
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55140000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_157180
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54140000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_157181
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54150000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_157182
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_157135
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54050000
	.word 0x00000001
		! -------- label,sizes,reg
	.long needgc_157141
	.word 0xb8004807
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_157147
	.word 0xb8004807
	.word 0x00002300
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x14000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_157162
	.word 0xb8004807
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157183
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_get_code_146467
 ! arguments : [$146469,$8] [$146470,$9] [$132985,$10] 
 ! results    : [$156185,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_get_code_146467:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157218
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157218:
	st	%r15, [%sp+92]
	st	%r9, [%sp+112]
code_157185:
funtop_156061:
sumarm_156069:
	ld	[%r10], %r17
	st	%r17, [%sp+108]
	ld	[%r10+4], %r17
	st	%r17, [%sp+104]
sumarm_156083:
	ld	[%sp+108], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+100]
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_157216:
	st	%r8, [%sp+96]
code_157188:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157189
	nop
code_157190:
	call	GCFromML ! delay slot empty
	nop
needgc_157189:
	ld	[%sp+96], %r16
	ld	[%sp+104], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	bl	cmpsi_157192
	nop
code_157193:
	or	%r0, 0, %r8
cmpsi_157192:
	cmp	%r8, 0
	bne	one_case_156105
	nop
zero_case_156104:
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r11
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_157217:
code_157195:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157196
	nop
code_157197:
	call	GCFromML ! delay slot empty
	nop
needgc_157196:
sumarm_156119:
	cmp	%r8, 0
	bne	sumarm_156120
	nop
code_157199:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(string_147410), %r8
	or	%r8, %lo(string_147410), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_156116 ! delay slot empty
	nop
sumarm_156120:
	ld	[%r8], %r9
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	ba	funtop_156061
	mov	%r8, %r10
code_157202:
	! done making self tail call
	ba	after_sum_156116
	or	%r0, 0, %r8
sumarm_156138:
after_sum_156116:
	ba	after_zeroone_156106 ! delay slot empty
	nop
one_case_156105:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 0, %r12
	! making closure call 
	sethi	%hi(strbindvar_r_extract_136245), %r8
	or	%r8, %lo(strbindvar_r_extract_136245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r13, %r15
	ld	[%sp+104], %r11
code_157215:
	mov	%r8, %r9
code_157207:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157208
	nop
code_157209:
	call	GCFromML ! delay slot empty
	nop
needgc_157208:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_156106:
	ba	after_sum_156080 ! delay slot empty
	nop
sumarm_156084:
after_sum_156080:
	ba	after_sum_156066 ! delay slot empty
	nop
sumarm_156070:
after_sum_156066:
code_157214:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_get_code_146467,(.-TextIOFn_get_code_146467)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157189
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01440000
		! -------- label,sizes,reg
	.long needgc_157196
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
		! -------- label,sizes,reg
	.long needgc_157208
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157215
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157216
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01440000
		! -------- label,sizes,reg
	.long code_157217
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.text
 	.align 8
	.global TextIOFn_generalizedInput_code_146462
 ! arguments : [$146464,$8] [$146465,$9] [$132982,$10] 
 ! results    : [$156055,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
TextIOFn_generalizedInput_code_146462:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157226
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157226:
	st	%r15, [%sp+92]
code_157219:
funtop_156049:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157220
	nop
code_157221:
	call	GCFromML ! delay slot empty
	nop
needgc_157220:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_get_code_146467), %r8
	or	%r8, %lo(TextIOFn_get_code_146467), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_157225:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_generalizedInput_code_146462,(.-TextIOFn_generalizedInput_code_146462)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157220
	.word 0xb8003006
	.word 0xbffc3c00
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_terminate_code_146474
 ! arguments : [$146476,$8] [$146477,$9] [$133008,$10] 
 ! results    : [$156048,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_terminate_code_146474:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157253
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157253:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
	mov	%r10, %r8
code_157227:
funtop_155993:
sumarm_156001:
	ld	[%r8], %r10
	ld	[%r8+12], %r8
	! ptr sub start
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	! ptr sub end
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! ptr sub end
sumarm_156023:
	cmp	%r8, 0
	bne	sumarm_156024
	nop
code_157228:
	! making closure call 
	sethi	%hi(_141006), %r8
	or	%r8, %lo(_141006), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157252:
code_157231:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_157235
	nop
code_157236:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_157235:
	ld	[%r2+800], %r10
	ld	[%sp+96], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r16
	ld	[%sp+100], %r17
	st	%r17, [%r16]
	ba	after_sum_156020
	or	%r0, 256, %r8
sumarm_156024:
	cmp	%r8, 1
	bne	sumarm_156041
	nop
code_157245:
	ba	after_sum_156020
	or	%r0, 256, %r8
sumarm_156041:
nomatch_sum_156021:
	sethi	%hi(record_147490), %r8
	or	%r8, %lo(record_147490), %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_156020:
	ba	after_sum_155998 ! delay slot empty
	nop
sumarm_156002:
after_sum_155998:
code_157251:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_terminate_code_146474,(.-TextIOFn_terminate_code_146474)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157252
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long afterMutateCheck_157235
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_findEOS_code_146481
 ! arguments : [$146483,$8] [$146484,$9] [$133021,$10] 
 ! results    : [$155992,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_findEOS_code_146481:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157268
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157268:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_157254:
funtop_155935:
sumarm_155943:
	ld	[%sp+96], %r17
	ld	[%r17+8], %r10
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! ptr sub start
	ld	[%r8], %r9
	! ptr sub end
sumarm_155961:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	ble	nomatch_sum_155959
	nop
code_157255:
	ld	[%r9], %r8
	! making direct call 
	ba	funtop_155935
	st	%r8, [%sp+96]
code_157256:
	! done making self tail call
	ba	after_sum_155958
	or	%r0, 0, %r8
sumarm_155962:
nomatch_sum_155959:
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157267:
	mov	%r8, %r9
code_157260:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157261
	nop
code_157262:
	call	GCFromML ! delay slot empty
	nop
needgc_157261:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_sum_155958:
	ba	after_sum_155940 ! delay slot empty
	nop
sumarm_155944:
after_sum_155940:
code_157266:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_findEOS_code_146481,(.-TextIOFn_findEOS_code_146481)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157261
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157267
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146491
 ! arguments : [$146493,$8] [$146494,$9] [$144876,$10] 
 ! results    : [$155934,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146491:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157284
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157284:
	st	%r15, [%sp+92]
	mov	%r9, %r8
	mov	%r10, %r18
code_157269:
funtop_155876:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157270
	nop
code_157271:
	call	GCFromML ! delay slot empty
	nop
needgc_157270:
	ld	[%r8], %r12
	ld	[%r8+4], %r9
	ld	[%r8+8], %r10
sumarm_155890:
	ld	[%r18+4], %r8
	! ptr sub start
	ld	[%r8], %r8
	! ptr sub end
sumarm_155906:
	cmp	%r8, 0
	bne	sumarm_155907
	nop
code_157273:
	sethi	%hi(string_147496), %r8
	or	%r8, %lo(string_147496), %r11
	! making closure call 
	ld	[%r12], %r13
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	mov	%r18, %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 96, %sp
code_157275:
	! done making tail call
	ba	after_sum_155903 ! delay slot empty
	nop
sumarm_155907:
	cmp	%r8, 1
	bne	sumarm_155920
	nop
code_157277:
	ba	after_sum_155903
	mov	%r9, %r8
sumarm_155920:
	ld	[%r8], %r9
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_155903 ! delay slot empty
	nop
sumarm_155923:
after_sum_155903:
	ba	after_sum_155887 ! delay slot empty
	nop
sumarm_155891:
after_sum_155887:
code_157282:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146491,(.-TextIOFn_anonfun_code_146491)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157270
	.word 0xb8003006
	.word 0x00040100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_input_code_146486
 ! arguments : [$146488,$8] [$146489,$9] [$133032,$10] 
 ! results    : [$155875,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_input_code_146486:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157299
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157299:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_157285:
funtop_155802:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157286
	nop
code_157287:
	call	GCFromML ! delay slot empty
	nop
needgc_157286:
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
sumarm_155816:
	ld	[%sp+96], %r17
	ld	[%r17], %r9
sumarm_155828:
	ld	[%r9+12], %r9
sumarm_155840:
	ba	after_sum_155837
	ld	[%r9+24], %r9
sumarm_155841:
after_sum_155837:
	ba	after_sum_155825
	mov	%r9, %r10
sumarm_155829:
after_sum_155825:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r9
	st	%r9, [%r4]
	st	%r12, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146491), %r9
	or	%r9, %lo(TextIOFn_anonfun_code_146491), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	ld	[%r8], %r11
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	mov	%r10, %r8
	jmpl	%r11, %r15
	mov	%r12, %r10
code_157297:
	mov	%r8, %r9
code_157292:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_157293:
	! done making tail call
	ba	after_sum_155813 ! delay slot empty
	nop
sumarm_155817:
after_sum_155813:
code_157296:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_input_code_146486,(.-TextIOFn_input_code_146486)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157297
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157286
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_input1_code_146506
 ! arguments : [$146508,$8] [$146509,$9] [$133039,$10] 
 ! results    : [$155801,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_input1_code_146506:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157344
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157344:
	st	%r15, [%sp+92]
	st	%r9, [%sp+120]
code_157300:
funtop_155604:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157301
	nop
code_157302:
	call	GCFromML ! delay slot empty
	nop
needgc_157301:
	ld	[%sp+120], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+116]
	ld	[%sp+120], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+112]
sumarm_155616:
	ld	[%r10], %r17
	st	%r17, [%sp+108]
	ld	[%r10+4], %r17
	st	%r17, [%sp+104]
sumarm_155630:
	ld	[%sp+108], %r17
	ld	[%r17+4], %r10
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_155627 ! delay slot empty
	nop
sumarm_155631:
after_sum_155627:
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_157341:
code_157307:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157308
	nop
code_157309:
	call	GCFromML ! delay slot empty
	nop
needgc_157308:
	ld	[%sp+104], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	bl	cmpsi_157311
	nop
code_157312:
	or	%r0, 0, %r8
cmpsi_157311:
	cmp	%r8, 0
	bne	one_case_155664
	nop
zero_case_155663:
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! ptr sub end
sumarm_155676:
	cmp	%r8, 0
	bne	sumarm_155677
	nop
sumarm_155684:
	ld	[%sp+108], %r17
	ld	[%r17+12], %r8
sumarm_155696:
	ba	after_sum_155693
	ld	[%r8+24], %r8
sumarm_155697:
after_sum_155693:
	ba	after_sum_155681
	mov	%r8, %r10
sumarm_155685:
after_sum_155681:
	sethi	%hi(string_147503), %r8
	or	%r8, %lo(string_147503), %r11
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r13
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+108], %r12
code_157342:
code_157318:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157319
	nop
code_157320:
	call	GCFromML ! delay slot empty
	nop
needgc_157319:
sumarm_155719:
	cmp	%r8, 0
	bne	sumarm_155720
	nop
code_157322:
	ba	after_sum_155716
	ld	[%sp+112], %r8
sumarm_155720:
	ld	[%r8], %r9
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	ba	funtop_155604
	mov	%r8, %r10
code_157324:
	! done making self tail call
	ba	after_sum_155716
	or	%r0, 0, %r8
sumarm_155724:
after_sum_155716:
	ba	after_sum_155673 ! delay slot empty
	nop
sumarm_155677:
	cmp	%r8, 1
	bne	sumarm_155744
	nop
code_157327:
	ba	after_sum_155673
	ld	[%sp+112], %r8
sumarm_155744:
	ld	[%r8], %r9
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	ba	funtop_155604
	mov	%r8, %r10
code_157329:
	! done making self tail call
	ba	after_sum_155673
	or	%r0, 0, %r8
sumarm_155747:
after_sum_155673:
	ba	after_zeroone_155665 ! delay slot empty
	nop
one_case_155664:
	! making closure call 
	sethi	%hi(strbindvar_r_sub_136246), %r8
	or	%r8, %lo(strbindvar_r_sub_136246), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_157343:
	mov	%r8, %r10
code_157334:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157335
	nop
code_157336:
	call	GCFromML ! delay slot empty
	nop
needgc_157335:
	ld	[%sp+104], %r17
	addcc	%r17, 1, %r9
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_155665:
	ba	after_sum_155613 ! delay slot empty
	nop
sumarm_155617:
after_sum_155613:
code_157340:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_input1_code_146506,(.-TextIOFn_input1_code_146506)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157341
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15450000
		! -------- label,sizes,reg
	.long code_157342
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x11000000
		! -------- label,sizes,reg
	.long needgc_157301
	.word 0xb8004006
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
		! -------- label,sizes,reg
	.long needgc_157308
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15450000
		! -------- label,sizes,reg
	.long needgc_157319
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x11000000
		! -------- label,sizes,reg
	.long needgc_157335
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_157343
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
 	.align 8
	.global TextIOFn_inputList_code_146515
 ! arguments : [$146517,$8] [$146518,$9] [$141058,$10] [$141059,$11] [$141060,$12] 
 ! results    : [$155603,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputList_code_146515:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157376
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157376:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
	st	%r10, [%sp+116]
	st	%r11, [%sp+112]
	st	%r12, [%sp+108]
code_157345:
funtop_155470:
sumarm_155478:
	ld	[%sp+116], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_157374:
code_157348:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157349
	nop
code_157350:
	call	GCFromML ! delay slot empty
	nop
needgc_157349:
	ld	[%sp+112], %r17
	subcc	%r8, %r17, %r17
	st	%r17, [%sp+100]
	ld	[%sp+108], %r16
	ld	[%sp+100], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	bge	cmpsi_157352
	nop
code_157353:
	or	%r0, 0, %r8
cmpsi_157352:
	cmp	%r8, 0
	bne	one_case_155503
	nop
zero_case_155502:
	or	%r0, 0, %r12
	! making closure call 
	sethi	%hi(strbindvar_r_extract_136245), %r8
	or	%r8, %lo(strbindvar_r_extract_136245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r13, %r15
	ld	[%sp+112], %r11
code_157375:
	st	%r8, [%sp+96]
code_157357:
	! done making normal call
	ld	[%sp+100], %r16
	ld	[%sp+108], %r17
	subcc	%r17, %r16, %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_157372:
code_157358:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157359
	nop
code_157360:
	call	GCFromML ! delay slot empty
	nop
needgc_157359:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
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
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_155504 ! delay slot empty
	nop
one_case_155503:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r12
	add	%r4, 8, %r4
	! done allocating 1 record
	! making closure call 
	sethi	%hi(strbindvar_r_extract_136245), %r8
	or	%r8, %lo(strbindvar_r_extract_136245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r13, %r15
	ld	[%sp+112], %r11
code_157373:
	mov	%r8, %r9
code_157365:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157366
	nop
code_157367:
	call	GCFromML ! delay slot empty
	nop
needgc_157366:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	ld	[%sp+108], %r16
	ld	[%sp+112], %r17
	addcc	%r17, %r16, %r9
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_155504:
	ba	after_sum_155475 ! delay slot empty
	nop
sumarm_155479:
after_sum_155475:
code_157371:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_inputList_code_146515,(.-TextIOFn_inputList_code_146515)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157349
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04110000
		! -------- label,sizes,reg
	.long code_157372
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157359
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157366
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
		! -------- label,sizes,reg
	.long code_157373
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
		! -------- label,sizes,reg
	.long code_157374
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04110000
		! -------- label,sizes,reg
	.long code_157375
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04100000
	.text
 	.align 8
	.global TextIOFn_nextBuf_code_146520
 ! arguments : [$146522,$8] [$146523,$9] [$141107,$10] [$141108,$11] 
 ! results    : [$155469,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_nextBuf_code_146520:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157412
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157412:
	st	%r15, [%sp+92]
	mov	%r9, %r8
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_157377:
funtop_155313:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
sumarm_155325:
	ld	[%sp+108], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+96]
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	! ptr sub start
	ld	[%r8], %r8
	! ptr sub end
sumarm_155343:
	cmp	%r8, 0
	bne	sumarm_155344
	nop
sumarm_155351:
	ld	[%sp+108], %r17
	ld	[%r17+12], %r8
sumarm_155363:
	ba	after_sum_155360
	ld	[%r8+24], %r8
sumarm_155364:
after_sum_155360:
	ba	after_sum_155348
	mov	%r8, %r10
sumarm_155352:
after_sum_155348:
	sethi	%hi(string_147515), %r8
	or	%r8, %lo(string_147515), %r11
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+108], %r12
code_157407:
code_157382:
	! done making normal call
sumarm_155386:
	cmp	%r8, 0
	bne	sumarm_155387
	nop
code_157383:
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_157408:
	mov	%r8, %r9
code_157386:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157387
	nop
code_157388:
	call	GCFromML ! delay slot empty
	nop
needgc_157387:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_155383 ! delay slot empty
	nop
sumarm_155387:
	ld	[%r8], %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_157391:
	! done making tail call
	ba	after_sum_155383 ! delay slot empty
	nop
sumarm_155414:
after_sum_155383:
	ba	after_sum_155340 ! delay slot empty
	nop
sumarm_155344:
	cmp	%r8, 1
	bne	sumarm_155429
	nop
code_157394:
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_157409:
	mov	%r8, %r9
code_157397:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157398
	nop
code_157399:
	call	GCFromML ! delay slot empty
	nop
needgc_157398:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_155340 ! delay slot empty
	nop
sumarm_155429:
	ld	[%r8], %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_157402:
	! done making tail call
	ba	after_sum_155340 ! delay slot empty
	nop
sumarm_155455:
after_sum_155340:
	ba	after_sum_155322 ! delay slot empty
	nop
sumarm_155326:
after_sum_155322:
code_157406:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_nextBuf_code_146520,(.-TextIOFn_nextBuf_code_146520)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157407
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
		! -------- label,sizes,reg
	.long code_157408
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long needgc_157387
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_157409
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long needgc_157398
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
 	.align 8
	.global TextIOFn_inputN_code_146531
 ! arguments : [$146533,$8] [$146534,$9] [$141049,$10] [$141050,$11] 
 ! results    : [$155312,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputN_code_146531:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157426
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157426:
	st	%r15, [%sp+92]
	mov	%r10, %r8
	mov	%r11, %r12
code_157413:
funtop_155266:
sumarm_155274:
	ld	[%r8], %r10
	ld	[%r8+4], %r11
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_157425:
code_157414:
	! done making normal call
	ld	[%r8], %r10
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_concat_136787), %r8
	or	%r8, %lo(strbindvar_r_concat_136787), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157424:
	mov	%r8, %r9
code_157417:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157418
	nop
code_157419:
	call	GCFromML ! delay slot empty
	nop
needgc_157418:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_155271 ! delay slot empty
	nop
sumarm_155275:
after_sum_155271:
code_157423:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_inputN_code_146531,(.-TextIOFn_inputN_code_146531)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157424
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157418
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157425
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146552
 ! arguments : [$146554,$8] [$146555,$9] [$132976,$10] 
 ! results    : [$155265,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146552:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157436
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157436:
	st	%r15, [%sp+92]
code_157427:
funtop_155235:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
	ld	[%r9+8], %r8
	addcc	%r10, %r8, %r10
	! making closure call 
	sethi	%hi(quot_136425), %r8
	or	%r8, %lo(quot_136425), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r11
code_157435:
code_157430:
	! done making normal call
	ld	[%sp+96], %r17
	smulcc	%r8, %r17, %r10
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
code_157431:
	! done making tail call
code_157433:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_anonfun_code_146552,(.-TextIOFn_anonfun_code_146552)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157435
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146563
 ! arguments : [$146565,$8] [$146566,$9] [$132979,$10] 
 ! results    : [$155234,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146563:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157442
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157442:
	st	%r15, [%sp+92]
code_157437:
funtop_155226:
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_157438:
	! done making tail call
code_157440:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146563,(.-TextIOFn_anonfun_code_146563)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_bigChunk_inner_code_146543
 ! arguments : [$146545,$8] [$146546,$9] [$133160,$10] 
 ! results    : [$155225,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_bigChunk_inner_code_146543:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157470
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157470:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_157443:
funtop_155088:
	ld	[%r8], %r17
	st	%r17, [%sp+104]
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	ld	[%r8+8], %r9
	ld	[%r8+12], %r17
	st	%r17, [%sp+96]
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_157469:
code_157444:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157445
	nop
code_157446:
	call	GCFromML ! delay slot empty
	nop
needgc_157445:
sumarm_155113:
	cmp	%r8, 0
	bne	sumarm_155114
	nop
code_157448:
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_157468:
code_157449:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157450
	nop
code_157451:
	call	GCFromML ! delay slot empty
	nop
needgc_157450:
	ba	after_sum_155110
	mov	%r8, %r10
sumarm_155114:
	ld	[%r8], %r8
	ba	after_sum_155110
	mov	%r8, %r10
sumarm_155124:
after_sum_155110:
sumarm_155137:
	ld	[%sp+96], %r17
	ld	[%r17+12], %r8
	ba	after_sum_155134
	mov	%r8, %r9
sumarm_155138:
after_sum_155134:
sumarm_155150:
	ld	[%r9+4], %r8
	ld	[%r9+24], %r11
sumarm_155164:
	ld	[%r8+36], %r9
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_155161 ! delay slot empty
	nop
sumarm_155165:
after_sum_155161:
	ba	after_sum_155147 ! delay slot empty
	nop
sumarm_155151:
after_sum_155147:
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	subcc	%r11, 1, %r9
	cmp	%r9, 0
	or	%r0, 1, %r8
	be	cmpui_157458
	nop
code_157459:
	or	%r0, 0, %r8
cmpui_157458:
	cmp	%r8, 0
	bne	one_case_155191
	nop
zero_case_155190:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 281, %r8
	st	%r8, [%r4]
	st	%r12, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146552), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_146552), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	after_zeroone_155192
	mov	%r8, %r9
one_case_155191:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146563), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_146563), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	mov	%r8, %r9
after_zeroone_155192:
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_157464:
	! done making tail call
code_157466:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_bigChunk_inner_code_146543,(.-TextIOFn_bigChunk_inner_code_146543)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157445
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_157468
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157450
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157469
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146574
 ! arguments : [$146576,$8] [$146577,$9] [$144895,$10] 
 ! results    : [$155087,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146574:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157486
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157486:
	st	%r15, [%sp+92]
	mov	%r10, %r18
code_157471:
funtop_155029:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157472
	nop
code_157473:
	call	GCFromML ! delay slot empty
	nop
needgc_157472:
	ld	[%r9], %r12
	ld	[%r9+4], %r10
	ld	[%r9+8], %r9
sumarm_155043:
	ld	[%r18+4], %r8
	! ptr sub start
	ld	[%r8], %r8
	! ptr sub end
sumarm_155059:
	cmp	%r8, 0
	bne	sumarm_155060
	nop
code_157475:
	sethi	%hi(string_147540), %r8
	or	%r8, %lo(string_147540), %r11
	! making closure call 
	ld	[%r12], %r13
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	mov	%r18, %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 96, %sp
code_157477:
	! done making tail call
	ba	after_sum_155056 ! delay slot empty
	nop
sumarm_155060:
	cmp	%r8, 1
	bne	sumarm_155073
	nop
code_157479:
	ba	after_sum_155056
	mov	%r9, %r8
sumarm_155073:
	ld	[%r8], %r9
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_155056 ! delay slot empty
	nop
sumarm_155076:
after_sum_155056:
	ba	after_sum_155040 ! delay slot empty
	nop
sumarm_155044:
after_sum_155040:
code_157484:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146574,(.-TextIOFn_anonfun_code_146574)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157472
	.word 0xb8003006
	.word 0x00040200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_loop_code_146586
 ! arguments : [$146588,$8] [$146589,$9] [$141163,$10] [$141164,$11] 
 ! results    : [$155026,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_loop_code_146586:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157505
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157505:
	st	%r15, [%sp+92]
	st	%r8, [%sp+100]
	st	%r9, [%sp+96]
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_157487:
funtop_154977:
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_157503:
code_157490:
	! done making normal call
	cmp	%r8, 0
	or	%r0, 1, %r8
	be	cmpui_157491
	nop
code_157492:
	or	%r0, 0, %r8
cmpui_157491:
	cmp	%r8, 0
	bne	one_case_154995
	nop
zero_case_154994:
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_157504:
code_157494:
	! done making normal call
	ld	[%r8], %r10
	ld	[%r8+4], %r11
	! making direct call 
	ld	[%sp+100], %r8
	call	TextIOFn_loop_code_146586
	ld	[%sp+96], %r9
code_157502:
	mov	%r8, %r9
code_157495:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157496
	nop
code_157497:
	call	GCFromML ! delay slot empty
	nop
needgc_157496:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_154996 ! delay slot empty
	nop
one_case_154995:
	or	%r0, 0, %r8
after_zeroone_154996:
code_157501:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_loop_code_146586,(.-TextIOFn_loop_code_146586)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157502
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long needgc_157496
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_157503
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_157504
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
	.text
 	.align 8
	.global TextIOFn_inputAll_code_146538
 ! arguments : [$146540,$8] [$146541,$9] [$133140,$10] 
 ! results    : [$154976,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputAll_code_146538:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157546
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157546:
	st	%r15, [%sp+92]
	st	%r10, [%sp+116]
code_157506:
funtop_154808:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157507
	nop
code_157508:
	call	GCFromML ! delay slot empty
	nop
needgc_157507:
	ld	[%r9], %r10
	ld	[%r9+4], %r17
	st	%r17, [%sp+104]
	ld	[%r9+8], %r17
	st	%r17, [%sp+100]
	ld	[%r9+12], %r17
	st	%r17, [%sp+112]
	ld	[%r9+16], %r17
	st	%r17, [%sp+96]
sumarm_154826:
	ld	[%sp+116], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+108]
sumarm_154838:
	ld	[%sp+108], %r17
	ba	after_sum_154835
	ld	[%r17+12], %r8
sumarm_154839:
after_sum_154835:
sumarm_154851:
	ld	[%r8+4], %r8
sumarm_154863:
	ld	[%r8+20], %r9
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_154860 ! delay slot empty
	nop
sumarm_154864:
after_sum_154860:
	ba	after_sum_154848 ! delay slot empty
	nop
sumarm_154852:
after_sum_154848:
	ld	[%r8], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_bigChunk_inner_code_146543), %r8
	or	%r8, %lo(TextIOFn_bigChunk_inner_code_146543), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 2, %r9
	sethi	%hi(vector_132792), %r8
	or	%r8, %lo(vector_132792), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call 
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_157540:
	mov	%r8, %r9
code_157518:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157519
	nop
code_157520:
	call	GCFromML ! delay slot empty
	nop
needgc_157519:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146574), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_146574), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_157544:
	mov	%r8, %r9
code_157523:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157524
	nop
code_157525:
	call	GCFromML ! delay slot empty
	nop
needgc_157524:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_loop_code_146586), %r8
	or	%r8, %lo(TextIOFn_loop_code_146586), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_157545:
code_157528:
	! done making normal call
	ld	[%r8], %r10
	ld	[%r8+4], %r11
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_157541:
	mov	%r8, %r10
code_157529:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_concat_136787), %r8
	or	%r8, %lo(strbindvar_r_concat_136787), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157542:
	st	%r8, [%sp+96]
code_157532:
	! done making normal call
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r11
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_157543:
	mov	%r8, %r9
code_157533:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157534
	nop
code_157535:
	call	GCFromML ! delay slot empty
	nop
needgc_157534:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_154823 ! delay slot empty
	nop
sumarm_154827:
after_sum_154823:
code_157539:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_inputAll_code_146538,(.-TextIOFn_inputAll_code_146538)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157540
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05550000
		! -------- label,sizes,reg
	.long needgc_157507
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
		! -------- label,sizes,reg
	.long needgc_157519
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05550000
		! -------- label,sizes,reg
	.long needgc_157524
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05400000
		! -------- label,sizes,reg
	.long code_157541
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
		! -------- label,sizes,reg
	.long code_157542
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
		! -------- label,sizes,reg
	.long code_157543
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157534
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157544
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05400000
		! -------- label,sizes,reg
	.long code_157545
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01410000
	.text
 	.align 8
	.global TextIOFn_inputExn_inner_code_146599
 ! arguments : [$146601,$8] [$146602,$9] [$144940,$10] [$144941,$11] [$144942,$12] 
 ! results    : [$154807,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputExn_inner_code_146599:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157557
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157557:
	st	%r15, [%sp+92]
code_157547:
funtop_154767:
sumarm_154775:
	ld	[%r10+4], %r8
sumarm_154787:
	ld	[%r8+24], %r10
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_157556:
code_157550:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_154784
	or	%r0, 0, %r8
sumarm_154788:
after_sum_154784:
	ba	after_sum_154772 ! delay slot empty
	nop
sumarm_154776:
after_sum_154772:
code_157555:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_inputExn_inner_code_146599,(.-TextIOFn_inputExn_inner_code_146599)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157556
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_closeIn_code_146604
 ! arguments : [$146606,$8] [$146607,$9] [$133270,$10] 
 ! results    : [$154766,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_closeIn_code_146604:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157599
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157599:
	st	%r15, [%sp+92]
	mov	%r9, %r12
code_157558:
funtop_154628:
sumarm_154636:
	ld	[%r10], %r8
sumarm_154648:
	ld	[%r8+12], %r8
	ba	after_sum_154645
	st	%r8, [%sp+104]
sumarm_154649:
after_sum_154645:
sumarm_154661:
	ld	[%sp+104], %r17
	ld	[%r17+4], %r9
	ld	[%sp+104], %r17
	ld	[%r17+20], %r17
	st	%r17, [%sp+100]
	! ptr sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r8
	! ptr sub end
sumarm_154679:
	cmp	%r8, 1
	bne	sumarm_154680
	nop
code_157560:
	ba	after_sum_154676
	or	%r0, 256, %r8
sumarm_154680:
nomatch_sum_154677:
sumarm_154691:
	ld	[%r9+52], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_157597:
code_157562:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_157566
	nop
code_157567:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_157566:
	or	%r0, 1, %r11
	ld	[%r2+800], %r10
	ld	[%sp+100], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%sp+100], %r17
	ld	[%r17], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+100], %r17
	st	%r11, [%r17]
	! allocating 1-record
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157575
	nop
code_157576:
	call	GCFromML ! delay slot empty
	nop
needgc_157575:
	or	%r0, 265, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r11
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(exn_handler_154708), %r8
	or	%r8, %lo(exn_handler_154708), %r10
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
	! making closure polycall
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_157596:
code_157580:
	! done making normal call
	ba	exn_handler_after_154709
	ld	[%r1+12], %r1
exn_handler_154708:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r17
	st	%r17, [%sp+104]
	mov	%r15, %r12
sumarm_154730:
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
sumarm_154742:
	ld	[%r8+24], %r10
	sethi	%hi(string_147579), %r8
	or	%r8, %lo(string_147579), %r11
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_157598:
code_157587:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_154739
	or	%r0, 0, %r8
sumarm_154743:
after_sum_154739:
	ba	after_sum_154727 ! delay slot empty
	nop
sumarm_154731:
after_sum_154727:
exn_handler_after_154709:
	ba	after_sum_154688 ! delay slot empty
	nop
sumarm_154692:
after_sum_154688:
after_sum_154676:
	ba	after_sum_154658 ! delay slot empty
	nop
sumarm_154662:
after_sum_154658:
	ba	after_sum_154633 ! delay slot empty
	nop
sumarm_154637:
after_sum_154633:
code_157595:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_closeIn_code_146604,(.-TextIOFn_closeIn_code_146604)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_157566
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_157575
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
		! -------- label,sizes,reg
	.long code_157596
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157597
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_157598
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_endOfStream_code_146611
 ! arguments : [$146613,$8] [$146614,$9] [$133298,$10] 
 ! results    : [$154627,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_endOfStream_code_146611:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157627
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157627:
	st	%r15, [%sp+92]
	st	%r9, [%sp+112]
code_157600:
funtop_154463:
sumarm_154471:
	ld	[%r10], %r17
	st	%r17, [%sp+108]
	ld	[%r10+4], %r17
	st	%r17, [%sp+104]
sumarm_154485:
	ld	[%sp+108], %r17
	ld	[%r17+12], %r11
	ld	[%sp+108], %r17
	ld	[%r17+8], %r10
	ld	[%sp+108], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+100]
	! ptr sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r9
	! ptr sub end
sumarm_154505:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	ble	nomatch_sum_154503
	nop
code_157601:
	ba	after_sum_154502
	or	%r0, 0, %r8
sumarm_154506:
nomatch_sum_154503:
sumarm_154518:
	ld	[%r11+20], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157626:
code_157605:
	! done making normal call
	ld	[%sp+104], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	be	cmpui_157606
	nop
code_157607:
	or	%r0, 0, %r8
cmpui_157606:
	cmp	%r8, 0
	bne	one_case_154540
	nop
zero_case_154539:
	ba	after_zeroone_154541
	or	%r0, 0, %r8
one_case_154540:
	! ptr sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r9
	! ptr sub end
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! ptr sub end
sumarm_154559:
	cmp	%r9, 0
	bne	sumarm_154560
	nop
sumarm_154568:
	cmp	%r8, 0
	bne	sumarm_154569
	nop
sumarm_154576:
	ld	[%sp+108], %r17
	ld	[%r17+12], %r8
sumarm_154588:
	ba	after_sum_154585
	ld	[%r8+24], %r8
sumarm_154589:
after_sum_154585:
	ba	after_sum_154573
	mov	%r8, %r10
sumarm_154577:
after_sum_154573:
	sethi	%hi(string_147592), %r8
	or	%r8, %lo(string_147592), %r11
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r13
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+108], %r12
code_157625:
code_157615:
	! done making normal call
sumarm_154611:
	cmp	%r8, 0
	bne	sumarm_154612
	nop
code_157616:
	ba	after_sum_154608
	or	%r0, 1, %r8
sumarm_154612:
nomatch_sum_154609:
	or	%r0, 0, %r8
after_sum_154608:
	ba	after_sum_154565 ! delay slot empty
	nop
sumarm_154569:
nomatch_sum_154566:
	or	%r0, 1, %r8
after_sum_154565:
	ba	after_sum_154556 ! delay slot empty
	nop
sumarm_154560:
nomatch_sum_154557:
	or	%r0, 1, %r8
after_sum_154556:
after_zeroone_154541:
	ba	after_sum_154515 ! delay slot empty
	nop
sumarm_154519:
after_sum_154515:
after_sum_154502:
	ba	after_sum_154482 ! delay slot empty
	nop
sumarm_154486:
after_sum_154482:
	ba	after_sum_154468 ! delay slot empty
	nop
sumarm_154472:
after_sum_154468:
code_157624:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_endOfStream_code_146611,(.-TextIOFn_endOfStream_code_146611)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157625
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157626
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01450000
	.text
 	.align 8
	.global TextIOFn_getData_code_146618
 ! arguments : [$146620,$8] [$146621,$9] [$133480,$10] 
 ! results    : [$154460,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getData_code_146618:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157639
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157639:
	st	%r15, [%sp+92]
	mov	%r8, %r12
	mov	%r9, %r11
code_157628:
funtop_154410:
sumarm_154418:
	or	%r0, 255, %r8
	cmp	%r10, %r8
	ble	nomatch_sum_154416
	nop
code_157629:
	ld	[%r10], %r9
sumarm_154430:
	ld	[%r9+4], %r8
	ld	[%r9+8], %r17
	st	%r17, [%sp+96]
	! ptr sub start
	ld	[%r8], %r10
	! ptr sub end
	! making direct call 
	mov	%r12, %r8
	call	TextIOFn_getData_code_146618
	mov	%r11, %r9
code_157638:
	mov	%r8, %r9
code_157630:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157631
	nop
code_157632:
	call	GCFromML ! delay slot empty
	nop
needgc_157631:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_154427 ! delay slot empty
	nop
sumarm_154431:
after_sum_154427:
	ba	after_sum_154415 ! delay slot empty
	nop
sumarm_154419:
nomatch_sum_154416:
	or	%r0, 0, %r8
after_sum_154415:
code_157637:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_getData_code_146618,(.-TextIOFn_getData_code_146618)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157631
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157638
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_getReader_code_146623
 ! arguments : [$146625,$8] [$146626,$9] [$133458,$10] 
 ! results    : [$154409,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getReader_code_146623:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157684
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157684:
	st	%r15, [%sp+92]
code_157640:
funtop_154245:
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157641
	nop
code_157642:
	call	GCFromML ! delay slot empty
	nop
needgc_157641:
	ld	[%r9], %r8
	ld	[%r9+4], %r17
	st	%r17, [%sp+108]
sumarm_154257:
	ld	[%r10], %r9
	ld	[%r10+4], %r17
	st	%r17, [%sp+104]
sumarm_154271:
	ld	[%r9+4], %r13
	ld	[%r9+12], %r12
	ld	[%r9+8], %r11
sumarm_154287:
	ld	[%r12+4], %r10
	! allocating 4-record
	or	%r0, 3873, %r9
	st	%r9, [%r4]
	st	%r11, [%r4+4]
	st	%r12, [%r4+8]
	st	%r10, [%r4+12]
	st	%r13, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	ba	after_sum_154284 ! delay slot empty
	nop
sumarm_154288:
after_sum_154284:
	ba	after_sum_154268 ! delay slot empty
	nop
sumarm_154272:
after_sum_154268:
	ld	[%r9], %r17
	st	%r17, [%sp+96]
	ld	[%r9+4], %r12
	ld	[%r9+8], %r17
	st	%r17, [%sp+112]
	ld	[%r9+12], %r17
	st	%r17, [%sp+100]
	! making closure call 
	ld	[%r8], %r11
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	mov	%r10, %r8
	jmpl	%r11, %r15
	mov	%r12, %r10
code_157677:
code_157646:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_157678:
code_157649:
	! done making normal call
	ld	[%sp+104], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	bl	cmpsi_157650
	nop
code_157651:
	or	%r0, 0, %r8
cmpsi_157650:
	cmp	%r8, 0
	bne	one_case_154332
	nop
zero_case_154331:
	! ptr sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r10
	! ptr sub end
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_157683:
	mov	%r8, %r10
code_157653:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_concat_136787), %r8
	or	%r8, %lo(strbindvar_r_concat_136787), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157679:
	mov	%r8, %r9
code_157656:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157657
	nop
code_157658:
	call	GCFromML ! delay slot empty
	nop
needgc_157657:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_154333 ! delay slot empty
	nop
one_case_154332:
	or	%r0, 0, %r12
	! making closure call 
	sethi	%hi(strbindvar_r_extract_136245), %r8
	or	%r8, %lo(strbindvar_r_extract_136245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r13, %r15
	ld	[%sp+104], %r11
code_157682:
	st	%r8, [%sp+96]
code_157663:
	! done making normal call
	! ptr sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r10
	! ptr sub end
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_157680:
	mov	%r8, %r9
code_157664:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157665
	nop
code_157666:
	call	GCFromML ! delay slot empty
	nop
needgc_157665:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! making closure call 
	sethi	%hi(strbindvar_r_concat_136787), %r8
	or	%r8, %lo(strbindvar_r_concat_136787), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157681:
	mov	%r8, %r9
code_157670:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157671
	nop
code_157672:
	call	GCFromML ! delay slot empty
	nop
needgc_157671:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_154333:
	ba	after_sum_154254 ! delay slot empty
	nop
sumarm_154258:
after_sum_154254:
code_157676:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_getReader_code_146623,(.-TextIOFn_getReader_code_146623)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157677
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01450000
		! -------- label,sizes,reg
	.long needgc_157641
	.word 0xb8004006
	.word 0x00000600
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157678
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01450000
		! -------- label,sizes,reg
	.long code_157679
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
		! -------- label,sizes,reg
	.long needgc_157657
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
		! -------- label,sizes,reg
	.long code_157680
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
		! -------- label,sizes,reg
	.long needgc_157665
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
		! -------- label,sizes,reg
	.long needgc_157671
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
		! -------- label,sizes,reg
	.long code_157681
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
		! -------- label,sizes,reg
	.long code_157682
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01440000
		! -------- label,sizes,reg
	.long code_157683
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.text
 	.align 8
	.global TextIOFn_getPosIn_code_146632
 ! arguments : [$146634,$8] [$146635,$9] [$133501,$10] 
 ! results    : [$154244,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getPosIn_code_146632:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157706
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157706:
	st	%r15, [%sp+92]
code_157685:
funtop_154138:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157686
	nop
code_157687:
	call	GCFromML ! delay slot empty
	nop
needgc_157686:
sumarm_154146:
	ld	[%r10], %r8
	ld	[%r10+4], %r11
sumarm_154160:
	ld	[%r8+12], %r10
	ld	[%r8], %r8
sumarm_154178:
	cmp	%r8, 0
	bne	sumarm_154179
	nop
sumarm_154187:
	ld	[%r10+4], %r8
sumarm_154199:
	ld	[%r8+24], %r10
	sethi	%hi(string_147649), %r8
	or	%r8, %lo(string_147649), %r11
	sethi	%hi(mk_137384), %r8
	or	%r8, %lo(mk_137384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_157705:
code_157695:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_154196
	or	%r0, 0, %r8
sumarm_154200:
after_sum_154196:
	ba	after_sum_154184 ! delay slot empty
	nop
sumarm_154188:
after_sum_154184:
	ba	after_sum_154175 ! delay slot empty
	nop
sumarm_154179:
	ld	[%r8], %r9
	! allocating 3-record
	or	%r0, 1049, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	ba	after_sum_154175 ! delay slot empty
	nop
sumarm_154225:
after_sum_154175:
	ba	after_sum_154157 ! delay slot empty
	nop
sumarm_154161:
after_sum_154157:
	ba	after_sum_154143 ! delay slot empty
	nop
sumarm_154147:
after_sum_154143:
code_157704:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_getPosIn_code_146632,(.-TextIOFn_getPosIn_code_146632)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157686
	.word 0xb8003006
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157705
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_readN_code_146642
 ! arguments : [$146644,$8] [$146645,$9] [$133568,$10] 
 ! results    : [$154135,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_readN_code_146642:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157735
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157735:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
	st	%r10, [%sp+104]
code_157707:
funtop_154043:
	ld	[%sp+100], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+96]
	ld	[%sp+100], %r17
	ld	[%r17+4], %r9
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	be	cmpui_157708
	nop
code_157709:
	or	%r0, 0, %r8
cmpui_157708:
	cmp	%r8, 0
	bne	one_case_154056
	nop
zero_case_154055:
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_157734:
	mov	%r8, %r10
code_157711:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157732:
	mov	%r8, %r9
code_157714:
	! done making normal call
	cmp	%r9, 0
	or	%r0, 1, %r8
	be	cmpui_157715
	nop
code_157716:
	or	%r0, 0, %r8
cmpui_157715:
	cmp	%r8, 0
	bne	one_case_154080
	nop
zero_case_154079:
	ld	[%sp+104], %r17
	subcc	%r17, %r9, %r8
	! making direct call 
	ba	funtop_154043
	st	%r8, [%sp+104]
code_157718:
	! done making self tail call
	ba	after_zeroone_154081
	or	%r0, 0, %r8
one_case_154080:
sumarm_154098:
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
sumarm_154110:
	ld	[%r8+24], %r10
	sethi	%hi(string_147706), %r8
	or	%r8, %lo(string_147706), %r11
	sethi	%hi(funarg_3_137516), %r8
	or	%r8, %lo(funarg_3_137516), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_157733:
code_157725:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_154107
	or	%r0, 0, %r8
sumarm_154111:
after_sum_154107:
	ba	after_sum_154095 ! delay slot empty
	nop
sumarm_154099:
after_sum_154095:
after_zeroone_154081:
	ba	after_zeroone_154057 ! delay slot empty
	nop
one_case_154056:
	or	%r0, 256, %r8
after_zeroone_154057:
code_157731:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_readN_code_146642,(.-TextIOFn_readN_code_146642)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157732
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_157733
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157734
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_filePosIn_code_146637
 ! arguments : [$146639,$8] [$146640,$9] [$133527,$10] 
 ! results    : [$154042,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_filePosIn_code_146637:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157774
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157774:
	st	%r15, [%sp+92]
code_157736:
funtop_153872:
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157737
	nop
code_157738:
	call	GCFromML ! delay slot empty
	nop
needgc_157737:
sumarm_153880:
	ld	[%r10], %r17
	st	%r17, [%sp+116]
	ld	[%r10+4], %r17
	st	%r17, [%sp+112]
	ld	[%r10+8], %r11
	ld	[%sp+112], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	be	cmpui_157740
	nop
code_157741:
	or	%r0, 0, %r8
cmpui_157740:
	cmp	%r8, 0
	bne	one_case_153897
	nop
zero_case_153896:
sumarm_153905:
	ld	[%r11+24], %r10
	ld	[%r11+4], %r9
sumarm_153919:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_153916 ! delay slot empty
	nop
sumarm_153920:
after_sum_153916:
	ba	after_sum_153902
	mov	%r8, %r9
sumarm_153906:
after_sum_153902:
	ld	[%r9], %r8
	ld	[%r9+4], %r9
	ld	[%r8+8], %r17
	st	%r17, [%sp+108]
	ld	[%r8+12], %r17
	st	%r17, [%sp+104]
sumarm_153948:
	or	%r0, 255, %r8
	ld	[%sp+108], %r17
	cmp	%r17, %r8
	ble	nomatch_sum_153946
	nop
sumarm_153967:
	or	%r0, 255, %r8
	ld	[%sp+104], %r17
	cmp	%r17, %r8
	ble	nomatch_sum_153965
	nop
code_157746:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_readN_code_146642), %r8
	or	%r8, %lo(TextIOFn_readN_code_146642), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure polycall
	ld	[%sp+108], %r17
	ld	[%r17], %r10
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_157768:
	st	%r8, [%sp+100]
code_157748:
	! done making normal call
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_157769:
code_157749:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_157770:
code_157750:
	! done making normal call
	! making closure polycall
	ld	[%sp+108], %r17
	ld	[%r17], %r10
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_157771:
	st	%r8, [%sp+96]
code_157751:
	! done making normal call
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_157772:
	mov	%r8, %r11
code_157752:
	! done making normal call
	! making closure call 
	sethi	%hi(_137527), %r8
	or	%r8, %lo(_137527), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_157755:
	! done making tail call
	ba	after_sum_153964 ! delay slot empty
	nop
sumarm_153968:
nomatch_sum_153965:
	sethi	%hi(_137467), %r8
	or	%r8, %lo(_137467), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_153964:
	ba	after_sum_153945 ! delay slot empty
	nop
sumarm_153949:
nomatch_sum_153946:
	sethi	%hi(_137467), %r8
	or	%r8, %lo(_137467), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_153945:
	ba	after_zeroone_153898
	mov	%r8, %r10
one_case_153897:
	ld	[%sp+116], %r10
after_zeroone_153898:
	ba	after_sum_153877
	mov	%r10, %r8
sumarm_153881:
after_sum_153877:
code_157767:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_filePosIn_code_146637,(.-TextIOFn_filePosIn_code_146637)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157737
	.word 0xb8004006
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157768
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_157769
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_157770
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long code_157771
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_157772
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_nextBuf_code_146651
 ! arguments : [$146653,$8] [$146654,$9] [$141460,$10] [$141461,$11] 
 ! results    : [$153871,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_nextBuf_code_146651:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157816
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157816:
	st	%r15, [%sp+92]
	mov	%r9, %r8
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_157775:
funtop_153700:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
sumarm_153712:
	ld	[%sp+104], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+100]
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	! ptr sub start
	ld	[%r8], %r8
	! ptr sub end
sumarm_153731:
	cmp	%r8, 0
	bne	sumarm_153732
	nop
sumarm_153739:
	ld	[%sp+104], %r17
	ld	[%r17+12], %r8
sumarm_153751:
	ba	after_sum_153748
	ld	[%r8+24], %r8
sumarm_153752:
after_sum_153748:
	ba	after_sum_153736
	mov	%r8, %r10
sumarm_153740:
after_sum_153736:
	sethi	%hi(string_147795), %r8
	or	%r8, %lo(string_147795), %r11
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+104], %r12
code_157811:
code_157780:
	! done making normal call
sumarm_153774:
	cmp	%r8, 0
	bne	sumarm_153775
	nop
code_157781:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	bne	one_case_153780
	nop
zero_case_153779:
	sethi	%hi(record_147778), %r8
	or	%r8, %lo(record_147778), %r8
	ba	after_zeroone_153781
	st	%r8, [%sp+96]
one_case_153780:
	or	%r0, 0, %r8
	st	%r8, [%sp+96]
after_zeroone_153781:
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_157812:
	mov	%r8, %r9
code_157787:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157788
	nop
code_157789:
	call	GCFromML ! delay slot empty
	nop
needgc_157788:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_153771 ! delay slot empty
	nop
sumarm_153775:
	ld	[%r8], %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_157792:
	! done making tail call
	ba	after_sum_153771 ! delay slot empty
	nop
sumarm_153810:
after_sum_153771:
	ba	after_sum_153728 ! delay slot empty
	nop
sumarm_153732:
	cmp	%r8, 1
	bne	sumarm_153824
	nop
code_157795:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	bne	one_case_153829
	nop
zero_case_153828:
	sethi	%hi(record_147778), %r8
	or	%r8, %lo(record_147778), %r8
	ba	after_zeroone_153830
	st	%r8, [%sp+96]
one_case_153829:
	or	%r0, 0, %r8
	st	%r8, [%sp+96]
after_zeroone_153830:
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_157813:
	mov	%r8, %r9
code_157801:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157802
	nop
code_157803:
	call	GCFromML ! delay slot empty
	nop
needgc_157802:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_153728 ! delay slot empty
	nop
sumarm_153824:
	ld	[%r8], %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_157806:
	! done making tail call
	ba	after_sum_153728 ! delay slot empty
	nop
sumarm_153858:
after_sum_153728:
	ba	after_sum_153709 ! delay slot empty
	nop
sumarm_153713:
after_sum_153709:
code_157810:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_nextBuf_code_146651,(.-TextIOFn_nextBuf_code_146651)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157811
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_157812
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
		! -------- label,sizes,reg
	.long code_157813
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
		! -------- label,sizes,reg
	.long needgc_157788
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
		! -------- label,sizes,reg
	.long needgc_157802
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.text
 	.align 8
	.global TextIOFn_scan_code_146665
 ! arguments : [$146667,$8] [$146668,$9] [$133673,$10] 
 ! results    : [$153655,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_scan_code_146665:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157852
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_157852:
	st	%r15, [%sp+92]
	st	%r9, [%sp+112]
	st	%r10, [%sp+116]
code_157817:
funtop_153548:
	ld	[%sp+112], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+108]
	ld	[%sp+112], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+104]
	ld	[%sp+112], %r17
	ld	[%r17+8], %r8
	ld	[%sp+112], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+100]
	ld	[%sp+112], %r17
	ld	[%r17+16], %r17
	st	%r17, [%sp+96]
	ld	[%sp+116], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	be	cmpui_157818
	nop
code_157819:
	or	%r0, 0, %r8
cmpui_157818:
	cmp	%r8, 0
	bne	one_case_153567
	nop
zero_case_153566:
	! making closure call 
	sethi	%hi(strbindvar_r_sub_136246), %r8
	or	%r8, %lo(strbindvar_r_sub_136246), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+104], %r10
	jmpl	%r12, %r15
	ld	[%sp+116], %r11
code_157851:
code_157823:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157824
	nop
code_157825:
	call	GCFromML ! delay slot empty
	nop
needgc_157824:
	cmp	%r8, 10
	or	%r0, 1, %r8
	be	cmpui_157827
	nop
code_157828:
	or	%r0, 0, %r8
cmpui_157827:
	cmp	%r8, 0
	bne	one_case_153586
	nop
zero_case_153585:
	ld	[%sp+116], %r17
	addcc	%r17, 1, %r8
	! making direct call 
	ba	funtop_153548
	st	%r8, [%sp+116]
code_157830:
	! done making self tail call
	ba	after_zeroone_153587
	or	%r0, 0, %r8
one_case_153586:
	ld	[%sp+116], %r17
	addcc	%r17, 1, %r8
	ld	[%sp+96], %r17
	subcc	%r8, %r17, %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r12
	add	%r4, 8, %r4
	! done allocating 1 record
	! making closure call 
	sethi	%hi(strbindvar_r_extract_136245), %r8
	or	%r8, %lo(strbindvar_r_extract_136245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+104], %r10
	jmpl	%r13, %r15
	ld	[%sp+96], %r11
code_157850:
	mov	%r8, %r9
code_157834:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157835
	nop
code_157836:
	call	GCFromML ! delay slot empty
	nop
needgc_157835:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	ld	[%sp+116], %r17
	addcc	%r17, 1, %r9
	! allocating 2-record
	or	%r0, 273, %r8
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
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_153587:
	ba	after_zeroone_153568 ! delay slot empty
	nop
one_case_153567:
	or	%r0, 0, %r12
	! making closure call 
	sethi	%hi(strbindvar_r_extract_136245), %r8
	or	%r8, %lo(strbindvar_r_extract_136245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+104], %r10
	jmpl	%r13, %r15
	ld	[%sp+96], %r11
code_157849:
	st	%r8, [%sp+96]
code_157841:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_157848:
code_157842:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157843
	nop
code_157844:
	call	GCFromML ! delay slot empty
	nop
needgc_157843:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
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
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
after_zeroone_153568:
code_157847:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_scan_code_146665,(.-TextIOFn_scan_code_146665)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157824
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01140000
		! -------- label,sizes,reg
	.long needgc_157835
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_157848
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157843
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157849
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00440000
		! -------- label,sizes,reg
	.long code_157850
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_157851
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01140000
	.text
 	.align 8
	.global TextIOFn_scanData_code_146656
 ! arguments : [$146658,$8] [$146659,$9] [$141491,$10] [$141492,$11] 
 ! results    : [$153547,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_scanData_code_146656:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157867
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157867:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_157853:
funtop_153501:
sumarm_153509:
	ld	[%sp+108], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_157866:
	mov	%r8, %r9
code_157856:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157857
	nop
code_157858:
	call	GCFromML ! delay slot empty
	nop
needgc_157857:
	! allocating 1 closures
	! allocating 5-record
	or	%r0, 2857, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	ld	[%sp+104], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_scan_code_146665), %r8
	or	%r8, %lo(TextIOFn_scan_code_146665), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+104], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_157861:
	! done making tail call
	ba	after_sum_153506 ! delay slot empty
	nop
sumarm_153510:
after_sum_153506:
code_157864:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_scanData_code_146656,(.-TextIOFn_scanData_code_146656)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157857
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
		! -------- label,sizes,reg
	.long code_157866
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
	.text
 	.align 8
	.global TextIOFn_inputLine_code_146681
 ! arguments : [$146683,$8] [$146684,$9] [$133612,$10] 
 ! results    : [$153500,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputLine_code_146681:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157892
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157892:
	st	%r15, [%sp+92]
code_157868:
funtop_153413:
	ld	[%r9], %r17
	st	%r17, [%sp+108]
	ld	[%r9+4], %r17
	st	%r17, [%sp+104]
sumarm_153425:
	ld	[%r10], %r17
	st	%r17, [%sp+100]
	ld	[%r10+4], %r17
	st	%r17, [%sp+96]
sumarm_153439:
	ld	[%sp+100], %r17
	ld	[%r17+8], %r10
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157890:
code_157871:
	! done making normal call
	ld	[%sp+96], %r17
	cmp	%r8, %r17
	or	%r0, 1, %r8
	be	cmpui_157872
	nop
code_157873:
	or	%r0, 0, %r8
cmpui_157872:
	cmp	%r8, 0
	bne	one_case_153461
	nop
zero_case_153460:
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r12, %r15
	ld	[%sp+96], %r11
code_157891:
code_157875:
	! done making normal call
	ba	after_zeroone_153462 ! delay slot empty
	nop
one_case_153461:
	or	%r0, 1, %r10
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_157889:
code_157877:
	! done making normal call
after_zeroone_153462:
	ld	[%r8], %r10
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_concat_136787), %r8
	or	%r8, %lo(strbindvar_r_concat_136787), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_157888:
	mov	%r8, %r9
code_157880:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157881
	nop
code_157882:
	call	GCFromML ! delay slot empty
	nop
needgc_157881:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_153436 ! delay slot empty
	nop
sumarm_153440:
after_sum_153436:
	ba	after_sum_153422 ! delay slot empty
	nop
sumarm_153426:
after_sum_153422:
code_157887:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_inputLine_code_146681,(.-TextIOFn_inputLine_code_146681)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157888
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_157881
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157889
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_157890
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_157891
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_outputExn_inner_code_146690
 ! arguments : [$146692,$8] [$146693,$9] [$145004,$10] [$145005,$11] [$145006,$12] 
 ! results    : [$153412,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_outputExn_inner_code_146690:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157903
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157903:
	st	%r15, [%sp+92]
code_157893:
funtop_153372:
sumarm_153380:
	ld	[%r10+4], %r8
sumarm_153392:
	ld	[%r8+20], %r10
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_157902:
code_157896:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_153389
	or	%r0, 0, %r8
sumarm_153393:
after_sum_153389:
	ba	after_sum_153377 ! delay slot empty
	nop
sumarm_153381:
after_sum_153377:
code_157901:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_outputExn_inner_code_146690,(.-TextIOFn_outputExn_inner_code_146690)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157902
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_isClosedOut_code_146695
 ! arguments : [$146697,$8] [$146698,$9] [$141586,$10] [$141587,$11] 
 ! results    : [$153371,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_isClosedOut_code_146695:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157914
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_157914:
	st	%r15, [%sp+92]
	mov	%r9, %r18
code_157904:
funtop_153329:
sumarm_153337:
	ld	[%r10+28], %r8
	! ptr sub start
	ld	[%r8], %r8
	! ptr sub end
sumarm_153353:
	cmp	%r8, 1
	bne	sumarm_153354
	nop
code_157905:
	sethi	%hi(mk_137905), %r8
	or	%r8, %lo(mk_137905), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	ld	[%r18], %r13
	ld	[%r18+4], %r8
	ld	[%r18+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 96, %sp
code_157908:
	! done making tail call
	ba	after_sum_153350 ! delay slot empty
	nop
sumarm_153354:
nomatch_sum_153351:
	or	%r0, 256, %r8
after_sum_153350:
	ba	after_sum_153334 ! delay slot empty
	nop
sumarm_153338:
after_sum_153334:
code_157912:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_isClosedOut_code_146695,(.-TextIOFn_isClosedOut_code_146695)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_flushBuffer_code_146702
 ! arguments : [$146704,$8] [$146705,$9] [$141602,$10] [$141603,$11] 
 ! results    : [$153328,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_flushBuffer_code_146702:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157946
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157946:
	st	%r15, [%sp+92]
	mov	%r11, %r13
code_157915:
funtop_153250:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157916
	nop
code_157917:
	call	GCFromML ! delay slot empty
	nop
needgc_157916:
sumarm_153258:
	ld	[%r10+24], %r8
	ld	[%r10+12], %r17
	st	%r17, [%sp+96]
	ld	[%r10+8], %r12
	! int sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r18
	! int sub end
	cmp	%r18, 0
	or	%r0, 1, %r11
	be	cmpui_157919
	nop
code_157920:
	or	%r0, 0, %r11
cmpui_157919:
	cmp	%r11, 0
	bne	one_case_153278
	nop
zero_case_153277:
	! allocating 1-record
	or	%r0, 9, %r11
	st	%r11, [%r4]
	st	%r18, [%r4+4]
	add	%r4, 4, %r11
	add	%r4, 8, %r4
	! done allocating 1 record
	! allocating 3-record
	or	%r0, 1817, %r18
	st	%r18, [%r4]
	st	%r9, [%r4+4]
	st	%r13, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r18
	add	%r4, 16, %r4
	! done allocating 3 record
	sethi	%hi(exn_handler_153291), %r9
	or	%r9, %lo(exn_handler_153291), %r13
	ld	[%r2+816], %r9
	sub	%sp, %r9, %r10
	! allocating 4-record
	or	%r0, 3105, %r9
	st	%r9, [%r4]
	st	%r13, [%r4+4]
	st	%r10, [%r4+8]
	st	%r18, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r9, %r1
	or	%r0, 0, %r18
	! making closure call 
	ld	[%r8], %r13
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	mov	%r10, %r8
	jmpl	%r13, %r15
	mov	%r18, %r10
code_157945:
code_157924:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_157928
	nop
code_157929:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_157928:
	or	%r0, 0, %r11
	ld	[%r2+800], %r10
	ld	[%sp+96], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r11, [%r17]
	or	%r0, 256, %r8
	ba	exn_handler_after_153292
	ld	[%r1+12], %r1
exn_handler_153291:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r9
	ld	[%r8+4], %r13
	ld	[%r8+8], %r10
	mov	%r15, %r12
	! making closure call 
	ld	[%r9], %r18
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	mov	%r13, %r11
	ld	[%sp+92], %r15
	jmpl	%r18, %r0
	add	%sp, 112, %sp
code_157939:
	! done making tail call
exn_handler_after_153292:
	ba	after_zeroone_153279 ! delay slot empty
	nop
one_case_153278:
	or	%r0, 256, %r8
after_zeroone_153279:
	ba	after_sum_153255 ! delay slot empty
	nop
sumarm_153259:
after_sum_153255:
code_157943:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_flushBuffer_code_146702,(.-TextIOFn_flushBuffer_code_146702)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_157916
	.word 0xb8003806
	.word 0x00002600
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_157928
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_157945
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_writeDirect_code_146714
 ! arguments : [$146716,$8] [$146717,$9] 
 ! results    : [$153238,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_writeDirect_code_146714:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_157980
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_157980:
	st	%r15, [%sp+92]
code_157947:
funtop_153163:
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157948
	nop
code_157949:
	call	GCFromML ! delay slot empty
	nop
needgc_157948:
	ld	[%r9], %r17
	st	%r17, [%sp+104]
	ld	[%r9+4], %r12
	ld	[%r9+8], %r8
	ld	[%r9+12], %r17
	st	%r17, [%sp+100]
	ld	[%r9+16], %r18
	ld	[%r9+20], %r17
	st	%r17, [%sp+96]
	ld	[%r9+24], %r10
	! allocating 2-record
	or	%r0, 785, %r9
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	st	%r18, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(exn_handler_153180), %r9
	or	%r9, %lo(exn_handler_153180), %r11
	ld	[%r2+816], %r9
	sub	%sp, %r9, %r10
	! allocating 4-record
	or	%r0, 3105, %r9
	st	%r9, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r13, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r9, %r1
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r10
	! int sub end
	cmp	%r10, 0
	or	%r0, 1, %r9
	be	cmpui_157953
	nop
code_157954:
	or	%r0, 0, %r9
cmpui_157953:
	cmp	%r9, 0
	bne	one_case_153199
	nop
zero_case_153198:
	! allocating 1-record
	or	%r0, 9, %r9
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r18
	add	%r4, 8, %r4
	! done allocating 1 record
	or	%r0, 0, %r11
	! making closure call 
	ld	[%r8], %r13
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	mov	%r10, %r8
	mov	%r11, %r10
	jmpl	%r13, %r15
	mov	%r18, %r11
code_157979:
code_157956:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_157960
	nop
code_157961:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_157960:
	or	%r0, 0, %r11
	ld	[%r2+800], %r10
	ld	[%sp+104], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+104], %r17
	st	%r11, [%r17]
	ba	after_zeroone_153200
	or	%r0, 256, %r8
one_case_153199:
	or	%r0, 256, %r8
after_zeroone_153200:
	or	%r0, 0, %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+96], %r12
code_157977:
code_157969:
	! done making normal call
	ba	exn_handler_after_153181
	ld	[%r1+12], %r1
exn_handler_153180:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r10
	ld	[%r8+4], %r18
	mov	%r15, %r12
	sethi	%hi(string_147817), %r8
	or	%r8, %lo(string_147817), %r11
	! making closure call 
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	mov	%r18, %r10
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_157974:
	! done making tail call
exn_handler_after_153181:
code_157976:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_writeDirect_code_146714,(.-TextIOFn_writeDirect_code_146714)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_157977
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_157948
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_157960
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_157979
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
 	.align 8
	.global TextIOFn_insert_code_146733
 ! arguments : [$146735,$8] [$146736,$9] [$133853,$10] 
 ! results    : [$153156,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_insert_code_146733:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158062
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_158062:
	st	%r15, [%sp+92]
	st	%r10, [%sp+136]
code_157981:
funtop_152955:
	ld	[%r9], %r17
	st	%r17, [%sp+132]
	ld	[%r9+4], %r17
	st	%r17, [%sp+104]
	ld	[%r9+8], %r17
	st	%r17, [%sp+128]
	ld	[%r9+12], %r17
	st	%r17, [%sp+124]
	ld	[%r9+16], %r17
	st	%r17, [%sp+120]
	ld	[%r9+20], %r17
	st	%r17, [%sp+116]
	ld	[%r9+24], %r17
	st	%r17, [%sp+112]
	ld	[%r9+28], %r17
	st	%r17, [%sp+100]
	! making closure call 
	sethi	%hi(strbindvar_r_length_138016), %r8
	or	%r8, %lo(strbindvar_r_length_138016), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_158056:
	st	%r8, [%sp+96]
code_157984:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_158053:
	st	%r8, [%sp+108]
code_157987:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_157988
	nop
code_157989:
	call	GCFromML ! delay slot empty
	nop
needgc_157988:
	ld	[%sp+96], %r16
	ld	[%sp+108], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	bge	cmpsi_157991
	nop
code_157992:
	or	%r0, 0, %r8
cmpsi_157991:
	cmp	%r8, 0
	bne	one_case_152998
	nop
zero_case_152997:
	! int sub start
	ld	[%sp+128], %r17
	ld	[%r17], %r18
	! int sub end
	ld	[%sp+96], %r17
	subcc	%r17, %r18, %r17
	st	%r17, [%sp+96]
	ld	[%sp+108], %r16
	ld	[%sp+96], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	bl	cmpsi_157994
	nop
code_157995:
	or	%r0, 0, %r8
cmpsi_157994:
	cmp	%r8, 0
	bne	one_case_153013
	nop
zero_case_153012:
	ld	[%sp+108], %r17
	addcc	%r18, %r17, %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158000
	nop
code_158001:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158000:
	ld	[%r2+800], %r10
	ld	[%sp+128], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+128], %r17
	st	%r11, [%r17]
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+136], %r17
	ld	[%r17], %r19
	ld	[%sp+136], %r17
	ld	[%r17+4], %r8
	ld	[%sp+136], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+108], %r12
	jmpl	%r19, %r15
	ld	[%sp+124], %r13
code_158052:
code_158008:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_153036
	nop
zero_case_153035:
	ld	[%sp+108], %r16
	ld	[%sp+96], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	be	cmpui_158010
	nop
code_158011:
	or	%r0, 0, %r8
cmpui_158010:
	ba	after_zeroone_153037 ! delay slot empty
	nop
one_case_153036:
	or	%r0, 1, %r8
after_zeroone_153037:
	cmp	%r8, 0
	bne	one_case_153048
	nop
zero_case_153047:
	ba	after_zeroone_153049
	or	%r0, 256, %r8
one_case_153048:
	sethi	%hi(string_147817), %r8
	or	%r8, %lo(string_147817), %r11
	! making closure call 
	ld	[%sp+132], %r17
	ld	[%r17], %r12
	ld	[%sp+132], %r17
	ld	[%r17+4], %r8
	ld	[%sp+132], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 144, %sp
code_158016:
	! done making tail call
after_zeroone_153049:
	ba	after_zeroone_153014 ! delay slot empty
	nop
one_case_153013:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r13
	add	%r4, 8, %r4
	! done allocating 1 record
	or	%r0, 0, %r11
	! making closure call 
	sethi	%hi(strbindvar_r_copyVec_138046), %r8
	or	%r8, %lo(strbindvar_r_copyVec_138046), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r19
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	mov	%r18, %r10
	ld	[%sp+112], %r12
	jmpl	%r19, %r15
	ld	[%sp+124], %r18
code_158059:
code_158020:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158021
	nop
code_158022:
	call	GCFromML ! delay slot empty
	nop
needgc_158021:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(exn_handler_153088), %r8
	or	%r8, %lo(exn_handler_153088), %r10
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
	or	%r0, 0, %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+120], %r17
	ld	[%r17], %r13
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+124], %r12
code_158057:
code_158026:
	! done making normal call
	ba	exn_handler_after_153089
	ld	[%r1+12], %r1
exn_handler_153088:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	ld	[%r8+4], %r17
	st	%r17, [%sp+116]
	mov	%r15, %r12
	sethi	%hi(string_147817), %r8
	or	%r8, %lo(string_147817), %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+116], %r10
code_158054:
code_158031:
	! done making normal call
exn_handler_after_153089:
	ld	[%sp+96], %r16
	ld	[%sp+108], %r17
	subcc	%r17, %r16, %r12
	or	%r0, 0, %r18
	! making closure call 
	ld	[%sp+136], %r17
	ld	[%r17], %r19
	ld	[%sp+136], %r17
	ld	[%r17+4], %r8
	ld	[%sp+136], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+96], %r11
	jmpl	%r19, %r15
	ld	[%sp+124], %r13
code_158055:
code_158032:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_153136
	nop
zero_case_153135:
	ld	[%sp+96], %r16
	ld	[%sp+108], %r17
	subcc	%r17, %r16, %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158037
	nop
code_158038:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158037:
	ld	[%r2+800], %r10
	ld	[%sp+128], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+128], %r17
	st	%r11, [%r17]
	ba	after_zeroone_153137
	or	%r0, 256, %r8
one_case_153136:
	sethi	%hi(string_147817), %r8
	or	%r8, %lo(string_147817), %r11
	! making closure call 
	ld	[%sp+132], %r17
	ld	[%r17], %r12
	ld	[%sp+132], %r17
	ld	[%r17+4], %r8
	ld	[%sp+132], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 144, %sp
code_158047:
	! done making tail call
after_zeroone_153137:
after_zeroone_153014:
	ba	after_zeroone_152999 ! delay slot empty
	nop
one_case_152998:
	! making closure polycall
	ld	[%sp+104], %r17
	ld	[%r17], %r10
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r10, %r0
	add	%sp, 144, %sp
code_158049:
	! done making tail call
after_zeroone_152999:
code_158051:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size TextIOFn_insert_code_146733,(.-TextIOFn_insert_code_146733)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158052
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
	.word 0x00000004
		! -------- label,sizes,reg
	.long code_158053
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55140000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_157988
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55140000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_158000
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long needgc_158021
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55040000
	.word 0x00000015
		! -------- label,sizes,reg
	.long afterMutateCheck_158037
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_158054
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_158055
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_158056
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55140000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_158057
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_158059
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55040000
	.word 0x00000015
	.text
 	.align 8
	.global TextIOFn_output_code_146709
 ! arguments : [$146711,$8] [$146712,$9] [$141624,$10] [$141625,$11] 
 ! results    : [$152954,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_output_code_146709:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158088
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158088:
	st	%r15, [%sp+92]
	mov	%r9, %r12
code_158063:
funtop_152841:
	add	%r4, 100, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158064
	nop
code_158065:
	call	GCFromML ! delay slot empty
	nop
needgc_158064:
	ld	[%r12], %r9
	ld	[%r12+4], %r8
	ld	[%r12+8], %r21
sumarm_152855:
	ld	[%r10+20], %r17
	st	%r17, [%sp+104]
	ld	[%r10+12], %r20
	ld	[%r10+8], %r19
	ld	[%r10+24], %r18
	ld	[%r10+16], %r13
	! allocating 1 closures
	! allocating 7-record
	sethi	%hi(32569), %r12
	or	%r12, %lo(32569), %r12
	st	%r12, [%r4]
	st	%r20, [%r4+4]
	st	%r19, [%r4+8]
	st	%r18, [%r4+12]
	st	%r13, [%r4+16]
	st	%r10, [%r4+20]
	st	%r11, [%r4+24]
	st	%r21, [%r4+28]
	add	%r4, 4, %r13
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r12
	st	%r12, [%r4]
	sethi	%hi(TextIOFn_writeDirect_code_146714), %r12
	or	%r12, %lo(TextIOFn_writeDirect_code_146714), %r12
	st	%r12, [%r4+4]
	or	%r0, 256, %r12
	st	%r12, [%r4+8]
	st	%r13, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 8-record
	sethi	%hi(65345), %r12
	or	%r12, %lo(65345), %r12
	st	%r12, [%r4]
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	st	%r20, [%r4+12]
	st	%r19, [%r4+16]
	st	%r18, [%r4+20]
	st	%r10, [%r4+24]
	st	%r11, [%r4+28]
	st	%r21, [%r4+32]
	add	%r4, 4, %r11
	add	%r4, 36, %r4
	! done allocating 8 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_insert_code_146733), %r8
	or	%r8, %lo(TextIOFn_insert_code_146733), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(string_147817), %r8
	or	%r8, %lo(string_147817), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158086:
code_158070:
	! done making normal call
	! ptr sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r8
	! ptr sub end
sumarm_152924:
	cmp	%r8, 0
	bne	sumarm_152925
	nop
code_158071:
	sethi	%hi(copyVec_133890), %r8
	or	%r8, %lo(copyVec_133890), %r10
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
code_158073:
	! done making tail call
	ba	after_sum_152921 ! delay slot empty
	nop
sumarm_152925:
	cmp	%r8, 1
	bne	sumarm_152936
	nop
code_158075:
	sethi	%hi(copyVec_133906), %r8
	or	%r8, %lo(copyVec_133906), %r10
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
code_158077:
	! done making tail call
	ba	after_sum_152921 ! delay slot empty
	nop
sumarm_152936:
	! making closure polycall
	ld	[%sp+100], %r17
	ld	[%r17], %r10
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r10, %r0
	add	%sp, 112, %sp
code_158079:
	! done making tail call
	ba	after_sum_152921 ! delay slot empty
	nop
sumarm_152946:
after_sum_152921:
	ba	after_sum_152852 ! delay slot empty
	nop
sumarm_152856:
after_sum_152852:
code_158083:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_output_code_146709,(.-TextIOFn_output_code_146709)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158064
	.word 0xb8003806
	.word 0x00001c00
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158086
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
 	.align 8
	.global TextIOFn_output1_code_146758
 ! arguments : [$146760,$8] [$146761,$9] [$141782,$10] [$141783,$11] 
 ! results    : [$152840,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_output1_code_146758:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158176
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_158176:
	st	%r15, [%sp+92]
	mov	%r9, %r8
	st	%r10, [%sp+124]
	st	%r11, [%sp+120]
code_158089:
funtop_152628:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+116]
	ld	[%r8+8], %r17
	st	%r17, [%sp+108]
sumarm_152642:
	ld	[%sp+124], %r17
	ld	[%r17+24], %r17
	st	%r17, [%sp+104]
	ld	[%sp+124], %r17
	ld	[%r17+20], %r17
	st	%r17, [%sp+96]
	ld	[%sp+124], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+100]
	ld	[%sp+124], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+112]
	sethi	%hi(string_147851), %r8
	or	%r8, %lo(string_147851), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+124], %r10
code_158173:
code_158091:
	! done making normal call
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! ptr sub end
sumarm_152672:
	cmp	%r8, 0
	bne	sumarm_152673
	nop
code_158092:
	! int sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	! int sub end
	addcc	%r11, 1, %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_update_136247), %r8
	or	%r8, %lo(strbindvar_r_update_136247), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r13, %r15
	ld	[%sp+120], %r12
code_158167:
code_158095:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158099
	nop
code_158100:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158099:
	ld	[%r2+800], %r10
	ld	[%sp+100], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+100], %r16
	ld	[%sp+96], %r17
	st	%r17, [%r16]
	! making closure call 
	sethi	%hi(strbindvar_r_length_138016), %r8
	or	%r8, %lo(strbindvar_r_length_138016), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_158165:
code_158109:
	! done making normal call
	ld	[%sp+96], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	be	cmpui_158110
	nop
code_158111:
	or	%r0, 0, %r8
cmpui_158110:
	cmp	%r8, 0
	bne	one_case_152711
	nop
zero_case_152710:
	ba	after_zeroone_152712
	or	%r0, 256, %r8
one_case_152711:
	sethi	%hi(string_147851), %r8
	or	%r8, %lo(string_147851), %r11
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+124], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_158115:
	! done making tail call
after_zeroone_152712:
	ba	after_sum_152669 ! delay slot empty
	nop
sumarm_152673:
	cmp	%r8, 1
	bne	sumarm_152726
	nop
code_158117:
	! int sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	! int sub end
	addcc	%r11, 1, %r17
	st	%r17, [%sp+96]
	! making closure call 
	sethi	%hi(strbindvar_r_update_136247), %r8
	or	%r8, %lo(strbindvar_r_update_136247), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r13, %r15
	ld	[%sp+120], %r12
code_158168:
code_158120:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158124
	nop
code_158125:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158124:
	ld	[%r2+800], %r10
	ld	[%sp+100], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+100], %r16
	ld	[%sp+96], %r17
	st	%r17, [%r16]
	! making closure call 
	sethi	%hi(strbindvar_r_length_138016), %r8
	or	%r8, %lo(strbindvar_r_length_138016), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_158166:
code_158134:
	! done making normal call
	ld	[%sp+96], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	be	cmpui_158135
	nop
code_158136:
	or	%r0, 0, %r8
cmpui_158135:
	cmp	%r8, 0
	bne	one_case_152764
	nop
zero_case_152763:
	! making closure call 
	sethi	%hi(isNL_133728), %r8
	or	%r8, %lo(isNL_133728), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_158175:
code_158139:
	! done making normal call
	ba	after_zeroone_152765 ! delay slot empty
	nop
one_case_152764:
	or	%r0, 1, %r8
after_zeroone_152765:
	cmp	%r8, 0
	bne	one_case_152780
	nop
zero_case_152779:
	ba	after_zeroone_152781
	or	%r0, 256, %r8
one_case_152780:
	sethi	%hi(string_147851), %r8
	or	%r8, %lo(string_147851), %r11
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+124], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_158144:
	! done making tail call
after_zeroone_152781:
	ba	after_sum_152669 ! delay slot empty
	nop
sumarm_152726:
	or	%r0, 0, %r11
	! making closure call 
	sethi	%hi(strbindvar_r_update_136247), %r8
	or	%r8, %lo(strbindvar_r_update_136247), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r13, %r15
	ld	[%sp+120], %r12
code_158174:
code_158148:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158149
	nop
code_158150:
	call	GCFromML ! delay slot empty
	nop
needgc_158149:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(exn_handler_152809), %r8
	or	%r8, %lo(exn_handler_152809), %r10
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
	or	%r0, 0, %r10
	sethi	%hi(record_147859), %r8
	or	%r8, %lo(record_147859), %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r13
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+112], %r12
code_158170:
code_158155:
	! done making normal call
	ba	exn_handler_after_152810
	ld	[%r1+12], %r1
exn_handler_152809:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r17
	st	%r17, [%sp+108]
	ld	[%r8+4], %r17
	st	%r17, [%sp+124]
	mov	%r15, %r12
	sethi	%hi(string_147851), %r8
	or	%r8, %lo(string_147851), %r11
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r13
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+124], %r10
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 128, %sp
code_158160:
	! done making tail call
exn_handler_after_152810:
	ba	after_sum_152669 ! delay slot empty
	nop
sumarm_152794:
after_sum_152669:
	ba	after_sum_152639 ! delay slot empty
	nop
sumarm_152643:
after_sum_152639:
code_158164:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_output1_code_146758,(.-TextIOFn_output1_code_146758)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158165
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44000000
		! -------- label,sizes,reg
	.long code_158166
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44000000
		! -------- label,sizes,reg
	.long code_158167
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45040000
		! -------- label,sizes,reg
	.long afterMutateCheck_158099
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45040000
		! -------- label,sizes,reg
	.long code_158168
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45040000
		! -------- label,sizes,reg
	.long afterMutateCheck_158124
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45040000
		! -------- label,sizes,reg
	.long needgc_158149
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x41500000
		! -------- label,sizes,reg
	.long code_158170
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158173
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45550000
		! -------- label,sizes,reg
	.long code_158174
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x41500000
		! -------- label,sizes,reg
	.long code_158175
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44000000
	.text
 	.align 8
	.global TextIOFn_flushOut_code_146769
 ! arguments : [$146771,$8] [$146772,$9] [$133979,$10] 
 ! results    : [$152627,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_flushOut_code_146769:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158183
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158183:
	st	%r15, [%sp+92]
code_158177:
funtop_152617:
	sethi	%hi(string_147869), %r8
	or	%r8, %lo(string_147869), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_158179:
	! done making tail call
code_158181:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_flushOut_code_146769,(.-TextIOFn_flushOut_code_146769)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_closeOut_code_146776
 ! arguments : [$146778,$8] [$146779,$9] [$133982,$10] 
 ! results    : [$152616,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_closeOut_code_146776:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158212
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158212:
	st	%r15, [%sp+92]
code_158184:
funtop_152545:
sumarm_152553:
	ld	[%r10], %r17
	st	%r17, [%sp+104]
	ld	[%r10+28], %r17
	st	%r17, [%sp+100]
	ld	[%r10+4], %r8
sumarm_152569:
	ld	[%r8+48], %r17
	st	%r17, [%sp+96]
	! ptr sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r8
	! ptr sub end
	cmp	%r8, 0
	bne	one_case_152583
	nop
zero_case_152582:
	sethi	%hi(string_147878), %r8
	or	%r8, %lo(string_147878), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158211:
code_158187:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158191
	nop
code_158192:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158191:
	or	%r0, 1, %r11
	ld	[%r2+800], %r10
	ld	[%sp+100], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%sp+100], %r17
	ld	[%r17], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+100], %r17
	st	%r11, [%r17]
	! making closure call 
	sethi	%hi(_141006), %r8
	or	%r8, %lo(_141006), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_158209:
code_158202:
	! done making normal call
	! making closure polycall
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r10, %r0
	add	%sp, 112, %sp
code_158203:
	! done making tail call
	ba	after_zeroone_152584 ! delay slot empty
	nop
one_case_152583:
	or	%r0, 256, %r8
after_zeroone_152584:
	ba	after_sum_152566 ! delay slot empty
	nop
sumarm_152570:
after_sum_152566:
	ba	after_sum_152550 ! delay slot empty
	nop
sumarm_152554:
after_sum_152550:
code_158208:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_closeOut_code_146776,(.-TextIOFn_closeOut_code_146776)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158209
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long afterMutateCheck_158191
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00350000
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_158211
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00350000
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
	.text
 	.align 8
	.global TextIOFn_write_code_146788
 ! arguments : [$146790,$8] [$146791,$9] [$141908,$10] [$141909,$11] [$141910,$12] 
 ! results    : [$152544,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_write_code_146788:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158225
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158225:
	st	%r15, [%sp+92]
	st	%r9, [%sp+96]
	st	%r10, [%sp+104]
	st	%r12, [%sp+100]
code_158213:
funtop_152499:
sumarm_152511:
	cmp	%r11, 0
	bne	sumarm_152512
	nop
code_158214:
	! making closure call 
	sethi	%hi(strbindvar_r_length_138016), %r8
	or	%r8, %lo(strbindvar_r_length_138016), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_158224:
code_158217:
	! done making normal call
	ld	[%sp+104], %r17
	subcc	%r8, %r17, %r8
	ba	after_sum_152508
	mov	%r8, %r12
sumarm_152512:
	ld	[%r11], %r8
	ba	after_sum_152508
	mov	%r8, %r12
sumarm_152528:
after_sum_152508:
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r13
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+104], %r11
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_158220:
	! done making tail call
code_158222:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_write_code_146788,(.-TextIOFn_write_code_146788)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158224
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_write_code_146795
 ! arguments : [$146797,$8] [$146798,$9] [$141951,$10] [$141952,$11] [$141953,$12] 
 ! results    : [$152498,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_write_code_146795:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158238
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158238:
	st	%r15, [%sp+92]
	st	%r9, [%sp+96]
	st	%r10, [%sp+104]
	st	%r12, [%sp+100]
code_158226:
funtop_152453:
sumarm_152465:
	cmp	%r11, 0
	bne	sumarm_152466
	nop
code_158227:
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_158237:
code_158230:
	! done making normal call
	ld	[%sp+104], %r17
	subcc	%r8, %r17, %r8
	ba	after_sum_152462
	mov	%r8, %r12
sumarm_152466:
	ld	[%r11], %r8
	ba	after_sum_152462
	mov	%r8, %r12
sumarm_152482:
after_sum_152462:
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r13
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+104], %r11
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_158233:
	! done making tail call
code_158235:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_write_code_146795,(.-TextIOFn_write_code_146795)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158237
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146802
 ! arguments : [$146804,$8] [$146805,$9] 
 ! results    : [$152452,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146802:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158244
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158244:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_158239:
funtop_152440:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158240:
	! done making tail call
code_158242:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146802,(.-TextIOFn_anonfun_code_146802)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146811
 ! arguments : [$146813,$8] [$146814,$9] 
 ! results    : [$152439,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146811:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158250
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158250:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_158245:
funtop_152427:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158246:
	! done making tail call
code_158248:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146811,(.-TextIOFn_anonfun_code_146811)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_146820
 ! arguments : [$146822,$8] [$146823,$9] 
 ! results    : [$152426,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_146820:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158256
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158256:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_158251:
funtop_152414:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158252:
	! done making tail call
code_158254:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_146820,(.-TextIOFn_anonfun_code_146820)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_mkOutstream_code_146783
 ! arguments : [$146785,$8] [$146786,$9] [$141867,$10] [$141868,$11] 
 ! results    : [$152413,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_mkOutstream_code_146783:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158344
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_158344:
	st	%r15, [%sp+92]
	st	%r10, [%sp+128]
	st	%r11, [%sp+124]
code_158257:
funtop_152144:
	ld	[%r9], %r17
	st	%r17, [%sp+120]
	ld	[%r9+4], %r17
	st	%r17, [%sp+116]
sumarm_152156:
	ld	[%sp+128], %r17
	ld	[%r17+28], %r17
	st	%r17, [%sp+96]
	ld	[%sp+128], %r17
	ld	[%r17+36], %r10
	ld	[%sp+128], %r17
	ld	[%r17+32], %r17
	st	%r17, [%sp+100]
sumarm_152177:
	cmp	%r10, 0
	bne	sumarm_152178
	nop
code_158258:
	sethi	%hi(anonfun_134042), %r8
	or	%r8, %lo(anonfun_134042), %r8
	ba	after_sum_152174
	st	%r8, [%sp+112]
sumarm_152178:
	! making closure call 
	sethi	%hi(_138315), %r8
	or	%r8, %lo(_138315), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_158342:
	mov	%r8, %r9
code_158263:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158264
	nop
code_158265:
	call	GCFromML ! delay slot empty
	nop
needgc_158264:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_write_code_146788), %r8
	or	%r8, %lo(TextIOFn_write_code_146788), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	after_sum_152174
	st	%r8, [%sp+112]
sumarm_152183:
after_sum_152174:
sumarm_152221:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne	sumarm_152222
	nop
code_158269:
	sethi	%hi(anonfun_134076), %r8
	or	%r8, %lo(anonfun_134076), %r8
	ba	after_sum_152218
	st	%r8, [%sp+108]
sumarm_152222:
	! making closure call 
	sethi	%hi(_138361), %r8
	or	%r8, %lo(_138361), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_158343:
	mov	%r8, %r9
code_158274:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158275
	nop
code_158276:
	call	GCFromML ! delay slot empty
	nop
needgc_158275:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_write_code_146795), %r8
	or	%r8, %lo(TextIOFn_write_code_146795), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	after_sum_152218
	st	%r8, [%sp+108]
sumarm_152227:
after_sum_152218:
	sethi	%hi(anonfun_134104), %r8
	or	%r8, %lo(anonfun_134104), %r10
	sethi	%hi(anonfun_134112), %r8
	or	%r8, %lo(anonfun_134112), %r11
	sethi	%hi(anonfun_134108), %r8
	or	%r8, %lo(anonfun_134108), %r12
	! making closure call 
	sethi	%hi(addCleaner_137245), %r8
	or	%r8, %lo(addCleaner_137245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_158336:
	st	%r8, [%sp+104]
code_158285:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call 
	sethi	%hi(strbindvar_r_array_138372), %r8
	or	%r8, %lo(strbindvar_r_array_138372), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_158338:
	st	%r8, [%sp+100]
code_158288:
	! done making normal call
	or	%r0, 0, %r9
	or	%r0, 1, %r8
	! initializing int/ptr array start
	sll	%r8, 2, %r11
	add	%r11, %r0, %r11
	or	%r0, 510, %r10
	cmp	%r8, %r10
	ble	array_int_small_152290
	nop
code_158289:
	call	save_regs_MLtoC
	mov	%r11, %r8
	call	alloc_bigintarray ! delay slot empty
	nop
code_158339:
	call	load_regs_MLtoC ! delay slot empty
	nop
	st	%r8, [%sp+96]
code_158290:
	ba	array_int_after_152289 ! delay slot empty
	nop
array_int_small_152290:
	sll	%r11, 3, %r11
	add	%r11, %r0, %r11
	or	%r11, 2, %r11
	or	%r0, 1, %r10
	add	%r10, %r8, %r10
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158292
	nop
code_158293:
	call	GCFromML ! delay slot empty
	nop
needgc_158292:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_152297
	add	%r10, %r0, %r10
array_init_loopto_152298:
	ld	[%sp+96], %r17
	add	%r17, %r10, %r8
	st	%r9, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_152297:
	cmp	%r10, 0
	bge	array_init_loopto_152298
	nop
array_int_after_152289:
	or	%r0, 1, %r8
	or	%r0, 0, %r9
	or	%r0, 510, %r10
	cmp	%r8, %r10
	ble	array_ptr_alloc_152309
	nop
code_158298:
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigptrarray ! delay slot empty
	nop
code_158340:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r31
code_158299:
	ba	array_ptr_aftert_152308 ! delay slot empty
	nop
array_ptr_alloc_152309:
	sll	%r8, 2, %r10
	add	%r10, %r0, %r10
	sll	%r10, 3, %r11
	add	%r11, %r0, %r11
	or	%r11, 5, %r11
	or	%r0, 1, %r10
	add	%r10, %r8, %r10
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158301
	nop
code_158302:
	call	GCFromML ! delay slot empty
	nop
needgc_158301:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r31
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_152315
	add	%r10, %r0, %r10
array_init_loopto_152316:
	add	%r31, %r10, %r8
	st	%r9, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_152315:
	cmp	%r10, 0
	bge	array_init_loopto_152316
	nop
array_ptr_aftert_152308:
	or	%r0, 1, %r8
	or	%r0, 510, %r9
	cmp	%r8, %r9
	ble	array_ptr_alloc_152326
	nop
code_158307:
	call	save_regs_MLtoC
	ld	[%sp+124], %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_158341:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r9
code_158308:
	ba	array_ptr_aftert_152325 ! delay slot empty
	nop
array_ptr_alloc_152326:
	sll	%r8, 2, %r9
	add	%r9, %r0, %r9
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 5, %r9
	or	%r0, 1, %r10
	add	%r10, %r8, %r10
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158310
	nop
code_158311:
	call	GCFromML ! delay slot empty
	nop
needgc_158310:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r9
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_152332
	add	%r10, %r0, %r10
array_init_loopto_152333:
	add	%r9, %r10, %r8
	ld	[%sp+124], %r17
	st	%r17, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_152332:
	cmp	%r10, 0
	bge	array_init_loopto_152333
	nop
array_ptr_aftert_152325:
	add	%r4, 136, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158316
	nop
code_158317:
	call	GCFromML ! delay slot empty
	nop
needgc_158316:
	! allocating 8-record
	sethi	%hi(65089), %r11
	or	%r11, %lo(65089), %r11
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_158321
	nop
code_158322:
	or	%r0, 0, %r8
cmpui_158321:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r11
	st	%r11, [%r4]
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_158325
	nop
code_158326:
	or	%r0, 0, %r8
cmpui_158325:
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	ld	[%sp+108], %r17
	st	%r17, [%r4+20]
	st	%r9, [%r4+24]
	ld	[%sp+112], %r17
	st	%r17, [%r4+28]
	st	%r31, [%r4+32]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 36, %r4
	! done allocating 8 record
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146802), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_146802), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r9
	st	%r9, [%r4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146811), %r9
	or	%r9, %lo(TextIOFn_anonfun_code_146811), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r9
	st	%r9, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_146820), %r9
	or	%r9, %lo(TextIOFn_anonfun_code_146820), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r9
	st	%r9, [%r4]
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! making closure call 
	sethi	%hi(rebindCleaner_138384), %r8
	or	%r8, %lo(rebindCleaner_138384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r10
code_158337:
code_158332:
	! done making normal call
	ba	after_sum_152153
	ld	[%sp+96], %r8
sumarm_152157:
after_sum_152153:
code_158335:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size TextIOFn_mkOutstream_code_146783,(.-TextIOFn_mkOutstream_code_146783)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158336
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_158337
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_158264
	.word 0xb8004807
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54010000
	.word 0x00000001
		! -------- label,sizes,reg
	.long needgc_158275
	.word 0xb8004807
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_158338
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55700000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_158339
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55740000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_158292
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55740000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_158340
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55750000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_158301
	.word 0xb8004809
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55750000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_158341
	.word 0xb8004809
	.word 0x80000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15750000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_158310
	.word 0xb8004809
	.word 0x80000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55750000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_158316
	.word 0xb8004809
	.word 0x80000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15750000
	.word 0x00000001
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_158342
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54010000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_158343
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55000000
	.word 0x00000001
	.text
 	.align 8
	.global TextIOFn_getWriter_code_146831
 ! arguments : [$146833,$8] [$146834,$9] [$134143,$10] 
 ! results    : [$152143,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getWriter_code_146831:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158355
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158355:
	st	%r15, [%sp+92]
code_158345:
funtop_152109:
sumarm_152117:
	ld	[%r10+20], %r17
	st	%r17, [%sp+100]
	ld	[%r10+4], %r17
	st	%r17, [%sp+96]
	sethi	%hi(string_147958), %r8
	or	%r8, %lo(string_147958), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158354:
code_158347:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158348
	nop
code_158349:
	call	GCFromML ! delay slot empty
	nop
needgc_158348:
	! ptr sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r9
	! ptr sub end
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_152114 ! delay slot empty
	nop
sumarm_152118:
after_sum_152114:
code_158353:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_getWriter_code_146831,(.-TextIOFn_getWriter_code_146831)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158348
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_158354
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_outputExn_inner_code_146838
 ! arguments : [$146840,$8] [$146841,$9] [$145027,$10] [$145028,$11] [$145029,$12] 
 ! results    : [$152108,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_outputExn_inner_code_146838:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158366
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158366:
	st	%r15, [%sp+92]
code_158356:
funtop_152068:
sumarm_152076:
	ld	[%r10+4], %r8
sumarm_152088:
	ld	[%r8+20], %r10
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_158365:
code_158359:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_152085
	or	%r0, 0, %r8
sumarm_152089:
after_sum_152085:
	ba	after_sum_152073 ! delay slot empty
	nop
sumarm_152077:
after_sum_152073:
code_158364:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_outputExn_inner_code_146838,(.-TextIOFn_outputExn_inner_code_146838)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158365
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_getPosOut_code_146843
 ! arguments : [$146845,$8] [$146846,$9] [$134155,$10] 
 ! results    : [$152067,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getPosOut_code_146843:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158408
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158408:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
code_158367:
funtop_151904:
sumarm_151912:
	ld	[%sp+100], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+96]
	sethi	%hi(string_147968), %r8
	or	%r8, %lo(string_147968), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_158405:
code_158369:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158370
	nop
code_158371:
	call	GCFromML ! delay slot empty
	nop
needgc_158370:
sumarm_151932:
	ld	[%sp+96], %r17
	ld	[%r17+8], %r12
sumarm_151948:
	or	%r0, 255, %r8
	cmp	%r12, %r8
	ble	nomatch_sum_151946
	nop
code_158373:
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r11
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(exn_handler_151958), %r8
	or	%r8, %lo(exn_handler_151958), %r10
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
	! making closure polycall
	ld	[%r12], %r10
	ld	[%r12+4], %r8
	jmpl	%r10, %r15
	ld	[%r12+8], %r9
code_158404:
	mov	%r8, %r9
code_158376:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158377
	nop
code_158378:
	call	GCFromML ! delay slot empty
	nop
needgc_158377:
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	exn_handler_after_151959
	ld	[%r1+12], %r1
exn_handler_151958:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	mov	%r15, %r12
sumarm_151988:
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
sumarm_152000:
	ld	[%r8+20], %r10
	sethi	%hi(string_147968), %r8
	or	%r8, %lo(string_147968), %r11
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_158406:
code_158386:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_151997
	or	%r0, 0, %r8
sumarm_152001:
after_sum_151997:
	ba	after_sum_151985 ! delay slot empty
	nop
sumarm_151989:
after_sum_151985:
exn_handler_after_151959:
	ba	after_sum_151945 ! delay slot empty
	nop
sumarm_151949:
nomatch_sum_151946:
sumarm_152029:
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
sumarm_152041:
	ld	[%r8+20], %r10
	sethi	%hi(string_147968), %r8
	or	%r8, %lo(string_147968), %r11
	sethi	%hi(mk_137384), %r8
	or	%r8, %lo(mk_137384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_158407:
code_158396:
	! done making normal call
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_152038
	or	%r0, 0, %r8
sumarm_152042:
after_sum_152038:
	ba	after_sum_152026 ! delay slot empty
	nop
sumarm_152030:
after_sum_152026:
after_sum_151945:
	ba	after_sum_151929 ! delay slot empty
	nop
sumarm_151933:
after_sum_151929:
	ba	after_sum_151909 ! delay slot empty
	nop
sumarm_151913:
after_sum_151909:
code_158403:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_getPosOut_code_146843,(.-TextIOFn_getPosOut_code_146843)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158370
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_158404
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long needgc_158377
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_158405
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_158406
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158407
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_filePosOut_code_146850
 ! arguments : [$146852,$8] [$146853,$9] [$134187,$10] 
 ! results    : [$151903,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_filePosOut_code_146850:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158416
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158416:
	st	%r15, [%sp+92]
code_158409:
funtop_151877:
sumarm_151885:
	ld	[%r10], %r17
	st	%r17, [%sp+96]
	ld	[%r10+4], %r10
	sethi	%hi(string_147979), %r8
	or	%r8, %lo(string_147979), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158415:
code_158411:
	! done making normal call
	ba	after_sum_151882
	ld	[%sp+96], %r8
sumarm_151886:
after_sum_151882:
code_158414:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_filePosOut_code_146850,(.-TextIOFn_filePosOut_code_146850)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158415
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_setPosOut_code_146857
 ! arguments : [$146859,$8] [$146860,$9] [$134195,$10] 
 ! results    : [$151876,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_setPosOut_code_146857:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158446
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158446:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_158417:
funtop_151757:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+108]
sumarm_151769:
	ld	[%r10], %r17
	st	%r17, [%sp+104]
	ld	[%r10+4], %r17
	st	%r17, [%sp+100]
sumarm_151783:
	ld	[%sp+100], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+96]
	sethi	%hi(string_147989), %r8
	or	%r8, %lo(string_147989), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_158445:
code_158419:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158420
	nop
code_158421:
	call	GCFromML ! delay slot empty
	nop
needgc_158420:
sumarm_151803:
	ld	[%sp+96], %r17
	ld	[%r17+12], %r12
sumarm_151819:
	or	%r0, 255, %r8
	cmp	%r12, %r8
	ble	nomatch_sum_151817
	nop
code_158423:
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(exn_handler_151833), %r8
	or	%r8, %lo(exn_handler_151833), %r10
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
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_158442:
code_158426:
	! done making normal call
	ba	exn_handler_after_151834
	ld	[%r1+12], %r1
exn_handler_151833:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r17
	st	%r17, [%sp+108]
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	mov	%r15, %r12
	sethi	%hi(string_147989), %r8
	or	%r8, %lo(string_147989), %r11
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r13
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_158431:
	! done making tail call
exn_handler_after_151834:
	ba	after_sum_151816 ! delay slot empty
	nop
sumarm_151820:
nomatch_sum_151817:
	sethi	%hi(string_147968), %r8
	or	%r8, %lo(string_147968), %r11
	sethi	%hi(mk_137384), %r8
	or	%r8, %lo(mk_137384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r13
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_158436:
	! done making tail call
after_sum_151816:
	ba	after_sum_151800 ! delay slot empty
	nop
sumarm_151804:
after_sum_151800:
	ba	after_sum_151780 ! delay slot empty
	nop
sumarm_151784:
after_sum_151780:
	ba	after_sum_151766 ! delay slot empty
	nop
sumarm_151770:
after_sum_151766:
code_158441:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_setPosOut_code_146857,(.-TextIOFn_setPosOut_code_146857)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158420
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
		! -------- label,sizes,reg
	.long code_158442
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158445
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00450000
	.text
 	.align 8
	.global TextIOFn_cpy_code_146878
 ! arguments : [$146880,$8] [$146881,$9] [$142117,$10] [$142118,$11] [$142119,$12] 
 ! results    : [$151706,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_cpy_code_146878:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158467
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_158467:
	st	%r15, [%sp+92]
	st	%r9, [%sp+112]
	st	%r10, [%sp+100]
	st	%r11, [%sp+120]
	st	%r12, [%sp+116]
code_158447:
funtop_151686:
	ld	[%sp+112], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+108]
	ld	[%sp+112], %r17
	ld	[%r17+4], %r10
	ld	[%sp+112], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+96]
	ld	[%sp+112], %r17
	ld	[%r17+12], %r8
	ld	[%sp+100], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	bl	cmpsi_158448
	nop
code_158449:
	or	%r0, 0, %r8
cmpsi_158448:
	cmp	%r8, 0
	bne	one_case_151703
	nop
zero_case_151702:
	ba	after_zeroone_151704
	ld	[%sp+116], %r8
one_case_151703:
	! making closure call 
	sethi	%hi(strbindvar_r_sub_136246), %r8
	or	%r8, %lo(strbindvar_r_sub_136246), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_158465:
	st	%r8, [%sp+104]
code_158454:
	! done making normal call
	! making closure call 
	sethi	%hi(strbindvar_r_update_136247), %r8
	or	%r8, %lo(strbindvar_r_update_136247), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+120], %r11
	jmpl	%r13, %r15
	ld	[%sp+104], %r12
code_158464:
code_158457:
	! done making normal call
	ld	[%sp+100], %r17
	addcc	%r17, 1, %r17
	st	%r17, [%sp+100]
	ld	[%sp+120], %r17
	addcc	%r17, 1, %r17
	st	%r17, [%sp+96]
	ld	[%sp+116], %r17
	cmp	%r17, 0
	bne	one_case_151738
	nop
zero_case_151737:
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_158466:
code_158459:
	! done making normal call
	ba	after_zeroone_151739 ! delay slot empty
	nop
one_case_151738:
	or	%r0, 1, %r8
after_zeroone_151739:
	! making direct call 
	ld	[%sp+100], %r16
	st	%r16, [%sp+100]
	ld	[%sp+96], %r16
	st	%r16, [%sp+120]
	ba	funtop_151686
	st	%r8, [%sp+116]
code_158461:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_151704:
code_158463:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_cpy_code_146878,(.-TextIOFn_cpy_code_146878)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158464
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05400000
		! -------- label,sizes,reg
	.long code_158465
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05410000
		! -------- label,sizes,reg
	.long code_158466
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.text
 	.align 8
	.global TextIOFn_copyVec_code_146872
 ! arguments : [$146874,$8] [$146875,$9] [$142112,$10] [$142113,$11] [$142114,$12] 
 ! results    : [$151685,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_copyVec_code_146872:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158486
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_158486:
	st	%r15, [%sp+92]
	st	%r10, [%sp+112]
	st	%r11, [%sp+108]
	st	%r12, [%sp+104]
code_158468:
funtop_151616:
	ld	[%r9], %r8
	ld	[%r9+4], %r17
	st	%r17, [%sp+100]
	ld	[%r9+8], %r17
	st	%r17, [%sp+96]
sumarm_151629:
	ld	[%r8+20], %r8
	! ptr sub start
	ld	[%r8], %r10
	! ptr sub end
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(PLUSEbuffer_mode_137877), %r8
	or	%r8, %lo(PLUSEbuffer_mode_137877), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158485:
code_158471:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158472
	nop
code_158473:
	call	GCFromML ! delay slot empty
	nop
needgc_158472:
	cmp	%r8, 0
	bne	one_case_151654
	nop
zero_case_151653:
	sethi	%hi(anonfun_133742), %r8
	ba	after_zeroone_151655
	or	%r8, %lo(anonfun_133742), %r8
one_case_151654:
	sethi	%hi(isNL_133728), %r8
	or	%r8, %lo(isNL_133728), %r8
after_zeroone_151655:
	ba	after_sum_151626
	mov	%r8, %r9
sumarm_151630:
after_sum_151626:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 1825, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_cpy_code_146878), %r8
	or	%r8, %lo(TextIOFn_cpy_code_146878), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 0, %r12
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+104], %r11
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 128, %sp
code_158481:
	! done making tail call
code_158483:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_copyVec_code_146872,(.-TextIOFn_copyVec_code_146872)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158472
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_158485
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_outputSubstr_code_146866
 ! arguments : [$146868,$8] [$146869,$9] [$142094,$10] [$142095,$11] 
 ! results    : [$151615,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_outputSubstr_code_146866:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158586
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_158586:
	st	%r15, [%sp+92]
	mov	%r9, %r8
	st	%r10, [%sp+132]
	st	%r11, [%sp+96]
code_158487:
funtop_151338:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+128]
	ld	[%r8+8], %r17
	st	%r17, [%sp+104]
sumarm_151352:
	ld	[%sp+132], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+124]
	ld	[%sp+132], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+120]
	sethi	%hi(string_148002), %r8
	or	%r8, %lo(string_148002), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_158584:
code_158489:
	! done making normal call
	! making closure call 
	sethi	%hi(base_136248), %r8
	or	%r8, %lo(base_136248), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_158575:
code_158492:
	! done making normal call
	ld	[%r8], %r17
	st	%r17, [%sp+116]
	ld	[%r8+4], %r17
	st	%r17, [%sp+112]
	ld	[%r8+8], %r17
	st	%r17, [%sp+108]
	! making closure call 
	sethi	%hi(strbindvar_r_length_138016), %r8
	or	%r8, %lo(strbindvar_r_length_138016), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_158576:
code_158495:
	! done making normal call
	add	%r4, 80, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158496
	nop
code_158497:
	call	GCFromML ! delay slot empty
	nop
needgc_158496:
	ld	[%sp+108], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r9
	bge	cmpsi_158499
	nop
code_158500:
	or	%r0, 0, %r9
cmpsi_158499:
	cmp	%r9, 0
	bne	one_case_151400
	nop
zero_case_151399:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r9
	st	%r9, [%r4]
	ld	[%sp+132], %r17
	st	%r17, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	ld	[%sp+120], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_copyVec_code_146872), %r9
	or	%r9, %lo(TextIOFn_copyVec_code_146872), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+104]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! int sub start
	ld	[%sp+124], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+100]
	! int sub end
	ld	[%sp+100], %r17
	subcc	%r8, %r17, %r17
	st	%r17, [%sp+96]
	ld	[%sp+108], %r16
	ld	[%sp+96], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	bl	cmpsi_158503
	nop
code_158504:
	or	%r0, 0, %r8
cmpsi_158503:
	cmp	%r8, 0
	bne	one_case_151429
	nop
zero_case_151428:
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r13
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+108], %r11
	jmpl	%r13, %r15
	ld	[%sp+100], %r12
code_158585:
code_158506:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_151443
	nop
zero_case_151442:
	ld	[%sp+108], %r16
	ld	[%sp+96], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	be	cmpui_158508
	nop
code_158509:
	or	%r0, 0, %r8
cmpui_158508:
	ba	after_zeroone_151444 ! delay slot empty
	nop
one_case_151443:
	or	%r0, 1, %r8
after_zeroone_151444:
	cmp	%r8, 0
	bne	one_case_151455
	nop
zero_case_151454:
	ld	[%sp+108], %r16
	ld	[%sp+100], %r17
	addcc	%r17, %r16, %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158515
	nop
code_158516:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158515:
	ld	[%r2+800], %r10
	ld	[%sp+124], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+124], %r17
	st	%r11, [%r17]
	ba	after_zeroone_151456
	or	%r0, 256, %r8
one_case_151455:
	sethi	%hi(string_148002), %r8
	or	%r8, %lo(string_148002), %r11
	! making closure call 
	ld	[%sp+128], %r17
	ld	[%r17], %r12
	ld	[%sp+128], %r17
	ld	[%r17+4], %r8
	ld	[%sp+128], %r17
	ld	[%r17+8], %r9
	ld	[%sp+132], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 144, %sp
code_158525:
	! done making tail call
after_zeroone_151456:
	ba	after_zeroone_151430 ! delay slot empty
	nop
one_case_151429:
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r13
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+96], %r11
	jmpl	%r13, %r15
	ld	[%sp+100], %r12
code_158580:
code_158527:
	! done making normal call
	sethi	%hi(string_148002), %r8
	or	%r8, %lo(string_148002), %r11
	! making closure call 
	ld	[%sp+128], %r17
	ld	[%r17], %r12
	ld	[%sp+128], %r17
	ld	[%r17+4], %r8
	ld	[%sp+128], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_158577:
code_158529:
	! done making normal call
	ld	[%sp+96], %r16
	ld	[%sp+108], %r17
	subcc	%r17, %r16, %r11
	or	%r0, 0, %r12
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r13
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+96], %r10
code_158578:
code_158530:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_151508
	nop
zero_case_151507:
	ld	[%sp+96], %r16
	ld	[%sp+108], %r17
	subcc	%r17, %r16, %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158535
	nop
code_158536:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158535:
	ld	[%r2+800], %r10
	ld	[%sp+124], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+124], %r17
	st	%r11, [%r17]
	ba	after_zeroone_151509
	or	%r0, 256, %r8
one_case_151508:
	sethi	%hi(string_148002), %r8
	or	%r8, %lo(string_148002), %r11
	! making closure call 
	ld	[%sp+128], %r17
	ld	[%r17], %r12
	ld	[%sp+128], %r17
	ld	[%r17+4], %r8
	ld	[%sp+128], %r17
	ld	[%r17+8], %r9
	ld	[%sp+132], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 144, %sp
code_158545:
	! done making tail call
after_zeroone_151509:
after_zeroone_151430:
	ba	after_zeroone_151401 ! delay slot empty
	nop
one_case_151400:
	ld	[%sp+132], %r17
	ld	[%r17+16], %r17
	st	%r17, [%sp+100]
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 8, %r4
	! done allocating 1 record
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+132], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(exn_handler_151542), %r8
	or	%r8, %lo(exn_handler_151542), %r10
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
	! int sub start
	ld	[%sp+124], %r17
	ld	[%r17], %r8
	! int sub end
	cmp	%r8, 0
	or	%r0, 1, %r8
	bg	cmpsi_158549
	nop
code_158550:
	or	%r0, 0, %r8
cmpsi_158549:
	cmp	%r8, 0
	bne	one_case_151561
	nop
zero_case_151560:
	ba	after_zeroone_151562
	or	%r0, 256, %r8
one_case_151561:
	ld	[%sp+132], %r17
	ld	[%r17+24], %r12
	! int sub start
	ld	[%sp+124], %r17
	ld	[%r17], %r9
	! int sub end
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r11
	add	%r4, 8, %r4
	! done allocating 1 record
	or	%r0, 0, %r10
	! making closure call 
	ld	[%r12], %r13
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+120], %r12
code_158583:
code_158553:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158557
	nop
code_158558:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158557:
	or	%r0, 0, %r11
	ld	[%r2+800], %r10
	ld	[%sp+124], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+124], %r17
	st	%r11, [%r17]
	or	%r0, 256, %r8
after_zeroone_151562:
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+96], %r11
	jmpl	%r13, %r15
	ld	[%sp+116], %r12
code_158574:
code_158565:
	! done making normal call
	ba	exn_handler_after_151543
	ld	[%r1+12], %r1
exn_handler_151542:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r17
	st	%r17, [%sp+104]
	ld	[%r8+4], %r17
	st	%r17, [%sp+132]
	mov	%r15, %r12
	sethi	%hi(string_148002), %r8
	or	%r8, %lo(string_148002), %r11
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r13
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+132], %r10
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 144, %sp
code_158570:
	! done making tail call
exn_handler_after_151543:
after_zeroone_151401:
	ba	after_sum_151349 ! delay slot empty
	nop
sumarm_151353:
after_sum_151349:
code_158573:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size TextIOFn_outputSubstr_code_146866,(.-TextIOFn_outputSubstr_code_146866)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158574
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158575
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50100000
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_158576
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54100000
	.word 0x00000005
		! -------- label,sizes,reg
	.long needgc_158496
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54100000
	.word 0x00000005
		! -------- label,sizes,reg
	.long afterMutateCheck_158515
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158577
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40100000
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_158578
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x00000005
		! -------- label,sizes,reg
	.long afterMutateCheck_158535
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_158557
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158580
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40100000
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_158583
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x44050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158584
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50110000
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_158585
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40000000
	.word 0x00000005
	.text
 	.align 8
	.global TextIOFn_setBufferMode_code_146899
 ! arguments : [$146901,$8] [$146902,$9] [$142202,$10] [$142203,$11] 
 ! results    : [$151337,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_setBufferMode_code_146899:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158623
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158623:
	st	%r15, [%sp+92]
	mov	%r10, %r13
	st	%r11, [%sp+100]
code_158587:
funtop_151282:
	ld	[%r9], %r10
	ld	[%r9+4], %r9
sumarm_151294:
	ld	[%r13+20], %r17
	st	%r17, [%sp+96]
sumarm_151306:
	ld	[%sp+100], %r17
	cmp	%r17, 2
	bne	sumarm_151307
	nop
code_158588:
	sethi	%hi(string_148030), %r8
	or	%r8, %lo(string_148030), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	mov	%r13, %r10
code_158621:
code_158590:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158594
	nop
code_158595:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158594:
	or	%r0, 2, %r11
	ld	[%r2+800], %r10
	ld	[%sp+96], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r11, [%r17]
	ba	after_sum_151303
	or	%r0, 256, %r8
sumarm_151307:
nomatch_sum_151304:
	sethi	%hi(string_148030), %r8
	or	%r8, %lo(string_148030), %r11
	! making closure call 
	ld	[%r10], %r12
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	jmpl	%r12, %r15
	mov	%r13, %r10
code_158622:
code_158605:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158609
	nop
code_158610:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158609:
	ld	[%r2+800], %r10
	ld	[%sp+96], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r16
	ld	[%sp+100], %r17
	st	%r17, [%r16]
	or	%r0, 256, %r8
after_sum_151303:
	ba	after_sum_151291 ! delay slot empty
	nop
sumarm_151295:
after_sum_151291:
code_158620:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_setBufferMode_code_146899,(.-TextIOFn_setBufferMode_code_146899)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158621
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long afterMutateCheck_158594
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long afterMutateCheck_158609
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_158622
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_getBufferMode_code_146908
 ! arguments : [$146910,$8] [$146911,$9] [$134336,$10] 
 ! results    : [$151281,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getBufferMode_code_146908:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158631
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158631:
	st	%r15, [%sp+92]
code_158624:
funtop_151254:
sumarm_151262:
	ld	[%r10+20], %r17
	st	%r17, [%sp+96]
	sethi	%hi(string_148045), %r8
	or	%r8, %lo(string_148045), %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158630:
code_158626:
	! done making normal call
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! ptr sub end
	ba	after_sum_151259 ! delay slot empty
	nop
sumarm_151263:
after_sum_151259:
code_158629:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_getBufferMode_code_146908,(.-TextIOFn_getBufferMode_code_146908)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158630
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_input_code_146915
 ! arguments : [$146917,$8] [$146918,$9] [$134383,$10] 
 ! results    : [$151247,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_input_code_146915:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158649
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158649:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_158632:
funtop_151232:
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_158648:
code_158633:
	! done making normal call
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158637
	nop
code_158638:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158637:
	ld	[%r2+800], %r9
	ld	[%sp+96], %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r11, [%r17]
code_158647:
	mov	%r12, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_input_code_146915,(.-TextIOFn_input_code_146915)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_158637
	.word 0xb8003806
	.word 0x00001800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_158648
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_input1_code_146922
 ! arguments : [$146924,$8] [$146925,$9] [$134391,$10] 
 ! results    : [$151193,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_input1_code_146922:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158673
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158673:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_158650:
funtop_151166:
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_158672:
code_158651:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158652
	nop
code_158653:
	call	GCFromML ! delay slot empty
	nop
needgc_158652:
sumarm_151188:
	cmp	%r8, 0
	bne	sumarm_151189
	nop
code_158655:
	ba	after_sum_151185
	or	%r0, 0, %r8
sumarm_151189:
	ld	[%r8], %r9
	ld	[%r8+4], %r12
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r11
	add	%r4, 8, %r4
	! done allocating 1 record
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158660
	nop
code_158661:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158660:
	ld	[%r2+800], %r9
	ld	[%sp+96], %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r12, [%r17]
	ba	after_sum_151185
	mov	%r11, %r8
sumarm_151194:
after_sum_151185:
code_158671:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_input1_code_146922,(.-TextIOFn_input1_code_146922)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158652
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long afterMutateCheck_158660
	.word 0xb8003806
	.word 0x00001800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_158672
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_inputN_code_146929
 ! arguments : [$146931,$8] [$146932,$9] [$142256,$10] [$142257,$11] 
 ! results    : [$151159,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputN_code_146929:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158691
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158691:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_158674:
funtop_151142:
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158690:
code_158675:
	! done making normal call
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158679
	nop
code_158680:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158679:
	ld	[%r2+800], %r9
	ld	[%sp+96], %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r11, [%r17]
code_158689:
	mov	%r12, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_inputN_code_146929,(.-TextIOFn_inputN_code_146929)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_158679
	.word 0xb8003806
	.word 0x00001800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_158690
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_inputAll_code_146936
 ! arguments : [$146938,$8] [$146939,$9] [$134416,$10] 
 ! results    : [$151135,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputAll_code_146936:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158709
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158709:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_158692:
funtop_151120:
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_158708:
code_158693:
	! done making normal call
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158697
	nop
code_158698:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158697:
	ld	[%r2+800], %r9
	ld	[%sp+96], %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r11, [%r17]
code_158707:
	mov	%r12, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_inputAll_code_146936,(.-TextIOFn_inputAll_code_146936)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_158697
	.word 0xb8003806
	.word 0x00001800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_158708
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_lookahead_code_146943
 ! arguments : [$146945,$8] [$146946,$9] [$134429,$10] 
 ! results    : [$151091,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_lookahead_code_146943:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158721
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158721:
	st	%r15, [%sp+92]
code_158710:
funtop_151064:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_158720:
code_158711:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158712
	nop
code_158713:
	call	GCFromML ! delay slot empty
	nop
needgc_158712:
sumarm_151086:
	cmp	%r8, 0
	bne	sumarm_151087
	nop
code_158715:
	ba	after_sum_151083
	or	%r0, 0, %r8
sumarm_151087:
	ld	[%r8], %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_151083 ! delay slot empty
	nop
sumarm_151092:
after_sum_151083:
code_158719:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_lookahead_code_146943,(.-TextIOFn_lookahead_code_146943)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158712
	.word 0xb8003006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158720
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_closeIn_code_146950
 ! arguments : [$146952,$8] [$146953,$9] [$134442,$10] 
 ! results    : [$151063,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_closeIn_code_146950:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158746
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158746:
	st	%r15, [%sp+92]
	st	%r10, [%sp+104]
code_158722:
funtop_151001:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158723
	nop
code_158724:
	call	GCFromML ! delay slot empty
	nop
needgc_158723:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r12
	! ptr sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	! ptr sub end
sumarm_151017:
	ld	[%r11], %r10
sumarm_151029:
	ld	[%r10+8], %r9
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	ba	after_sum_151026 ! delay slot empty
	nop
sumarm_151030:
after_sum_151026:
	ba	after_sum_151014 ! delay slot empty
	nop
sumarm_151018:
after_sum_151014:
	ld	[%r8], %r10
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_158744:
code_158728:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_158745:
	mov	%r8, %r11
code_158729:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158733
	nop
code_158734:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158733:
	ld	[%r2+800], %r9
	ld	[%sp+104], %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%sp+104], %r17
	ld	[%r17], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+104], %r17
	st	%r11, [%r17]
	or	%r0, 256, %r8
code_158743:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_closeIn_code_146950,(.-TextIOFn_closeIn_code_146950)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158744
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_158723
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_158745
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_158733
	.word 0xb8003806
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.text
 	.align 8
	.global TextIOFn_endOfStream_code_146959
 ! arguments : [$146961,$8] [$146962,$9] [$134460,$10] 
 ! results    : [$151000,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_endOfStream_code_146959:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158752
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158752:
	st	%r15, [%sp+92]
code_158747:
funtop_150988:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158748:
	! done making tail call
code_158750:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_endOfStream_code_146959,(.-TextIOFn_endOfStream_code_146959)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_getPosIn_code_146966
 ! arguments : [$146968,$8] [$146969,$9] [$134463,$10] 
 ! results    : [$150987,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getPosIn_code_146966:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158758
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158758:
	st	%r15, [%sp+92]
code_158753:
funtop_150975:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158754:
	! done making tail call
code_158756:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_getPosIn_code_146966,(.-TextIOFn_getPosIn_code_146966)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_output_code_146973
 ! arguments : [$146975,$8] [$146976,$9] [$142290,$10] [$142291,$11] 
 ! results    : [$150974,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_output_code_146973:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158764
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158764:
	st	%r15, [%sp+92]
code_158759:
funtop_150961:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_158760:
	! done making tail call
code_158762:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_output_code_146973,(.-TextIOFn_output_code_146973)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_output1_code_146980
 ! arguments : [$146982,$8] [$146983,$9] [$142298,$10] [$142299,$11] 
 ! results    : [$150960,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_output1_code_146980:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158770
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158770:
	st	%r15, [%sp+92]
code_158765:
funtop_150947:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_158766:
	! done making tail call
code_158768:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_output1_code_146980,(.-TextIOFn_output1_code_146980)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_flushOut_code_146987
 ! arguments : [$146989,$8] [$146990,$9] [$134482,$10] 
 ! results    : [$150946,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_flushOut_code_146987:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158776
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158776:
	st	%r15, [%sp+92]
code_158771:
funtop_150934:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158772:
	! done making tail call
code_158774:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_flushOut_code_146987,(.-TextIOFn_flushOut_code_146987)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_closeOut_code_146994
 ! arguments : [$146996,$8] [$146997,$9] [$134485,$10] 
 ! results    : [$150933,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_closeOut_code_146994:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158782
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158782:
	st	%r15, [%sp+92]
code_158777:
funtop_150921:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158778:
	! done making tail call
code_158780:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_closeOut_code_146994,(.-TextIOFn_closeOut_code_146994)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_getPosOut_code_147001
 ! arguments : [$147003,$8] [$147004,$9] [$134488,$10] 
 ! results    : [$150920,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_getPosOut_code_147001:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158788
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158788:
	st	%r15, [%sp+92]
code_158783:
funtop_150908:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158784:
	! done making tail call
code_158786:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_getPosOut_code_147001,(.-TextIOFn_getPosOut_code_147001)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_setPosOut_code_147008
 ! arguments : [$147010,$8] [$147011,$9] [$142312,$10] [$142313,$11] 
 ! results    : [$150907,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_setPosOut_code_147008:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158807
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158807:
	st	%r15, [%sp+92]
	mov	%r9, %r12
	mov	%r10, %r18
	mov	%r11, %r13
code_158789:
funtop_150883:
sumarm_150891:
	ld	[%r13+4], %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158793
	nop
code_158794:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158793:
	ld	[%r2+800], %r9
	mov	%r18, %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%r18], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	st	%r11, [%r18]
	! making closure call 
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	mov	%r13, %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158802:
	! done making tail call
	ba	after_sum_150888 ! delay slot empty
	nop
sumarm_150892:
after_sum_150888:
code_158805:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_setPosOut_code_147008,(.-TextIOFn_setPosOut_code_147008)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_158793
	.word 0xb8003006
	.word 0x00043800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_mkInstream_code_147015
 ! arguments : [$147017,$8] [$147018,$9] [$134501,$10] 
 ! results    : [$150869,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_mkInstream_code_147015:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158821
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158821:
	st	%r15, [%sp+92]
	mov	%r10, %r11
code_158808:
funtop_150863:
	or	%r0, 1, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_150875
	nop
code_158809:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_158820:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_158810:
	ba	array_ptr_aftert_150874 ! delay slot empty
	nop
array_ptr_alloc_150875:
	sll	%r9, 2, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r10
	add	%r10, %r0, %r10
	or	%r10, 5, %r10
	or	%r0, 1, %r8
	add	%r8, %r9, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158812
	nop
code_158813:
	call	GCFromML ! delay slot empty
	nop
needgc_158812:
	! storing tag
	st	%r10, [%r4]
	add	%r4, 4, %r10
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_150881
	add	%r9, %r0, %r9
array_init_loopto_150882:
	add	%r10, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_150881:
	cmp	%r9, 0
	bge	array_init_loopto_150882
	nop
array_ptr_aftert_150874:
code_158819:
	mov	%r10, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_mkInstream_code_147015,(.-TextIOFn_mkInstream_code_147015)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158820
	.word 0xb8003006
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_158812
	.word 0xb8003006
	.word 0x80000800
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_getInstream_code_147020
 ! arguments : [$147022,$8] [$147023,$9] [$134504,$10] 
 ! results    : [$150861,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
TextIOFn_getInstream_code_147020:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158825
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158825:
	st	%r15, [%sp+92]
code_158822:
funtop_150856:
	! ptr sub start
	ld	[%r10], %r8
	! ptr sub end
code_158824:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_getInstream_code_147020,(.-TextIOFn_getInstream_code_147020)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_setInstream_code_147025
 ! arguments : [$147027,$8] [$147028,$9] [$142319,$10] [$142320,$11] 
 ! results    : [$150855,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
TextIOFn_setInstream_code_147025:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158841
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158841:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_158826:
funtop_150849:
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158830
	nop
code_158831:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158830:
	ld	[%r2+800], %r9
	mov	%r12, %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%r12], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	st	%r11, [%r12]
	or	%r0, 256, %r8
code_158840:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_setInstream_code_147025,(.-TextIOFn_setInstream_code_147025)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_158830
	.word 0xb8003006
	.word 0xbffc3800
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_mkOutstream_code_147030
 ! arguments : [$147032,$8] [$147033,$9] [$134513,$10] 
 ! results    : [$150835,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_mkOutstream_code_147030:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158855
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158855:
	st	%r15, [%sp+92]
	mov	%r10, %r11
code_158842:
funtop_150829:
	or	%r0, 1, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_150841
	nop
code_158843:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_158854:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_158844:
	ba	array_ptr_aftert_150840 ! delay slot empty
	nop
array_ptr_alloc_150841:
	sll	%r9, 2, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r10
	add	%r10, %r0, %r10
	or	%r10, 5, %r10
	or	%r0, 1, %r8
	add	%r8, %r9, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158846
	nop
code_158847:
	call	GCFromML ! delay slot empty
	nop
needgc_158846:
	! storing tag
	st	%r10, [%r4]
	add	%r4, 4, %r10
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_150847
	add	%r9, %r0, %r9
array_init_loopto_150848:
	add	%r10, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_150847:
	cmp	%r9, 0
	bge	array_init_loopto_150848
	nop
array_ptr_aftert_150840:
code_158853:
	mov	%r10, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_mkOutstream_code_147030,(.-TextIOFn_mkOutstream_code_147030)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158854
	.word 0xb8003006
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_158846
	.word 0xb8003006
	.word 0x80000800
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_getOutstream_code_147035
 ! arguments : [$147037,$8] [$147038,$9] [$134516,$10] 
 ! results    : [$150827,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
TextIOFn_getOutstream_code_147035:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158859
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158859:
	st	%r15, [%sp+92]
code_158856:
funtop_150822:
	! ptr sub start
	ld	[%r10], %r8
	! ptr sub end
code_158858:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_getOutstream_code_147035,(.-TextIOFn_getOutstream_code_147035)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_setOutstream_code_147040
 ! arguments : [$147042,$8] [$147043,$9] [$142324,$10] [$142325,$11] 
 ! results    : [$150821,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
TextIOFn_setOutstream_code_147040:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158875
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158875:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_158860:
funtop_150815:
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158864
	nop
code_158865:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158864:
	ld	[%r2+800], %r9
	mov	%r12, %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%r12], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	st	%r11, [%r12]
	or	%r0, 256, %r8
code_158874:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_setOutstream_code_147040,(.-TextIOFn_setOutstream_code_147040)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_158864
	.word 0xb8003006
	.word 0xbffc3800
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_openOut_code_147045
 ! arguments : [$147047,$8] [$147048,$9] [$134564,$10] 
 ! results    : [$150799,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_openOut_code_147045:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158922
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158922:
	st	%r15, [%sp+92]
code_158876:
funtop_150685:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158877
	nop
code_158878:
	call	GCFromML ! delay slot empty
	nop
needgc_158877:
	ld	[%r9], %r17
	st	%r17, [%sp+104]
	ld	[%r9+4], %r17
	st	%r17, [%sp+100]
	ld	[%r9+8], %r13
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r12
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(exn_handler_150694), %r8
	or	%r8, %lo(exn_handler_150694), %r11
	ld	[%r2+816], %r8
	sub	%sp, %r8, %r9
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r8, %r1
	! making closure call 
	ld	[%r13], %r11
	ld	[%r13+4], %r8
	jmpl	%r11, %r15
	ld	[%r13+8], %r9
code_158920:
	st	%r8, [%sp+96]
code_158882:
	! done making normal call
sumarm_150715:
	ld	[%sp+96], %r17
	ld	[%r17+16], %r8
sumarm_150731:
	cmp	%r8, 0
	bne	sumarm_150732
	nop
code_158883:
	ba	after_sum_150728
	or	%r0, 0, %r8
sumarm_150732:
	ld	[%r8], %r10
	! making closure call 
	sethi	%hi(kind_138991), %r8
	or	%r8, %lo(kind_138991), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_158921:
	mov	%r8, %r10
code_158887:
	! done making normal call
	sethi	%hi(OS_STR_c_INT), %r8
	or	%r8, %lo(OS_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	ld	[%r8+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_158890
	nop
code_158891:
	or	%r0, 0, %r8
cmpui_158890:
	cmp	%r8, 0
	be	else_case_150766
	nop
code_158892:
	sethi	%hi(tty_138997), %r8
	or	%r8, %lo(tty_138997), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ba	after_ite_150767
	ld	[%r8], %r11
else_case_150766:
	sethi	%hi(tty_138997), %r8
	ld	[%r8+%lo(tty_138997)], %r11
after_ite_150767:
	! making closure call 
	sethi	%hi(PLUSEiodesc_kind_138988), %r8
	or	%r8, %lo(PLUSEiodesc_kind_138988), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_158916:
code_158899:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_150779
	nop
zero_case_150778:
	ba	after_zeroone_150780
	or	%r0, 0, %r8
one_case_150779:
	or	%r0, 1, %r8
after_zeroone_150780:
	ba	after_sum_150728 ! delay slot empty
	nop
sumarm_150737:
after_sum_150728:
	ba	after_sum_150712
	mov	%r8, %r11
sumarm_150716:
after_sum_150712:
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_158917:
	mov	%r8, %r10
code_158904:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_158918:
code_158905:
	! done making normal call
	ba	exn_handler_after_150695
	ld	[%r1+12], %r1
exn_handler_150694:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r10
	mov	%r15, %r12
	sethi	%hi(string_148118), %r8
	or	%r8, %lo(string_148118), %r11
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_158919:
code_158912:
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
exn_handler_after_150695:
code_158915:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_openOut_code_147045,(.-TextIOFn_openOut_code_147045)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_158916
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_158917
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long needgc_158877
	.word 0xb8003806
	.word 0x00000600
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158918
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158919
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158920
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_158921
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
 	.align 8
	.global TextIOFn_openAppend_code_147056
 ! arguments : [$147058,$8] [$147059,$9] [$134577,$10] 
 ! results    : [$150669,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_openAppend_code_147056:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158946
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158946:
	st	%r15, [%sp+92]
code_158923:
funtop_150631:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_158924
	nop
code_158925:
	call	GCFromML ! delay slot empty
	nop
needgc_158924:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
	ld	[%r9+8], %r13
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r12
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(exn_handler_150640), %r8
	or	%r8, %lo(exn_handler_150640), %r11
	ld	[%r2+816], %r8
	sub	%sp, %r8, %r9
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r8, %r1
	! making closure call 
	ld	[%r13], %r11
	ld	[%r13+4], %r8
	jmpl	%r11, %r15
	ld	[%r13+8], %r9
code_158945:
	mov	%r8, %r10
code_158929:
	! done making normal call
	or	%r0, 2, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_158942:
	mov	%r8, %r10
code_158930:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_158943:
code_158931:
	! done making normal call
	ba	exn_handler_after_150641
	ld	[%r1+12], %r1
exn_handler_150640:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r10
	mov	%r15, %r12
	sethi	%hi(string_148129), %r8
	or	%r8, %lo(string_148129), %r11
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_158944:
code_158938:
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
exn_handler_after_150641:
code_158941:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_openAppend_code_147056,(.-TextIOFn_openAppend_code_147056)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_158924
	.word 0xb8003806
	.word 0x00000600
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158942
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_158943
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158944
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_158945
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_inputLine_code_147067
 ! arguments : [$147069,$8] [$147070,$9] [$134589,$10] 
 ! results    : [$150624,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_inputLine_code_147067:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158964
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_158964:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_158947:
funtop_150609:
	! ptr sub start
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_158963:
code_158948:
	! done making normal call
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_158952
	nop
code_158953:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_158952:
	ld	[%r2+800], %r9
	ld	[%sp+96], %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r11, [%r17]
code_158962:
	mov	%r12, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_inputLine_code_147067,(.-TextIOFn_inputLine_code_147067)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_158952
	.word 0xb8003806
	.word 0x00001800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_158963
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_outputSubstr_code_147074
 ! arguments : [$147076,$8] [$147077,$9] [$142382,$10] [$142383,$11] 
 ! results    : [$150608,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_outputSubstr_code_147074:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158970
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158970:
	st	%r15, [%sp+92]
code_158965:
funtop_150595:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_158966:
	! done making tail call
code_158968:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_outputSubstr_code_147074,(.-TextIOFn_outputSubstr_code_147074)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147088
 ! arguments : [$147090,$8] [$147091,$9] 
 ! results    : [$150594,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147088:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158976
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158976:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_158971:
funtop_150582:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158972:
	! done making tail call
code_158974:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_147088,(.-TextIOFn_anonfun_code_147088)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147097
 ! arguments : [$147099,$8] [$147100,$9] 
 ! results    : [$150581,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147097:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_158982
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_158982:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_158977:
funtop_150569:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_158978:
	! done making tail call
code_158980:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_147097,(.-TextIOFn_anonfun_code_147097)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_mkStdOut_code_147081
 ! arguments : [$147083,$8] [$147084,$9] 
 ! results    : [$150522,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_mkStdOut_code_147081:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159032
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159032:
	st	%r15, [%sp+92]
code_158983:
funtop_150392:
	ld	[%r9], %r17
	st	%r17, [%sp+104]
	ld	[%r9+4], %r17
	st	%r17, [%sp+100]
	ld	[%r9+8], %r9
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_159030:
	st	%r8, [%sp+96]
code_158984:
	! done making normal call
sumarm_150411:
	ld	[%sp+96], %r17
	ld	[%r17+16], %r8
sumarm_150427:
	cmp	%r8, 0
	bne	sumarm_150428
	nop
code_158985:
	ba	after_sum_150424
	or	%r0, 0, %r8
sumarm_150428:
	ld	[%r8], %r10
	! making closure call 
	sethi	%hi(kind_138991), %r8
	or	%r8, %lo(kind_138991), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159031:
	mov	%r8, %r10
code_158989:
	! done making normal call
	sethi	%hi(OS_STR_c_INT), %r8
	or	%r8, %lo(OS_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	ld	[%r8+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_158992
	nop
code_158993:
	or	%r0, 0, %r8
cmpui_158992:
	cmp	%r8, 0
	be	else_case_150466
	nop
code_158994:
	sethi	%hi(tty_138997), %r8
	or	%r8, %lo(tty_138997), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ba	after_ite_150467
	ld	[%r8], %r11
else_case_150466:
	sethi	%hi(tty_138997), %r8
	ld	[%r8+%lo(tty_138997)], %r11
after_ite_150467:
	! making closure call 
	sethi	%hi(PLUSEiodesc_kind_138988), %r8
	or	%r8, %lo(PLUSEiodesc_kind_138988), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_159027:
code_159001:
	! done making normal call
	cmp	%r8, 0
	bne	one_case_150479
	nop
zero_case_150478:
	ba	after_zeroone_150480
	or	%r0, 0, %r8
one_case_150479:
	or	%r0, 1, %r8
after_zeroone_150480:
	ba	after_sum_150424 ! delay slot empty
	nop
sumarm_150433:
after_sum_150424:
	ba	after_sum_150408
	mov	%r8, %r11
sumarm_150412:
after_sum_150408:
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_159028:
	mov	%r8, %r12
code_159006:
	! done making normal call
	add	%r4, 84, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159007
	nop
code_159008:
	call	GCFromML ! delay slot empty
	nop
needgc_159007:
sumarm_150499:
	ld	[%r12], %r11
	! allocating 2-record
	or	%r0, 273, %r10
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159012
	nop
code_159013:
	or	%r0, 0, %r8
cmpui_159012:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	st	%r10, [%r4]
	st	%r12, [%r4+4]
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159016
	nop
code_159017:
	or	%r0, 0, %r8
cmpui_159016:
	st	%r11, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_150496 ! delay slot empty
	nop
sumarm_150500:
after_sum_150496:
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	ld	[%r8+4], %r10
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147088), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147088), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147097), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147097), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(anonfun_134660), %r8
	or	%r8, %lo(anonfun_134660), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! making closure call 
	sethi	%hi(rebindCleaner_138384), %r8
	or	%r8, %lo(rebindCleaner_138384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_159029:
code_159024:
	! done making normal call
	ld	[%sp+96], %r16
	st	%r16, [%sp+96]
code_159026:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_mkStdOut_code_147081,(.-TextIOFn_mkStdOut_code_147081)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_159027
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_159028
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_159029
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long needgc_159007
	.word 0xb8003806
	.word 0x00001000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_159030
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_159031
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147116
 ! arguments : [$147118,$8] [$147119,$9] 
 ! results    : [$150391,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147116:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159038
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_159038:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_159033:
funtop_150379:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_159034:
	! done making tail call
code_159036:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_147116,(.-TextIOFn_anonfun_code_147116)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147125
 ! arguments : [$147127,$8] [$147128,$9] 
 ! results    : [$150378,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147125:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159044
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_159044:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_159039:
funtop_150366:
	ld	[%r8], %r9
	ld	[%r8+4], %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_159040:
	! done making tail call
code_159042:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_147125,(.-TextIOFn_anonfun_code_147125)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_mkStdErr_code_147109
 ! arguments : [$147111,$8] [$147112,$9] 
 ! results    : [$150319,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_mkStdErr_code_147109:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159071
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159071:
	st	%r15, [%sp+92]
code_159045:
funtop_150269:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
	ld	[%r9+8], %r9
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_159070:
	mov	%r8, %r10
code_159046:
	! done making normal call
	or	%r0, 2, %r11
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_159069:
	mov	%r8, %r12
code_159047:
	! done making normal call
	add	%r4, 84, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159048
	nop
code_159049:
	call	GCFromML ! delay slot empty
	nop
needgc_159048:
sumarm_150296:
	ld	[%r12], %r11
	! allocating 2-record
	or	%r0, 273, %r10
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159053
	nop
code_159054:
	or	%r0, 0, %r8
cmpui_159053:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	st	%r10, [%r4]
	st	%r12, [%r4+4]
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159057
	nop
code_159058:
	or	%r0, 0, %r8
cmpui_159057:
	st	%r11, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_150293 ! delay slot empty
	nop
sumarm_150297:
after_sum_150293:
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	ld	[%r8+4], %r10
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147116), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147116), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147125), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147125), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(anonfun_134686), %r8
	or	%r8, %lo(anonfun_134686), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! making closure call 
	sethi	%hi(rebindCleaner_138384), %r8
	or	%r8, %lo(rebindCleaner_138384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_159068:
code_159065:
	! done making normal call
	ld	[%sp+96], %r16
	st	%r16, [%sp+96]
code_159067:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_mkStdErr_code_147109,(.-TextIOFn_mkStdErr_code_147109)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_159068
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_159069
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long needgc_159048
	.word 0xb8003806
	.word 0x00001000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_159070
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_tryInput_code_147142
 ! arguments : [$147144,$8] [$147145,$9] [$141209,$10] [$141210,$11] [$141211,$12] 
 ! results    : [$150268,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_tryInput_code_147142:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159089
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159089:
	st	%r15, [%sp+92]
	st	%r9, [%sp+96]
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
	st	%r12, [%sp+100]
code_159072:
funtop_150210:
sumarm_150218:
	ld	[%sp+108], %r17
	ld	[%r17+8], %r10
	! making closure call 
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159087:
code_159075:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159076
	nop
code_159077:
	call	GCFromML ! delay slot empty
	nop
needgc_159076:
	ld	[%sp+104], %r17
	subcc	%r8, %r17, %r9
	ld	[%sp+100], %r17
	cmp	%r9, %r17
	or	%r0, 1, %r8
	bge	cmpsi_159079
	nop
code_159080:
	or	%r0, 0, %r8
cmpsi_159079:
	cmp	%r8, 0
	bne	one_case_150243
	nop
zero_case_150242:
	ld	[%sp+100], %r17
	subcc	%r17, %r9, %r11
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_159082:
	! done making tail call
	ba	after_zeroone_150244 ! delay slot empty
	nop
one_case_150243:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
after_zeroone_150244:
	ba	after_sum_150215 ! delay slot empty
	nop
sumarm_150219:
after_sum_150215:
code_159086:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_tryInput_code_147142,(.-TextIOFn_tryInput_code_147142)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159076
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00410000
		! -------- label,sizes,reg
	.long code_159087
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00410000
	.text
 	.align 8
	.global TextIOFn_nextBuf_code_147147
 ! arguments : [$147149,$8] [$147150,$9] [$141220,$10] [$141221,$11] 
 ! results    : [$150209,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_nextBuf_code_147147:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159130
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159130:
	st	%r15, [%sp+92]
	mov	%r9, %r13
	mov	%r10, %r18
	st	%r11, [%sp+104]
code_159090:
funtop_150040:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159091
	nop
code_159092:
	call	GCFromML ! delay slot empty
	nop
needgc_159091:
	ld	[%r13], %r9
	ld	[%r13+4], %r17
	st	%r17, [%sp+100]
	ld	[%r13+8], %r8
	ld	[%r13+12], %r12
	ld	[%r13+16], %r10
	ld	[%r13+20], %r17
	st	%r17, [%sp+96]
sumarm_150060:
	ld	[%r18+4], %r11
	! ptr sub start
	ld	[%r11], %r11
	! ptr sub end
sumarm_150076:
	cmp	%r11, 0
	bne	sumarm_150077
	nop
code_159094:
	! allocating 3-record
	or	%r0, 25, %r11
	st	%r11, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r18
	add	%r4, 16, %r4
	! done allocating 3 record
	sethi	%hi(exn_handler_150080), %r8
	or	%r8, %lo(exn_handler_150080), %r13
	ld	[%r2+816], %r8
	sub	%sp, %r8, %r11
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	st	%r13, [%r4+4]
	st	%r11, [%r4+8]
	st	%r18, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r8, %r1
	sethi	%hi(string_147549), %r8
	or	%r8, %lo(string_147549), %r11
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_159127:
code_159098:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159099
	nop
code_159100:
	call	GCFromML ! delay slot empty
	nop
needgc_159099:
sumarm_150104:
	cmp	%r8, 0
	bne	sumarm_150105
	nop
code_159102:
	ld	[%sp+104], %r16
	ld	[%sp+96], %r17
	subcc	%r17, %r16, %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_150101 ! delay slot empty
	nop
sumarm_150105:
	ld	[%r8], %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+104], %r12
code_159128:
code_159104:
	! done making normal call
	ba	after_sum_150101 ! delay slot empty
	nop
sumarm_150122:
after_sum_150101:
	ba	exn_handler_after_150081
	ld	[%r1+12], %r1
exn_handler_150080:
	ld	[%r1+8], %r9
	ld	[%r1+12], %r1
	ld	[%r9], %r17
	st	%r17, [%sp+96]
	ld	[%r9+4], %r8
	ld	[%r9+8], %r17
	st	%r17, [%sp+104]
	mov	%r15, %r11
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159109
	nop
code_159110:
	call	GCFromML ! delay slot empty
	nop
needgc_159109:
	ld	[%r11], %r10
exnarm_150141:
	sethi	%hi(stamp_136957), %r9
	ld	[%r9+%lo(stamp_136957)], %r9
	cmp	%r10, %r9
	bne	exnarm_150144
	nop
code_159113:
	ld	[%r11+4], %r9
	ld	[%r9+8], %r10
	ld	[%r10], %r9
exnarm_150153:
	cmp	%r9, %r8
	bne	exnarm_150155
	nop
code_159114:
	ld	[%r10+4], %r8
	ld	[%sp+104], %r16
	ld	[%sp+96], %r17
	subcc	%r17, %r16, %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	afterPLUSexncase_150152 ! delay slot empty
	nop
exnarm_150155:
	mov	%r11, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
afterPLUSexncase_150152:
	ba	afterPLUSexncase_150140 ! delay slot empty
	nop
exnarm_150144:
	mov	%r11, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
afterPLUSexncase_150140:
exn_handler_after_150081:
	ba	after_sum_150073 ! delay slot empty
	nop
sumarm_150077:
	cmp	%r11, 1
	bne	sumarm_150179
	nop
code_159120:
	ld	[%sp+104], %r16
	ld	[%sp+96], %r17
	subcc	%r17, %r16, %r9
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_150073 ! delay slot empty
	nop
sumarm_150179:
	ld	[%r11], %r10
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_159122:
	! done making tail call
	ba	after_sum_150073 ! delay slot empty
	nop
sumarm_150195:
after_sum_150073:
	ba	after_sum_150057 ! delay slot empty
	nop
sumarm_150061:
after_sum_150057:
code_159126:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_nextBuf_code_147147,(.-TextIOFn_nextBuf_code_147147)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159091
	.word 0xb8003806
	.word 0x00042000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159127
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long needgc_159099
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long needgc_159109
	.word 0xb8003806
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159128
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_canInput_code_147137
 ! arguments : [$147139,$8] [$147140,$9] [$141190,$10] [$141191,$11] 
 ! results    : [$150039,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_canInput_code_147137:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159167
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_159167:
	st	%r15, [%sp+92]
	st	%r11, [%sp+116]
code_159131:
funtop_149874:
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159132
	nop
code_159133:
	call	GCFromML ! delay slot empty
	nop
needgc_159132:
	ld	[%r9], %r17
	st	%r17, [%sp+112]
	ld	[%r9+4], %r17
	st	%r17, [%sp+108]
sumarm_149886:
	ld	[%sp+116], %r17
	cmp	%r17, 0
	or	%r0, 1, %r17
	st	%r17, [%sp+104]
	bl	cmpsi_159135
	nop
code_159136:
	or	%r0, 0, %r17
	st	%r17, [%sp+104]
cmpsi_159135:
	ld	[%r10], %r17
	st	%r17, [%sp+100]
	ld	[%r10+4], %r17
	st	%r17, [%sp+96]
sumarm_149904:
	ld	[%sp+100], %r17
	ld	[%r17+12], %r9
sumarm_149916:
	ld	[%r9+16], %r8
sumarm_149932:
	cmp	%r8, 0
	bne	sumarm_149933
	nop
sumarm_149941:
	ld	[%r9+4], %r8
sumarm_149953:
	ld	[%r8+24], %r10
	sethi	%hi(string_147549), %r8
	or	%r8, %lo(string_147549), %r11
	sethi	%hi(mk_136899), %r8
	or	%r8, %lo(mk_136899), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_159165:
code_159143:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159144
	nop
code_159145:
	call	GCFromML ! delay slot empty
	nop
needgc_159144:
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_149950
	or	%r0, 0, %r8
sumarm_149954:
after_sum_149950:
	ba	after_sum_149938 ! delay slot empty
	nop
sumarm_149942:
after_sum_149938:
	ba	after_sum_149929 ! delay slot empty
	nop
sumarm_149933:
	ba	after_sum_149929 ! delay slot empty
	nop
sumarm_149979:
after_sum_149929:
	ba	after_sum_149913 ! delay slot empty
	nop
sumarm_149917:
after_sum_149913:
	ba	after_sum_149901 ! delay slot empty
	nop
sumarm_149905:
after_sum_149901:
	! allocating 2 closures
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_nextBuf_code_147147), %r9
	or	%r9, %lo(TextIOFn_nextBuf_code_147147), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	or	%r0, 258, %r9
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_tryInput_code_147142), %r9
	or	%r9, %lo(TextIOFn_tryInput_code_147142), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	or	%r0, 258, %r9
	st	%r9, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 6-record
	sethi	%hi(6961), %r9
	or	%r9, %lo(6961), %r9
	st	%r9, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r10, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	st	%r8, [%r4+20]
	ld	[%sp+116], %r17
	st	%r17, [%r4+24]
	add	%r4, 4, %r8
	add	%r4, 28, %r4
	! done allocating 6 record
	st	%r8, [%r11+8]
	st	%r11, [%r10+8]
	! done allocating 2 closures
	ld	[%sp+104], %r17
	cmp	%r17, 0
	bne	one_case_150022
	nop
zero_case_150021:
	! making closure call 
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+96], %r11
	ld	[%sp+116], %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 128, %sp
code_159157:
	! done making tail call
	ba	after_zeroone_150023 ! delay slot empty
	nop
one_case_150022:
	sethi	%hi(mk_136989), %r8
	or	%r8, %lo(mk_136989), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_zeroone_150023:
	ba	after_sum_149883 ! delay slot empty
	nop
sumarm_149887:
after_sum_149883:
code_159164:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_canInput_code_147137,(.-TextIOFn_canInput_code_147137)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159132
	.word 0xb8004006
	.word 0x00000600
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_159144
	.word 0xb8004006
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
		! -------- label,sizes,reg
	.long code_159165
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147173
 ! arguments : [$147175,$8] [$147176,$9] [$133371,$10] 
 ! results    : [$149861,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147173:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159177
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159177:
	st	%r15, [%sp+92]
code_159168:
funtop_149834:
	ld	[%r9], %r17
	st	%r17, [%sp+96]
	ld	[%r9+4], %r9
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159176:
code_159169:
	! done making normal call
sumarm_149856:
	cmp	%r8, 0
	bne	sumarm_149857
	nop
code_159170:
	ld	[%sp+96], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+816], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_sum_149853
	or	%r0, 0, %r8
sumarm_149857:
	ba	after_sum_149853 ! delay slot empty
	nop
sumarm_149862:
after_sum_149853:
code_159175:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_anonfun_code_147173,(.-TextIOFn_anonfun_code_147173)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_159176
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147182
 ! arguments : [$147184,$8] [$147185,$9] 
 ! results    : [$149830,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147182:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159186
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_159186:
	st	%r15, [%sp+92]
code_159178:
funtop_149816:
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_159185:
	mov	%r8, %r9
code_159179:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159180
	nop
code_159181:
	call	GCFromML ! delay slot empty
	nop
needgc_159180:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
code_159184:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_147182,(.-TextIOFn_anonfun_code_147182)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159180
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159185
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147190
 ! arguments : [$147192,$8] [$147193,$9] 
 ! results    : [$149815,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147190:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159202
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_159202:
	st	%r15, [%sp+92]
	mov	%r9, %r12
code_159187:
funtop_149808:
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_159191
	nop
code_159192:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159191:
	or	%r0, 1, %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%r12], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	st	%r11, [%r12]
	or	%r0, 256, %r8
code_159201:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_147190,(.-TextIOFn_anonfun_code_147190)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_159191
	.word 0xb8003006
	.word 0xbffc3000
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147197
 ! arguments : [$147199,$8] [$147200,$9] 
 ! results    : [$149807,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147197:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159218
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_159218:
	st	%r15, [%sp+92]
	mov	%r9, %r12
code_159203:
funtop_149800:
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_159207
	nop
code_159208:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159207:
	or	%r0, 1, %r11
	ld	[%r2+800], %r10
	mov	%r12, %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%r12], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	st	%r11, [%r12]
	or	%r0, 256, %r8
code_159217:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_anonfun_code_147197,(.-TextIOFn_anonfun_code_147197)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_159207
	.word 0xb8003006
	.word 0xbffc3000
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
 	.align 8
	.global TextIOFn_mkInstream_code_147168
 ! arguments : [$147170,$8] [$147171,$9] [$141296,$10] [$141297,$11] 
 ! results    : [$149793,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_mkInstream_code_147168:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159301
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_159301:
	st	%r15, [%sp+92]
	st	%r10, [%sp+124]
	st	%r11, [%sp+120]
code_159219:
funtop_149476:
	add	%r4, 64, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159220
	nop
code_159221:
	call	GCFromML ! delay slot empty
	nop
needgc_159220:
	ld	[%r9], %r11
	ld	[%r9+4], %r10
	ld	[%r9+8], %r17
	st	%r17, [%sp+116]
sumarm_149491:
	ld	[%sp+124], %r17
	ld	[%r17+12], %r18
	ld	[%sp+124], %r17
	ld	[%r17+8], %r13
	ld	[%sp+124], %r17
	ld	[%r17+32], %r12
	ld	[%sp+124], %r17
	ld	[%r17+44], %r9
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r12, [%r4+8]
	st	%r13, [%r4+12]
	st	%r18, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	ba	after_sum_149488 ! delay slot empty
	nop
sumarm_149492:
after_sum_149488:
	ld	[%r8], %r12
	ld	[%r8+4], %r13
	ld	[%r8+8], %r9
	ld	[%r8+12], %r8
sumarm_149529:
	cmp	%r12, 0
	bne	sumarm_149530
	nop
code_159224:
	sethi	%hi(anonfun_133355), %r12
	or	%r12, %lo(anonfun_133355), %r12
	ba	after_sum_149526
	st	%r12, [%sp+112]
sumarm_149530:
	ba	after_sum_149526
	st	%r12, [%sp+112]
sumarm_149535:
after_sum_149526:
sumarm_149552:
	cmp	%r13, 0
	bne	sumarm_149553
	nop
code_159228:
	or	%r0, 0, %r11
	ba	after_sum_149549
	st	%r11, [%sp+108]
sumarm_149553:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r12
	st	%r12, [%r4]
	st	%r11, [%r4+4]
	st	%r13, [%r4+8]
	add	%r4, 4, %r12
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r11
	st	%r11, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147173), %r11
	or	%r11, %lo(TextIOFn_anonfun_code_147173), %r11
	st	%r11, [%r4+4]
	or	%r0, 256, %r11
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	after_sum_149549
	st	%r11, [%sp+108]
sumarm_149558:
after_sum_149549:
sumarm_149599:
	or	%r0, 255, %r11
	cmp	%r9, %r11
	ble	nomatch_sum_149597
	nop
sumarm_149622:
	or	%r0, 255, %r11
	cmp	%r8, %r11
	ble	nomatch_sum_149620
	nop
code_159233:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147182), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147182), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	after_sum_149619 ! delay slot empty
	nop
sumarm_149623:
nomatch_sum_149620:
	sethi	%hi(anonfun_142922), %r8
	or	%r8, %lo(anonfun_142922), %r8
after_sum_149619:
	ba	after_sum_149596
	st	%r8, [%sp+104]
sumarm_149600:
nomatch_sum_149597:
	sethi	%hi(anonfun_142935), %r8
	or	%r8, %lo(anonfun_142935), %r8
	st	%r8, [%sp+104]
after_sum_149596:
	or	%r0, 1, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_149651
	nop
code_159239:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r10, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_159296:
	call	load_regs_MLtoC ! delay slot empty
	nop
	st	%r8, [%sp+100]
code_159240:
	ba	array_ptr_aftert_149650 ! delay slot empty
	nop
array_ptr_alloc_149651:
	sll	%r9, 2, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r11
	add	%r11, %r0, %r11
	or	%r11, 5, %r11
	or	%r0, 1, %r8
	add	%r8, %r9, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159242
	nop
code_159243:
	call	GCFromML ! delay slot empty
	nop
needgc_159242:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r17
	st	%r17, [%sp+100]
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_149657
	add	%r9, %r0, %r9
array_init_loopto_149658:
	ld	[%sp+100], %r17
	add	%r17, %r9, %r8
	st	%r10, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_149657:
	cmp	%r9, 0
	bge	array_init_loopto_149658
	nop
array_ptr_aftert_149650:
	or	%r0, 1, %r10
	or	%r0, 0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_ptr_alloc_149669
	nop
code_159248:
	call	save_regs_MLtoC
	mov	%r10, %r8
	call	alloc_bigptrarray ! delay slot empty
	nop
code_159297:
	call	load_regs_MLtoC ! delay slot empty
	nop
	st	%r8, [%sp+96]
code_159249:
	ba	array_ptr_aftert_149668 ! delay slot empty
	nop
array_ptr_alloc_149669:
	sll	%r10, 2, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r11
	add	%r11, %r0, %r11
	or	%r11, 5, %r11
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159251
	nop
code_159252:
	call	GCFromML ! delay slot empty
	nop
needgc_159251:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_149675
	add	%r10, %r0, %r10
array_init_loopto_149676:
	ld	[%sp+96], %r17
	add	%r17, %r10, %r8
	st	%r9, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_149675:
	cmp	%r10, 0
	bge	array_init_loopto_149676
	nop
array_ptr_aftert_149668:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159257
	nop
code_159258:
	call	GCFromML ! delay slot empty
	nop
needgc_159257:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147190), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147190), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147197), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147197), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(anonfun_133419), %r8
	or	%r8, %lo(anonfun_133419), %r12
	! making closure call 
	sethi	%hi(addCleaner_137245), %r8
	or	%r8, %lo(addCleaner_137245), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_159300:
	mov	%r8, %r31
code_159265:
	! done making normal call
	or	%r0, 1, %r8
	or	%r0, 510, %r9
	cmp	%r8, %r9
	ble	array_ptr_alloc_149714
	nop
code_159266:
	call	save_regs_MLtoC
	ld	[%sp+100], %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_159298:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r9
code_159267:
	ba	array_ptr_aftert_149713 ! delay slot empty
	nop
array_ptr_alloc_149714:
	sll	%r8, 2, %r9
	add	%r9, %r0, %r9
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 5, %r9
	or	%r0, 1, %r10
	add	%r10, %r8, %r10
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159269
	nop
code_159270:
	call	GCFromML ! delay slot empty
	nop
needgc_159269:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r9
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_149720
	add	%r10, %r0, %r10
array_init_loopto_149721:
	add	%r9, %r10, %r8
	ld	[%sp+100], %r17
	st	%r17, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_149720:
	cmp	%r10, 0
	bge	array_init_loopto_149721
	nop
array_ptr_aftert_149713:
	add	%r4, 64, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159275
	nop
code_159276:
	call	GCFromML ! delay slot empty
	nop
needgc_159275:
	! allocating 7-record
	sethi	%hi(32313), %r11
	or	%r11, %lo(32313), %r11
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159280
	nop
code_159281:
	or	%r0, 0, %r8
cmpui_159280:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r11
	st	%r11, [%r4]
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r10
	ld	[%r2+812], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159284
	nop
code_159285:
	or	%r0, 0, %r8
cmpui_159284:
	st	%r31, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	st	%r9, [%r4+16]
	ld	[%sp+108], %r17
	st	%r17, [%r4+20]
	ld	[%sp+96], %r17
	st	%r17, [%r4+24]
	ld	[%sp+112], %r17
	st	%r17, [%r4+28]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 32, %r4
	! done allocating 7 record
sumarm_149752:
	ld	[%sp+120], %r17
	cmp	%r17, 0
	bne	sumarm_149753
	nop
code_159286:
	! making closure polycall
	ld	[%sp+104], %r17
	ld	[%r17], %r10
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_159299:
	mov	%r8, %r9
code_159287:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159288
	nop
code_159289:
	call	GCFromML ! delay slot empty
	nop
needgc_159288:
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	sethi	%hi(string_147410), %r8
	or	%r8, %lo(string_147410), %r8
	st	%r8, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	ba	after_sum_149749
	mov	%r8, %r9
sumarm_149753:
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	ld	[%sp+120], %r17
	st	%r17, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	ba	after_sum_149749
	mov	%r8, %r9
sumarm_149773:
after_sum_149749:
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
code_159295:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_mkInstream_code_147168,(.-TextIOFn_mkInstream_code_147168)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159220
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50000000
		! -------- label,sizes,reg
	.long code_159296
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
		! -------- label,sizes,reg
	.long needgc_159242
	.word 0xb8004006
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
		! -------- label,sizes,reg
	.long code_159297
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55540000
		! -------- label,sizes,reg
	.long needgc_159251
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55540000
		! -------- label,sizes,reg
	.long needgc_159257
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
		! -------- label,sizes,reg
	.long code_159298
	.word 0xb8004008
	.word 0x00000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_159269
	.word 0xb8004008
	.word 0x00000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_159275
	.word 0xb8004008
	.word 0x00000200
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
		! worddata
	.word 0x00000002
	.long CleanIO_STR_c_INT
		! -------- label,sizes,reg
	.long code_159299
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_159288
	.word 0xb8004006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_159300
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.text
 	.align 8
	.global TextIOFn_setPosIn_code_147208
 ! arguments : [$147210,$8] [$147211,$9] [$133590,$10] 
 ! results    : [$149475,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_setPosIn_code_147208:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159323
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_159323:
	st	%r15, [%sp+92]
code_159302:
funtop_149384:
	ld	[%r9], %r17
	st	%r17, [%sp+112]
	ld	[%r9+4], %r17
	st	%r17, [%sp+108]
	ld	[%r9+8], %r9
sumarm_149398:
	ld	[%r10+8], %r17
	st	%r17, [%sp+104]
sumarm_149410:
	ld	[%sp+104], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159322:
	st	%r8, [%sp+100]
code_159303:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159304
	nop
code_159305:
	call	GCFromML ! delay slot empty
	nop
needgc_159304:
sumarm_149428:
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	ba	after_sum_149425 ! delay slot empty
	nop
sumarm_149429:
after_sum_149425:
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r11
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_159318:
code_159308:
	! done making normal call
	ld	[%sp+96], %r17
	ld	[%r17+12], %r10
	! making closure call 
	sethi	%hi(_137603), %r8
	or	%r8, %lo(_137603), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159319:
	mov	%r8, %r9
code_159311:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_159320:
code_159312:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_159313:
	! done making tail call
	ba	after_sum_149407 ! delay slot empty
	nop
sumarm_149411:
after_sum_149407:
	ba	after_sum_149395 ! delay slot empty
	nop
sumarm_149399:
after_sum_149395:
code_159317:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_setPosIn_code_147208,(.-TextIOFn_setPosIn_code_147208)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_159318
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00410000
		! -------- label,sizes,reg
	.long needgc_159304
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01510000
		! -------- label,sizes,reg
	.long code_159319
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00410000
		! -------- label,sizes,reg
	.long code_159320
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00410000
		! -------- label,sizes,reg
	.long code_159322
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01510000
	.text
 	.align 8
	.global TextIOFn_canInput_code_147219
 ! arguments : [$147221,$8] [$147222,$9] [$142266,$10] [$142267,$11] 
 ! results    : [$149383,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_canInput_code_147219:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159329
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_159329:
	st	%r15, [%sp+92]
code_159324:
funtop_149370:
	! ptr sub start
	ld	[%r10], %r10
	! ptr sub end
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_159325:
	! done making tail call
code_159327:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size TextIOFn_canInput_code_147219,(.-TextIOFn_canInput_code_147219)

	.section	".rodata"
	.text
 	.align 8
	.global TextIOFn_setPosIn_code_147226
 ! arguments : [$147228,$8] [$147229,$9] [$142286,$10] [$142287,$11] 
 ! results    : [$149369,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_setPosIn_code_147226:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159347
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159347:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
	mov	%r11, %r10
code_159330:
funtop_149357:
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159346:
	mov	%r8, %r11
code_159331:
	! done making normal call
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_159335
	nop
code_159336:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159335:
	ld	[%r2+800], %r9
	ld	[%sp+96], %r10
	or	%r0, 0, %r8
	st	%r10, [%r9]
	st	%r8, [%r9+4]
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	st	%r8, [%r9+8]
	add	%r9, 12, %r8
	st	%r8, [%r2+800]
	ld	[%sp+96], %r17
	st	%r11, [%r17]
	or	%r0, 256, %r8
code_159345:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_setPosIn_code_147226,(.-TextIOFn_setPosIn_code_147226)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_159335
	.word 0xb8003806
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_159346
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_openIn_code_147233
 ! arguments : [$147235,$8] [$147236,$9] [$134552,$10] 
 ! results    : [$149341,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_openIn_code_147233:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159371
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159371:
	st	%r15, [%sp+92]
code_159348:
funtop_149303:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159349
	nop
code_159350:
	call	GCFromML ! delay slot empty
	nop
needgc_159349:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
	ld	[%r9+8], %r13
	! allocating 1-record
	or	%r0, 265, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r12
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(exn_handler_149312), %r8
	or	%r8, %lo(exn_handler_149312), %r11
	ld	[%r2+816], %r8
	sub	%sp, %r8, %r9
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r8, %r1
	! making closure call 
	ld	[%r13], %r11
	ld	[%r13+4], %r8
	jmpl	%r11, %r15
	ld	[%r13+8], %r9
code_159370:
	mov	%r8, %r10
code_159354:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_159367:
	mov	%r8, %r10
code_159355:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_159368:
code_159356:
	! done making normal call
	ba	exn_handler_after_149313
	ld	[%r1+12], %r1
exn_handler_149312:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r10
	mov	%r15, %r12
	sethi	%hi(string_148110), %r8
	or	%r8, %lo(string_148110), %r11
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_159369:
code_159363:
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
exn_handler_after_149313:
code_159366:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_openIn_code_147233,(.-TextIOFn_openIn_code_147233)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159349
	.word 0xb8003806
	.word 0x00000600
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159367
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_159368
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159369
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159370
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_openString_code_147244
 ! arguments : [$147246,$8] [$147247,$9] [$134602,$10] 
 ! results    : [$149286,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_openString_code_147244:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159396
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159396:
	st	%r15, [%sp+92]
code_159372:
funtop_149250:
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159373
	nop
code_159374:
	call	GCFromML ! delay slot empty
	nop
needgc_159373:
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
	ld	[%r9+8], %r12
	sethi	%hi(exn_handler_149259), %r8
	or	%r8, %lo(exn_handler_149259), %r11
	ld	[%r2+816], %r8
	sub	%sp, %r8, %r9
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	st	%r1, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	mov	%r8, %r1
	! making closure call 
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_159395:
	mov	%r8, %r10
code_159378:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_159392:
	mov	%r8, %r10
code_159379:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_159393:
code_159380:
	! done making normal call
	ba	exn_handler_after_149260
	ld	[%r1+12], %r1
exn_handler_149259:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	mov	%r15, %r12
	sethi	%hi(string_148138), %r8
	or	%r8, %lo(string_148138), %r10
	sethi	%hi(string_148110), %r8
	or	%r8, %lo(string_148110), %r11
	! making closure call 
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_159394:
code_159388:
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
exn_handler_after_149260:
code_159391:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_openString_code_147244,(.-TextIOFn_openString_code_147244)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159373
	.word 0xb8003806
	.word 0x00000600
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159392
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_159393
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159394
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159395
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_mkStdIn_code_147255
 ! arguments : [$147257,$8] [$147258,$9] 
 ! results    : [$149235,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_mkStdIn_code_147255:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159423
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159423:
	st	%r15, [%sp+92]
code_159397:
funtop_149161:
	ld	[%r9], %r17
	st	%r17, [%sp+96]
	ld	[%r9+4], %r9
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_159422:
	mov	%r8, %r10
code_159398:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r9
code_159421:
	mov	%r8, %r12
code_159399:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159400
	nop
code_159401:
	call	GCFromML ! delay slot empty
	nop
needgc_159400:
sumarm_149186:
	ld	[%r12], %r8
sumarm_149198:
	ld	[%r8+12], %r8
sumarm_149210:
	ld	[%r8], %r11
	! allocating 2-record
	or	%r0, 273, %r10
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159405
	nop
code_159406:
	or	%r0, 0, %r8
cmpui_159405:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	st	%r10, [%r4]
	st	%r12, [%r4+4]
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_159409
	nop
code_159410:
	or	%r0, 0, %r8
cmpui_159409:
	st	%r11, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_sum_149207 ! delay slot empty
	nop
sumarm_149211:
after_sum_149207:
	ba	after_sum_149195 ! delay slot empty
	nop
sumarm_149199:
after_sum_149195:
	ba	after_sum_149183 ! delay slot empty
	nop
sumarm_149187:
after_sum_149183:
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	ld	[%r8+4], %r10
	sethi	%hi(record_148158), %r8
	or	%r8, %lo(record_148158), %r11
	! making closure call 
	sethi	%hi(rebindCleaner_138384), %r8
	or	%r8, %lo(rebindCleaner_138384), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_159420:
code_159417:
	! done making normal call
	ld	[%sp+96], %r16
	st	%r16, [%sp+96]
code_159419:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_mkStdIn_code_147255,(.-TextIOFn_mkStdIn_code_147255)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_159420
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_159421
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_159400
	.word 0xb8003806
	.word 0x00001000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_159422
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
 	.align 8
	.global TextIOFn_anonfun_code_147264
 ! arguments : [$147266,$8] [$147267,$9] 
 ! results    : [$149160,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_anonfun_code_147264:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159439
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_159439:
	st	%r15, [%sp+92]
	mov	%r9, %r8
code_159424:
funtop_149106:
	ld	[%r8], %r17
	st	%r17, [%sp+120]
	ld	[%r8+4], %r17
	st	%r17, [%sp+116]
	ld	[%r8+8], %r9
	ld	[%r8+12], %r17
	st	%r17, [%sp+112]
	ld	[%r8+16], %r17
	st	%r17, [%sp+108]
	ld	[%r8+20], %r17
	st	%r17, [%sp+104]
	ld	[%r8+24], %r17
	st	%r17, [%sp+100]
	ld	[%r8+28], %r17
	st	%r17, [%sp+96]
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_159438:
	mov	%r8, %r11
code_159425:
	! done making normal call
	! making closure call 
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r10
code_159433:
code_159426:
	! done making normal call
	! making closure polycall
	ld	[%sp+112], %r17
	ld	[%r17], %r10
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_159434:
	mov	%r8, %r11
code_159427:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_159435:
code_159428:
	! done making normal call
	! making closure polycall
	ld	[%sp+108], %r17
	ld	[%r17], %r10
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_159436:
	mov	%r8, %r11
code_159429:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_159430:
	! done making tail call
code_159432:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size TextIOFn_anonfun_code_147264,(.-TextIOFn_anonfun_code_147264)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_159433
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05450000
		! -------- label,sizes,reg
	.long code_159434
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04450000
		! -------- label,sizes,reg
	.long code_159435
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04410000
		! -------- label,sizes,reg
	.long code_159436
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04010000
		! -------- label,sizes,reg
	.long code_159438
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15550000
	.text
 	.align 8
	.global TextIOFn_print_code_147285
 ! arguments : [$147287,$8] [$147288,$9] [$134714,$10] 
 ! results    : [$149105,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_print_code_147285:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159447
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_159447:
	st	%r15, [%sp+92]
	mov	%r9, %r8
	mov	%r10, %r11
code_159440:
funtop_149084:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	ld	[%r8+8], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_159446:
code_159441:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_159442:
	! done making tail call
code_159444:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_print_code_147285,(.-TextIOFn_print_code_147285)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_159446
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
 	.align 8
	.global TextIOFn_functor_var_r_code_146441
 ! arguments : [$146443,$8] [$135931,$9] [$146444,$10] [$132593,$11] 
 ! results    : [$149080,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_functor_var_r_code_146441:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 336, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_159576
	mov	%sp, %fp
	add	%sp, 336, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 336, %sp
code_159576:
	st	%r15, [%sp+92]
code_159448:
funtop_148190:
	add	%r4, 1028, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159449
	nop
code_159450:
	call	GCFromML ! delay slot empty
	nop
needgc_159449:
	ld	[%r11], %r17
	st	%r17, [%sp+116]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148199), %r8
	or	%r8, %lo(record_148199), %r9
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	or	%r0, 0, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_extendStream_code_146451), %r8
	or	%r8, %lo(TextIOFn_extendStream_code_146451), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148223), %r8
	or	%r8, %lo(record_148223), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148230), %r10
	or	%r10, %lo(record_148230), %r17
	st	%r17, [%sp+108]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148236), %r10
	or	%r10, %lo(record_148236), %r17
	st	%r17, [%sp+96]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1305, %r10
	st	%r10, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	or	%r0, 0, %r10
	st	%r10, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(TextIOFn_input_code_146486), %r10
	or	%r10, %lo(TextIOFn_input_code_146486), %r10
	st	%r10, [%r4+4]
	or	%r0, 256, %r10
	st	%r10, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+324]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r10
	st	%r10, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r10
	st	%r10, [%r4+8]
	add	%r4, 4, %r11
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(TextIOFn_input1_code_146506), %r10
	or	%r10, %lo(TextIOFn_input1_code_146506), %r10
	st	%r10, [%r4+4]
	or	%r0, 256, %r10
	st	%r10, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+320]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 2 closures
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(TextIOFn_nextBuf_code_146520), %r10
	or	%r10, %lo(TextIOFn_nextBuf_code_146520), %r10
	st	%r10, [%r4+4]
	or	%r0, 256, %r10
	st	%r10, [%r4+8]
	or	%r0, 258, %r10
	st	%r10, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(TextIOFn_inputList_code_146515), %r10
	or	%r10, %lo(TextIOFn_inputList_code_146515), %r10
	st	%r10, [%r4+4]
	or	%r0, 256, %r10
	st	%r10, [%r4+8]
	or	%r0, 258, %r10
	st	%r10, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 2-record
	or	%r0, 785, %r10
	st	%r10, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	st	%r10, [%r12+8]
	st	%r12, [%r11+8]
	! done allocating 2 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r10
	st	%r10, [%r4]
	sethi	%hi(TextIOFn_inputN_code_146531), %r10
	or	%r10, %lo(TextIOFn_inputN_code_146531), %r10
	st	%r10, [%r4+4]
	or	%r0, 256, %r10
	st	%r10, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+316]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(4649), %r10
	or	%r10, %lo(4649), %r10
	st	%r10, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	st	%r8, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	or	%r0, 0, %r8
	st	%r8, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_inputAll_code_146538), %r8
	or	%r8, %lo(TextIOFn_inputAll_code_146538), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+312]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 537, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_closeIn_code_146604), %r8
	or	%r8, %lo(TextIOFn_closeIn_code_146604), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+308]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_endOfStream_code_146611), %r8
	or	%r8, %lo(TextIOFn_endOfStream_code_146611), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+112], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+304]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148345), %r8
	or	%r8, %lo(record_148345), %r9
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 17, %r8
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
	sethi	%hi(TextIOFn_getReader_code_146623), %r8
	or	%r8, %lo(TextIOFn_getReader_code_146623), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+300]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148364), %r8
	or	%r8, %lo(record_148364), %r17
	st	%r17, [%sp+296]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148370), %r8
	or	%r8, %lo(record_148370), %r17
	st	%r17, [%sp+292]
	! done allocating 1 closures
	! allocating 2 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_scanData_code_146656), %r8
	or	%r8, %lo(TextIOFn_scanData_code_146656), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	or	%r0, 258, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_nextBuf_code_146651), %r9
	or	%r9, %lo(TextIOFn_nextBuf_code_146651), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	or	%r0, 258, %r9
	st	%r9, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	st	%r10, [%r8+8]
	! allocating 2-record
	or	%r0, 785, %r9
	st	%r9, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	st	%r9, [%r10+8]
	! done allocating 2 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r9
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_inputLine_code_146681), %r8
	or	%r8, %lo(TextIOFn_inputLine_code_146681), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+288]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148412), %r8
	or	%r8, %lo(record_148412), %r12
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 537, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_isClosedOut_code_146695), %r8
	or	%r8, %lo(TextIOFn_isClosedOut_code_146695), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 537, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_flushBuffer_code_146702), %r8
	or	%r8, %lo(TextIOFn_flushBuffer_code_146702), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 793, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(TextIOFn_output_code_146709), %r9
	or	%r9, %lo(TextIOFn_output_code_146709), %r9
	st	%r9, [%r4+4]
	or	%r0, 256, %r9
	st	%r9, [%r4+8]
	st	%r8, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+284]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 793, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_output1_code_146758), %r8
	or	%r8, %lo(TextIOFn_output1_code_146758), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+280]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_flushOut_code_146769), %r8
	or	%r8, %lo(TextIOFn_flushOut_code_146769), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+276]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_closeOut_code_146776), %r8
	or	%r8, %lo(TextIOFn_closeOut_code_146776), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+272]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+276], %r17
	st	%r17, [%r4+4]
	ld	[%sp+272], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_mkOutstream_code_146783), %r8
	or	%r8, %lo(TextIOFn_mkOutstream_code_146783), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+268]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_getWriter_code_146831), %r8
	or	%r8, %lo(TextIOFn_getWriter_code_146831), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+264]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_getPosOut_code_146843), %r8
	or	%r8, %lo(TextIOFn_getPosOut_code_146843), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+260]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_filePosOut_code_146850), %r8
	or	%r8, %lo(TextIOFn_filePosOut_code_146850), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+256]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_setPosOut_code_146857), %r8
	or	%r8, %lo(TextIOFn_setPosOut_code_146857), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+252]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 793, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_outputSubstr_code_146866), %r8
	or	%r8, %lo(TextIOFn_outputSubstr_code_146866), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+248]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_setBufferMode_code_146899), %r8
	or	%r8, %lo(TextIOFn_setBufferMode_code_146899), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+244]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_getBufferMode_code_146908), %r8
	or	%r8, %lo(TextIOFn_getBufferMode_code_146908), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+240]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_input_code_146915), %r8
	or	%r8, %lo(TextIOFn_input_code_146915), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+324], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+236]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_input1_code_146922), %r8
	or	%r8, %lo(TextIOFn_input1_code_146922), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+320], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+232]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_inputN_code_146929), %r8
	or	%r8, %lo(TextIOFn_inputN_code_146929), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+316], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+228]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_inputAll_code_146936), %r8
	or	%r8, %lo(TextIOFn_inputAll_code_146936), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+312], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+224]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_lookahead_code_146943), %r8
	or	%r8, %lo(TextIOFn_lookahead_code_146943), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+320], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+220]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	ld	[%sp+308], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_closeIn_code_146950), %r8
	or	%r8, %lo(TextIOFn_closeIn_code_146950), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+216]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_endOfStream_code_146959), %r8
	or	%r8, %lo(TextIOFn_endOfStream_code_146959), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+304], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+212]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 537, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_getPosIn_code_146966), %r8
	or	%r8, %lo(TextIOFn_getPosIn_code_146966), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+296], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+208]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_output_code_146973), %r8
	or	%r8, %lo(TextIOFn_output_code_146973), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+284], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+204]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_output1_code_146980), %r8
	or	%r8, %lo(TextIOFn_output1_code_146980), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+280], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+200]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_flushOut_code_146987), %r8
	or	%r8, %lo(TextIOFn_flushOut_code_146987), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+276], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+196]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_closeOut_code_146994), %r8
	or	%r8, %lo(TextIOFn_closeOut_code_146994), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+272], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+192]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_getPosOut_code_147001), %r8
	or	%r8, %lo(TextIOFn_getPosOut_code_147001), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+260], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+188]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_setPosOut_code_147008), %r8
	or	%r8, %lo(TextIOFn_setPosOut_code_147008), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+252], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+184]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148680), %r8
	or	%r8, %lo(record_148680), %r17
	st	%r17, [%sp+180]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148686), %r8
	or	%r8, %lo(record_148686), %r17
	st	%r17, [%sp+176]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148692), %r8
	or	%r8, %lo(record_148692), %r17
	st	%r17, [%sp+172]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148698), %r8
	or	%r8, %lo(record_148698), %r17
	st	%r17, [%sp+168]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148704), %r8
	or	%r8, %lo(record_148704), %r17
	st	%r17, [%sp+164]
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_148710), %r8
	or	%r8, %lo(record_148710), %r17
	st	%r17, [%sp+160]
	! done allocating 1 closures
	ld	[%sp+116], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+104]
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1305, %r8
	st	%r8, [%r4]
	ld	[%sp+268], %r17
	st	%r17, [%r4+4]
	ld	[%sp+168], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_openOut_code_147045), %r8
	or	%r8, %lo(TextIOFn_openOut_code_147045), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+156]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+116], %r17
	ld	[%r17+12], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1305, %r8
	st	%r8, [%r4]
	ld	[%sp+268], %r17
	st	%r17, [%r4+4]
	ld	[%sp+168], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_openAppend_code_147056), %r8
	or	%r8, %lo(TextIOFn_openAppend_code_147056), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+152]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_inputLine_code_147067), %r8
	or	%r8, %lo(TextIOFn_inputLine_code_147067), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+288], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+148]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_outputSubstr_code_147074), %r8
	or	%r8, %lo(TextIOFn_outputSubstr_code_147074), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+248], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+144]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+116], %r17
	ld	[%r17+36], %r17
	st	%r17, [%sp+100]
	ld	[%sp+116], %r17
	ld	[%r17+24], %r17
	st	%r17, [%sp+96]
	ld	[%sp+116], %r17
	ld	[%r17+28], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+276], %r17
	st	%r17, [%r4+4]
	ld	[%sp+268], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_mkStdOut_code_147081), %r8
	or	%r8, %lo(TextIOFn_mkStdOut_code_147081), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+140]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+116], %r17
	ld	[%r17+32], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+276], %r17
	st	%r17, [%r4+4]
	ld	[%sp+268], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_mkStdErr_code_147109), %r8
	or	%r8, %lo(TextIOFn_mkStdErr_code_147109), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+136]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(anonfun_132801), %r8
	or	%r8, %lo(anonfun_132801), %r10
	! making closure call 
	sethi	%hi(_136252), %r8
	or	%r8, %lo(_136252), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159573:
code_159516:
	! done making normal call
	add	%r4, 108, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159517
	nop
code_159518:
	call	GCFromML ! delay slot empty
	nop
needgc_159517:
	sethi	%hi(exncounter), %r8
	or	%r8, %lo(exncounter), %r9
	ld	[%r9], %r11
	add	%r11, 1, %r8
	st	%r8, [%r9]
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_148824), %r8
	or	%r8, %lo(string_148824), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 273, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_canInput_code_147137), %r8
	or	%r8, %lo(TextIOFn_canInput_code_147137), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+132]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	or	%r0, 0, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_mkInstream_code_147168), %r8
	or	%r8, %lo(TextIOFn_mkInstream_code_147168), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+128]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 537, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+8]
	ld	[%sp+292], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_setPosIn_code_147208), %r8
	or	%r8, %lo(TextIOFn_setPosIn_code_147208), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+124]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(anonfun_134351), %r8
	or	%r8, %lo(anonfun_134351), %r10
	! making closure call 
	sethi	%hi(_136252), %r8
	or	%r8, %lo(_136252), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_159574:
code_159528:
	! done making normal call
	add	%r4, 124, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159529
	nop
code_159530:
	call	GCFromML ! delay slot empty
	nop
needgc_159529:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_canInput_code_147219), %r8
	or	%r8, %lo(TextIOFn_canInput_code_147219), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+132], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+120]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_setPosIn_code_147226), %r8
	or	%r8, %lo(TextIOFn_setPosIn_code_147226), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+124], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+116]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1305, %r8
	st	%r8, [%r4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+4]
	ld	[%sp+180], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_openIn_code_147233), %r8
	or	%r8, %lo(TextIOFn_openIn_code_147233), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1305, %r8
	st	%r8, [%r4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+4]
	ld	[%sp+180], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_openString_code_147244), %r8
	or	%r8, %lo(TextIOFn_openString_code_147244), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+108]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_mkStdIn_code_147255), %r8
	or	%r8, %lo(TextIOFn_mkStdIn_code_147255), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+104]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure polycall
	ld	[%sp+104], %r17
	ld	[%r17], %r10
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_159575:
	mov	%r8, %r10
code_159537:
	! done making normal call
	! making closure call 
	ld	[%sp+180], %r17
	ld	[%r17], %r11
	ld	[%sp+180], %r17
	ld	[%r17+4], %r8
	ld	[%sp+180], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_159568:
	st	%r8, [%sp+100]
code_159538:
	! done making normal call
	! making closure polycall
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	ld	[%r17+4], %r8
	ld	[%sp+140], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_159569:
	mov	%r8, %r10
code_159539:
	! done making normal call
	! making closure call 
	ld	[%sp+168], %r17
	ld	[%r17], %r11
	ld	[%sp+168], %r17
	ld	[%r17+4], %r8
	ld	[%sp+168], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_159570:
	st	%r8, [%sp+96]
code_159540:
	! done making normal call
	! making closure polycall
	ld	[%sp+136], %r17
	ld	[%r17], %r10
	ld	[%sp+136], %r17
	ld	[%r17+4], %r8
	ld	[%sp+136], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_159571:
	mov	%r8, %r10
code_159541:
	! done making normal call
	! making closure call 
	ld	[%sp+168], %r17
	ld	[%r17], %r11
	ld	[%sp+168], %r17
	ld	[%r17+4], %r8
	ld	[%sp+168], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_159572:
	mov	%r8, %r13
code_159542:
	! done making normal call
	add	%r4, 52, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159543
	nop
code_159544:
	call	GCFromML ! delay slot empty
	nop
needgc_159543:
	! allocating 1 closures
	! allocating 8-record
	sethi	%hi(64577), %r8
	or	%r8, %lo(64577), %r8
	st	%r8, [%r4]
	ld	[%sp+172], %r17
	st	%r17, [%r4+4]
	ld	[%sp+160], %r17
	st	%r17, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	ld	[%sp+140], %r17
	st	%r17, [%r4+16]
	ld	[%sp+136], %r17
	st	%r17, [%r4+20]
	ld	[%sp+100], %r17
	st	%r17, [%r4+24]
	ld	[%sp+96], %r17
	st	%r17, [%r4+28]
	st	%r13, [%r4+32]
	add	%r4, 4, %r9
	add	%r4, 36, %r4
	! done allocating 8 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_anonfun_code_147264), %r8
	or	%r8, %lo(TextIOFn_anonfun_code_147264), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_159550
	nop
code_159551:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159550:
	sethi	%hi(stdStrmHook_139250), %r8
	or	%r8, %lo(stdStrmHook_139250), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	ld	[%r2+800], %r10
	mov	%r11, %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	ld	[%r11], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+800]
	st	%r12, [%r11]
	! allocating 1 closures
	! allocating 3-record
	add	%r4, 292, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159561
	nop
code_159562:
	call	GCFromML ! delay slot empty
	nop
needgc_159561:
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+204], %r17
	st	%r17, [%r4+4]
	ld	[%sp+196], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(TextIOFn_print_code_147285), %r8
	or	%r8, %lo(TextIOFn_print_code_147285), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+256], %r17
	st	%r17, [%r4+4]
	ld	[%sp+288], %r17
	st	%r17, [%r4+8]
	ld	[%sp+248], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 23-record
	sethi	%hi(2146828217), %r8
	or	%r8, %lo(2146828217), %r8
	st	%r8, [%r4]
	ld	[%sp+324], %r17
	st	%r17, [%r4+4]
	ld	[%sp+320], %r17
	st	%r17, [%r4+8]
	ld	[%sp+316], %r17
	st	%r17, [%r4+12]
	ld	[%sp+312], %r17
	st	%r17, [%r4+16]
	ld	[%sp+132], %r17
	st	%r17, [%r4+20]
	ld	[%sp+308], %r17
	st	%r17, [%r4+24]
	ld	[%sp+304], %r17
	st	%r17, [%r4+28]
	ld	[%sp+128], %r17
	st	%r17, [%r4+32]
	ld	[%sp+300], %r17
	st	%r17, [%r4+36]
	ld	[%sp+296], %r17
	st	%r17, [%r4+40]
	ld	[%sp+124], %r17
	st	%r17, [%r4+44]
	ld	[%sp+292], %r17
	st	%r17, [%r4+48]
	ld	[%sp+284], %r17
	st	%r17, [%r4+52]
	ld	[%sp+280], %r17
	st	%r17, [%r4+56]
	ld	[%sp+276], %r17
	st	%r17, [%r4+60]
	ld	[%sp+272], %r17
	st	%r17, [%r4+64]
	ld	[%sp+244], %r17
	st	%r17, [%r4+68]
	ld	[%sp+240], %r17
	st	%r17, [%r4+72]
	ld	[%sp+268], %r17
	st	%r17, [%r4+76]
	ld	[%sp+264], %r17
	st	%r17, [%r4+80]
	ld	[%sp+260], %r17
	st	%r17, [%r4+84]
	ld	[%sp+252], %r17
	st	%r17, [%r4+88]
	st	%r9, [%r4+92]
	add	%r4, 4, %r10
	add	%r4, 96, %r4
	! done allocating 23 record
	! allocating 12-record
	sethi	%hi(1047649), %r8
	or	%r8, %lo(1047649), %r8
	st	%r8, [%r4]
	ld	[%sp+164], %r17
	st	%r17, [%r4+4]
	ld	[%sp+160], %r17
	st	%r17, [%r4+8]
	ld	[%sp+148], %r17
	st	%r17, [%r4+12]
	ld	[%sp+144], %r17
	st	%r17, [%r4+16]
	ld	[%sp+112], %r17
	st	%r17, [%r4+20]
	ld	[%sp+108], %r17
	st	%r17, [%r4+24]
	ld	[%sp+156], %r17
	st	%r17, [%r4+28]
	ld	[%sp+152], %r17
	st	%r17, [%r4+32]
	ld	[%sp+100], %r17
	st	%r17, [%r4+36]
	ld	[%sp+96], %r17
	st	%r17, [%r4+40]
	st	%r13, [%r4+44]
	st	%r11, [%r4+48]
	add	%r4, 4, %r9
	add	%r4, 52, %r4
	! done allocating 12 record
	! allocating 23-record
	sethi	%hi(1493172153), %r8
	or	%r8, %lo(1493172153), %r8
	st	%r8, [%r4]
	sethi	%hi(record_148182), %r8
	or	%r8, %lo(record_148182), %r8
	st	%r8, [%r4+4]
	ld	[%sp+236], %r17
	st	%r17, [%r4+8]
	ld	[%sp+232], %r17
	st	%r17, [%r4+12]
	ld	[%sp+228], %r17
	st	%r17, [%r4+16]
	ld	[%sp+224], %r17
	st	%r17, [%r4+20]
	ld	[%sp+120], %r17
	st	%r17, [%r4+24]
	ld	[%sp+220], %r17
	st	%r17, [%r4+28]
	ld	[%sp+216], %r17
	st	%r17, [%r4+32]
	ld	[%sp+212], %r17
	st	%r17, [%r4+36]
	ld	[%sp+204], %r17
	st	%r17, [%r4+40]
	ld	[%sp+200], %r17
	st	%r17, [%r4+44]
	ld	[%sp+196], %r17
	st	%r17, [%r4+48]
	ld	[%sp+192], %r17
	st	%r17, [%r4+52]
	st	%r10, [%r4+56]
	ld	[%sp+208], %r17
	st	%r17, [%r4+60]
	ld	[%sp+116], %r17
	st	%r17, [%r4+64]
	ld	[%sp+180], %r17
	st	%r17, [%r4+68]
	ld	[%sp+176], %r17
	st	%r17, [%r4+72]
	ld	[%sp+172], %r17
	st	%r17, [%r4+76]
	ld	[%sp+188], %r17
	st	%r17, [%r4+80]
	ld	[%sp+184], %r17
	st	%r17, [%r4+84]
	ld	[%sp+168], %r17
	st	%r17, [%r4+88]
	st	%r9, [%r4+92]
	add	%r4, 4, %r8
	add	%r4, 96, %r4
	! done allocating 23 record
code_159567:
	ld	[%sp+92], %r15
	retl
	add	%sp, 336, %sp
	.size TextIOFn_functor_var_r_code_146441,(.-TextIOFn_functor_var_r_code_146441)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_159449
	.word 0xb800a80a
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_159517
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01150000
	.word 0x50005550
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long needgc_159529
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40150000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159568
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159569
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55540000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159570
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55540000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159571
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159572
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long needgc_159543
	.word 0xb800a80a
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long afterMutateCheck_159550
	.word 0xb800a80a
	.word 0x00003000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55450000
	.word 0x50005505
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long needgc_159561
	.word 0xb800a80a
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55450000
	.word 0x50005505
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159573
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01150000
	.word 0x50005550
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159574
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40150000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
		! -------- label,sizes,reg
	.long code_159575
	.word 0xb800a80a
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x50005555
	.word 0x55555555
	.word 0x55415555
	.word 0x00000005
	.text
 	.align 8
	.global TextIOFn_main
 ! arguments : 
 ! results    : [$148189,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
TextIOFn_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+808], %r16
	cmp	%sp, %r16
	bg	code_160131
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_160131:
	st	%r15, [%sp+92]
code_159577:
funtop_147299:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_160130:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_159578:
	sethi	%hi(CharVector_STR_c_INT), %r8
	or	%r8, %lo(CharVector_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label vector_TYC
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(vector_132792+-4), %r8
	st	%r9, [%r8+%lo(vector_132792+-4)]
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 36, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_159585
	nop
code_159586:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159585:
	sethi	%hi(vector_132792), %r8
	or	%r8, %lo(vector_132792), %r12
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
	sethi	%hi(CharVector_STR_c_INT), %r8
	or	%r8, %lo(CharVector_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label elem_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(elem_132805+-4), %r8
	st	%r9, [%r8+%lo(elem_132805+-4)]
	sethi	%hi(elem_132805), %r8
	or	%r8, %lo(elem_132805), %r12
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
	sethi	%hi(CharArray_STR_c_INT), %r8
	or	%r8, %lo(CharArray_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label array_TYC
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(_134916+-4), %r8
	st	%r9, [%r8+%lo(_134916+-4)]
	sethi	%hi(_134916), %r8
	or	%r8, %lo(_134916), %r12
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
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159621
	nop
code_159622:
	call	GCFromML ! delay slot empty
	nop
needgc_159621:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(elem_132805), %r8
	or	%r8, %lo(elem_132805), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+4]
	sethi	%hi(vector_132792), %r8
	or	%r8, %lo(vector_132792), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_c_134918+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_c_134918+-4)]
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_159632
	nop
code_159633:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159632:
	sethi	%hi(strbindvar_c_134918), %r8
	or	%r8, %lo(strbindvar_c_134918), %r12
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
	! allocating 2-record
	! done allocating 2 record
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(TextPrimIO_STR_c_INT), %r8
	or	%r8, %lo(TextPrimIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label vector_TYC
	ld	[%r8+4], %r17
	st	%r17, [%sp+108]
	sethi	%hi(TextPrimIO_STR_c_INT), %r8
	or	%r8, %lo(TextPrimIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label pos_TYC
	ld	[%r8+12], %r9
	! allocating 1-record
	! done allocating 1 record
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_159648
	nop
code_159649:
	call	GCFromML ! delay slot empty
	nop
needgc_159648:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	sethi	%hi(record_147349), %r8
	or	%r8, %lo(record_147349), %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r17
	st	%r17, [%sp+104]
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(TextPrimIO_STR_c_INT), %r8
	or	%r8, %lo(TextPrimIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label array_TYC
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	sethi	%hi(CharVector_STR_r_INT), %r8
	or	%r8, %lo(CharVector_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	or	%r0, 111, %r10
	sethi	%hi(strbindvar_r_extract_136245+-4), %r9
	st	%r10, [%r9+%lo(strbindvar_r_extract_136245+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 48, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_159660
	nop
code_159661:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159660:
	sethi	%hi(strbindvar_r_extract_136245), %r9
	or	%r9, %lo(strbindvar_r_extract_136245), %r13
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
	sethi	%hi(CharVector_STR_r_INT), %r8
	or	%r8, %lo(CharVector_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_sub_136246+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_sub_136246+-4)]
	sethi	%hi(strbindvar_r_sub_136246), %r8
	or	%r8, %lo(strbindvar_r_sub_136246), %r12
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
	sethi	%hi(CharArray_STR_r_INT), %r8
	or	%r8, %lo(CharArray_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_update_136247+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_update_136247+-4)]
	sethi	%hi(strbindvar_r_update_136247), %r8
	or	%r8, %lo(strbindvar_r_update_136247), %r12
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
	sethi	%hi(Substring_STR_r_INT), %r8
	or	%r8, %lo(Substring_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(base_136248+-4), %r8
	st	%r9, [%r8+%lo(base_136248+-4)]
	sethi	%hi(base_136248), %r8
	or	%r8, %lo(base_136248), %r12
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
	or	%r0, 0, %r13
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(vector_eq_r_INT), %r8
	or	%r8, %lo(vector_eq_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_160122:
code_159710:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_136252+-4), %r9
	st	%r10, [%r9+%lo(_136252+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_159715
	nop
code_159716:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159715:
	sethi	%hi(_136252), %r9
	or	%r9, %lo(_136252), %r13
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
	sethi	%hi(anonfun_132801), %r8
	or	%r8, %lo(anonfun_132801), %r8
	! done allocating 1 closures
	sethi	%hi(IO_STR_r_INT), %r8
	or	%r8, %lo(IO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r17
	st	%r17, [%sp+96]
	ld	[%sp+96], %r17
	ld	[%r17+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_136309+-4), %r8
	st	%r9, [%r8+%lo(mk_136309+-4)]
	sethi	%hi(mk_136309), %r8
	or	%r8, %lo(mk_136309), %r12
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
	sethi	%hi(CharVector_STR_r_INT), %r8
	or	%r8, %lo(CharVector_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_length_136348+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_length_136348+-4)]
	sethi	%hi(strbindvar_r_length_136348), %r8
	or	%r8, %lo(strbindvar_r_length_136348), %r12
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
	sethi	%hi(CleanIO_STR_r_INT), %r8
	or	%r8, %lo(CleanIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r12
	sethi	%hi(CleanIO_STR_c_INT), %r8
	or	%r8, %lo(CleanIO_STR_c_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_147466), %r8
	or	%r8, %lo(record_147466), %r10
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
code_160123:
code_159759:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_141006+-4), %r9
	st	%r10, [%r9+%lo(_141006+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 96, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_159764
	nop
code_159765:
	sub	%r4, 96, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159764:
	sethi	%hi(_141006), %r9
	or	%r9, %lo(_141006), %r13
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
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(CharVector_STR_r_INT), %r8
	or	%r8, %lo(CharVector_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_concat_136787+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_concat_136787+-4)]
	sethi	%hi(strbindvar_r_concat_136787), %r8
	or	%r8, %lo(strbindvar_r_concat_136787), %r12
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
	sethi	%hi(Int_STR_r_INT), %r8
	or	%r8, %lo(Int_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+48], %r13
	or	%r0, 111, %r9
	sethi	%hi(quot_136425+-4), %r8
	st	%r9, [%r8+%lo(quot_136425+-4)]
	sethi	%hi(quot_136425), %r8
	or	%r8, %lo(quot_136425), %r12
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
	sethi	%hi(IO_STR_r_INT), %r8
	or	%r8, %lo(IO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_136899+-4), %r8
	st	%r9, [%r8+%lo(mk_136899+-4)]
	sethi	%hi(mk_136899), %r8
	or	%r8, %lo(mk_136899), %r12
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
	ld	[%sp+96], %r17
	ld	[%r17], %r9
	sethi	%hi(stamp_136957), %r8
	st	%r9, [%r8+%lo(stamp_136957)]
	sethi	%hi(Size_r_INT), %r8
	or	%r8, %lo(Size_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_136989+-4), %r8
	st	%r9, [%r8+%lo(mk_136989+-4)]
	sethi	%hi(mk_136989), %r8
	or	%r8, %lo(mk_136989), %r12
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
	sethi	%hi(IO_STR_r_INT), %r8
	or	%r8, %lo(IO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_137126+-4), %r8
	st	%r9, [%r8+%lo(mk_137126+-4)]
	sethi	%hi(mk_137126), %r8
	or	%r8, %lo(mk_137126), %r12
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
	sethi	%hi(anonfun_133355), %r8
	or	%r8, %lo(anonfun_133355), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_142922), %r8
	or	%r8, %lo(anonfun_142922), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_142935), %r8
	or	%r8, %lo(anonfun_142935), %r8
	! done allocating 1 closures
	sethi	%hi(CleanIO_STR_r_INT), %r8
	or	%r8, %lo(CleanIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(addCleaner_137245+-4), %r8
	st	%r9, [%r8+%lo(addCleaner_137245+-4)]
	sethi	%hi(addCleaner_137245), %r8
	or	%r8, %lo(addCleaner_137245), %r12
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
	sethi	%hi(anonfun_133419), %r8
	or	%r8, %lo(anonfun_133419), %r8
	! done allocating 1 closures
	sethi	%hi(IO_STR_r_INT), %r8
	or	%r8, %lo(IO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_137384+-4), %r8
	st	%r9, [%r8+%lo(mk_137384+-4)]
	sethi	%hi(mk_137384), %r8
	or	%r8, %lo(mk_137384), %r12
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
	sethi	%hi(Fail_r_INT), %r8
	or	%r8, %lo(Fail_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	sethi	%hi(string_147686), %r8
	or	%r8, %lo(string_147686), %r10
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_160124:
code_159868:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_137467+-4), %r9
	st	%r10, [%r9+%lo(_137467+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_159873
	nop
code_159874:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159873:
	sethi	%hi(_137467), %r9
	or	%r9, %lo(_137467), %r13
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
	sethi	%hi(string_147721), %r8
	or	%r8, %lo(string_147721), %r10
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_160125:
code_159886:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(funarg_3_137516+-4), %r9
	st	%r10, [%r9+%lo(funarg_3_137516+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_159891
	nop
code_159892:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159891:
	sethi	%hi(funarg_3_137516), %r9
	or	%r9, %lo(funarg_3_137516), %r13
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
	sethi	%hi(before_r_INT), %r8
	or	%r8, %lo(before_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_160126:
code_159905:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_137527+-4), %r9
	st	%r10, [%r9+%lo(_137527+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_159910
	nop
code_159911:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159910:
	sethi	%hi(_137527), %r9
	or	%r9, %lo(_137527), %r13
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
	ld	[%r8+16], %r10
	sethi	%hi(record_147363), %r8
	or	%r8, %lo(record_147363), %r9
	or	%r0, 256, %r11
	! making closure call 
	ld	[%r10], %r12
	ld	[%r10+4], %r8
	jmpl	%r12, %r15
	ld	[%r10+8], %r10
code_160127:
code_159925:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_137603+-4), %r9
	st	%r10, [%r9+%lo(_137603+-4)]
	ld	[%r2+800], %r9
	ld	[%r2+804], %r10
	add	%r9, 48, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_159930
	nop
code_159931:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159930:
	sethi	%hi(_137603), %r9
	or	%r9, %lo(_137603), %r13
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
	! allocating 2-record
	! done allocating 2 record
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(isNL_133728), %r8
	or	%r8, %lo(isNL_133728), %r8
	! done allocating 1 closures
	sethi	%hi(IO_STR_r_INT), %r8
	or	%r8, %lo(IO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_137905+-4), %r8
	st	%r9, [%r8+%lo(mk_137905+-4)]
	sethi	%hi(mk_137905), %r8
	or	%r8, %lo(mk_137905), %r12
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
	sethi	%hi(CharArray_STR_r_INT), %r8
	or	%r8, %lo(CharArray_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_length_138016+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_length_138016+-4)]
	sethi	%hi(strbindvar_r_length_138016), %r8
	or	%r8, %lo(strbindvar_r_length_138016), %r12
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
	sethi	%hi(CharArray_STR_r_INT), %r8
	or	%r8, %lo(CharArray_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+44], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_copyVec_138046+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_copyVec_138046+-4)]
	sethi	%hi(strbindvar_r_copyVec_138046), %r8
	or	%r8, %lo(strbindvar_r_copyVec_138046), %r12
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
	sethi	%hi(copyVec_133890), %r8
	or	%r8, %lo(copyVec_133890), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(copyVec_133906), %r8
	or	%r8, %lo(copyVec_133906), %r8
	! done allocating 1 closures
	! allocating 1-record
	! done allocating 1 record
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(iterate_r_134035), %r8
	or	%r8, %lo(iterate_r_134035), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134042), %r8
	or	%r8, %lo(anonfun_134042), %r8
	! done allocating 1 closures
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(iterate_r_134035), %r8
	or	%r8, %lo(iterate_r_134035), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_160128:
	mov	%r8, %r13
code_159984:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_138315+-4), %r8
	st	%r9, [%r8+%lo(_138315+-4)]
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_159989
	nop
code_159990:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_159989:
	sethi	%hi(_138315), %r8
	or	%r8, %lo(_138315), %r12
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
	sethi	%hi(anonfun_134076), %r8
	or	%r8, %lo(anonfun_134076), %r8
	! done allocating 1 closures
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(iterate_r_134035), %r8
	or	%r8, %lo(iterate_r_134035), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+108], %r9
code_160129:
	mov	%r8, %r13
code_160003:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_138361+-4), %r8
	st	%r9, [%r8+%lo(_138361+-4)]
	ld	[%r2+800], %r8
	ld	[%r2+804], %r9
	add	%r8, 96, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_160008
	nop
code_160009:
	sub	%r4, 96, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_160008:
	sethi	%hi(_138361), %r8
	or	%r8, %lo(_138361), %r12
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
	sethi	%hi(anonfun_134104), %r8
	or	%r8, %lo(anonfun_134104), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134108), %r8
	or	%r8, %lo(anonfun_134108), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134112), %r8
	or	%r8, %lo(anonfun_134112), %r8
	! done allocating 1 closures
	sethi	%hi(CharArray_STR_r_INT), %r8
	or	%r8, %lo(CharArray_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_array_138372+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_array_138372+-4)]
	sethi	%hi(strbindvar_r_array_138372), %r8
	or	%r8, %lo(strbindvar_r_array_138372), %r12
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
	sethi	%hi(CleanIO_STR_r_INT), %r8
	or	%r8, %lo(CleanIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r13
	or	%r0, 111, %r9
	sethi	%hi(rebindCleaner_138384+-4), %r8
	st	%r9, [%r8+%lo(rebindCleaner_138384+-4)]
	sethi	%hi(rebindCleaner_138384), %r8
	or	%r8, %lo(rebindCleaner_138384), %r12
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
	sethi	%hi(IO_STR_r_INT), %r8
	or	%r8, %lo(IO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r13
	or	%r0, 111, %r9
	sethi	%hi(PLUSEbuffer_mode_137877+-4), %r8
	st	%r9, [%r8+%lo(PLUSEbuffer_mode_137877+-4)]
	sethi	%hi(PLUSEbuffer_mode_137877), %r8
	or	%r8, %lo(PLUSEbuffer_mode_137877), %r12
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
	sethi	%hi(anonfun_133742), %r8
	or	%r8, %lo(anonfun_133742), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134351), %r8
	or	%r8, %lo(anonfun_134351), %r8
	! done allocating 1 closures
	sethi	%hi(OS_STR_r_INT), %r8
	or	%r8, %lo(OS_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+28], %r8
	ld	[%r8+4], %r18
	or	%r0, 111, %r10
	sethi	%hi(PLUSEiodesc_kind_138988+-4), %r9
	st	%r10, [%r9+%lo(PLUSEiodesc_kind_138988+-4)]
	sethi	%hi(PLUSEiodesc_kind_138988), %r9
	or	%r9, %lo(PLUSEiodesc_kind_138988), %r13
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
	st	%r18, [%r9]
	ld	[%r8+16], %r18
	or	%r0, 111, %r10
	sethi	%hi(kind_138991+-4), %r9
	st	%r10, [%r9+%lo(kind_138991+-4)]
	sethi	%hi(kind_138991), %r9
	or	%r9, %lo(kind_138991), %r13
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
	st	%r18, [%r9]
	ld	[%r8+20], %r8
	ld	[%r8+12], %r8
	sethi	%hi(OS_STR_c_INT), %r9
	or	%r9, %lo(OS_STR_c_INT), %r10
	ld	[%r2+812], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+16], %r9
	ld	[%r9+4], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_160085
	nop
code_160086:
	or	%r0, 0, %r9
cmpui_160085:
	cmp	%r9, 0
	be	else_case_148102
	nop
code_160087:
	or	%r0, 111, %r10
	sethi	%hi(tty_138997+-4), %r9
	st	%r10, [%r9+%lo(tty_138997+-4)]
	sethi	%hi(tty_138997), %r9
	or	%r9, %lo(tty_138997), %r13
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
	ba	after_ite_148103
	st	%r8, [%r9]
else_case_148102:
	or	%r0, 9, %r10
	sethi	%hi(tty_138997+-4), %r9
	st	%r10, [%r9+%lo(tty_138997+-4)]
	or	%r0, 23, %r10
	sethi	%hi(tty_138997+4), %r9
	st	%r10, [%r9+%lo(tty_138997+4)]
	sethi	%hi(tty_138997), %r9
	st	%r8, [%r9+%lo(tty_138997)]
after_ite_148103:
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134633), %r8
	or	%r8, %lo(anonfun_134633), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134637), %r8
	or	%r8, %lo(anonfun_134637), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134641), %r8
	or	%r8, %lo(anonfun_134641), %r8
	! done allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134660), %r8
	or	%r8, %lo(anonfun_134660), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_134686), %r8
	or	%r8, %lo(anonfun_134686), %r8
	! done allocating 1 closures
	sethi	%hi(CleanIO_STR_r_INT), %r8
	or	%r8, %lo(CleanIO_STR_r_INT), %r9
	ld	[%r2+812], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(stdStrmHook_139250+-4), %r8
	st	%r9, [%r8+%lo(stdStrmHook_139250+-4)]
	sethi	%hi(stdStrmHook_139250), %r8
	or	%r8, %lo(stdStrmHook_139250), %r12
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
	! allocating 4-record
	! done allocating 4 record
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(functor_var_r_132591), %r8
	or	%r8, %lo(functor_var_r_132591), %r8
	! done allocating 1 closures
	or	%r0, 256, %r8
code_160121:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size TextIOFn_main,(.-TextIOFn_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_160122
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_160123
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_160124
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_160125
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long code_160126
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00440000
		! -------- label,sizes,reg
	.long code_160127
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00440000
		! -------- label,sizes,reg
	.long code_160128
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_160129
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_159585
	.word 0xb8003806
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_159621
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_159632
	.word 0xb8003806
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_159648
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long afterMutateCheck_159660
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long afterMutateCheck_159715
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long afterMutateCheck_159764
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long afterMutateCheck_159873
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long afterMutateCheck_159891
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
		! -------- label,sizes,reg
	.long afterMutateCheck_159910
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00440000
		! -------- label,sizes,reg
	.long afterMutateCheck_159930
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00440000
		! -------- label,sizes,reg
	.long afterMutateCheck_159989
	.word 0xb8003806
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long afterMutateCheck_160008
	.word 0xb8003806
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_160130
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
TextIOFn_unit_CODE_END_VAL:
	.section	".rodata"
		! endgcinfo with filler for alignment
	.globl TextIOFn_unit_GCTABLE_END_VAL
TextIOFn_unit_GCTABLE_END_VAL:
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl TextIOFn_unit_GLOBALS_BEGIN_VAL
TextIOFn_unit_GLOBALS_BEGIN_VAL:
		! Global
	.word 0x00000037
vector_132792:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
elem_132805:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_134916:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_c_134918:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000211
record_147345:
	.long TextIOFn_functor_var_c_code_146295
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl TextIOFn_FCT_c_INT
TextIOFn_FCT_c_INT:
	.long record_147345
	.long record_147345
		! static record tag
	.word 0x00000011
record_147349:
	.word 0x00000005
	.word 0x00000000
		! static record tag
	.word 0x00000009
record_147363:
	.word 0x00000009
		! Global
	.word 0x00000037
strbindvar_r_extract_136245:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_sub_136246:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_update_136247:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
base_136248:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000002
string_147410:
		! string size = 0
	! .ascii "" (zero length string)
		! Global
	.word 0x00000037
_136252:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_132801:
	.long TextIOFn_anonfun_code_146300
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
mk_136309:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_length_136348:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000011
record_147466:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x00000037
_141006:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_147488:
		! string size = 5
	.ascii "match"
.align 4
		! static record tag
	.word 0x00000619
record_147490:
	.word 0x00000102
	.word 0x00000100
	.long string_147488
	.word 0x0000002a
string_147496:
		! string size = 5
	.ascii "input"
.align 4
	.word 0x00000032
string_147503:
		! string size = 6
	.ascii "input1"
.align 4
	.word 0x00000032
string_147515:
		! string size = 6
	.ascii "inputN"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_concat_136787:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
quot_136425:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000042
string_147540:
		! string size = 8
	.ascii "inputAll"
.align 4
	.word 0x00000042
string_147549:
		! string size = 8
	.ascii "canInput"
.align 4
		! Global
	.word 0x00000037
mk_136899:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
stamp_136957:
	.word 0x00000102
		! Global
	.word 0x00000037
mk_136989:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000003a
string_147579:
		! string size = 7
	.ascii "closeIn"
.align 4
	.word 0x0000005a
string_147592:
		! string size = 11
	.ascii "endOfStream"
.align 4
		! Global
	.word 0x00000037
mk_137126:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_133355:
	.long TextIOFn_anonfun_code_146305
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_142922:
	.long TextIOFn_anonfun_code_146310
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_142935:
	.long TextIOFn_anonfun_code_146315
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
addCleaner_137245:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_133419:
	.long TextIOFn_anonfun_code_146320
	.word 0x00000100
	.word 0x00000100
	.word 0x00000042
string_147649:
		! string size = 8
	.ascii "getPosIn"
.align 4
		! Global
	.word 0x00000037
mk_137384:
	.word 0x00000102
	.word 0x00000102
	.word 0x000000aa
string_147686:
		! string size = 21
	.ascii "filePosIn: impossible"
.align 4
		! Global
	.word 0x00000037
_137467:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000004a
string_147706:
		! string size = 9
	.ascii "filePosIn"
.align 4
	.word 0x00000072
string_147721:
		! string size = 14
	.ascii "bogus position"
.align 4
		! Global
	.word 0x00000037
funarg_3_137516:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_137527:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_137603:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000000a
string_147769:
		! string size = 1
	.ascii "\n"
.align 4
		! static record tag
	.word 0x00000311
record_147778:
	.long string_147769
	.word 0x00000000
	.word 0x0000004a
string_147795:
		! string size = 9
	.ascii "inputLine"
.align 4
		! static record tag
	.word 0x00000619
isNL_133728:
	.long TextIOFn_isNL_code_146325
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
mk_137905:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000032
string_147817:
		! string size = 6
	.ascii "output"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_length_138016:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_copyVec_138046:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
copyVec_133890:
	.long TextIOFn_copyVec_code_146330
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
copyVec_133906:
	.long TextIOFn_copyVec_code_146335
	.word 0x00000100
	.word 0x00000100
	.word 0x0000003a
string_147851:
		! string size = 7
	.ascii "output1"
.align 4
		! static record tag
	.word 0x00000009
record_147859:
	.word 0x00000001
	.word 0x00000042
string_147869:
		! string size = 8
	.ascii "flushOut"
.align 4
	.word 0x00000042
string_147878:
		! string size = 8
	.ascii "closeOut"
.align 4
		! static record tag
	.word 0x00000619
iterate_r_134035:
	.long TextIOFn_iterate_r_code_146351
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_134042:
	.long TextIOFn_anonfun_code_146381
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
_138315:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_134076:
	.long TextIOFn_anonfun_code_146386
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
_138361:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_134104:
	.long TextIOFn_anonfun_code_146391
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_134108:
	.long TextIOFn_anonfun_code_146396
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_134112:
	.long TextIOFn_anonfun_code_146401
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
strbindvar_r_array_138372:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
rebindCleaner_138384:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000004a
string_147958:
		! string size = 9
	.ascii "getWriter"
.align 4
	.word 0x0000004a
string_147968:
		! string size = 9
	.ascii "getPosOut"
.align 4
	.word 0x00000052
string_147979:
		! string size = 10
	.ascii "filePosOut"
.align 4
	.word 0x0000004a
string_147989:
		! string size = 9
	.ascii "setPosOut"
.align 4
	.word 0x00000062
string_148002:
		! string size = 12
	.ascii "outputSubstr"
.align 4
		! Global
	.word 0x00000037
PLUSEbuffer_mode_137877:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_133742:
	.long TextIOFn_anonfun_code_146406
	.word 0x00000100
	.word 0x00000100
	.word 0x0000006a
string_148030:
		! string size = 13
	.ascii "setBufferMode"
.align 4
	.word 0x0000006a
string_148045:
		! string size = 13
	.ascii "getBufferMode"
.align 4
		! static record tag
	.word 0x00000619
anonfun_134351:
	.long TextIOFn_anonfun_code_146411
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
PLUSEiodesc_kind_138988:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
kind_138991:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
tty_138997:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000032
string_148110:
		! string size = 6
	.ascii "openIn"
.align 4
	.word 0x0000003a
string_148118:
		! string size = 7
	.ascii "openOut"
.align 4
	.word 0x00000052
string_148129:
		! string size = 10
	.ascii "openAppend"
.align 4
	.word 0x00000042
string_148138:
		! string size = 8
	.ascii "<string>"
.align 4
		! static record tag
	.word 0x00000619
anonfun_134633:
	.long TextIOFn_anonfun_code_146416
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_134637:
	.long TextIOFn_anonfun_code_146421
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_134641:
	.long TextIOFn_anonfun_code_146426
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000719
record_148158:
	.long anonfun_134633
	.long anonfun_134641
	.long anonfun_134637
		! static record tag
	.word 0x00000619
anonfun_134660:
	.long TextIOFn_anonfun_code_146431
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_134686:
	.long TextIOFn_anonfun_code_146436
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
stdStrmHook_139250:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000f21
record_148182:
	.word 0x00000100
	.word 0x00000100
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl TextIOFn_FCT_r_INT
TextIOFn_FCT_r_INT:
	.long functor_var_r_132591
	.long functor_var_r_132591
		! static record tag
	.word 0x00000619
functor_var_r_132591:
	.long TextIOFn_functor_var_r_code_146441
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148199:
	.long TextIOFn_chunkSzOfIBuf_code_146446
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148223:
	.long TextIOFn_generalizedInput_code_146462
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148230:
	.long TextIOFn_terminate_code_146474
	.word 0x00000100
	.word 0x00000001
		! static record tag
	.word 0x00000619
record_148236:
	.long TextIOFn_findEOS_code_146481
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148345:
	.long TextIOFn_getData_code_146618
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148364:
	.long TextIOFn_getPosIn_code_146632
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148370:
	.long TextIOFn_filePosIn_code_146637
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148412:
	.long TextIOFn_outputExn_inner_code_146690
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148680:
	.long TextIOFn_mkInstream_code_147015
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148686:
	.long TextIOFn_getInstream_code_147020
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148692:
	.long TextIOFn_setInstream_code_147025
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148698:
	.long TextIOFn_mkOutstream_code_147030
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148704:
	.long TextIOFn_getOutstream_code_147035
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
record_148710:
	.long TextIOFn_setOutstream_code_147040
	.word 0x00000100
	.word 0x00000100
	.word 0x00000052
string_148824:
		! string size = 10
	.ascii "WouldBlock"
.align 4
		! static record tag
	.word 0x00000009
record_156766:
	.word 0x00000008
		! static record tag
	.word 0x00000719
record_156768:
	.long record_156766
	.long record_156766
	.long record_156766
		! static record tag
	.word 0x00000009
record_156780:
	.word 0x00000008
		! static record tag
	.word 0x00000009
record_156784:
	.word 0x00000008
		! static record tag
	.word 0x00000009
record_156788:
	.word 0x00000008
		! static record tag
	.word 0x00000009
record_156792:
	.word 0x00000008
		! static record tag
	.word 0x00000211
record_156796:
	.word 0x00000000
	.long record_156780
		! static record tag
	.word 0x00000211
record_156799:
	.word 0x00000000
	.long record_156788
		! Module closure
	.word 0x00000619
	.globl TextIOFn_unit_closure
TextIOFn_unit_closure:
	.long TextIOFn_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl TextIOFn_unit
TextIOFn_unit:
	.long TextIOFn_unit_closure
	.long TextIOFn_unit_closure
	.globl TextIOFn_unit_GLOBALS_END_VAL
TextIOFn_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.long 0 !filler

	.data
	.align 8
	.globl TextIOFn_unit_TRACE_GLOBALS_BEGIN_VAL
TextIOFn_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long stdStrmHook_139250
	.long tty_138997
	.long kind_138991
	.long PLUSEiodesc_kind_138988
	.long PLUSEbuffer_mode_137877
	.long rebindCleaner_138384
	.long strbindvar_r_array_138372
	.long _138361
	.long _138315
	.long strbindvar_r_copyVec_138046
	.long strbindvar_r_length_138016
	.long mk_137905
	.long _137603
	.long _137527
	.long funarg_3_137516
	.long _137467
	.long mk_137384
	.long addCleaner_137245
	.long mk_137126
	.long mk_136989
	.long mk_136899
	.long quot_136425
	.long strbindvar_r_concat_136787
	.long _141006
	.long strbindvar_r_length_136348
	.long mk_136309
	.long _136252
	.long base_136248
	.long strbindvar_r_update_136247
	.long strbindvar_r_sub_136246
	.long strbindvar_r_extract_136245
	.long strbindvar_c_134918
	.long _134916
	.long elem_132805
	.long vector_132792
	.globl TextIOFn_unit_TRACE_GLOBALS_END_VAL
TextIOFn_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
