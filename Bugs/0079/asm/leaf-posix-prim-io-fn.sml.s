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
	.globl PosixPrimIOFn_unit_GCTABLE_BEGIN_VAL
PosixPrimIOFn_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl PosixPrimIOFn_unit_CODE_END_VAL
	.globl PosixPrimIOFn_unit_CODE_BEGIN_VAL
PosixPrimIOFn_unit_CODE_BEGIN_VAL:
	.text
	.align 8
	.global PosixPrimIOFn_functor_var_c_code_812517
 ! arguments : [$812519,$8] [$788942,$9] 
 ! results    : [$818879,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
PosixPrimIOFn_functor_var_c_code_812517:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_818914
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_818914:
	st	%r15, [%sp+92]
code_818900:
funtop_818869:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_818901
	nop
code_818902:
	call	GCFromML ! delay slot empty
	nop
needgc_818901:
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	sethi	%hi(Word8Vector_STR_c_INT), %r8
	or	%r8, %lo(Word8Vector_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(strbindvar_c_789415), %r8
	or	%r8, %lo(strbindvar_c_789415), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
code_818913:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_functor_var_c_code_812517,(.-PosixPrimIOFn_functor_var_c_code_812517)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_818901
	.word 0x00180007
	.word 0x00170000
	.word 0xbffc3e00
	.word 0xbffc3c00
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_812534
 ! arguments : [$812536,$8] [$812537,$9] [$789430,$10] 
 ! results    : [$818868,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_812534:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_818920
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_818920:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_818915:
funtop_818854:
	! Proj_c at label type_802612_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_802611_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_818916:
	! done making tail call
code_818918:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_anonfun_code_812534,(.-PosixPrimIOFn_anonfun_code_812534)

	.section	".rodata"
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_812527
 ! arguments : [$812529,$8] [$812530,$9] [$789428,$10] 
 ! results    : [$818853,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_812527:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_818936
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_818936:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_818921:
funtop_818809:
	! Proj_c at label type_802612_INT
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_802611_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+96]
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+96], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_818934:
	mov	%r8, %r10
code_818924:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_818925
	nop
code_818926:
	call	GCFromML ! delay slot empty
	nop
needgc_818925:
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
	sethi	%hi(PosixPrimIOFn_anonfun_code_812534), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_812534), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
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
	ld	[%sp+96], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_818935:
code_818931:
	! done making normal call
code_818933:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_anonfun_code_812527,(.-PosixPrimIOFn_anonfun_code_812527)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_818925
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_818934
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_818935
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_announce_inner_code_812547
 ! arguments : [$812549,$8] [$812550,$9] [$789426,$10] 
 ! results    : [$812550,$8] 
 ! destroys   :  $10 $8
 ! modifies   :  $10 $8
PosixPrimIOFn_announce_inner_code_812547:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_818940
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_818940:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_818937:
funtop_818804:
code_818939:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_announce_inner_code_812547,(.-PosixPrimIOFn_announce_inner_code_812547)

	.section	".rodata"
	.text
	.align 8
	.global PosixPrimIOFn_announce_r_code_812522
 ! arguments : [$812524,$8] [$789420,$9] [$812525,$10] [$789421,$11] 
 ! results    : [$818803,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_announce_r_code_812522:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_818954
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_818954:
	st	%r15, [%sp+92]
code_818941:
funtop_818753:
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_818942
	nop
code_818943:
	call	GCFromML ! delay slot empty
	nop
needgc_818942:
	! Proj_c at label 'b_TYV
	ld	[%r9+4], %r11
	! Proj_c at label 'c_TYV
	ld	[%r9+8], %r10
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	or	%r0, 6, %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 1-record
	! done allocating 1 record
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_anonfun_code_812527), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_812527), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_announce_inner_code_812547), %r8
	or	%r8, %lo(PosixPrimIOFn_announce_inner_code_812547), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r18
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! Proj_c at label 'a_TYV
	ld	[%r9], %r12
	sethi	%hi(record_818767), %r8
	or	%r8, %lo(record_818767), %r10
	! making closure call
	sethi	%hi(vararg_INT), %r8
	or	%r8, %lo(vararg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	mov	%r12, %r9
	jmpl	%r13, %r15
	mov	%r18, %r12
code_818953:
code_818950:
	! done making normal call
code_818952:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_announce_r_code_812522,(.-PosixPrimIOFn_announce_r_code_812522)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_818942
	.word 0x00180007
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_818953
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_isRegFile_code_812556
 ! arguments : [$812558,$8] [$812559,$9] [$789437,$10] 
 ! results    : [$818752,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_isRegFile_code_812556:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_818966
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_818966:
	st	%r15, [%sp+92]
code_818955:
funtop_818732:
	! making closure call
	sethi	%hi(_802663), %r8
	or	%r8, %lo(_802663), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_818965:
	mov	%r8, %r10
code_818958:
	! done making normal call
	! making closure call
	sethi	%hi(_802675), %r8
	or	%r8, %lo(_802675), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_818961:
	! done making tail call
code_818963:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_isRegFile_code_812556,(.-PosixPrimIOFn_isRegFile_code_812556)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_818965
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_endPos_code_812566
 ! arguments : [$812568,$8] [$812569,$9] 
 ! results    : [$818731,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_endPos_code_812566:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819003
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819003:
	st	%r15, [%sp+92]
code_818967:
funtop_818651:
	ld	[%r9], %r12
	ld	[%r9+4], %r16
	st	%r16, [%sp+96]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_819002:
code_818971:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_818670
	nop
zero_case_818669:
	ba	after_zeroone_818671
	or	%r0, 256, %r8
one_case_818670:
	sethi	%hi(mk_794882), %r8
	or	%r8, %lo(mk_794882), %r9
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
after_zeroone_818671:
	sethi	%hi(string_814433), %r8
	or	%r8, %lo(string_814433), %r10
	! making closure call
	sethi	%hi(_794951), %r8
	or	%r8, %lo(_794951), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_818997:
	mov	%r8, %r12
code_818980:
	! done making normal call
	sethi	%hi(strbindvar_r_fstat_794758), %r8
	or	%r8, %lo(strbindvar_r_fstat_794758), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_818998:
	mov	%r8, %r12
code_818983:
	! done making normal call
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	sethi	%hi(type_802662), %r8
	or	%r8, %lo(type_802662), %r9
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
	mov	%r18, %r9
code_818999:
	mov	%r8, %r9
code_818990:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_819000:
	mov	%r8, %r10
code_818991:
	! done making normal call
	! making closure call
	sethi	%hi(_802807), %r8
	or	%r8, %lo(_802807), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_818994:
	! done making tail call
code_818996:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_endPos_code_812566,(.-PosixPrimIOFn_endPos_code_812566)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_818997
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_818998
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_818999
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819000
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_819002
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_getPos_code_812575
 ! arguments : [$812577,$8] [$812578,$9] 
 ! results    : [$818650,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
PosixPrimIOFn_getPos_code_812575:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819007
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_819007:
	st	%r15, [%sp+92]
code_819004:
funtop_818645:
	! int sub start
	ld	[%r9], %r8
	! int sub end
code_819006:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_getPos_code_812575,(.-PosixPrimIOFn_getPos_code_812575)

	.section	".rodata"
	.text
	.align 8
	.global PosixPrimIOFn_setPos_code_812582
 ! arguments : [$812584,$8] [$812585,$9] [$789470,$10] 
 ! results    : [$818644,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_setPos_code_812582:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819043
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819043:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
code_819008:
funtop_818582:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r12
	ld	[%r9+8], %r16
	st	%r16, [%sp+96]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_819042:
code_819012:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_818603
	nop
zero_case_818602:
	ba	after_zeroone_818604
	or	%r0, 256, %r8
one_case_818603:
	sethi	%hi(mk_794882), %r8
	or	%r8, %lo(mk_794882), %r9
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
after_zeroone_818604:
	sethi	%hi(string_814386), %r8
	or	%r8, %lo(string_814386), %r10
	! making closure call
	sethi	%hi(_794896), %r8
	or	%r8, %lo(_794896), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819039:
	mov	%r8, %r12
code_819021:
	! done making normal call
	sethi	%hi(strbindvar_r_lseek_794899), %r8
	or	%r8, %lo(strbindvar_r_lseek_794899), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819040:
	mov	%r8, %r9
code_819024:
	! done making normal call
	or	%r0, 2, %r12
	! making closure call
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r13, %r15
	ld	[%sp+100], %r11
code_819041:
	mov	%r8, %r11
code_819025:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_819029
	nop
code_819030:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_819029:
	ld	[%r2+792], %r10
	ld	[%sp+104], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	ld	[%sp+104], %r17
	st	%r11, [%r17]
	or	%r0, 256, %r8
code_819038:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_setPos_code_812582,(.-PosixPrimIOFn_setPos_code_812582)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819039
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00130000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819040
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00130000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819041
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_819029
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819042
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00130000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_verifyPos_code_812593
 ! arguments : [$812595,$8] [$812596,$9] 
 ! results    : [$818577,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_verifyPos_code_812593:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819066
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819066:
	st	%r15, [%sp+92]
code_819044:
funtop_818549:
	ld	[%r9], %r16
	st	%r16, [%sp+100]
	ld	[%r9+4], %r16
	st	%r16, [%sp+96]
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819065:
	mov	%r8, %r11
code_819047:
	! done making normal call
	or	%r0, 0, %r12
	! making closure call
	sethi	%hi(strbindvar_r_lseek_794899), %r8
	or	%r8, %lo(strbindvar_r_lseek_794899), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+96], %r10
code_819064:
	mov	%r8, %r11
code_819050:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_819054
	nop
code_819055:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_819054:
	ld	[%r2+792], %r10
	ld	[%sp+100], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	ld	[%sp+100], %r17
	st	%r11, [%r17]
code_819063:
	mov	%r11, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_verifyPos_code_812593,(.-PosixPrimIOFn_verifyPos_code_812593)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819064
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long afterMutateCheck_819054
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_819065
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00070000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_posFns_code_812561
 ! arguments : [$812563,$8] [$812564,$9] [$802681,$10] [$802682,$11] 
 ! results    : [$818395,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_posFns_code_812561:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819139
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_819139:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
	st	%r11, [%sp+96]
code_819067:
funtop_818341:
	! making closure call
	sethi	%hi(isRegFile_802647), %r8
	or	%r8, %lo(isRegFile_802647), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_819137:
code_819069:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819070
	nop
code_819071:
	call	GCFromML ! delay slot empty
	nop
needgc_819070:
	cmp	%r8, 0
	bne,pn	%icc,one_case_818353
	nop
zero_case_818352:
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819138:
	mov	%r8, %r12
code_819076:
	! done making normal call
	or	%r0, 1, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_818373
	nop
code_819077:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r12, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_819134:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_819078:
	ba	array_int_after_818372 ! delay slot empty
	nop
array_int_small_818373:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819080
	nop
code_819081:
	call	GCFromML ! delay slot empty
	nop
needgc_819080:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_818380
	add	%r9, %r0, %r9
array_init_loopto_818381:
	add	%r11, %r9, %r8
	st	%r12, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_818380:
	cmp	%r9, 0
	bge	array_init_loopto_818381
	nop
array_int_after_818372:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819086
	nop
code_819087:
	call	GCFromML ! delay slot empty
	nop
needgc_819086:
	! allocating 5-record
	sethi	%hi(gctag_794865), %r8
	ld	[%r8+%lo(gctag_794865)], %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	or	%r0, 0, %r8
	st	%r8, [%r4+12]
	st	%r11, [%r4+16]
	or	%r0, 0, %r8
	st	%r8, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
	ba	after_zeroone_818354 ! delay slot empty
	nop
one_case_818353:
	! allocating 1 closures
	or	%r0, 273, %r10
	sethi	%hi(Posix_STR_c_INT), %r8
	or	%r8, %lo(Posix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	ld	[%r8+12], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_819093
	nop
code_819094:
	or	%r0, 0, %r8
cmpui_819093:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 2-record
	st	%r10, [%r4]
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
	sethi	%hi(PosixPrimIOFn_endPos_code_812566), %r8
	or	%r8, %lo(PosixPrimIOFn_endPos_code_812566), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819132:
	mov	%r8, %r11
code_819098:
	! done making normal call
	or	%r0, 1, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_818443
	nop
code_819099:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_819135:
	call	load_regs_MLtoC ! delay slot empty
	nop
	st	%r8, [%sp+108]
code_819100:
	ba	array_int_after_818442 ! delay slot empty
	nop
array_int_small_818443:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819102
	nop
code_819103:
	call	GCFromML ! delay slot empty
	nop
needgc_819102:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r16
	st	%r16, [%sp+108]
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_818450
	add	%r9, %r0, %r9
array_init_loopto_818451:
	ld	[%sp+108], %r17
	add	%r17, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_818450:
	cmp	%r9, 0
	bge	array_init_loopto_818451
	nop
array_int_after_818442:
	add	%r4, 76, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819108
	nop
code_819109:
	call	GCFromML ! delay slot empty
	nop
needgc_819108:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_getPos_code_812575), %r8
	or	%r8, %lo(PosixPrimIOFn_getPos_code_812575), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+104]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 793, %r10
	sethi	%hi(Posix_STR_c_INT), %r8
	or	%r8, %lo(Posix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	ld	[%r8+12], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_819114
	nop
code_819115:
	or	%r0, 0, %r8
cmpui_819114:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 3-record
	st	%r10, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_setPos_code_812582), %r8
	or	%r8, %lo(PosixPrimIOFn_setPos_code_812582), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 273, %r10
	sethi	%hi(Posix_STR_c_INT), %r8
	or	%r8, %lo(Posix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	ld	[%r8+12], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_819119
	nop
code_819120:
	or	%r0, 0, %r8
cmpui_819119:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 2-record
	st	%r10, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_verifyPos_code_812593), %r8
	or	%r8, %lo(PosixPrimIOFn_verifyPos_code_812593), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure polycall
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+8], %r9
code_819133:
	mov	%r8, %r10
code_819122:
	! done making normal call
	! making closure call
	sethi	%hi(_794995), %r8
	or	%r8, %lo(_794995), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819136:
code_819125:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819126
	nop
code_819127:
	call	GCFromML ! delay slot empty
	nop
needgc_819126:
	! allocating 5-record
	sethi	%hi(gctag_794865), %r8
	ld	[%r8+%lo(gctag_794865)], %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	ld	[%sp+96], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
after_zeroone_818354:
code_819131:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_posFns_code_812561,(.-PosixPrimIOFn_posFns_code_812561)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819132
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01070000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819133
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long needgc_819070
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x00070000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_794597
		! -------- label,sizes,reg
	.long code_819134
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819080
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819086
	.word 0x00200007
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_819135
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01070000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_819102
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01070000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_819108
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01470000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819136
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long needgc_819126
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long code_819137
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00070000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819138
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_812614
 ! arguments : [$812616,$8] [$812617,$9] [$790033,$10] 
 ! results    : [$818271,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_812614:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819181
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_819181:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_819140:
funtop_818228:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819141
	nop
code_819142:
	call	GCFromML ! delay slot empty
	nop
needgc_819141:
	! Proj_c at label type_803607_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_803601_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label type_796171_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_796162_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+104]
	ld	[%r9], %r16
	st	%r16, [%sp+112]
	ld	[%r9+4], %r13
	or	%r0, 793, %r9
	ld	[%sp+100], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_819144
	nop
code_819145:
	or	%r0, 0, %r8
cmpui_819144:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 3-record
	st	%r9, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+112], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r12
	add	%r4, 16, %r4
	! done allocating 3 record
	sethi	%hi(exn_handler_818247), %r8
	or	%r8, %lo(exn_handler_818247), %r11
	ld	[%r2+808], %r8
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
code_819179:
	mov	%r8, %r10
code_819148:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819149
	nop
code_819150:
	call	GCFromML ! delay slot empty
	nop
needgc_819149:
	ld	[%sp+108], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_818275
	nop
code_819152:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_818276
	nop
code_819153:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_818275
	nop
code_819154:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_818275
	nop
code_819155:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_818275
	nop
dynamic_nobox_818276:
	ba	xinject_sum_dyn_after_818272
	mov	%r10, %r8
dynamic_box_818275:
	or	%r0, 9, %r9
	ld	[%sp+104], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_819158
	nop
code_819159:
	or	%r0, 0, %r8
cmpui_819158:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1-record
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
xinject_sum_dyn_after_818272:
	ba	exn_handler_after_818248
	ld	[%r1+12], %r1
exn_handler_818247:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	ld	[%r8+4], %r16
	st	%r16, [%sp+104]
	ld	[%r8+8], %r16
	st	%r16, [%sp+112]
	st	%r15, [%sp+108]
	ld	[%sp+108], %r17
	ld	[%r17], %r9
exnarm_818287:
	sethi	%hi(stamp_795358), %r8
	ld	[%r8+%lo(stamp_795358)], %r8
	cmp	%r9, %r8
	bne	exnarm_818290
	nop
code_819164:
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%r8+4], %r9
sumarm_818304:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	ble	nomatch_sum_818302
	nop
code_819165:
	ld	[%r9], %r10
	sethi	%hi(again_795373), %r8
	ld	[%r8+%lo(again_795373)], %r11
	! making closure call
	sethi	%hi(PLUSEsyserror_795370), %r8
	or	%r8, %lo(PLUSEsyserror_795370), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_819180:
code_819169:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_818329
	nop
zero_case_818328:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_zeroone_818330
	or	%r0, 0, %r8
one_case_818329:
	ld	[%sp+112], %r8
after_zeroone_818330:
	ba	after_sum_818301 ! delay slot empty
	nop
sumarm_818305:
nomatch_sum_818302:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_818301:
	ba	afterPLUSexncase_818286 ! delay slot empty
	nop
exnarm_818290:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
afterPLUSexncase_818286:
exn_handler_after_818248:
code_819178:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_anonfun_code_812614,(.-PosixPrimIOFn_anonfun_code_812614)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819179
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long needgc_819141
	.word 0x00200009
	.word 0x00170000
	.word 0x00000200
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_819149
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! worddata
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_819180
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03440000
		! worddata
	.word 0x00000000
	.word 0x00000064
	.text
	.align 8
	.global PosixPrimIOFn_handleBlock_inner_code_812607
 ! arguments : [$812609,$8] [$812610,$9] [$790031,$10] 
 ! results    : [$818227,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_handleBlock_inner_code_812607:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819199
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_819199:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+104]
	mov	%r10, %r12
code_819182:
funtop_818166:
	! Proj_c at label type_803607_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_803601_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_796171_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+116]
	! Proj_c at label type_796162_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
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
	ld	[%sp+112], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_819198:
	mov	%r8, %r11
code_819185:
	! done making normal call
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819186
	nop
code_819187:
	call	GCFromML ! delay slot empty
	nop
needgc_819186:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	ld	[%sp+116], %r17
	st	%r17, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	or	%r0, 529, %r9
	ld	[%sp+108], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_819189
	nop
code_819190:
	or	%r0, 0, %r8
cmpui_819189:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_anonfun_code_812614), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_812614), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
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
code_819197:
code_819194:
	! done making normal call
code_819196:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_handleBlock_inner_code_812607,(.-PosixPrimIOFn_handleBlock_inner_code_812607)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819197
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819186
	.word 0x00200009
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05750000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819198
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05750000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global PosixPrimIOFn_handleBlock_r_code_812602
 ! arguments : [$812604,$8] [$790026,$9] [$812605,$10] [$790027,$11] 
 ! results    : [$818160,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_handleBlock_r_code_812602:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819215
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819215:
	st	%r15, [%sp+92]
code_819200:
funtop_818123:
	! Proj_c at label 'a_TYV
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	! Proj_c at label 'b_TYV
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+100], %r9
code_819214:
	st	%r8, [%sp+96]
code_819203:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_794813), %r8
	or	%r8, %lo(type_794813), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+100], %r9
code_819213:
	mov	%r8, %r9
code_819206:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819207
	nop
code_819208:
	call	GCFromML ! delay slot empty
	nop
needgc_819207:
	! done making constructor call
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_handleBlock_inner_code_812607), %r8
	or	%r8, %lo(PosixPrimIOFn_handleBlock_inner_code_812607), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 0, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_819212:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_handleBlock_r_code_812602,(.-PosixPrimIOFn_handleBlock_r_code_812602)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819213
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_819207
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_819214
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
	.text
	.align 8
	.global PosixPrimIOFn_readVec_code_812646
 ! arguments : [$812648,$8] [$812649,$9] [$789632,$10] 
 ! results    : [$818087,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_readVec_code_812646:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819253
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819253:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
code_819216:
funtop_818055:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r16
	st	%r16, [%sp+96]
	sethi	%hi(string_814572), %r8
	or	%r8, %lo(string_814572), %r10
	! making closure call
	sethi	%hi(_795246), %r8
	or	%r8, %lo(_795246), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819252:
	mov	%r8, %r12
code_819220:
	! done making normal call
	sethi	%hi(strbindvar_r_readVec_795249), %r8
	or	%r8, %lo(strbindvar_r_readVec_795249), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819247:
	mov	%r8, %r9
code_819223:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_819248:
	st	%r8, [%sp+100]
code_819224:
	! done making normal call
	! making closure call
	sethi	%hi(strbindvar_r_length_795257), %r8
	or	%r8, %lo(strbindvar_r_length_795257), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_819249:
	mov	%r8, %r10
code_819227:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+96]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819250:
	mov	%r8, %r11
code_819230:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_795216), %r8
	or	%r8, %lo(PLUS_795216), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_819251:
	mov	%r8, %r11
code_819233:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_819237
	nop
code_819238:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_819237:
	ld	[%r2+792], %r10
	ld	[%sp+104], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	ld	[%sp+104], %r17
	st	%r11, [%r17]
	ld	[%sp+100], %r16
	st	%r16, [%sp+100]
code_819246:
	ld	[%sp+100], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_readVec_code_812646,(.-PosixPrimIOFn_readVec_code_812646)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819247
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00130000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819248
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819249
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_819250
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_819251
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long afterMutateCheck_819237
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_819252
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00130000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_readArr_code_812655
 ! arguments : [$812657,$8] [$812658,$9] [$802949,$10] [$802950,$11] [$802951,$12] 
 ! results    : [$818028,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_readArr_code_812655:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819291
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819291:
	st	%r15, [%sp+92]
code_819254:
funtop_817989:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819255
	nop
code_819256:
	call	GCFromML ! delay slot empty
	nop
needgc_819255:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	! allocating 3-record
	sethi	%hi(record_gctag_802953), %r8
	ld	[%r8+%lo(record_gctag_802953)], %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	sethi	%hi(string_814650), %r8
	or	%r8, %lo(string_814650), %r10
	! making closure call
	sethi	%hi(_795273), %r8
	or	%r8, %lo(_795273), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819290:
	mov	%r8, %r12
code_819262:
	! done making normal call
	sethi	%hi(strbindvar_r_readArr_795276), %r8
	or	%r8, %lo(strbindvar_r_readArr_795276), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819286:
	mov	%r8, %r9
code_819265:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r12, %r15
	ld	[%sp+96], %r11
code_819287:
	st	%r8, [%sp+96]
code_819266:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_819288:
	mov	%r8, %r11
code_819269:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_795216), %r8
	or	%r8, %lo(PLUS_795216), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_819289:
	mov	%r8, %r11
code_819272:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_819276
	nop
code_819277:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_819276:
	ld	[%r2+792], %r10
	ld	[%sp+104], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	ld	[%sp+104], %r17
	st	%r11, [%r17]
	ld	[%sp+96], %r16
	st	%r16, [%sp+96]
code_819285:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_readArr_code_812655,(.-PosixPrimIOFn_readArr_code_812655)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_819255
	.word 0x001c0009
	.word 0x00170000
	.word 0x00001200
	.word 0x00000800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! worddata
	.word 0x80000000
	.long type_794601
		! -------- label,sizes,reg
	.long code_819286
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x001d0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819287
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819288
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819289
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_819276
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819290
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x001d0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_812676
 ! arguments : [$812678,$8] [$812679,$9] [$789672,$10] 
 ! results    : [$817988,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_812676:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819327
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_819327:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+116]
code_819292:
funtop_817894:
	! Proj_c at label type_803006_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label type_795290_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+100]
	ld	[%r9], %r12
	ld	[%r9+4], %r16
	st	%r16, [%sp+112]
	ld	[%r9+8], %r16
	st	%r16, [%sp+104]
	ld	[%r9+12], %r16
	st	%r16, [%sp+108]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_819325:
code_819296:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_817923
	nop
zero_case_817922:
	ba	after_zeroone_817924
	or	%r0, 256, %r8
one_case_817923:
	sethi	%hi(mk_794882), %r8
	or	%r8, %lo(mk_794882), %r9
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
after_zeroone_817924:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	ld	[%sp+112], %r9
code_819321:
code_819305:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_817945
	nop
zero_case_817944:
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(_802883), %r8
	or	%r8, %lo(_802883), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819326:
	mov	%r8, %r11
code_819309:
	! done making normal call
	! making closure call
	sethi	%hi(strbindvar_r_setfl_795148), %r8
	or	%r8, %lo(strbindvar_r_setfl_795148), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r10
code_819323:
code_819312:
	! done making normal call
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	or	%r0, 1, %r11
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	mov	%r9, %r8
	jmpl	%r12, %r15
	ld	[%sp+112], %r9
code_819324:
code_819316:
	! done making normal call
	ba	after_zeroone_817946 ! delay slot empty
	nop
one_case_817945:
	or	%r0, 256, %r8
after_zeroone_817946:
	! making closure call
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_819318:
	! done making tail call
code_819320:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_anonfun_code_812676,(.-PosixPrimIOFn_anonfun_code_812676)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819321
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0d750000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819323
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0d450000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819324
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c450000
		! worddata
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819325
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0d750000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000001
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819326
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0d750000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global PosixPrimIOFn_blockWrap_inner_code_812669
 ! arguments : [$812671,$8] [$812672,$9] [$789670,$10] 
 ! results    : [$817893,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_blockWrap_inner_code_812669:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819347
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_819347:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_819328:
funtop_817827:
	! Proj_c at label type_803006_INT
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_795290_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+104]
	ld	[%r9], %r16
	st	%r16, [%sp+108]
	ld	[%r9+4], %r16
	st	%r16, [%sp+112]
	ld	[%r9+8], %r16
	st	%r16, [%sp+96]
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r11
	ld	[%sp+100], %r9
	jmpl	%r13, %r15
	ld	[%sp+104], %r10
code_819346:
	mov	%r8, %r9
code_819331:
	! done making normal call
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819332
	nop
code_819333:
	call	GCFromML ! delay slot empty
	nop
needgc_819332:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 2849, %r12
	sethi	%hi(Posix_STR_c_INT), %r10
	or	%r10, %lo(Posix_STR_c_INT), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10+12], %r10
	ld	[%r10+12], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819337
	nop
code_819338:
	or	%r0, 0, %r10
cmpui_819337:
	sll	%r10, 10, %r10
	add	%r10, %r0, %r10
	or	%r10, %r12, %r12
	! allocating 4-record
	st	%r12, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	st	%r9, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(PosixPrimIOFn_anonfun_code_812676), %r9
	or	%r9, %lo(PosixPrimIOFn_anonfun_code_812676), %r9
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
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
	ld	[%sp+100], %r9
	jmpl	%r13, %r15
	ld	[%sp+104], %r10
code_819345:
code_819342:
	! done making normal call
code_819344:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_blockWrap_inner_code_812669,(.-PosixPrimIOFn_blockWrap_inner_code_812669)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819345
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819332
	.word 0x00200009
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01570000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819346
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01570000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_blockWrap_r_code_812664
 ! arguments : [$812666,$8] [$789665,$9] [$812667,$10] [$789666,$11] 
 ! results    : [$817822,$8] 
 ! destroys   :  $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_blockWrap_r_code_812664:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819359
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_819359:
	st	%r15, [%sp+92]
	mov	%r9, %r13
code_819348:
funtop_817786:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819349
	nop
code_819350:
	call	GCFromML ! delay slot empty
	nop
needgc_819349:
	ld	[%r10], %r9
	ld	[%r10+4], %r8
	ld	[%r10+8], %r18
	! Proj_c at label 'a_TYV
	ld	[%r13], %r12
	! Proj_c at label 'b_TYV
	ld	[%r13+4], %r11
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r10
	st	%r10, [%r4]
	st	%r12, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r13
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 793, %r12
	sethi	%hi(Posix_STR_c_INT), %r10
	or	%r10, %lo(Posix_STR_c_INT), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10+12], %r10
	ld	[%r10+12], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819354
	nop
code_819355:
	or	%r0, 0, %r10
cmpui_819354:
	sll	%r10, 10, %r10
	add	%r10, %r0, %r10
	or	%r10, %r12, %r12
	! allocating 3-record
	st	%r12, [%r4]
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	st	%r18, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_blockWrap_inner_code_812669), %r8
	or	%r8, %lo(PosixPrimIOFn_blockWrap_inner_code_812669), %r8
	st	%r8, [%r4+4]
	st	%r13, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_819358:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_blockWrap_r_code_812664,(.-PosixPrimIOFn_blockWrap_r_code_812664)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_819349
	.word 0x00180007
	.word 0x00170000
	.word 0xbff82400
	.word 0xbff80000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_812713
 ! arguments : [$812715,$8] [$812716,$9] [$789695,$10] 
 ! results    : [$817712,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_812713:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819439
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_819439:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+128]
code_819360:
funtop_817591:
	! Proj_c at label type_803038_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_803032_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label type_795350_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+124]
	! Proj_c at label type_795323_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+104]
	ld	[%r9], %r12
	ld	[%r9+4], %r16
	st	%r16, [%sp+120]
	ld	[%r9+8], %r16
	st	%r16, [%sp+116]
	ld	[%r9+12], %r16
	st	%r16, [%sp+112]
	ld	[%r9+16], %r16
	st	%r16, [%sp+108]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_819438:
code_819364:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_817628
	nop
zero_case_817627:
	ba	after_zeroone_817629
	or	%r0, 256, %r8
one_case_817628:
	sethi	%hi(mk_794882), %r8
	or	%r8, %lo(mk_794882), %r9
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
after_zeroone_817629:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	ld	[%sp+120], %r9
code_819434:
code_819373:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819374
	nop
code_819375:
	call	GCFromML ! delay slot empty
	nop
needgc_819374:
	cmp	%r8, 0
	bne,pn	%icc,one_case_817650
	nop
zero_case_817649:
	ba	after_zeroone_817651
	or	%r0, 256, %r8
one_case_817650:
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_819381
	nop
code_819382:
	or	%r0, 0, %r8
cmpui_819381:
	cmp	%r8, 0
	be,pn	%icc,else_case_817666
	nop
code_819383:
	sethi	%hi(nonblock_795192), %r8
	or	%r8, %lo(nonblock_795192), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ba	after_ite_817667
	ld	[%r8], %r11
else_case_817666:
	sethi	%hi(nonblock_795192), %r8
	ld	[%r8+%lo(nonblock_795192)], %r11
after_ite_817667:
	! making closure call
	sethi	%hi(strbindvar_r_setfl_795148), %r8
	or	%r8, %lo(strbindvar_r_setfl_795148), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_819433:
code_819390:
	! done making normal call
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	or	%r0, 0, %r11
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	mov	%r9, %r8
	jmpl	%r12, %r15
	ld	[%sp+120], %r9
code_819436:
code_819394:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819395
	nop
code_819396:
	call	GCFromML ! delay slot empty
	nop
needgc_819395:
after_zeroone_817651:
	or	%r0, 793, %r9
	ld	[%sp+100], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_819398
	nop
code_819399:
	or	%r0, 0, %r8
cmpui_819398:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 3-record
	st	%r9, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+116], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	sethi	%hi(exn_handler_817688), %r8
	or	%r8, %lo(exn_handler_817688), %r10
	ld	[%r2+808], %r8
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
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+128], %r10
code_819435:
	mov	%r8, %r10
code_819402:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819403
	nop
code_819404:
	call	GCFromML ! delay slot empty
	nop
needgc_819403:
	ld	[%sp+124], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_817716
	nop
code_819406:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_817717
	nop
code_819407:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_817716
	nop
code_819408:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_817716
	nop
code_819409:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_817716
	nop
dynamic_nobox_817717:
	ba	xinject_sum_dyn_after_817713
	mov	%r10, %r8
dynamic_box_817716:
	or	%r0, 9, %r9
	ld	[%sp+104], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_819412
	nop
code_819413:
	or	%r0, 0, %r8
cmpui_819412:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1-record
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
xinject_sum_dyn_after_817713:
	ba	exn_handler_after_817689
	ld	[%r1+12], %r1
exn_handler_817688:
	ld	[%r1+8], %r8
	ld	[%r1+12], %r1
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	ld	[%r8+4], %r16
	st	%r16, [%sp+104]
	ld	[%r8+8], %r16
	st	%r16, [%sp+116]
	st	%r15, [%sp+108]
	ld	[%sp+108], %r17
	ld	[%r17], %r9
exnarm_817728:
	sethi	%hi(stamp_795358), %r8
	ld	[%r8+%lo(stamp_795358)], %r8
	cmp	%r9, %r8
	bne	exnarm_817731
	nop
code_819418:
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%r8+4], %r9
sumarm_817745:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	ble	nomatch_sum_817743
	nop
code_819419:
	ld	[%r9], %r10
	sethi	%hi(again_795373), %r8
	ld	[%r8+%lo(again_795373)], %r11
	! making closure call
	sethi	%hi(PLUSEsyserror_795370), %r8
	or	%r8, %lo(PLUSEsyserror_795370), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_819437:
code_819423:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_817774
	nop
zero_case_817773:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_zeroone_817775
	or	%r0, 0, %r8
one_case_817774:
	ld	[%sp+116], %r8
after_zeroone_817775:
	ba	after_sum_817742 ! delay slot empty
	nop
sumarm_817746:
nomatch_sum_817743:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_817742:
	ba	afterPLUSexncase_817727 ! delay slot empty
	nop
exnarm_817731:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
afterPLUSexncase_817727:
exn_handler_after_817689:
code_819432:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size PosixPrimIOFn_anonfun_code_812713,(.-PosixPrimIOFn_anonfun_code_812713)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819433
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5c550000
	.word 0x00000003
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819434
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f550000
	.word 0x00000003
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819435
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40100000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819374
	.word 0x00240010
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x5f550000
	.word 0x00000003
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
	.word 0x80000000
	.long type_794597
		! -------- label,sizes,reg
	.long code_819436
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x4c550000
	.word 0x00000003
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_819395
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x4c550000
	.word 0x00000003
		! worddata
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long needgc_819403
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x40100000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000068
		! -------- label,sizes,reg
	.long code_819437
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c440000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000064
		! -------- label,sizes,reg
	.long code_819438
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5f550000
	.word 0x00000003
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000000
	.word 0x00000064
	.word 0x00000002
	.word 0x00000060
	.text
	.align 8
	.global PosixPrimIOFn_noBlockWrap_inner_code_812706
 ! arguments : [$812708,$8] [$812709,$9] [$789693,$10] 
 ! results    : [$817590,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_noBlockWrap_inner_code_812706:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819461
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_819461:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_819440:
funtop_817511:
	! Proj_c at label type_803038_INT
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! Proj_c at label type_803032_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_795350_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+116]
	! Proj_c at label type_795323_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+120]
	ld	[%r9], %r16
	st	%r16, [%sp+124]
	ld	[%r9+4], %r16
	st	%r16, [%sp+104]
	ld	[%r9+8], %r16
	st	%r16, [%sp+100]
	ld	[%r9+12], %r16
	st	%r16, [%sp+108]
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
	ld	[%sp+120], %r10
code_819460:
	mov	%r8, %r9
code_819443:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819444
	nop
code_819445:
	call	GCFromML ! delay slot empty
	nop
needgc_819444:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	ld	[%sp+116], %r17
	st	%r17, [%r4+12]
	ld	[%sp+120], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	sethi	%hi(4905), %r12
	or	%r12, %lo(4905), %r12
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r10
	bgu	cmpui_819447
	nop
code_819448:
	or	%r0, 0, %r10
cmpui_819447:
	sll	%r10, 10, %r10
	add	%r10, %r0, %r10
	or	%r10, %r12, %r12
	sethi	%hi(Posix_STR_c_INT), %r10
	or	%r10, %lo(Posix_STR_c_INT), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10+12], %r10
	ld	[%r10+12], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819451
	nop
code_819452:
	or	%r0, 0, %r10
cmpui_819451:
	sll	%r10, 11, %r10
	add	%r10, %r0, %r10
	or	%r10, %r12, %r12
	! allocating 5-record
	st	%r12, [%r4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	st	%r9, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(PosixPrimIOFn_anonfun_code_812713), %r9
	or	%r9, %lo(PosixPrimIOFn_anonfun_code_812713), %r9
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
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
	ld	[%sp+96], %r10
code_819459:
code_819456:
	! done making normal call
code_819458:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_noBlockWrap_inner_code_812706,(.-PosixPrimIOFn_noBlockWrap_inner_code_812706)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819459
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819444
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55dd0000
		! worddata
	.word 0x00000000
	.word 0x00000060
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819460
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55dd0000
		! worddata
	.word 0x00000000
	.word 0x00000060
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_noBlockWrap_r_code_812701
 ! arguments : [$812703,$8] [$789688,$9] [$812704,$10] [$789689,$11] 
 ! results    : [$817506,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_noBlockWrap_r_code_812701:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819483
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_819483:
	st	%r15, [%sp+92]
code_819462:
funtop_817444:
	ld	[%r10], %r16
	st	%r16, [%sp+100]
	ld	[%r10+4], %r16
	st	%r16, [%sp+104]
	ld	[%r10+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label 'a_TYV
	ld	[%r9], %r16
	st	%r16, [%sp+112]
	! Proj_c at label 'b_TYV
	ld	[%r9+4], %r16
	st	%r16, [%sp+116]
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+116], %r9
code_819482:
	st	%r8, [%sp+96]
code_819465:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_794813), %r8
	or	%r8, %lo(type_794813), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+116], %r9
code_819481:
	mov	%r8, %r9
code_819468:
	add	%r4, 56, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819469
	nop
code_819470:
	call	GCFromML ! delay slot empty
	nop
needgc_819469:
	! done making constructor call
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+116], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r8
	add	%r4, 20, %r4
	! done allocating 4 record
	or	%r0, 801, %r11
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r9
	bgu	cmpui_819472
	nop
code_819473:
	or	%r0, 0, %r9
cmpui_819472:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r11, %r11
	sethi	%hi(Posix_STR_c_INT), %r9
	or	%r9, %lo(Posix_STR_c_INT), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+12], %r9
	ld	[%r9+12], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_819476
	nop
code_819477:
	or	%r0, 0, %r9
cmpui_819476:
	sll	%r9, 11, %r9
	add	%r9, %r0, %r9
	or	%r9, %r11, %r11
	! allocating 4-record
	st	%r11, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	or	%r0, 0, %r9
	st	%r9, [%r4+12]
	ld	[%sp+108], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(PosixPrimIOFn_noBlockWrap_inner_code_812706), %r9
	or	%r9, %lo(PosixPrimIOFn_noBlockWrap_inner_code_812706), %r9
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_819480:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_noBlockWrap_r_code_812701,(.-PosixPrimIOFn_noBlockWrap_r_code_812701)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819481
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05d50000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_819469
	.word 0x00200009
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05d50000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819482
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05d40000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_close_code_812747
 ! arguments : [$812749,$8] [$812750,$9] 
 ! results    : [$817441,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_close_code_812747:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819517
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819517:
	st	%r15, [%sp+92]
code_819484:
funtop_817366:
	ld	[%r9], %r16
	st	%r16, [%sp+96]
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	ld	[%sp+96], %r9
code_819515:
code_819488:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_817385
	nop
zero_case_817384:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	or	%r0, 1, %r11
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	mov	%r9, %r8
	jmpl	%r12, %r15
	ld	[%sp+96], %r9
code_819516:
code_819493:
	! done making normal call
	sethi	%hi(string_814828), %r8
	or	%r8, %lo(string_814828), %r10
	! making closure call
	sethi	%hi(_795444), %r8
	or	%r8, %lo(_795444), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819511:
	mov	%r8, %r12
code_819497:
	! done making normal call
	sethi	%hi(strbindvar_r_close_795447), %r8
	or	%r8, %lo(strbindvar_r_close_795447), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819512:
	mov	%r8, %r12
code_819500:
	! done making normal call
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	sethi	%hi(record_814318), %r8
	or	%r8, %lo(record_814318), %r10
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
	mov	%r18, %r9
code_819513:
	mov	%r8, %r9
code_819506:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_819507:
	! done making tail call
	ba	after_zeroone_817386 ! delay slot empty
	nop
one_case_817385:
	or	%r0, 256, %r8
after_zeroone_817386:
code_819510:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_close_code_812747,(.-PosixPrimIOFn_close_code_812747)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819511
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819512
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819513
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819515
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000d0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819516
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_avail_code_812756
 ! arguments : [$812758,$8] [$812759,$9] 
 ! results    : [$817363,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_avail_code_812756:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819546
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819546:
	st	%r15, [%sp+92]
code_819518:
funtop_817287:
	ld	[%r9], %r12
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	ld	[%r9+8], %r16
	st	%r16, [%sp+96]
	ld	[%r9+12], %r16
	st	%r16, [%sp+104]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_819544:
code_819522:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_817310
	nop
zero_case_817309:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_817315
	nop
zero_case_817314:
	ba	after_zeroone_817316
	or	%r0, 0, %r8
one_case_817315:
	! making closure call
	sethi	%hi(_802663), %r8
	or	%r8, %lo(_802663), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_819545:
	mov	%r8, %r10
code_819528:
	! done making normal call
	! making closure call
	sethi	%hi(_802807), %r8
	or	%r8, %lo(_802807), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819542:
	mov	%r8, %r10
code_819531:
	! done making normal call
	! int sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	! int sub end
	! making closure call
	sethi	%hi(MINUS_795475), %r8
	or	%r8, %lo(MINUS_795475), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_819543:
	mov	%r8, %r9
code_819534:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819535
	nop
code_819536:
	call	GCFromML ! delay slot empty
	nop
needgc_819535:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
after_zeroone_817316:
	ba	after_zeroone_817311 ! delay slot empty
	nop
one_case_817310:
	sethi	%hi(record_814885), %r8
	or	%r8, %lo(record_814885), %r8
after_zeroone_817311:
code_819541:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_avail_code_812756,(.-PosixPrimIOFn_avail_code_812756)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819542
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_819543
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819535
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_819544
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00350000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819545
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global PosixPrimIOFn_mkReader_code_812641
 ! arguments : [$812643,$8] [$812644,$9] [$802831,$10] [$802832,$11] [$802833,$12] 
 ! results    : [$817279,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_mkReader_code_812641:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819646
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_819646:
	st	%r15, [%sp+92]
	st	%r10, [%sp+148]
	st	%r11, [%sp+96]
	st	%r12, [%sp+144]
code_819547:
funtop_816934:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	or	%r0, 1, %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	mov	%r12, %r8
code_819645:
	st	%r8, [%sp+108]
code_819551:
	! done making normal call
	! making closure call
	sethi	%hi(posFns_789442), %r8
	or	%r8, %lo(posFns_789442), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r12, %r15
	ld	[%sp+148], %r11
code_819635:
code_819553:
	! done making normal call
	ld	[%r8+12], %r16
	st	%r16, [%sp+104]
	ld	[%r8+4], %r16
	st	%r16, [%sp+140]
	ld	[%r8+8], %r16
	st	%r16, [%sp+136]
	ld	[%r8], %r16
	st	%r16, [%sp+132]
	ld	[%r8+16], %r16
	st	%r16, [%sp+128]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	mov	%r10, %r8
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_819636:
	mov	%r8, %r9
code_819557:
	! done making normal call
	add	%r4, 148, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819558
	nop
code_819559:
	call	GCFromML ! delay slot empty
	nop
needgc_819558:
	! allocating 1 closures
	or	%r0, 273, %r11
	sethi	%hi(Posix_STR_c_INT), %r8
	or	%r8, %lo(Posix_STR_c_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	ld	[%r8+12], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_819563
	nop
code_819564:
	or	%r0, 0, %r8
cmpui_819563:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r11
	! allocating 2-record
	st	%r11, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+148], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_readVec_code_812646), %r8
	or	%r8, %lo(PosixPrimIOFn_readVec_code_812646), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+124]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 273, %r8
	sethi	%hi(Posix_STR_c_INT), %r10
	or	%r10, %lo(Posix_STR_c_INT), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10+12], %r10
	ld	[%r10+12], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819568
	nop
code_819569:
	or	%r0, 0, %r10
cmpui_819568:
	sll	%r10, 9, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+148], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_readArr_code_812655), %r8
	or	%r8, %lo(PosixPrimIOFn_readArr_code_812655), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+120]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 793, %r8
	sethi	%hi(Posix_STR_c_INT), %r10
	or	%r10, %lo(Posix_STR_c_INT), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10+12], %r10
	ld	[%r10+12], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819573
	nop
code_819574:
	or	%r0, 0, %r10
cmpui_819573:
	sll	%r10, 10, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 3-record
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+148], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_blockWrap_r_code_812664), %r8
	or	%r8, %lo(PosixPrimIOFn_blockWrap_r_code_812664), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 793, %r8
	sethi	%hi(Posix_STR_c_INT), %r10
	or	%r10, %lo(Posix_STR_c_INT), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10+12], %r10
	ld	[%r10+12], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819578
	nop
code_819579:
	or	%r0, 0, %r10
cmpui_819578:
	sll	%r10, 10, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 3-record
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+148], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_noBlockWrap_r_code_812701), %r8
	or	%r8, %lo(PosixPrimIOFn_noBlockWrap_r_code_812701), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 273, %r8
	sethi	%hi(Posix_STR_c_INT), %r9
	or	%r9, %lo(Posix_STR_c_INT), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+12], %r9
	ld	[%r9+12], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_819583
	nop
code_819584:
	or	%r0, 0, %r9
cmpui_819583:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+148], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_close_code_812747), %r8
	or	%r8, %lo(PosixPrimIOFn_close_code_812747), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+116]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call
	sethi	%hi(isRegFile_802647), %r8
	or	%r8, %lo(isRegFile_802647), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+148], %r10
code_819633:
	mov	%r8, %r9
code_819587:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819588
	nop
code_819589:
	call	GCFromML ! delay slot empty
	nop
needgc_819588:
	! allocating 1 closures
	or	%r0, 1825, %r8
	sethi	%hi(Posix_STR_c_INT), %r10
	or	%r10, %lo(Posix_STR_c_INT), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	ld	[%r10+12], %r10
	ld	[%r10+12], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819593
	nop
code_819594:
	or	%r0, 0, %r10
cmpui_819593:
	sll	%r10, 11, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 4-record
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+148], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_avail_code_812756), %r8
	or	%r8, %lo(PosixPrimIOFn_avail_code_812756), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_795508), %r8
	or	%r8, %lo(type_795508), %r9
	ld	[%r2+804], %r8
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
code_819634:
	mov	%r8, %r9
code_819598:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_819637:
	st	%r8, [%sp+108]
code_819599:
	! done making normal call
	sethi	%hi(type_795526), %r8
	or	%r8, %lo(type_795526), %r9
	ld	[%r2+804], %r8
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
code_819638:
	mov	%r8, %r9
code_819602:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_819639:
	st	%r8, [%sp+104]
code_819603:
	! done making normal call
	sethi	%hi(type_795508), %r8
	or	%r8, %lo(type_795508), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_819640:
	mov	%r8, %r9
code_819606:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_819641:
	st	%r8, [%sp+100]
code_819607:
	! done making normal call
	sethi	%hi(type_795526), %r8
	or	%r8, %lo(type_795526), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_819642:
	mov	%r8, %r9
code_819610:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_819643:
	st	%r8, [%sp+96]
code_819611:
	! done making normal call
	! making closure call
	sethi	%hi(_803242), %r8
	or	%r8, %lo(_803242), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+148], %r10
code_819644:
	mov	%r8, %r9
code_819614:
	! done making normal call
	add	%r4, 72, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819615
	nop
code_819616:
	call	GCFromML ! delay slot empty
	nop
needgc_819615:
	sethi	%hi(type_795623), %r8
	or	%r8, %lo(type_795623), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_817253
	nop
code_819620:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_817254
	nop
code_819621:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_817253
	nop
code_819622:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_817253
	nop
code_819623:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_817253
	nop
dynamic_nobox_817254:
	ba	xinject_sum_dyn_after_817247 ! delay slot empty
	nop
dynamic_box_817253:
	or	%r0, 9, %r8
	sethi	%hi(type_803241), %r10
	or	%r10, %lo(type_803241), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819628
	nop
code_819629:
	or	%r0, 0, %r10
cmpui_819628:
	sll	%r10, 8, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 1-record
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	mov	%r8, %r9
xinject_sum_dyn_after_817247:
	! allocating 15-record
	sethi	%hi(gctag_795686), %r8
	ld	[%r8+%lo(gctag_795686)], %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	ld	[%sp+132], %r17
	st	%r17, [%r4+8]
	ld	[%sp+140], %r17
	st	%r17, [%r4+12]
	ld	[%sp+136], %r17
	st	%r17, [%r4+16]
	st	%r9, [%r4+20]
	ld	[%sp+112], %r17
	st	%r17, [%r4+24]
	ld	[%sp+144], %r17
	st	%r17, [%r4+28]
	ld	[%sp+128], %r17
	st	%r17, [%r4+32]
	ld	[%sp+100], %r17
	st	%r17, [%r4+36]
	sethi	%hi(4096), %r8
	st	%r8, [%r4+40]
	ld	[%sp+96], %r17
	st	%r17, [%r4+44]
	ld	[%sp+108], %r17
	st	%r17, [%r4+48]
	ld	[%sp+104], %r17
	st	%r17, [%r4+52]
	ld	[%sp+116], %r17
	st	%r17, [%r4+56]
	or	%r0, 0, %r8
	st	%r8, [%r4+60]
	add	%r4, 4, %r8
	add	%r4, 64, %r4
	! done allocating 15 record
code_819632:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size PosixPrimIOFn_mkReader_code_812641,(.-PosixPrimIOFn_mkReader_code_812641)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819633
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54550000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819634
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55050000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819635
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00430000
	.word 0x00000d00
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819636
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_819558
	.word 0x00280012
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_819588
	.word 0x00280014
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x54550000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_794597
		! -------- label,sizes,reg
	.long code_819637
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55050000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819638
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819639
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819640
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55510000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819641
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15510000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819642
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15540000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819643
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
	.word 0x00000dff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819644
	.word 0x00280010
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05550000
	.word 0x000001ff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
		! -------- label,sizes,reg
	.long needgc_819615
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x05550000
	.word 0x000001ff
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000000
	.long type_803241
		! -------- label,sizes,reg
	.long code_819645
	.word 0x0028000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
	.word 0x00000d00
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_openRd_code_812771
 ! arguments : [$812773,$8] [$812774,$9] [$789838,$10] 
 ! results    : [$816933,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_openRd_code_812771:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819667
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819667:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+108]
	st	%r10, [%sp+104]
code_819647:
funtop_816882:
	sethi	%hi(string_815175), %r8
	or	%r8, %lo(string_815175), %r10
	! making closure call
	sethi	%hi(_795712), %r8
	or	%r8, %lo(_795712), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819666:
	mov	%r8, %r12
code_819651:
	! done making normal call
	sethi	%hi(strbindvar_r_openf_795715), %r8
	or	%r8, %lo(strbindvar_r_openf_795715), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819662:
	st	%r8, [%sp+100]
code_819654:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(_803299), %r8
	or	%r8, %lo(_803299), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819663:
	mov	%r8, %r12
code_819657:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call
	ld	[%sp+100], %r17
	ld	[%r17], %r13
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+104], %r10
code_819664:
	mov	%r8, %r10
code_819658:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call
	ld	[%sp+108], %r17
	ld	[%r17], %r13
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_819659:
	! done making tail call
code_819661:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_openRd_code_812771,(.-PosixPrimIOFn_openRd_code_812771)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819662
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_819663
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_819664
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_819666
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
	.text
	.align 8
	.global PosixPrimIOFn_putV_code_812785
 ! arguments : [$812787,$8] [$812788,$9] [$803460,$10] [$803461,$11] 
 ! results    : [$816855,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_putV_code_812785:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819701
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819701:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
	st	%r10, [%sp+96]
	st	%r11, [%sp+100]
code_819668:
funtop_816827:
	sethi	%hi(string_815361), %r8
	or	%r8, %lo(string_815361), %r10
	! making closure call
	sethi	%hi(_796095), %r8
	or	%r8, %lo(_796095), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819700:
	mov	%r8, %r12
code_819672:
	! done making normal call
	sethi	%hi(strbindvar_r_writeVec_796098), %r8
	or	%r8, %lo(strbindvar_r_writeVec_796098), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819696:
	mov	%r8, %r9
code_819675:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_819697:
	st	%r8, [%sp+96]
code_819676:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_819698:
	mov	%r8, %r11
code_819679:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_795216), %r8
	or	%r8, %lo(PLUS_795216), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_819699:
	mov	%r8, %r11
code_819682:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_819686
	nop
code_819687:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_819686:
	ld	[%r2+792], %r10
	ld	[%sp+104], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	ld	[%sp+104], %r17
	st	%r11, [%r17]
	ld	[%sp+96], %r16
	st	%r16, [%sp+96]
code_819695:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_putV_code_812785,(.-PosixPrimIOFn_putV_code_812785)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819696
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00170000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819697
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819698
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819699
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_819686
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819700
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00170000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_putA_code_812792
 ! arguments : [$812794,$8] [$812795,$9] [$803510,$10] [$803511,$11] 
 ! results    : [$816800,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_putA_code_812792:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819735
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819735:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
	st	%r10, [%sp+96]
	st	%r11, [%sp+100]
code_819702:
funtop_816772:
	sethi	%hi(string_815378), %r8
	or	%r8, %lo(string_815378), %r10
	! making closure call
	sethi	%hi(_795273), %r8
	or	%r8, %lo(_795273), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819734:
	mov	%r8, %r12
code_819706:
	! done making normal call
	sethi	%hi(strbindvar_r_writeArr_796120), %r8
	or	%r8, %lo(strbindvar_r_writeArr_796120), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819730:
	mov	%r8, %r9
code_819709:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_819731:
	st	%r8, [%sp+96]
code_819710:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_819732:
	mov	%r8, %r11
code_819713:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_795216), %r8
	or	%r8, %lo(PLUS_795216), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_819733:
	mov	%r8, %r11
code_819716:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_819720
	nop
code_819721:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_819720:
	ld	[%r2+792], %r10
	ld	[%sp+104], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	ld	[%sp+104], %r17
	st	%r11, [%r17]
	ld	[%sp+96], %r16
	st	%r16, [%sp+96]
code_819729:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_putA_code_812792,(.-PosixPrimIOFn_putA_code_812792)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819730
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00170000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819731
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819732
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819733
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_819720
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_819734
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00170000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_812809
 ! arguments : [$812811,$8] [$812812,$9] [$790014,$10] 
 ! results    : [$816771,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_812809:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819789
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_819789:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+128]
code_819736:
funtop_816629:
	! Proj_c at label type_803571_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_796139_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%r9], %r12
	ld	[%r9+4], %r16
	st	%r16, [%sp+124]
	ld	[%r9+8], %r16
	st	%r16, [%sp+120]
	ld	[%r9+12], %r16
	st	%r16, [%sp+116]
	ld	[%r9+16], %r16
	st	%r16, [%sp+112]
	ld	[%r9+20], %r16
	st	%r16, [%sp+108]
	ld	[%r9+24], %r16
	st	%r16, [%sp+104]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	mov	%r12, %r9
code_819786:
code_819740:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_816664
	nop
zero_case_816663:
	ba	after_zeroone_816665
	or	%r0, 256, %r8
one_case_816664:
	sethi	%hi(mk_794882), %r8
	or	%r8, %lo(mk_794882), %r9
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
after_zeroone_816665:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	ld	[%sp+124], %r9
code_819779:
	mov	%r8, %r10
code_819749:
	! done making normal call
	! making closure call
	sethi	%hi(PLUSEbool_796056), %r8
	or	%r8, %lo(PLUSEbool_796056), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_819782:
code_819752:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_816696
	nop
zero_case_816695:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	mov	%r9, %r8
	ld	[%sp+124], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_819787:
code_819757:
	! done making normal call
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	ld	[%sp+124], %r9
code_819783:
code_819761:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_816722
	nop
zero_case_816721:
	! making closure call
	sethi	%hi(_802883), %r8
	or	%r8, %lo(_802883), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_819788:
code_819765:
	! done making normal call
	ba	after_zeroone_816723
	st	%r8, [%sp+104]
one_case_816722:
	ld	[%sp+112], %r16
	st	%r16, [%sp+104]
after_zeroone_816723:
	sethi	%hi(string_815310), %r8
	or	%r8, %lo(string_815310), %r10
	! making closure call
	sethi	%hi(_796032), %r8
	or	%r8, %lo(_796032), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819781:
	mov	%r8, %r12
code_819770:
	! done making normal call
	sethi	%hi(strbindvar_r_setfl_795148), %r8
	or	%r8, %lo(strbindvar_r_setfl_795148), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819784:
	mov	%r8, %r9
code_819773:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+116], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_819785:
code_819774:
	! done making normal call
	ba	after_zeroone_816697 ! delay slot empty
	nop
one_case_816696:
	or	%r0, 256, %r8
after_zeroone_816697:
	! making closure call
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	ld	[%sp+128], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 144, %sp
code_819776:
	! done making tail call
code_819778:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size PosixPrimIOFn_anonfun_code_812809,(.-PosixPrimIOFn_anonfun_code_812809)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819779
	.word 0x00240012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f750000
	.word 0x00000003
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000000
	.long type_795155
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_807552
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819781
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c750000
	.word 0x00000003
		! worddata
	.word 0x80000000
	.long type_795155
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819782
	.word 0x00240012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f750000
	.word 0x00000003
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000000
	.long type_795155
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_807552
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819783
	.word 0x00240010
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x3f450000
	.word 0x00000003
		! worddata
	.word 0x80000000
	.long type_795155
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_807552
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819784
	.word 0x0024000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c750000
	.word 0x00000003
		! worddata
	.word 0x80000000
	.long type_795155
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819785
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c450000
	.word 0x00000003
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819786
	.word 0x00240012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f750000
	.word 0x00000003
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000000
	.long type_795155
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_807552
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819787
	.word 0x00240010
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x7f450000
	.word 0x00000003
		! worddata
	.word 0x80000000
	.long type_795155
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_807552
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_819788
	.word 0x0024000c
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x0c450000
	.word 0x00000003
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000002
	.word 0x00000060
	.text
	.align 8
	.global PosixPrimIOFn_write_inner_code_812804
 ! arguments : [$812806,$8] [$812807,$9] [$803569,$10] [$803570,$11] 
 ! results    : [$816628,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_write_inner_code_812804:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819817
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_819817:
	st	%r15, [%sp+92]
	mov	%r10, %r22
	mov	%r11, %r21
code_819790:
funtop_816552:
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819791
	nop
code_819792:
	call	GCFromML ! delay slot empty
	nop
needgc_819791:
	! Proj_c at label type_803571_INT
	ld	[%r8], %r20
	! Proj_c at label type_796139_INT
	ld	[%r8+4], %r19
	ld	[%r9], %r18
	ld	[%r9+4], %r13
	ld	[%r9+8], %r12
	ld	[%r9+12], %r11
	ld	[%r9+16], %r10
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r20, [%r4+4]
	st	%r19, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(9017), %r8
	or	%r8, %lo(9017), %r8
	sethi	%hi(type_807552), %r23
	or	%r23, %lo(type_807552), %r24
	ld	[%r2+804], %r23
	add	%r24, %r23, %r23
	ld	[%r23], %r23
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_819796
	nop
code_819797:
	or	%r0, 0, %r23
cmpui_819796:
	sll	%r23, 10, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	sethi	%hi(Posix_STR_c_INT), %r23
	or	%r23, %lo(Posix_STR_c_INT), %r24
	ld	[%r2+804], %r23
	add	%r24, %r23, %r23
	ld	[%r23], %r23
	ld	[%r23+12], %r23
	ld	[%r23+12], %r23
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_819800
	nop
code_819801:
	or	%r0, 0, %r23
cmpui_819800:
	sll	%r23, 11, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	sethi	%hi(type_795155), %r23
	or	%r23, %lo(type_795155), %r24
	ld	[%r2+804], %r23
	add	%r24, %r23, %r23
	ld	[%r23], %r23
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_819804
	nop
code_819805:
	or	%r0, 0, %r23
cmpui_819804:
	sll	%r23, 12, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	sethi	%hi(type_794597), %r23
	or	%r23, %lo(type_794597), %r24
	ld	[%r2+804], %r23
	add	%r24, %r23, %r23
	ld	[%r23], %r23
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_819808
	nop
code_819809:
	or	%r0, 0, %r23
cmpui_819808:
	sll	%r23, 14, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	! allocating 7-record
	st	%r8, [%r4]
	st	%r18, [%r4+4]
	st	%r13, [%r4+8]
	st	%r12, [%r4+12]
	st	%r11, [%r4+16]
	st	%r10, [%r4+20]
	st	%r22, [%r4+24]
	st	%r21, [%r4+28]
	add	%r4, 4, %r10
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_anonfun_code_812809), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_812809), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
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
	mov	%r19, %r9
	jmpl	%r13, %r15
	mov	%r20, %r10
code_819816:
code_819813:
	! done making normal call
code_819815:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_write_inner_code_812804,(.-PosixPrimIOFn_write_inner_code_812804)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819816
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_819791
	.word 0x00180009
	.word 0x00170000
	.word 0x00400300
	.word 0x00200000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! worddata
	.word 0x80000000
	.long type_794597
	.text
	.align 8
	.global PosixPrimIOFn_write_r_code_812799
 ! arguments : [$812801,$8] [$790007,$9] [$812802,$10] [$790008,$11] 
 ! results    : [$816547,$8] 
 ! destroys   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_write_r_code_812799:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819837
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_819837:
	st	%r15, [%sp+92]
code_819818:
funtop_816495:
	add	%r4, 52, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819819
	nop
code_819820:
	call	GCFromML ! delay slot empty
	nop
needgc_819819:
	ld	[%r10], %r18
	ld	[%r10+4], %r13
	ld	[%r10+8], %r12
	ld	[%r10+12], %r11
	ld	[%r10+16], %r10
	! Proj_c at label 'b_TYV
	ld	[%r9+4], %r19
	! Proj_c at label 'a_TYV
	ld	[%r9], %r9
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r19, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 809, %r8
	sethi	%hi(type_807552), %r19
	or	%r19, %lo(type_807552), %r20
	ld	[%r2+804], %r19
	add	%r20, %r19, %r19
	ld	[%r19], %r19
	cmp	%r19, 3
	or	%r0, 1, %r19
	bgu	cmpui_819824
	nop
code_819825:
	or	%r0, 0, %r19
cmpui_819824:
	sll	%r19, 10, %r19
	add	%r19, %r0, %r19
	or	%r19, %r8, %r8
	sethi	%hi(Posix_STR_c_INT), %r19
	or	%r19, %lo(Posix_STR_c_INT), %r20
	ld	[%r2+804], %r19
	add	%r20, %r19, %r19
	ld	[%r19], %r19
	ld	[%r19+12], %r19
	ld	[%r19+12], %r19
	cmp	%r19, 3
	or	%r0, 1, %r19
	bgu	cmpui_819828
	nop
code_819829:
	or	%r0, 0, %r19
cmpui_819828:
	sll	%r19, 11, %r19
	add	%r19, %r0, %r19
	or	%r19, %r8, %r8
	sethi	%hi(type_795155), %r19
	or	%r19, %lo(type_795155), %r20
	ld	[%r2+804], %r19
	add	%r20, %r19, %r19
	ld	[%r19], %r19
	cmp	%r19, 3
	or	%r0, 1, %r19
	bgu	cmpui_819832
	nop
code_819833:
	or	%r0, 0, %r19
cmpui_819832:
	sll	%r19, 12, %r19
	add	%r19, %r0, %r19
	or	%r19, %r8, %r8
	! allocating 5-record
	st	%r8, [%r4]
	st	%r18, [%r4+4]
	st	%r13, [%r4+8]
	st	%r12, [%r4+12]
	st	%r11, [%r4+16]
	st	%r10, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_write_inner_code_812804), %r8
	or	%r8, %lo(PosixPrimIOFn_write_inner_code_812804), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_819836:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_write_r_code_812799,(.-PosixPrimIOFn_write_r_code_812799)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_819819
	.word 0x00180007
	.word 0x00170000
	.word 0xbfe00600
	.word 0xbfe00000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_close_code_812844
 ! arguments : [$812846,$8] [$812847,$9] 
 ! results    : [$816492,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_close_code_812844:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819871
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819871:
	st	%r15, [%sp+92]
code_819838:
funtop_816417:
	ld	[%r9], %r16
	st	%r16, [%sp+96]
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	jmpl	%r11, %r15
	ld	[%sp+96], %r9
code_819869:
code_819842:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_816436
	nop
zero_case_816435:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 0, %r10
	or	%r0, 1, %r11
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	mov	%r9, %r8
	jmpl	%r12, %r15
	ld	[%sp+96], %r9
code_819870:
code_819847:
	! done making normal call
	sethi	%hi(string_814828), %r8
	or	%r8, %lo(string_814828), %r10
	! making closure call
	sethi	%hi(_795444), %r8
	or	%r8, %lo(_795444), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819865:
	mov	%r8, %r12
code_819851:
	! done making normal call
	sethi	%hi(strbindvar_r_close_795447), %r8
	or	%r8, %lo(strbindvar_r_close_795447), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819866:
	mov	%r8, %r12
code_819854:
	! done making normal call
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	sethi	%hi(record_814318), %r8
	or	%r8, %lo(record_814318), %r10
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
	mov	%r18, %r9
code_819867:
	mov	%r8, %r9
code_819860:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_819861:
	! done making tail call
	ba	after_zeroone_816437 ! delay slot empty
	nop
one_case_816436:
	or	%r0, 256, %r8
after_zeroone_816437:
code_819864:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_close_code_812844,(.-PosixPrimIOFn_close_code_812844)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819865
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819866
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819867
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819869
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000d0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819870
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_mkWriter_code_812780
 ! arguments : [$812782,$8] [$812783,$9] [$803318,$10] [$803319,$11] [$803320,$12] [$803321,$13] [$803322,$18] 
 ! results    : [$816410,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_mkWriter_code_812780:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819974
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_819974:
	st	%r15, [%sp+92]
	st	%r10, [%sp+148]
	st	%r11, [%sp+96]
	st	%r12, [%sp+100]
	st	%r13, [%sp+144]
	st	%r18, [%sp+140]
code_819872:
funtop_816071:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	or	%r0, 1, %r9
	or	%r0, 0, %r10
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	jmpl	%r11, %r15
	mov	%r12, %r8
code_819973:
	st	%r8, [%sp+108]
code_819876:
	! done making normal call
	! making closure call
	sethi	%hi(posFns_789442), %r8
	or	%r8, %lo(posFns_789442), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r12, %r15
	ld	[%sp+148], %r11
code_819963:
code_819878:
	! done making normal call
	ld	[%r8+12], %r16
	st	%r16, [%sp+104]
	ld	[%r8+4], %r16
	st	%r16, [%sp+136]
	ld	[%r8+8], %r16
	st	%r16, [%sp+132]
	ld	[%r8], %r16
	st	%r16, [%sp+128]
	ld	[%r8+16], %r16
	st	%r16, [%sp+124]
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	or	%r0, 1, %r9
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	mov	%r10, %r8
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_819964:
	st	%r8, [%sp+96]
code_819882:
	! done making normal call
	ld	[%sp+100], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_816119
	nop
zero_case_816118:
	or	%r0, 0, %r8
	ba	after_zeroone_816120
	mov	%r8, %r10
one_case_816119:
	sethi	%hi(_795932), %r8
	or	%r8, %lo(_795932), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r10
after_zeroone_816120:
	! making closure call
	sethi	%hi(_802883), %r8
	or	%r8, %lo(_802883), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819961:
	mov	%r8, %r10
code_819889:
	! done making normal call
	add	%r4, 124, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819890
	nop
code_819891:
	call	GCFromML ! delay slot empty
	nop
needgc_819890:
	! allocating 2-record
	sethi	%hi(gctag_795916), %r8
	ld	[%r8+%lo(gctag_795916)], %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	sethi	%hi(gctag_795916), %r9
	ld	[%r9+%lo(gctag_795916)], %r9
	st	%r9, [%r4]
	sethi	%hi(strbindvar_c_789413), %r9
	or	%r9, %lo(strbindvar_c_789413), %r11
	ld	[%r2+804], %r9
	add	%r11, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_819897
	nop
code_819898:
	or	%r0, 0, %r9
cmpui_819897:
	cmp	%r9, 0
	be,pn	%icc,else_case_816165
	nop
code_819899:
	sethi	%hi(nonblock_795192), %r9
	or	%r9, %lo(nonblock_795192), %r11
	ld	[%r2+804], %r9
	add	%r11, %r9, %r9
	ba	after_ite_816166
	ld	[%r9], %r9
else_case_816165:
	sethi	%hi(nonblock_795192), %r9
	ld	[%r9+%lo(nonblock_795192)], %r9
after_ite_816166:
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_putV_code_812785), %r8
	or	%r8, %lo(PosixPrimIOFn_putV_code_812785), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+120]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_putA_code_812792), %r8
	or	%r8, %lo(PosixPrimIOFn_putA_code_812792), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+116]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 809, %r8
	sethi	%hi(type_807552), %r11
	or	%r11, %lo(type_807552), %r12
	ld	[%r2+804], %r11
	add	%r12, %r11, %r11
	ld	[%r11], %r11
	cmp	%r11, 3
	or	%r0, 1, %r11
	bgu	cmpui_819908
	nop
code_819909:
	or	%r0, 0, %r11
cmpui_819908:
	sll	%r11, 10, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r8
	sethi	%hi(Posix_STR_c_INT), %r11
	or	%r11, %lo(Posix_STR_c_INT), %r12
	ld	[%r2+804], %r11
	add	%r12, %r11, %r11
	ld	[%r11], %r11
	ld	[%r11+12], %r11
	ld	[%r11+12], %r11
	cmp	%r11, 3
	or	%r0, 1, %r11
	bgu	cmpui_819912
	nop
code_819913:
	or	%r0, 0, %r11
cmpui_819912:
	sll	%r11, 11, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r8
	sethi	%hi(type_795155), %r11
	or	%r11, %lo(type_795155), %r12
	ld	[%r2+804], %r11
	add	%r12, %r11, %r11
	ld	[%r11], %r11
	cmp	%r11, 3
	or	%r0, 1, %r11
	bgu	cmpui_819916
	nop
code_819917:
	or	%r0, 0, %r11
cmpui_819916:
	sll	%r11, 12, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r8
	! allocating 5-record
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+148], %r17
	st	%r17, [%r4+16]
	st	%r10, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_write_r_code_812799), %r8
	or	%r8, %lo(PosixPrimIOFn_write_r_code_812799), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	or	%r0, 273, %r8
	sethi	%hi(Posix_STR_c_INT), %r9
	or	%r9, %lo(Posix_STR_c_INT), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+12], %r9
	ld	[%r9+12], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_819921
	nop
code_819922:
	or	%r0, 0, %r9
cmpui_819921:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+148], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_close_code_812844), %r8
	or	%r8, %lo(PosixPrimIOFn_close_code_812844), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_796278), %r8
	or	%r8, %lo(type_796278), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_819962:
	st	%r8, [%sp+100]
code_819926:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_819965:
	st	%r8, [%sp+108]
code_819927:
	! done making normal call
	sethi	%hi(type_796322), %r8
	or	%r8, %lo(type_796322), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	or	%r0, 256, %r11
	! making closure call
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	jmpl	%r12, %r15
	ld	[%r17+8], %r10
code_819966:
	st	%r8, [%sp+96]
code_819930:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_819967:
	st	%r8, [%sp+104]
code_819931:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_819968:
	mov	%r8, %r10
code_819932:
	! done making normal call
	! making closure call
	sethi	%hi(_796369), %r8
	or	%r8, %lo(_796369), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819969:
	st	%r8, [%sp+100]
code_819935:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_819970:
	mov	%r8, %r10
code_819936:
	! done making normal call
	! making closure call
	sethi	%hi(_796427), %r8
	or	%r8, %lo(_796427), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819971:
	st	%r8, [%sp+96]
code_819939:
	! done making normal call
	! making closure call
	sethi	%hi(_803242), %r8
	or	%r8, %lo(_803242), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+148], %r10
code_819972:
	mov	%r8, %r9
code_819942:
	! done making normal call
	add	%r4, 68, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_819943
	nop
code_819944:
	call	GCFromML ! delay slot empty
	nop
needgc_819943:
	sethi	%hi(type_795623), %r8
	or	%r8, %lo(type_795623), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_816385
	nop
code_819948:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_816386
	nop
code_819949:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_816385
	nop
code_819950:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_816385
	nop
code_819951:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_816385
	nop
dynamic_nobox_816386:
	ba	xinject_sum_dyn_after_816379 ! delay slot empty
	nop
dynamic_box_816385:
	or	%r0, 9, %r8
	sethi	%hi(type_803241), %r10
	or	%r10, %lo(type_803241), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_819956
	nop
code_819957:
	or	%r0, 0, %r10
cmpui_819956:
	sll	%r10, 8, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 1-record
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
	mov	%r8, %r9
xinject_sum_dyn_after_816379:
	! allocating 14-record
	sethi	%hi(gctag_796576), %r8
	ld	[%r8+%lo(gctag_796576)], %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	ld	[%sp+128], %r17
	st	%r17, [%r4+8]
	ld	[%sp+136], %r17
	st	%r17, [%r4+12]
	ld	[%sp+132], %r17
	st	%r17, [%r4+16]
	st	%r9, [%r4+20]
	ld	[%sp+144], %r17
	st	%r17, [%r4+24]
	ld	[%sp+124], %r17
	st	%r17, [%r4+28]
	ld	[%sp+108], %r17
	st	%r17, [%r4+32]
	ld	[%sp+140], %r17
	st	%r17, [%r4+36]
	ld	[%sp+104], %r17
	st	%r17, [%r4+40]
	ld	[%sp+100], %r17
	st	%r17, [%r4+44]
	ld	[%sp+96], %r17
	st	%r17, [%r4+48]
	ld	[%sp+112], %r17
	st	%r17, [%r4+52]
	or	%r0, 0, %r8
	st	%r8, [%r4+56]
	add	%r4, 4, %r8
	add	%r4, 60, %r4
	! done allocating 14 record
code_819960:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size PosixPrimIOFn_mkWriter_code_812780,(.-PosixPrimIOFn_mkWriter_code_812780)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819961
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0510000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819962
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5010000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819963
	.word 0x0028000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x004f0000
	.word 0x00000d00
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000000
	.long type_794597
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819964
	.word 0x00280014
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc05c0000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_819890
	.word 0x00280014
	.word 0x00170000
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0xc0510000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_795155
		! -------- label,sizes,reg
	.long code_819965
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5050000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819966
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5440000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819967
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5450000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819968
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc5510000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819969
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc5510000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819970
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc1540000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819971
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc1540000
	.word 0x00000d3f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_819972
	.word 0x00280010
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc1550000
	.word 0x0000013f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
		! -------- label,sizes,reg
	.long needgc_819943
	.word 0x00280012
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0xc1550000
	.word 0x0000013f
		! worddata
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807749
	.word 0x80000000
	.long reify_807751
	.word 0x80000000
	.long reify_807753
	.word 0x80000000
	.long type_803241
		! -------- label,sizes,reg
	.long code_819973
	.word 0x0028000e
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000f0000
	.word 0x00000d00
		! worddata
	.word 0x80000000
	.long type_794597
	.word 0x80000000
	.long type_794597
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_createFile_code_812855
 ! arguments : [$812857,$8] [$812858,$9] [$803896,$10] [$803897,$11] [$803898,$12] 
 ! results    : [$816070,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_createFile_code_812855:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_819989
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_819989:
	st	%r15, [%sp+92]
	st	%r9, [%sp+96]
	st	%r10, [%sp+100]
	st	%r11, [%sp+104]
	st	%r12, [%sp+108]
code_819975:
funtop_816040:
	sethi	%hi(string_815779), %r8
	or	%r8, %lo(string_815779), %r10
	! making closure call
	sethi	%hi(_796807), %r8
	or	%r8, %lo(_796807), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_819988:
	mov	%r8, %r12
code_819979:
	! done making normal call
	sethi	%hi(strbindvar_r_createf_796810), %r8
	or	%r8, %lo(strbindvar_r_createf_796810), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_819986:
	mov	%r8, %r9
code_819982:
	! done making normal call
	! making closure call
	ld	[%r9], %r18
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+104], %r11
	ld	[%sp+108], %r12
	ld	[%sp+96], %r13
	ld	[%sp+92], %r15
	jmpl	%r18, %r0
	add	%sp, 112, %sp
code_819983:
	! done making tail call
code_819985:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_createFile_code_812855,(.-PosixPrimIOFn_createFile_code_812855)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_819986
	.word 0x001c000d
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00f70000
		! worddata
	.word 0x80000000
	.long type_803888
	.word 0x80000007
	.long strbindvar_c_789413
	.word 0x80000006
	.long strbindvar_c_789413
		! -------- label,sizes,reg
	.long code_819988
	.word 0x001c000d
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00f70000
		! worddata
	.word 0x80000000
	.long type_803888
	.word 0x80000007
	.long strbindvar_c_789413
	.word 0x80000006
	.long strbindvar_c_789413
	.text
	.align 8
	.global PosixPrimIOFn_openWr_code_812862
 ! arguments : [$812864,$8] [$812865,$9] [$790232,$10] 
 ! results    : [$816039,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_openWr_code_812862:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_820006
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_820006:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+100]
code_819990:
funtop_815999:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r10
	or	%r0, 2, %r11
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_819993
	nop
code_819994:
	or	%r0, 0, %r8
cmpui_819993:
	cmp	%r8, 0
	be,pn	%icc,else_case_816020
	nop
code_819995:
	sethi	%hi(trunc_796840), %r8
	or	%r8, %lo(trunc_796840), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ba	after_ite_816021
	ld	[%r8], %r12
else_case_816020:
	sethi	%hi(trunc_796840), %r8
	ld	[%r8+%lo(trunc_796840)], %r12
after_ite_816021:
	! making closure call
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_820004:
	mov	%r8, %r10
code_820000:
	! done making normal call
	or	%r0, 1, %r11
	or	%r0, 0, %r12
	sethi	%hi(4096), %r18
	! making closure call
	ld	[%sp+104], %r17
	ld	[%r17], %r19
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r13
	ld	[%sp+92], %r15
	jmpl	%r19, %r0
	add	%sp, 112, %sp
code_820001:
	! done making tail call
code_820003:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_openWr_code_812862,(.-PosixPrimIOFn_openWr_code_812862)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_820004
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
	.align 8
	.global PosixPrimIOFn_openApp_code_812873
 ! arguments : [$812875,$8] [$812876,$9] [$790256,$10] 
 ! results    : [$815998,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_openApp_code_812873:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_820023
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_820023:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+100]
code_820007:
funtop_815958:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r10
	or	%r0, 2, %r11
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_820010
	nop
code_820011:
	or	%r0, 0, %r8
cmpui_820010:
	cmp	%r8, 0
	be,pn	%icc,else_case_815979
	nop
code_820012:
	sethi	%hi(append_796882), %r8
	or	%r8, %lo(append_796882), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ba	after_ite_815980
	ld	[%r8], %r12
else_case_815979:
	sethi	%hi(append_796882), %r8
	ld	[%r8+%lo(append_796882)], %r12
after_ite_815980:
	! making closure call
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_820021:
	mov	%r8, %r10
code_820017:
	! done making normal call
	or	%r0, 1, %r11
	or	%r0, 1, %r12
	sethi	%hi(4096), %r18
	! making closure call
	ld	[%sp+104], %r17
	ld	[%r17], %r19
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r13
	ld	[%sp+92], %r15
	jmpl	%r19, %r0
	add	%sp, 112, %sp
code_820018:
	! done making tail call
code_820020:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_openApp_code_812873,(.-PosixPrimIOFn_openApp_code_812873)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_820021
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
	.align 8
	.global PosixPrimIOFn_functor_var_r_code_812636
 ! arguments : [$812638,$8] [$794676,$9] [$812639,$10] [$788943,$11] 
 ! results    : [$815935,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_functor_var_r_code_812636:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_820067
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_820067:
	st	%r15, [%sp+92]
	st	%r9, [%sp+116]
code_820024:
funtop_815820:
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820025
	nop
code_820026:
	call	GCFromML ! delay slot empty
	nop
needgc_820025:
	ld	[%r11], %r16
	st	%r16, [%sp+112]
	ld	[%sp+112], %r17
	ld	[%r17+8], %r8
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_mkReader_code_812641), %r8
	or	%r8, %lo(PosixPrimIOFn_mkReader_code_812641), %r8
	st	%r8, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+108]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_openRd_code_812771), %r8
	or	%r8, %lo(PosixPrimIOFn_openRd_code_812771), %r8
	st	%r8, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+104]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+112], %r17
	ld	[%r17+16], %r8
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_mkWriter_code_812780), %r8
	or	%r8, %lo(PosixPrimIOFn_mkWriter_code_812780), %r8
	st	%r8, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(31424393), %r16
	st	%r16, [%sp+96]
	ld	[%sp+96], %r17
	or	%r17, %lo(31424393), %r16
	st	%r16, [%sp+96]
	sethi	%hi(type_803888), %r8
	or	%r8, %lo(type_803888), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_820033
	nop
code_820034:
	or	%r0, 0, %r8
cmpui_820033:
	sll	%r8, 21, %r8
	add	%r8, %r0, %r8
	ld	[%sp+96], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+96]
	sethi	%hi(_796775), %r8
	or	%r8, %lo(_796775), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(_803893), %r8
	or	%r8, %lo(_803893), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_820066:
	mov	%r8, %r11
code_820039:
	! done making normal call
	add	%r4, 144, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820040
	nop
code_820041:
	call	GCFromML ! delay slot empty
	nop
needgc_820040:
	! allocating 1 closures
	or	%r0, 537, %r10
	sethi	%hi(type_803888), %r8
	or	%r8, %lo(type_803888), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_820045
	nop
code_820046:
	or	%r0, 0, %r8
cmpui_820045:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 3-record
	st	%r10, [%r4]
	sethi	%hi(PosixPrimIOFn_createFile_code_812855), %r8
	or	%r8, %lo(PosixPrimIOFn_createFile_code_812855), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_openWr_code_812862), %r8
	or	%r8, %lo(PosixPrimIOFn_openWr_code_812862), %r8
	st	%r8, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r12
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_openApp_code_812873), %r8
	or	%r8, %lo(PosixPrimIOFn_openApp_code_812873), %r8
	st	%r8, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 17-record
	ld	[%sp+96], %r17
	st	%r17, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	sethi	%hi(Word8Vector_STR_r_INT), %r12
	or	%r12, %lo(Word8Vector_STR_r_INT), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+8]
	sethi	%hi(FileSys_794726), %r12
	or	%r12, %lo(FileSys_794726), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+12]
	sethi	%hi(IO_794727), %r12
	or	%r12, %lo(IO_794727), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+16]
	sethi	%hi(strbindvar_r_PLUSEfile_desc_794728), %r12
	or	%r12, %lo(strbindvar_r_PLUSEfile_desc_794728), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+20]
	sethi	%hi(fromInt_794729), %r12
	or	%r12, %lo(fromInt_794729), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+24]
	sethi	%hi(announce_r_789434), %r12
	or	%r12, %lo(announce_r_789434), %r12
	st	%r12, [%r4+28]
	sethi	%hi(4096), %r12
	st	%r12, [%r4+32]
	sethi	%hi(isRegFile_789436), %r12
	or	%r12, %lo(isRegFile_789436), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+36]
	sethi	%hi(posFns_789442), %r12
	or	%r12, %lo(posFns_789442), %r12
	st	%r12, [%r4+40]
	ld	[%sp+108], %r17
	st	%r17, [%r4+44]
	ld	[%sp+104], %r17
	st	%r17, [%r4+48]
	ld	[%sp+100], %r17
	st	%r17, [%r4+52]
	st	%r11, [%r4+56]
	st	%r10, [%r4+60]
	st	%r9, [%r4+64]
	st	%r8, [%r4+68]
	add	%r4, 4, %r8
	add	%r4, 72, %r4
	! done allocating 17 record
code_820065:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_functor_var_r_code_812636,(.-PosixPrimIOFn_functor_var_r_code_812636)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_820066
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! -------- label,sizes,reg
	.long needgc_820025
	.word 0x00200007
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
		! -------- label,sizes,reg
	.long needgc_820040
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000800
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! worddata
	.word 0x80000000
	.long type_803888
	.text
	.align 8
	.global PosixPrimIOFn_main
 ! arguments : 
 ! results    : [$815819,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_821869
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_821869:
	st	%r15, [%sp+92]
code_820068:
funtop_814005:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_821859:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_820069:
	sethi	%hi(PLUSO_bool_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label bool_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_794597+-4), %r8
	st	%r9, [%r8+%lo(type_794597+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 24, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_820076
	nop
code_820077:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820076:
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r12
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
	sethi	%hi(PreOS_STR_c_INT), %r8
	or	%r8, %lo(PreOS_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label IO_STR
	ld	[%r8+12], %r8
	! Proj_c at label +O_iodesc_INT
	ld	[%r8], %r16
	st	%r16, [%sp+112]
	sethi	%hi(PLUSO_option_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label option_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_794600+-4), %r8
	st	%r9, [%r8+%lo(type_794600+-4)]
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r12
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
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	or	%r0, 2, %r9
	ld	[%r8], %r10
	jmpl	%r10, %r15
	ld	[%r8+4], %r8
code_821820:
	mov	%r8, %r13
code_820104:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(type_794601+-4), %r8
	st	%r9, [%r8+%lo(type_794601+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 120, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_820109
	nop
code_820110:
	sub	%r4, 120, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820109:
	sethi	%hi(type_794601), %r8
	or	%r8, %lo(type_794601), %r12
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
	sethi	%hi(Posix_STR_c_INT), %r8
	or	%r8, %lo(Posix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label FileSys_STR
	ld	[%r8+16], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_c_789413+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_c_789413+-4)]
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r12
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
	sethi	%hi(Posix_STR_c_INT), %r8
	or	%r8, %lo(Posix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label IO_STR
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_c_789415+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_c_789415+-4)]
	sethi	%hi(strbindvar_c_789415), %r8
	or	%r8, %lo(strbindvar_c_789415), %r12
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
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label file_desc_TYC
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(file_desc_789417+-4), %r8
	st	%r9, [%r8+%lo(file_desc_789417+-4)]
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r12
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
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(Posix_STR_r_INT), %r8
	or	%r8, %lo(Posix_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r13
	or	%r0, 111, %r9
	sethi	%hi(FileSys_794726+-4), %r8
	st	%r9, [%r8+%lo(FileSys_794726+-4)]
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r12
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
	sethi	%hi(Posix_STR_r_INT), %r8
	or	%r8, %lo(Posix_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(IO_794727+-4), %r8
	st	%r9, [%r8+%lo(IO_794727+-4)]
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r12
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
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_PLUSEfile_desc_794728+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_PLUSEfile_desc_794728+-4)]
	sethi	%hi(strbindvar_r_PLUSEfile_desc_794728), %r8
	or	%r8, %lo(strbindvar_r_PLUSEfile_desc_794728), %r12
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
	sethi	%hi(Position_STR_r_INT), %r8
	or	%r8, %lo(Position_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r13
	or	%r0, 111, %r9
	sethi	%hi(fromInt_794729+-4), %r8
	st	%r9, [%r8+%lo(fromInt_794729+-4)]
	sethi	%hi(fromInt_794729), %r8
	or	%r8, %lo(fromInt_794729), %r12
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
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r8
	! done allocating 1 closures
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+68], %r16
	st	%r16, [%sp+100]
	ld	[%sp+100], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+96]
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+80], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_fstat_794758+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_fstat_794758+-4)]
	sethi	%hi(strbindvar_r_fstat_794758), %r8
	or	%r8, %lo(strbindvar_r_fstat_794758), %r12
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
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label ST_STR
	ld	[%r8+44], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_802662+-4), %r8
	st	%r9, [%r8+%lo(type_802662+-4)]
	sethi	%hi(type_802662), %r8
	or	%r8, %lo(type_802662), %r12
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
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_802662), %r8
	or	%r8, %lo(type_802662), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	sethi	%hi(strbindvar_r_fstat_794758), %r8
	or	%r8, %lo(strbindvar_r_fstat_794758), %r11
	ld	[%r2+804], %r8
	add	%r11, %r8, %r8
	ld	[%r8], %r12
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r11
	ld	[%r2+804], %r8
	add	%r11, %r8, %r8
	ld	[%r8], %r11
	ld	[%r11], %r13
	ld	[%r11+4], %r8
	jmpl	%r13, %r15
	ld	[%r11+8], %r11
code_821821:
code_820240:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_802663+-4), %r9
	st	%r10, [%r9+%lo(_802663+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820245
	nop
code_820246:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820245:
	sethi	%hi(_802663), %r9
	or	%r9, %lo(_802663), %r13
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
	st	%r8, [%r9]
	sethi	%hi(type_802662), %r8
	or	%r8, %lo(type_802662), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
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
	mov	%r12, %r9
	jmpl	%r13, %r15
	ld	[%sp+96], %r12
code_821822:
code_820263:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_802675+-4), %r9
	st	%r10, [%r9+%lo(_802675+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820268
	nop
code_820269:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820268:
	sethi	%hi(_802675), %r9
	or	%r9, %lo(_802675), %r13
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
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(isRegFile_802647), %r8
	or	%r8, %lo(isRegFile_802647), %r8
	! done allocating 1 closures
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label file_desc_TYC
	ld	[%r8+8], %r18
	sethi	%hi(type_794597), %r8
	or	%r8, %lo(type_794597), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	sethi	%hi(isRegFile_802647), %r8
	or	%r8, %lo(isRegFile_802647), %r12
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
	mov	%r18, %r9
code_821823:
code_820288:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(isRegFile_789436+-4), %r9
	st	%r10, [%r9+%lo(isRegFile_789436+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820293
	nop
code_820294:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820293:
	sethi	%hi(isRegFile_789436), %r9
	or	%r9, %lo(isRegFile_789436), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814220), %r8
	or	%r8, %lo(record_814220), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821824:
code_820308:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_794788+-4), %r9
	st	%r10, [%r9+%lo(type_794788+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820313
	nop
code_820314:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820313:
	sethi	%hi(type_794788), %r9
	or	%r9, %lo(type_794788), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814235), %r8
	or	%r8, %lo(record_814235), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821825:
code_820328:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_794792+-4), %r9
	st	%r10, [%r9+%lo(type_794792+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820333
	nop
code_820334:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820333:
	sethi	%hi(type_794792), %r9
	or	%r9, %lo(type_794792), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814249), %r8
	or	%r8, %lo(record_814249), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821826:
code_820348:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_794796+-4), %r9
	st	%r10, [%r9+%lo(type_794796+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 48, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820353
	nop
code_820354:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820353:
	sethi	%hi(type_794796), %r9
	or	%r9, %lo(type_794796), %r13
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
	st	%r8, [%r9]
	sethi	%hi(PLUSO_bool_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r9
	sethi	%hi(PLUSNbool_out_794804), %r8
	st	%r9, [%r8+%lo(PLUSNbool_out_794804)]
	sethi	%hi(PLUSO_option_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r9
	sethi	%hi(PLUSNoption_in_794810), %r8
	st	%r9, [%r8+%lo(PLUSNoption_in_794810)]
	sethi	%hi(PLUSO_option_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label option_sum_INT
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_794813+-4), %r8
	st	%r9, [%r8+%lo(type_794813+-4)]
	sethi	%hi(type_794813), %r8
	or	%r8, %lo(type_794813), %r12
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
	or	%r0, 2089, %r8
	sethi	%hi(type_794788), %r9
	or	%r9, %lo(type_794788), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_820385
	nop
code_820386:
	or	%r0, 0, %r9
cmpui_820385:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794792), %r9
	or	%r9, %lo(type_794792), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_820389
	nop
code_820390:
	or	%r0, 0, %r9
cmpui_820389:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794796), %r9
	or	%r9, %lo(type_794796), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_820393
	nop
code_820394:
	or	%r0, 0, %r9
cmpui_820393:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794788), %r9
	or	%r9, %lo(type_794788), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_820397
	nop
code_820398:
	or	%r0, 0, %r9
cmpui_820397:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_794865), %r9
	st	%r8, [%r9+%lo(gctag_794865)]
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(IO_STR_r_INT), %r8
	or	%r8, %lo(IO_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_794882+-4), %r8
	st	%r9, [%r8+%lo(mk_794882+-4)]
	sethi	%hi(mk_794882), %r8
	or	%r8, %lo(mk_794882), %r12
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
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(strbindvar_c_789415), %r8
	or	%r8, %lo(strbindvar_c_789415), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label file_desc_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_794887+-4), %r8
	st	%r9, [%r8+%lo(type_794887+-4)]
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r12
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
	sethi	%hi(strbindvar_c_789415), %r8
	or	%r8, %lo(strbindvar_c_789415), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label whence_TYC
	ld	[%r8+8], %r10
	! allocating 5-record
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820426
	nop
code_820427:
	call	GCFromML ! delay slot empty
	nop
needgc_820426:
	sethi	%hi(7209), %r8
	or	%r8, %lo(7209), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 3, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(Position_STR_c_INT), %r8
	or	%r8, %lo(Position_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	st	%r10, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(Position_STR_c_INT), %r8
	or	%r8, %lo(Position_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821860:
code_820437:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_794896+-4), %r9
	st	%r10, [%r9+%lo(_794896+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820442
	nop
code_820443:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820442:
	sethi	%hi(_794896), %r9
	or	%r9, %lo(_794896), %r13
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
	st	%r8, [%r9]
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_lseek_794899+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_lseek_794899+-4)]
	sethi	%hi(strbindvar_r_lseek_794899), %r8
	or	%r8, %lo(strbindvar_r_lseek_794899), %r12
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
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+40], %r8
	ld	[%sp+100], %r17
	ld	[%r17+52], %r16
	st	%r16, [%sp+96]
	! allocating 3-record
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820468
	nop
code_820469:
	call	GCFromML ! delay slot empty
	nop
needgc_820468:
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_802662), %r8
	or	%r8, %lo(type_802662), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821861:
code_820477:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_794951+-4), %r9
	st	%r10, [%r9+%lo(_794951+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820482
	nop
code_820483:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820482:
	sethi	%hi(_794951), %r9
	or	%r9, %lo(_794951), %r13
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
	st	%r8, [%r9]
	sethi	%hi(type_802662), %r8
	or	%r8, %lo(type_802662), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	sethi	%hi(Position_STR_c_INT), %r8
	or	%r8, %lo(Position_STR_c_INT), %r9
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
	mov	%r12, %r9
	jmpl	%r13, %r15
	ld	[%sp+96], %r12
code_821827:
	mov	%r8, %r13
code_820500:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_802807+-4), %r8
	st	%r9, [%r8+%lo(_802807+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_820505
	nop
code_820506:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820505:
	sethi	%hi(_802807), %r8
	or	%r8, %lo(_802807), %r12
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
	sethi	%hi(Position_STR_c_INT), %r8
	or	%r8, %lo(Position_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r13
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(ignore_r_INT), %r8
	or	%r8, %lo(ignore_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821828:
	mov	%r8, %r13
code_820521:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_794995+-4), %r8
	st	%r9, [%r8+%lo(_794995+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_820526
	nop
code_820527:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820526:
	sethi	%hi(_794995), %r8
	or	%r8, %lo(_794995), %r12
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
	sethi	%hi(posFns_789442), %r8
	or	%r8, %lo(posFns_789442), %r8
	! done allocating 1 closures
	sethi	%hi(PLUSO_bool_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_814489), %r8
	or	%r8, %lo(record_814489), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821829:
	mov	%r8, %r13
code_820544:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(reify_807753+-4), %r8
	st	%r9, [%r8+%lo(reify_807753+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_820549
	nop
code_820550:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820549:
	sethi	%hi(reify_807753), %r8
	or	%r8, %lo(reify_807753), %r12
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
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_814503), %r8
	or	%r8, %lo(record_814503), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821830:
	mov	%r8, %r13
code_820564:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(reify_807751+-4), %r8
	st	%r9, [%r8+%lo(reify_807751+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_820569
	nop
code_820570:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820569:
	sethi	%hi(reify_807751), %r8
	or	%r8, %lo(reify_807751), %r12
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
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_814517), %r8
	or	%r8, %lo(record_814517), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821831:
	mov	%r8, %r13
code_820584:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(reify_807749+-4), %r8
	st	%r9, [%r8+%lo(reify_807749+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 24, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_820589
	nop
code_820590:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820589:
	sethi	%hi(reify_807749), %r8
	or	%r8, %lo(reify_807749), %r12
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
	sethi	%hi(Position_STR_r_INT), %r8
	or	%r8, %lo(Position_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+56], %r13
	or	%r0, 111, %r9
	sethi	%hi(PLUS_795216+-4), %r8
	st	%r9, [%r8+%lo(PLUS_795216+-4)]
	sethi	%hi(PLUS_795216), %r8
	or	%r8, %lo(PLUS_795216), %r12
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
	! allocating 4-record
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820613
	nop
code_820614:
	call	GCFromML ! delay slot empty
	nop
needgc_820613:
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	or	%r0, 2, %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	sethi	%hi(Word8Vector_STR_c_INT), %r8
	or	%r8, %lo(Word8Vector_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label vector_TYC
	ld	[%r8], %r16
	st	%r16, [%sp+108]
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821862:
code_820622:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_795246+-4), %r9
	st	%r10, [%r9+%lo(_795246+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820627
	nop
code_820628:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820627:
	sethi	%hi(_795246), %r9
	or	%r9, %lo(_795246), %r13
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
	st	%r8, [%r9]
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_readVec_795249+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_readVec_795249+-4)]
	sethi	%hi(strbindvar_r_readVec_795249), %r8
	or	%r8, %lo(strbindvar_r_readVec_795249), %r12
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
	sethi	%hi(Word8Vector_STR_r_INT), %r8
	or	%r8, %lo(Word8Vector_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_length_795257+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_length_795257+-4)]
	sethi	%hi(strbindvar_r_length_795257), %r8
	or	%r8, %lo(strbindvar_r_length_795257), %r12
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
	or	%r0, 1049, %r10
	sethi	%hi(type_794601), %r8
	or	%r8, %lo(type_794601), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_820665
	nop
code_820666:
	or	%r0, 0, %r8
cmpui_820665:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	sethi	%hi(record_gctag_802953), %r8
	st	%r10, [%r8+%lo(record_gctag_802953)]
	sethi	%hi(Word8Array_STR_c_INT), %r8
	or	%r8, %lo(Word8Array_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label array_TYC
	ld	[%r8], %r10
	! allocating 5-record
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820670
	nop
code_820671:
	call	GCFromML ! delay slot empty
	nop
needgc_820670:
	sethi	%hi(7209), %r8
	or	%r8, %lo(7209), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 3, %r8
	st	%r8, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_794601), %r8
	or	%r8, %lo(type_794601), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	st	%r10, [%r4+20]
	add	%r4, 4, %r16
	st	%r16, [%sp+104]
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	ld	[%sp+104], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821863:
code_820679:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_795273+-4), %r9
	st	%r10, [%r9+%lo(_795273+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 48, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820684
	nop
code_820685:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820684:
	sethi	%hi(_795273), %r9
	or	%r9, %lo(_795273), %r13
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
	st	%r8, [%r9]
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+28], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_readArr_795276+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_readArr_795276+-4)]
	sethi	%hi(strbindvar_r_readArr_795276), %r8
	or	%r8, %lo(strbindvar_r_readArr_795276), %r12
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
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_setfl_795148+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_setfl_795148+-4)]
	sethi	%hi(strbindvar_r_setfl_795148), %r8
	or	%r8, %lo(strbindvar_r_setfl_795148), %r12
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
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+56], %r16
	st	%r16, [%sp+100]
	ld	[%sp+100], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+96]
	sethi	%hi(PLUSO_list_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_list_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r9
	sethi	%hi(PLUSNlist_in_795152), %r8
	st	%r9, [%r8+%lo(PLUSNlist_in_795152)]
	sethi	%hi(strbindvar_c_789415), %r8
	or	%r8, %lo(strbindvar_c_789415), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label O_STR
	ld	[%r8+24], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_795155+-4), %r8
	st	%r9, [%r8+%lo(type_795155+-4)]
	sethi	%hi(type_795155), %r8
	or	%r8, %lo(type_795155), %r12
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
	ld	[%r8], %r16
	st	%r16, [%sp+140]
	! start making constructor call
	sethi	%hi(type_795155), %r8
	or	%r8, %lo(type_795155), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_821832:
code_820741:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807552+-4), %r9
	st	%r10, [%r9+%lo(type_807552+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820746
	nop
code_820747:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820746:
	sethi	%hi(type_807552), %r9
	or	%r9, %lo(type_807552), %r13
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
	st	%r8, [%r9]
	sethi	%hi(type_807552), %r8
	or	%r8, %lo(type_807552), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_795155), %r8
	or	%r8, %lo(type_795155), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r12
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	mov	%r12, %r10
	jmpl	%r13, %r15
	ld	[%sp+96], %r12
code_821833:
code_820764:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_802883+-4), %r9
	st	%r10, [%r9+%lo(_802883+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820769
	nop
code_820770:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820769:
	sethi	%hi(_802883), %r9
	or	%r9, %lo(_802883), %r13
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
	st	%r8, [%r9]
	ld	[%sp+100], %r17
	ld	[%r17+28], %r8
	sethi	%hi(strbindvar_c_789413), %r9
	or	%r9, %lo(strbindvar_c_789413), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_820783
	nop
code_820784:
	or	%r0, 0, %r9
cmpui_820783:
	cmp	%r9, 0
	be,pn	%icc,else_case_814744
	nop
code_820785:
	or	%r0, 111, %r10
	sethi	%hi(nonblock_795192+-4), %r9
	st	%r10, [%r9+%lo(nonblock_795192+-4)]
	sethi	%hi(nonblock_795192), %r9
	or	%r9, %lo(nonblock_795192), %r13
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
	ba	after_ite_814745
	st	%r8, [%r9]
else_case_814744:
	or	%r0, 9, %r10
	sethi	%hi(nonblock_795192+-4), %r9
	st	%r10, [%r9+%lo(nonblock_795192+-4)]
	or	%r0, 23, %r10
	sethi	%hi(nonblock_795192+4), %r9
	st	%r10, [%r9+%lo(nonblock_795192+4)]
	sethi	%hi(nonblock_795192), %r9
	st	%r8, [%r9+%lo(nonblock_795192)]
after_ite_814745:
	sethi	%hi(OS_STR_r_INT), %r8
	or	%r8, %lo(OS_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r8
	ld	[%r8], %r9
	sethi	%hi(stamp_795358), %r8
	st	%r9, [%r8+%lo(stamp_795358)]
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(OS_STR_c_INT), %r8
	or	%r8, %lo(OS_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label syserror_TYC
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821854:
code_820807:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(reify_807574+-4), %r9
	st	%r10, [%r9+%lo(reify_807574+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820812
	nop
code_820813:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820812:
	sethi	%hi(reify_807574), %r9
	or	%r9, %lo(reify_807574), %r13
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
	st	%r8, [%r9]
	sethi	%hi(PLUSO_option_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r9
	sethi	%hi(PLUSNoption_out_795361), %r8
	st	%r9, [%r8+%lo(PLUSNoption_out_795361)]
	sethi	%hi(OS_STR_c_INT), %r8
	or	%r8, %lo(OS_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label syserror_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_795362+-4), %r8
	st	%r9, [%r8+%lo(type_795362+-4)]
	sethi	%hi(type_795362), %r8
	or	%r8, %lo(type_795362), %r12
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
	sethi	%hi(OS_STR_r_INT), %r8
	or	%r8, %lo(OS_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(PLUSEsyserror_795370+-4), %r8
	st	%r9, [%r8+%lo(PLUSEsyserror_795370+-4)]
	sethi	%hi(PLUSEsyserror_795370), %r8
	or	%r8, %lo(PLUSEsyserror_795370), %r12
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
	sethi	%hi(Posix_STR_r_INT), %r8
	or	%r8, %lo(Posix_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r9
	sethi	%hi(again_795373), %r8
	st	%r9, [%r8+%lo(again_795373)]
	! allocating 3-record
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820854
	nop
code_820855:
	call	GCFromML ! delay slot empty
	nop
needgc_820854:
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(record_814318), %r8
	or	%r8, %lo(record_814318), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821864:
code_820862:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_795444+-4), %r9
	st	%r10, [%r9+%lo(_795444+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820867
	nop
code_820868:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820867:
	sethi	%hi(_795444), %r9
	or	%r9, %lo(_795444), %r13
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
	st	%r8, [%r9]
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_close_795447+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_close_795447+-4)]
	sethi	%hi(strbindvar_r_close_795447), %r8
	or	%r8, %lo(strbindvar_r_close_795447), %r12
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
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(Int32_STR_c_INT), %r8
	or	%r8, %lo(Int32_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821834:
code_820895:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_795460+-4), %r9
	st	%r10, [%r9+%lo(type_795460+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820900
	nop
code_820901:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820900:
	sethi	%hi(type_795460), %r9
	or	%r9, %lo(type_795460), %r13
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
	st	%r8, [%r9]
	sethi	%hi(Position_STR_r_INT), %r8
	or	%r8, %lo(Position_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+60], %r13
	or	%r0, 111, %r9
	sethi	%hi(MINUS_795475+-4), %r8
	st	%r9, [%r8+%lo(MINUS_795475+-4)]
	sethi	%hi(MINUS_795475), %r8
	or	%r8, %lo(MINUS_795475), %r12
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
	! allocating 1-record
	! done allocating 1 record
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820924
	nop
code_820925:
	call	GCFromML ! delay slot empty
	nop
needgc_820924:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	or	%r0, 2, %r8
	st	%r8, [%r4+4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(type_795508+-4), %r9
	st	%r10, [%r9+%lo(type_795508+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820931
	nop
code_820932:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820931:
	sethi	%hi(type_795508), %r9
	or	%r9, %lo(type_795508), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814896), %r8
	or	%r8, %lo(record_814896), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821835:
code_820946:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807737+-4), %r9
	st	%r10, [%r9+%lo(type_807737+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820951
	nop
code_820952:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820951:
	sethi	%hi(type_807737), %r9
	or	%r9, %lo(type_807737), %r13
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
	st	%r8, [%r9]
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_820963
	nop
code_820964:
	call	GCFromML ! delay slot empty
	nop
needgc_820963:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(type_795526+-4), %r9
	st	%r10, [%r9+%lo(type_795526+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820970
	nop
code_820971:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820970:
	sethi	%hi(type_795526), %r9
	or	%r9, %lo(type_795526), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814919), %r8
	or	%r8, %lo(record_814919), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821836:
code_820985:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807726+-4), %r9
	st	%r10, [%r9+%lo(type_807726+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_820990
	nop
code_820991:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_820990:
	sethi	%hi(type_807726), %r9
	or	%r9, %lo(type_807726), %r13
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
	st	%r8, [%r9]
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+108], %r9
code_821837:
code_821004:
	! done making constructor call
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814941), %r8
	or	%r8, %lo(record_814941), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821856:
code_821008:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807711+-4), %r9
	st	%r10, [%r9+%lo(type_807711+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821013
	nop
code_821014:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821013:
	sethi	%hi(type_807711), %r9
	or	%r9, %lo(type_807711), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814955), %r8
	or	%r8, %lo(record_814955), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821838:
code_821028:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807696+-4), %r9
	st	%r10, [%r9+%lo(type_807696+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821033
	nop
code_821034:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821033:
	sethi	%hi(type_807696), %r9
	or	%r9, %lo(type_807696), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814969), %r8
	or	%r8, %lo(record_814969), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821839:
code_821048:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807693+-4), %r9
	st	%r10, [%r9+%lo(type_807693+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821053
	nop
code_821054:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821053:
	sethi	%hi(type_807693), %r9
	or	%r9, %lo(type_807693), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_814987), %r8
	or	%r8, %lo(record_814987), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821840:
code_821068:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807690+-4), %r9
	st	%r10, [%r9+%lo(type_807690+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821073
	nop
code_821074:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821073:
	sethi	%hi(type_807690), %r9
	or	%r9, %lo(type_807690), %r13
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
	st	%r8, [%r9]
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r12
	! Proj_c at label iodesc_TYC
	ld	[%sp+112], %r17
	ld	[%r17], %r18
	or	%r0, 111, %r9
	sethi	%hi(type_803241+-4), %r8
	st	%r9, [%r8+%lo(type_803241+-4)]
	sethi	%hi(type_803241), %r8
	or	%r8, %lo(type_803241), %r13
	ld	[%r2+804], %r11
	ld	[%r2+792], %r10
	mov	%r13, %r9
	mov	%r11, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r13, %r11, %r8
	ld	[%r8], %r8
	st	%r8, [%r10+8]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	add	%r13, %r11, %r8
	st	%r18, [%r8]
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_803241), %r8
	or	%r8, %lo(type_803241), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r18
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	jmpl	%r13, %r15
	mov	%r18, %r10
code_821841:
code_821103:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_803242+-4), %r9
	st	%r10, [%r9+%lo(_803242+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821108
	nop
code_821109:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821108:
	sethi	%hi(_803242), %r9
	or	%r9, %lo(_803242), %r13
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
	st	%r8, [%r9]
	! start making constructor call
	sethi	%hi(type_794813), %r8
	or	%r8, %lo(type_794813), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(type_803241), %r8
	or	%r8, %lo(type_803241), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821842:
code_821124:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_795623+-4), %r9
	st	%r10, [%r9+%lo(type_795623+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821129
	nop
code_821130:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821129:
	sethi	%hi(type_795623), %r9
	or	%r9, %lo(type_795623), %r13
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
	st	%r8, [%r9]
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(type_803241), %r8
	or	%r8, %lo(type_803241), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821843:
code_821145:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807686+-4), %r9
	st	%r10, [%r9+%lo(type_807686+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821150
	nop
code_821151:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821150:
	sethi	%hi(type_807686), %r9
	or	%r9, %lo(type_807686), %r13
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
	st	%r8, [%r9]
	sethi	%hi(2121849), %r8
	or	%r8, %lo(2121849), %r8
	sethi	%hi(type_807690), %r9
	or	%r9, %lo(type_807690), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821164
	nop
code_821165:
	or	%r0, 0, %r9
cmpui_821164:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794788), %r9
	or	%r9, %lo(type_794788), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821168
	nop
code_821169:
	or	%r0, 0, %r9
cmpui_821168:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794792), %r9
	or	%r9, %lo(type_794792), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821172
	nop
code_821173:
	or	%r0, 0, %r9
cmpui_821172:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794796), %r9
	or	%r9, %lo(type_794796), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821176
	nop
code_821177:
	or	%r0, 0, %r9
cmpui_821176:
	sll	%r9, 11, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807686), %r9
	or	%r9, %lo(type_807686), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821180
	nop
code_821181:
	or	%r0, 0, %r9
cmpui_821180:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794788), %r9
	or	%r9, %lo(type_794788), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821184
	nop
code_821185:
	or	%r0, 0, %r9
cmpui_821184:
	sll	%r9, 15, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807711), %r9
	or	%r9, %lo(type_807711), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821188
	nop
code_821189:
	or	%r0, 0, %r9
cmpui_821188:
	sll	%r9, 16, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807696), %r9
	or	%r9, %lo(type_807696), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821192
	nop
code_821193:
	or	%r0, 0, %r9
cmpui_821192:
	sll	%r9, 18, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807737), %r9
	or	%r9, %lo(type_807737), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821196
	nop
code_821197:
	or	%r0, 0, %r9
cmpui_821196:
	sll	%r9, 19, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807726), %r9
	or	%r9, %lo(type_807726), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821200
	nop
code_821201:
	or	%r0, 0, %r9
cmpui_821200:
	sll	%r9, 20, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807693), %r9
	or	%r9, %lo(type_807693), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821204
	nop
code_821205:
	or	%r0, 0, %r9
cmpui_821204:
	sll	%r9, 22, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_795686), %r9
	st	%r8, [%r9+%lo(gctag_795686)]
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label open_mode_TYC
	ld	[%r8+24], %r16
	st	%r16, [%sp+136]
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label O_STR
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_795704+-4), %r8
	st	%r9, [%r8+%lo(type_795704+-4)]
	sethi	%hi(type_795704), %r8
	or	%r8, %lo(type_795704), %r12
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
	! allocating 5-record
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821221
	nop
code_821222:
	call	GCFromML ! delay slot empty
	nop
needgc_821221:
	sethi	%hi(7209), %r8
	or	%r8, %lo(7209), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 3, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	ld	[%sp+136], %r17
	st	%r17, [%r4+16]
	sethi	%hi(type_795704), %r8
	or	%r8, %lo(type_795704), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821865:
code_821232:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_795712+-4), %r9
	st	%r10, [%r9+%lo(_795712+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821237
	nop
code_821238:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821237:
	sethi	%hi(_795712), %r9
	or	%r9, %lo(_795712), %r13
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
	st	%r8, [%r9]
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+84], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_openf_795715+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_openf_795715+-4)]
	sethi	%hi(strbindvar_r_openf_795715), %r8
	or	%r8, %lo(strbindvar_r_openf_795715), %r12
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
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+60], %r8
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+68], %r16
	st	%r16, [%sp+132]
	ld	[%sp+132], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+96]
	! start making constructor call
	sethi	%hi(type_795704), %r8
	or	%r8, %lo(type_795704), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_821844:
code_821267:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_807803+-4), %r9
	st	%r10, [%r9+%lo(type_807803+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821272
	nop
code_821273:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821272:
	sethi	%hi(type_807803), %r9
	or	%r9, %lo(type_807803), %r13
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
	st	%r8, [%r9]
	sethi	%hi(type_807803), %r8
	or	%r8, %lo(type_807803), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_795704), %r8
	or	%r8, %lo(type_795704), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r12
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r11
	mov	%r12, %r10
	jmpl	%r13, %r15
	ld	[%sp+96], %r12
code_821845:
code_821290:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_803299+-4), %r9
	st	%r10, [%r9+%lo(_803299+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821295
	nop
code_821296:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821295:
	sethi	%hi(_803299), %r9
	or	%r9, %lo(_803299), %r13
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
	st	%r8, [%r9]
	ld	[%sp+100], %r17
	ld	[%r17+24], %r9
	or	%r0, 17, %r8
	sethi	%hi(type_795155), %r10
	or	%r10, %lo(type_795155), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_821309
	nop
code_821310:
	or	%r0, 0, %r10
cmpui_821309:
	sll	%r10, 8, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	sethi	%hi(type_807552), %r10
	or	%r10, %lo(type_807552), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_821313
	nop
code_821314:
	or	%r0, 0, %r10
cmpui_821313:
	sll	%r10, 9, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	sethi	%hi(gctag_795916), %r10
	st	%r8, [%r10+%lo(gctag_795916)]
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821316
	nop
code_821317:
	call	GCFromML ! delay slot empty
	nop
needgc_821316:
	sethi	%hi(gctag_795916), %r8
	ld	[%r8+%lo(gctag_795916)], %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(_795932+-4), %r9
	st	%r10, [%r9+%lo(_795932+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821324
	nop
code_821325:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821324:
	sethi	%hi(_795932), %r9
	or	%r9, %lo(_795932), %r13
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
	st	%r8, [%r9]
	sethi	%hi(PLUSO_bool_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(PLUSEbool_796056+-4), %r8
	st	%r9, [%r8+%lo(PLUSEbool_796056+-4)]
	sethi	%hi(PLUSEbool_796056), %r8
	or	%r8, %lo(PLUSEbool_796056), %r12
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
	! allocating 4-record
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821348
	nop
code_821349:
	call	GCFromML ! delay slot empty
	nop
needgc_821348:
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_795155), %r8
	or	%r8, %lo(type_795155), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(record_814318), %r8
	or	%r8, %lo(record_814318), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821866:
code_821358:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_796032+-4), %r9
	st	%r10, [%r9+%lo(_796032+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821363
	nop
code_821364:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821363:
	sethi	%hi(_796032), %r9
	or	%r9, %lo(_796032), %r13
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
	st	%r8, [%r9]
	! allocating 5-record
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821375
	nop
code_821376:
	call	GCFromML ! delay slot empty
	nop
needgc_821375:
	sethi	%hi(7209), %r8
	or	%r8, %lo(7209), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 3, %r8
	st	%r8, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_794601), %r8
	or	%r8, %lo(type_794601), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	ld	[%sp+108], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_794887), %r8
	or	%r8, %lo(type_794887), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821867:
code_821384:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_796095+-4), %r9
	st	%r10, [%r9+%lo(_796095+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821389
	nop
code_821390:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821389:
	sethi	%hi(_796095), %r9
	or	%r9, %lo(_796095), %r13
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
	st	%r8, [%r9]
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_writeVec_796098+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_writeVec_796098+-4)]
	sethi	%hi(strbindvar_r_writeVec_796098), %r8
	or	%r8, %lo(strbindvar_r_writeVec_796098), %r12
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
	sethi	%hi(IO_794727), %r8
	or	%r8, %lo(IO_794727), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+36], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_writeArr_796120+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_writeArr_796120+-4)]
	sethi	%hi(strbindvar_r_writeArr_796120), %r8
	or	%r8, %lo(strbindvar_r_writeArr_796120), %r12
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
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(type_795362), %r8
	or	%r8, %lo(type_795362), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821846:
code_821429:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(reify_808039+-4), %r9
	st	%r10, [%r9+%lo(reify_808039+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821434
	nop
code_821435:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821434:
	sethi	%hi(reify_808039), %r9
	or	%r9, %lo(reify_808039), %r13
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
	st	%r8, [%r9]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(handleBlock_r_790063), %r8
	or	%r8, %lo(handleBlock_r_790063), %r8
	! done allocating 1 closures
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821447
	nop
code_821448:
	call	GCFromML ! delay slot empty
	nop
needgc_821447:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	sethi	%hi(PreInt_STR_c_INT), %r8
	or	%r8, %lo(PreInt_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(type_796278+-4), %r9
	st	%r10, [%r9+%lo(type_796278+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821456
	nop
code_821457:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821456:
	sethi	%hi(type_796278), %r9
	or	%r9, %lo(type_796278), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_815418), %r8
	or	%r8, %lo(record_815418), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821847:
code_821471:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_808219+-4), %r9
	st	%r10, [%r9+%lo(type_808219+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821476
	nop
code_821477:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821476:
	sethi	%hi(type_808219), %r9
	or	%r9, %lo(type_808219), %r13
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
	st	%r8, [%r9]
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821488
	nop
code_821489:
	call	GCFromML ! delay slot empty
	nop
needgc_821488:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	sethi	%hi(PreInt_STR_c_INT), %r8
	or	%r8, %lo(PreInt_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(type_796322+-4), %r9
	st	%r10, [%r9+%lo(type_796322+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821497
	nop
code_821498:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821497:
	sethi	%hi(type_796322), %r9
	or	%r9, %lo(type_796322), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_815443), %r8
	or	%r8, %lo(record_815443), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821848:
code_821512:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_808202+-4), %r9
	st	%r10, [%r9+%lo(type_808202+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821517
	nop
code_821518:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821517:
	sethi	%hi(type_808202), %r9
	or	%r9, %lo(type_808202), %r13
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
	st	%r8, [%r9]
	sethi	%hi(type_796278), %r8
	or	%r8, %lo(type_796278), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r13
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(handleBlock_r_790063), %r8
	or	%r8, %lo(handleBlock_r_790063), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821849:
code_821532:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_796369+-4), %r9
	st	%r10, [%r9+%lo(_796369+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821537
	nop
code_821538:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821537:
	sethi	%hi(_796369), %r9
	or	%r9, %lo(_796369), %r13
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
	st	%r8, [%r9]
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(PreInt_STR_c_INT), %r8
	or	%r8, %lo(PreInt_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821850:
code_821553:
	! done making constructor call
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_815483), %r8
	or	%r8, %lo(record_815483), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821857:
code_821557:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_808173+-4), %r9
	st	%r10, [%r9+%lo(type_808173+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821562
	nop
code_821563:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821562:
	sethi	%hi(type_808173), %r9
	or	%r9, %lo(type_808173), %r13
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
	st	%r8, [%r9]
	sethi	%hi(type_796322), %r8
	or	%r8, %lo(type_796322), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r13
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(handleBlock_r_790063), %r8
	or	%r8, %lo(handleBlock_r_790063), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821851:
code_821577:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_796427+-4), %r9
	st	%r10, [%r9+%lo(_796427+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821582
	nop
code_821583:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821582:
	sethi	%hi(_796427), %r9
	or	%r9, %lo(_796427), %r13
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
	st	%r8, [%r9]
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_794600), %r8
	or	%r8, %lo(type_794600), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_815512), %r8
	or	%r8, %lo(record_815512), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_821852:
code_821597:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_808144+-4), %r9
	st	%r10, [%r9+%lo(type_808144+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821602
	nop
code_821603:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821602:
	sethi	%hi(type_808144), %r9
	or	%r9, %lo(type_808144), %r13
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
	st	%r8, [%r9]
	sethi	%hi(1056881), %r8
	or	%r8, %lo(1056881), %r8
	sethi	%hi(type_807690), %r9
	or	%r9, %lo(type_807690), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821616
	nop
code_821617:
	or	%r0, 0, %r9
cmpui_821616:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794788), %r9
	or	%r9, %lo(type_794788), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821620
	nop
code_821621:
	or	%r0, 0, %r9
cmpui_821620:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794792), %r9
	or	%r9, %lo(type_794792), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821624
	nop
code_821625:
	or	%r0, 0, %r9
cmpui_821624:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794796), %r9
	or	%r9, %lo(type_794796), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821628
	nop
code_821629:
	or	%r0, 0, %r9
cmpui_821628:
	sll	%r9, 11, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807686), %r9
	or	%r9, %lo(type_807686), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821632
	nop
code_821633:
	or	%r0, 0, %r9
cmpui_821632:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_794788), %r9
	or	%r9, %lo(type_794788), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821636
	nop
code_821637:
	or	%r0, 0, %r9
cmpui_821636:
	sll	%r9, 14, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_808219), %r9
	or	%r9, %lo(type_808219), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821640
	nop
code_821641:
	or	%r0, 0, %r9
cmpui_821640:
	sll	%r9, 15, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_808202), %r9
	or	%r9, %lo(type_808202), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821644
	nop
code_821645:
	or	%r0, 0, %r9
cmpui_821644:
	sll	%r9, 17, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_808173), %r9
	or	%r9, %lo(type_808173), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821648
	nop
code_821649:
	or	%r0, 0, %r9
cmpui_821648:
	sll	%r9, 18, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_808144), %r9
	or	%r9, %lo(type_808144), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821652
	nop
code_821653:
	or	%r0, 0, %r9
cmpui_821652:
	sll	%r9, 19, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_807693), %r9
	or	%r9, %lo(type_807693), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821656
	nop
code_821657:
	or	%r0, 0, %r9
cmpui_821656:
	sll	%r9, 21, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_796576), %r9
	st	%r8, [%r9+%lo(gctag_796576)]
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+64], %r8
	ld	[%r8+12], %r16
	st	%r16, [%sp+128]
	ld	[%r8+28], %r16
	st	%r16, [%sp+108]
	ld	[%r8+32], %r16
	st	%r16, [%sp+100]
	ld	[%r8+44], %r16
	st	%r16, [%sp+104]
	ld	[%r8+48], %r16
	st	%r16, [%sp+96]
	ld	[%r8+60], %r16
	st	%r16, [%sp+112]
	ld	[%r8+64], %r16
	st	%r16, [%sp+116]
	sethi	%hi(strbindvar_c_789413), %r8
	or	%r8, %lo(strbindvar_c_789413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label S_STR
	ld	[%r8+16], %r16
	st	%r16, [%sp+124]
	! Proj_c at label mode_TYC
	ld	[%sp+124], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+120]
	! start making constructor call
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	ld	[%r17+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+120], %r9
code_821855:
	mov	%r8, %r11
code_821663:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(type_808371+-4), %r8
	st	%r9, [%r8+%lo(type_808371+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_821668
	nop
code_821669:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821668:
	sethi	%hi(type_808371), %r8
	or	%r8, %lo(type_808371), %r10
	ld	[%r2+804], %r9
	ld	[%r2+792], %r8
	mov	%r10, %r13
	mov	%r9, %r12
	st	%r13, [%r8]
	st	%r12, [%r8+4]
	add	%r10, %r9, %r12
	ld	[%r12], %r12
	st	%r12, [%r8+8]
	add	%r8, 12, %r8
	st	%r8, [%r2+792]
	add	%r10, %r9, %r8
	st	%r11, [%r8]
	or	%r0, 17, %r8
	ld	[%sp+120], %r17
	cmp	%r17, 3
	or	%r0, 1, %r9
	bgu	cmpui_821680
	nop
code_821681:
	or	%r0, 0, %r9
cmpui_821680:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_808371), %r9
	or	%r9, %lo(type_808371), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821684
	nop
code_821685:
	or	%r0, 0, %r9
cmpui_821684:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	! allocating 2-record
	add	%r4, 72, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821686
	nop
code_821687:
	call	GCFromML ! delay slot empty
	nop
needgc_821686:
	st	%r8, [%r4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+4]
	or	%r0, 0, %r9
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(_796775+-4), %r9
	st	%r10, [%r9+%lo(_796775+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821693
	nop
code_821694:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821693:
	sethi	%hi(_796775), %r9
	or	%r9, %lo(_796775), %r13
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
	st	%r8, [%r9]
	! Proj_c at label flags_TYC
	ld	[%sp+124], %r17
	ld	[%r17], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_803888+-4), %r8
	st	%r9, [%r8+%lo(type_803888+-4)]
	sethi	%hi(type_803888), %r8
	or	%r8, %lo(type_803888), %r12
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
	! start making constructor call
	sethi	%hi(type_803888), %r8
	or	%r8, %lo(type_803888), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_821853:
	mov	%r8, %r12
code_821717:
	! done making constructor call
	sethi	%hi(type_803888), %r8
	or	%r8, %lo(type_803888), %r9
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
	mov	%r12, %r9
	jmpl	%r13, %r15
	ld	[%sp+128], %r12
code_821858:
code_821722:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_803893+-4), %r9
	st	%r10, [%r9+%lo(_803893+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_821727
	nop
code_821728:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821727:
	sethi	%hi(_803893), %r9
	or	%r9, %lo(_803893), %r13
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
	st	%r8, [%r9]
	! allocating 6-record
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_821739
	nop
code_821740:
	call	GCFromML ! delay slot empty
	nop
needgc_821739:
	sethi	%hi(15409), %r8
	or	%r8, %lo(15409), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 4, %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	ld	[%sp+136], %r17
	st	%r17, [%r4+16]
	sethi	%hi(type_795704), %r8
	or	%r8, %lo(type_795704), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+20]
	ld	[%sp+120], %r17
	st	%r17, [%r4+24]
	add	%r4, 4, %r9
	add	%r4, 28, %r4
	! done allocating 6 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_814331), %r8
	or	%r8, %lo(record_814331), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(file_desc_789417), %r8
	or	%r8, %lo(file_desc_789417), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_789434), %r8
	or	%r8, %lo(announce_r_789434), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_821868:
	mov	%r8, %r13
code_821750:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_796807+-4), %r8
	st	%r9, [%r8+%lo(_796807+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 48, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_821755
	nop
code_821756:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_821755:
	sethi	%hi(_796807), %r8
	or	%r8, %lo(_796807), %r12
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
	sethi	%hi(FileSys_794726), %r8
	or	%r8, %lo(FileSys_794726), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_createf_796810+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_createf_796810+-4)]
	sethi	%hi(strbindvar_r_createf_796810), %r8
	or	%r8, %lo(strbindvar_r_createf_796810), %r12
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
	ld	[%sp+132], %r17
	ld	[%r17+52], %r8
	sethi	%hi(strbindvar_c_789413), %r9
	or	%r9, %lo(strbindvar_c_789413), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821781
	nop
code_821782:
	or	%r0, 0, %r9
cmpui_821781:
	cmp	%r9, 0
	be,pn	%icc,else_case_815799
	nop
code_821783:
	or	%r0, 111, %r10
	sethi	%hi(trunc_796840+-4), %r9
	st	%r10, [%r9+%lo(trunc_796840+-4)]
	sethi	%hi(trunc_796840), %r9
	or	%r9, %lo(trunc_796840), %r13
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
	ba	after_ite_815800
	st	%r8, [%r9]
else_case_815799:
	or	%r0, 9, %r10
	sethi	%hi(trunc_796840+-4), %r9
	st	%r10, [%r9+%lo(trunc_796840+-4)]
	or	%r0, 23, %r10
	sethi	%hi(trunc_796840+4), %r9
	st	%r10, [%r9+%lo(trunc_796840+4)]
	sethi	%hi(trunc_796840), %r9
	st	%r8, [%r9+%lo(trunc_796840)]
after_ite_815800:
	ld	[%sp+132], %r17
	ld	[%r17+24], %r8
	sethi	%hi(strbindvar_c_789413), %r9
	or	%r9, %lo(strbindvar_c_789413), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_821800
	nop
code_821801:
	or	%r0, 0, %r9
cmpui_821800:
	cmp	%r9, 0
	be,pn	%icc,else_case_815811
	nop
code_821802:
	or	%r0, 111, %r10
	sethi	%hi(append_796882+-4), %r9
	st	%r10, [%r9+%lo(append_796882+-4)]
	sethi	%hi(append_796882), %r9
	or	%r9, %lo(append_796882), %r13
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
	ba	after_ite_815812
	st	%r8, [%r9]
else_case_815811:
	or	%r0, 9, %r10
	sethi	%hi(append_796882+-4), %r9
	st	%r10, [%r9+%lo(append_796882+-4)]
	or	%r0, 23, %r10
	sethi	%hi(append_796882+4), %r9
	st	%r10, [%r9+%lo(append_796882+4)]
	sethi	%hi(append_796882), %r9
	st	%r8, [%r9+%lo(append_796882)]
after_ite_815812:
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(functor_var_r_788941), %r8
	or	%r8, %lo(functor_var_r_788941), %r8
	! done allocating 1 closures
	or	%r0, 256, %r8
code_821819:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size PosixPrimIOFn_main,(.-PosixPrimIOFn_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_821820
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821821
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821822
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821823
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821824
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821825
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821826
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821827
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821828
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821829
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821830
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821831
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821832
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821833
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821834
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821835
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821836
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821837
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821838
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821839
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821840
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821841
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821842
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821843
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821844
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821845
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821846
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821847
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821848
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821849
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821850
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821851
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821852
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821853
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_821854
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821855
	.word 0x00240014
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5fff0000
	.word 0x00000055
		! worddata
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
		! -------- label,sizes,reg
	.long afterMutateCheck_820076
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820109
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820245
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820268
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820293
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820313
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820333
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820353
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_820426
	.word 0x00240008
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820442
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_820468
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820482
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820505
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820526
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820549
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820569
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820589
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_820613
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820627
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_820670
	.word 0x00240008
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820684
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01500000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_820746
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820769
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820812
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_820854
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820867
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820900
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_820924
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820931
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820951
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_820963
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820970
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_820990
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821856
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_821013
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_821033
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_821053
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_821073
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_821108
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_821129
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_821150
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_821221
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000050
		! -------- label,sizes,reg
	.long afterMutateCheck_821237
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000050
		! -------- label,sizes,reg
	.long afterMutateCheck_821272
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821295
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_821316
	.word 0x0024000a
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! worddata
	.word 0x80000006
	.long strbindvar_c_789413
		! -------- label,sizes,reg
	.long afterMutateCheck_821324
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_821348
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821363
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_821375
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821389
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821434
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_821447
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821456
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821476
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_821488
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821497
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821517
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821537
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821857
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821562
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821582
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821602
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_821668
	.word 0x00240014
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5fff0000
	.word 0x00000055
		! worddata
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
		! -------- label,sizes,reg
	.long needgc_821686
	.word 0x00240014
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x5fff0000
	.word 0x00000055
		! worddata
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
	.word 0x80000025
	.long strbindvar_c_789413
		! -------- label,sizes,reg
	.long afterMutateCheck_821693
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_821858
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000014
		! -------- label,sizes,reg
	.long afterMutateCheck_821727
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000014
		! -------- label,sizes,reg
	.long needgc_821739
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000014
		! -------- label,sizes,reg
	.long afterMutateCheck_821755
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000004
		! -------- label,sizes,reg
	.long code_821859
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821860
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821861
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821862
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821863
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01500000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_821864
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_821865
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000050
		! -------- label,sizes,reg
	.long code_821866
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821867
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_821868
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000004
	.text
PosixPrimIOFn_unit_CODE_END_VAL:
	.section	".rodata"
		! endgcinfo with filler for alignment
	.globl PosixPrimIOFn_unit_GCTABLE_END_VAL
PosixPrimIOFn_unit_GCTABLE_END_VAL:
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl PosixPrimIOFn_unit_GLOBALS_BEGIN_VAL
PosixPrimIOFn_unit_GLOBALS_BEGIN_VAL:
		! Global
	.word 0x00000037
type_794597:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_794600:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_794601:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_c_789413:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_c_789415:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
file_desc_789417:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000211
record_814080:
	.long PosixPrimIOFn_functor_var_c_code_812517
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl PosixPrimIO_FCT_c_INT
PosixPrimIO_FCT_c_INT:
	.long record_814080
	.long record_814080
		! Global
	.word 0x00000037
FileSys_794726:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
IO_794727:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_PLUSEfile_desc_794728:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
fromInt_794729:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
announce_r_789434:
	.long PosixPrimIOFn_announce_r_code_812522
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
strbindvar_r_fstat_794758:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_802662:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_802663:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_802675:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
isRegFile_802647:
	.long PosixPrimIOFn_isRegFile_code_812556
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
isRegFile_789436:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814220:
	.word 0x00000009
		! Global
	.word 0x00000037
type_794788:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814235:
	.word 0x00000009
		! Global
	.word 0x00000037
type_794792:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814249:
	.word 0x00000009
		! Global
	.word 0x00000037
type_794796:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNbool_out_794804:
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNoption_in_794810:
	.word 0x00000102
		! Global
	.word 0x00000037
type_794813:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_794865:
	.word 0x00000102
		! static record tag
	.word 0x00000011
record_814318:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x00000037
mk_794882:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000211
record_814331:
	.word 0x00000001
	.word 0x00000000
		! Global
	.word 0x00000037
type_794887:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_794896:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_814386:
		! string size = 5
	.ascii "lseek"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_lseek_794899:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_794951:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_814433:
		! string size = 5
	.ascii "fstat"
.align 4
		! Global
	.word 0x00000037
_802807:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_794995:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
posFns_789442:
	.long PosixPrimIOFn_posFns_code_812561
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000009
record_814489:
	.word 0x00000009
		! Global
	.word 0x00000037
reify_807753:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814503:
	.word 0x00000009
		! Global
	.word 0x00000037
reify_807751:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814517:
	.word 0x00000009
		! Global
	.word 0x00000037
reify_807749:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
PLUS_795216:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_795246:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000022
string_814572:
		! string size = 4
	.ascii "read"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_readVec_795249:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_length_795257:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
record_gctag_802953:
	.word 0x00000102
		! Global
	.word 0x00000037
_795273:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000003a
string_814650:
		! string size = 7
	.ascii "readBuf"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_readArr_795276:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_setfl_795148:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNlist_in_795152:
	.word 0x00000102
		! Global
	.word 0x00000037
type_795155:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_807552:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_802883:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
nonblock_795192:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
stamp_795358:
	.word 0x00000102
		! Global
	.word 0x00000037
reify_807574:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNoption_out_795361:
	.word 0x00000102
		! Global
	.word 0x00000037
type_795362:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
PLUSEsyserror_795370:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
again_795373:
	.word 0x00000102
		! Global
	.word 0x00000037
_795444:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_814828:
		! string size = 5
	.ascii "close"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_close_795447:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_795460:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
MINUS_795475:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814885:
	.word 0x00000000
		! Global
	.word 0x00000037
type_795508:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814896:
	.word 0x00000009
		! Global
	.word 0x00000037
type_807737:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_795526:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814919:
	.word 0x00000009
		! Global
	.word 0x00000037
type_807726:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814941:
	.word 0x00000009
		! Global
	.word 0x00000037
type_807711:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814955:
	.word 0x00000009
		! Global
	.word 0x00000037
type_807696:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814969:
	.word 0x00000009
		! Global
	.word 0x00000037
type_807693:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_814987:
	.word 0x00000009
		! Global
	.word 0x00000037
type_807690:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_803241:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_803242:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_795623:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_807686:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_795686:
	.word 0x00000102
		! Global
	.word 0x00000037
type_795704:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_795712:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_815175:
		! string size = 5
	.ascii "openf"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_openf_795715:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_807803:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_803299:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_795916:
	.word 0x00000102
		! Global
	.word 0x00000037
_795932:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
PLUSEbool_796056:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_796032:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_815310:
		! string size = 5
	.ascii "setfl"
.align 4
		! Global
	.word 0x00000037
_796095:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000042
string_815361:
		! string size = 8
	.ascii "writeVec"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_writeVec_796098:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000042
string_815378:
		! string size = 8
	.ascii "writeArr"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_writeArr_796120:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
reify_808039:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
handleBlock_r_790063:
	.long PosixPrimIOFn_handleBlock_r_code_812602
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
type_796278:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_815418:
	.word 0x00000009
		! Global
	.word 0x00000037
type_808219:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_796322:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_815443:
	.word 0x00000009
		! Global
	.word 0x00000037
type_808202:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_796369:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_815483:
	.word 0x00000009
		! Global
	.word 0x00000037
type_808173:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_796427:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_815512:
	.word 0x00000009
		! Global
	.word 0x00000037
type_808144:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_796576:
	.word 0x00000102
		! Global
	.word 0x00000037
type_808371:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_796775:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_803888:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_803893:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_796807:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000003a
string_815779:
		! string size = 7
	.ascii "createf"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_createf_796810:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
trunc_796840:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
append_796882:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x0000006f
	.globl PosixPrimIO_FCT_r_INT
PosixPrimIO_FCT_r_INT:
	.long functor_var_r_788941
	.long functor_var_r_788941
		! static record tag
	.word 0x00000619
functor_var_r_788941:
	.long PosixPrimIOFn_functor_var_r_code_812636
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000009
record_818767:
	.word 0x00000009
		! Module closure
	.word 0x00000619
	.globl PosixPrimIOFn_unit_closure
PosixPrimIOFn_unit_closure:
	.long PosixPrimIOFn_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl PosixPrimIOFn_unit
PosixPrimIOFn_unit:
	.long PosixPrimIOFn_unit_closure
	.long PosixPrimIOFn_unit_closure
	.globl PosixPrimIOFn_unit_GLOBALS_END_VAL
PosixPrimIOFn_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.long 0 !filler

	.data
	.align 8
	.globl PosixPrimIOFn_unit_TRACE_GLOBALS_BEGIN_VAL
PosixPrimIOFn_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long append_796882
	.long trunc_796840
	.long strbindvar_r_createf_796810
	.long _796807
	.long _803893
	.long type_803888
	.long _796775
	.long type_808371
	.long type_808144
	.long _796427
	.long type_808173
	.long _796369
	.long type_808202
	.long type_796322
	.long type_808219
	.long type_796278
	.long reify_808039
	.long strbindvar_r_writeArr_796120
	.long strbindvar_r_writeVec_796098
	.long _796095
	.long _796032
	.long PLUSEbool_796056
	.long _795932
	.long _803299
	.long type_807803
	.long strbindvar_r_openf_795715
	.long _795712
	.long type_795704
	.long type_807686
	.long type_795623
	.long _803242
	.long type_803241
	.long type_807690
	.long type_807693
	.long type_807696
	.long type_807711
	.long type_807726
	.long type_795526
	.long type_807737
	.long type_795508
	.long MINUS_795475
	.long type_795460
	.long strbindvar_r_close_795447
	.long _795444
	.long PLUSEsyserror_795370
	.long type_795362
	.long reify_807574
	.long nonblock_795192
	.long _802883
	.long type_807552
	.long type_795155
	.long strbindvar_r_setfl_795148
	.long strbindvar_r_readArr_795276
	.long _795273
	.long strbindvar_r_length_795257
	.long strbindvar_r_readVec_795249
	.long _795246
	.long PLUS_795216
	.long reify_807749
	.long reify_807751
	.long reify_807753
	.long _794995
	.long _802807
	.long _794951
	.long strbindvar_r_lseek_794899
	.long _794896
	.long type_794887
	.long mk_794882
	.long type_794813
	.long type_794796
	.long type_794792
	.long type_794788
	.long isRegFile_789436
	.long _802675
	.long _802663
	.long type_802662
	.long strbindvar_r_fstat_794758
	.long fromInt_794729
	.long strbindvar_r_PLUSEfile_desc_794728
	.long IO_794727
	.long FileSys_794726
	.long file_desc_789417
	.long strbindvar_c_789415
	.long strbindvar_c_789413
	.long type_794601
	.long type_794600
	.long type_794597
	.globl PosixPrimIOFn_unit_TRACE_GLOBALS_END_VAL
PosixPrimIOFn_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
