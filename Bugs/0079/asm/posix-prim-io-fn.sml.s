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
	.global PosixPrimIOFn_functor_var_c_code_157466
 ! arguments : [$157468,$8] [$143491,$9] 
 ! results    : [$162800,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
PosixPrimIOFn_functor_var_c_code_157466:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162835
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_162835:
	st	%r15, [%sp+92]
code_162821:
funtop_162790:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_162822
	nop
code_162823:
	call	GCFromML ! delay slot empty
	nop
needgc_162822:
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
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(strbindvar_c_143964), %r8
	or	%r8, %lo(strbindvar_c_143964), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
code_162834:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_functor_var_c_code_157466,(.-PosixPrimIOFn_functor_var_c_code_157466)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_162822
	.word 0x00180007
	.word 0x00170000
	.word 0xbffc3e00
	.word 0xbffc3c00
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_157483
 ! arguments : [$157485,$8] [$157486,$9] [$143979,$10] 
 ! results    : [$162789,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_157483:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162841
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_162841:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_162836:
funtop_162775:
	! Proj_c at label type_154065_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_154064_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_162837:
	! done making tail call
code_162839:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_anonfun_code_157483,(.-PosixPrimIOFn_anonfun_code_157483)

	.section	".rodata"
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_157476
 ! arguments : [$157478,$8] [$157479,$9] [$143977,$10] 
 ! results    : [$162774,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_157476:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162857
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_162857:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_162842:
funtop_162730:
	! Proj_c at label type_154065_INT
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_154064_INT
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
code_162855:
	mov	%r8, %r10
code_162845:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_162846
	nop
code_162847:
	call	GCFromML ! delay slot empty
	nop
needgc_162846:
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
	sethi	%hi(PosixPrimIOFn_anonfun_code_157483), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_157483), %r8
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
code_162856:
code_162852:
	! done making normal call
code_162854:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_anonfun_code_157476,(.-PosixPrimIOFn_anonfun_code_157476)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_162846
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_162855
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_162856
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_announce_inner_code_157496
 ! arguments : [$157498,$8] [$157499,$9] [$143975,$10] 
 ! results    : [$157499,$8] 
 ! destroys   :  $10 $8
 ! modifies   :  $10 $8
PosixPrimIOFn_announce_inner_code_157496:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162861
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_162861:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_162858:
funtop_162725:
code_162860:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_announce_inner_code_157496,(.-PosixPrimIOFn_announce_inner_code_157496)

	.section	".rodata"
	.text
	.align 8
	.global PosixPrimIOFn_announce_r_code_157471
 ! arguments : [$157473,$8] [$143969,$9] [$157474,$10] [$143970,$11] 
 ! results    : [$162724,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_announce_r_code_157471:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162875
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_162875:
	st	%r15, [%sp+92]
code_162862:
funtop_162674:
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_162863
	nop
code_162864:
	call	GCFromML ! delay slot empty
	nop
needgc_162863:
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
	sethi	%hi(PosixPrimIOFn_anonfun_code_157476), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_157476), %r8
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
	sethi	%hi(PosixPrimIOFn_announce_inner_code_157496), %r8
	or	%r8, %lo(PosixPrimIOFn_announce_inner_code_157496), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r18
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! Proj_c at label 'a_TYV
	ld	[%r9], %r12
	sethi	%hi(record_162688), %r8
	or	%r8, %lo(record_162688), %r10
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
code_162874:
code_162871:
	! done making normal call
code_162873:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_announce_r_code_157471,(.-PosixPrimIOFn_announce_r_code_157471)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_162863
	.word 0x00180007
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_162874
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_isRegFile_code_157505
 ! arguments : [$157507,$8] [$157508,$9] [$143986,$10] 
 ! results    : [$162673,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_isRegFile_code_157505:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162887
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_162887:
	st	%r15, [%sp+92]
code_162876:
funtop_162653:
	! making closure call
	sethi	%hi(_154116), %r8
	or	%r8, %lo(_154116), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_162886:
	mov	%r8, %r10
code_162879:
	! done making normal call
	! making closure call
	sethi	%hi(_154128), %r8
	or	%r8, %lo(_154128), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_162882:
	! done making tail call
code_162884:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_isRegFile_code_157505,(.-PosixPrimIOFn_isRegFile_code_157505)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_162886
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_endPos_code_157515
 ! arguments : [$157517,$8] [$157518,$9] 
 ! results    : [$162652,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_endPos_code_157515:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162924
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_162924:
	st	%r15, [%sp+92]
code_162888:
funtop_162572:
	ld	[%r9], %r12
	ld	[%r9+4], %r16
	st	%r16, [%sp+96]
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_162923:
code_162892:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_162591
	nop
zero_case_162590:
	ba	after_zeroone_162592
	or	%r0, 256, %r8
one_case_162591:
	sethi	%hi(mk_148921), %r8
	or	%r8, %lo(mk_148921), %r9
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
after_zeroone_162592:
	sethi	%hi(string_158354), %r8
	or	%r8, %lo(string_158354), %r10
	! making closure call
	sethi	%hi(_148990), %r8
	or	%r8, %lo(_148990), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_162918:
	mov	%r8, %r12
code_162901:
	! done making normal call
	sethi	%hi(strbindvar_r_fstat_148797), %r8
	or	%r8, %lo(strbindvar_r_fstat_148797), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_162919:
	mov	%r8, %r12
code_162904:
	! done making normal call
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	sethi	%hi(type_154115), %r8
	or	%r8, %lo(type_154115), %r9
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
code_162920:
	mov	%r8, %r9
code_162911:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_162921:
	mov	%r8, %r10
code_162912:
	! done making normal call
	! making closure call
	sethi	%hi(_154260), %r8
	or	%r8, %lo(_154260), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_162915:
	! done making tail call
code_162917:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_endPos_code_157515,(.-PosixPrimIOFn_endPos_code_157515)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_162918
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
	.long code_162919
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
	.long code_162920
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
	.long code_162921
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_162923
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
	.global PosixPrimIOFn_getPos_code_157524
 ! arguments : [$157526,$8] [$157527,$9] 
 ! results    : [$162571,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
PosixPrimIOFn_getPos_code_157524:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162928
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_162928:
	st	%r15, [%sp+92]
code_162925:
funtop_162566:
	! int sub start
	ld	[%r9], %r8
	! int sub end
code_162927:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_getPos_code_157524,(.-PosixPrimIOFn_getPos_code_157524)

	.section	".rodata"
	.text
	.align 8
	.global PosixPrimIOFn_setPos_code_157531
 ! arguments : [$157533,$8] [$157534,$9] [$144019,$10] 
 ! results    : [$162565,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_setPos_code_157531:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162964
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_162964:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
code_162929:
funtop_162503:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r12
	ld	[%r9+8], %r16
	st	%r16, [%sp+96]
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_162963:
code_162933:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_162524
	nop
zero_case_162523:
	ba	after_zeroone_162525
	or	%r0, 256, %r8
one_case_162524:
	sethi	%hi(mk_148921), %r8
	or	%r8, %lo(mk_148921), %r9
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
after_zeroone_162525:
	sethi	%hi(string_158307), %r8
	or	%r8, %lo(string_158307), %r10
	! making closure call
	sethi	%hi(_148935), %r8
	or	%r8, %lo(_148935), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_162960:
	mov	%r8, %r12
code_162942:
	! done making normal call
	sethi	%hi(strbindvar_r_lseek_148938), %r8
	or	%r8, %lo(strbindvar_r_lseek_148938), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_162961:
	mov	%r8, %r9
code_162945:
	! done making normal call
	or	%r0, 2, %r12
	! making closure call
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r13, %r15
	ld	[%sp+100], %r11
code_162962:
	mov	%r8, %r11
code_162946:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_162950
	nop
code_162951:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_162950:
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
code_162959:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_setPos_code_157531,(.-PosixPrimIOFn_setPos_code_157531)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_162960
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
	.long code_162961
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
	.long code_162962
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_162950
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_162963
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
	.global PosixPrimIOFn_verifyPos_code_157542
 ! arguments : [$157544,$8] [$157545,$9] 
 ! results    : [$162498,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_verifyPos_code_157542:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_162987
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_162987:
	st	%r15, [%sp+92]
code_162965:
funtop_162470:
	ld	[%r9], %r16
	st	%r16, [%sp+100]
	ld	[%r9+4], %r16
	st	%r16, [%sp+96]
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_162986:
	mov	%r8, %r11
code_162968:
	! done making normal call
	or	%r0, 0, %r12
	! making closure call
	sethi	%hi(strbindvar_r_lseek_148938), %r8
	or	%r8, %lo(strbindvar_r_lseek_148938), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+96], %r10
code_162985:
	mov	%r8, %r11
code_162971:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_162975
	nop
code_162976:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_162975:
	ld	[%r2+792], %r10
	ld	[%sp+100], %r9
	or	%r0, 0, %r8
	st	%r9, [%r10]
	st	%r8, [%r10+4]
	add	%r10, 12, %r8
	st	%r8, [%r2+792]
	ld	[%sp+100], %r17
	st	%r11, [%r17]
code_162984:
	mov	%r11, %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_verifyPos_code_157542,(.-PosixPrimIOFn_verifyPos_code_157542)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_162985
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long afterMutateCheck_162975
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_162986
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
	.global PosixPrimIOFn_posFns_code_157510
 ! arguments : [$157512,$8] [$157513,$9] [$154134,$10] [$154135,$11] 
 ! results    : [$162316,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_posFns_code_157510:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163060
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163060:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
	st	%r11, [%sp+96]
code_162988:
funtop_162262:
	! making closure call
	sethi	%hi(isRegFile_154100), %r8
	or	%r8, %lo(isRegFile_154100), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_163058:
code_162990:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_162991
	nop
code_162992:
	call	GCFromML ! delay slot empty
	nop
needgc_162991:
	cmp	%r8, 0
	bne,pn	%icc,one_case_162274
	nop
zero_case_162273:
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163059:
	mov	%r8, %r12
code_162997:
	! done making normal call
	or	%r0, 1, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_162294
	nop
code_162998:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r12, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_163055:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_162999:
	ba	array_int_after_162293 ! delay slot empty
	nop
array_int_small_162294:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163001
	nop
code_163002:
	call	GCFromML ! delay slot empty
	nop
needgc_163001:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_162301
	add	%r9, %r0, %r9
array_init_loopto_162302:
	add	%r11, %r9, %r8
	st	%r12, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_162301:
	cmp	%r9, 0
	bge	array_init_loopto_162302
	nop
array_int_after_162293:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163007
	nop
code_163008:
	call	GCFromML ! delay slot empty
	nop
needgc_163007:
	! allocating 5-record
	sethi	%hi(gctag_148904), %r8
	ld	[%r8+%lo(gctag_148904)], %r8
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
	ba	after_zeroone_162275 ! delay slot empty
	nop
one_case_162274:
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
	bgu	cmpui_163014
	nop
code_163015:
	or	%r0, 0, %r8
cmpui_163014:
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
	sethi	%hi(PosixPrimIOFn_endPos_code_157515), %r8
	or	%r8, %lo(PosixPrimIOFn_endPos_code_157515), %r8
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
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163053:
	mov	%r8, %r11
code_163019:
	! done making normal call
	or	%r0, 1, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_162364
	nop
code_163020:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_163056:
	call	load_regs_MLtoC ! delay slot empty
	nop
	st	%r8, [%sp+108]
code_163021:
	ba	array_int_after_162363 ! delay slot empty
	nop
array_int_small_162364:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163023
	nop
code_163024:
	call	GCFromML ! delay slot empty
	nop
needgc_163023:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r16
	st	%r16, [%sp+108]
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_162371
	add	%r9, %r0, %r9
array_init_loopto_162372:
	ld	[%sp+108], %r17
	add	%r17, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_162371:
	cmp	%r9, 0
	bge	array_init_loopto_162372
	nop
array_int_after_162363:
	add	%r4, 76, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163029
	nop
code_163030:
	call	GCFromML ! delay slot empty
	nop
needgc_163029:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_getPos_code_157524), %r8
	or	%r8, %lo(PosixPrimIOFn_getPos_code_157524), %r8
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
	bgu	cmpui_163035
	nop
code_163036:
	or	%r0, 0, %r8
cmpui_163035:
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
	sethi	%hi(PosixPrimIOFn_setPos_code_157531), %r8
	or	%r8, %lo(PosixPrimIOFn_setPos_code_157531), %r8
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
	bgu	cmpui_163040
	nop
code_163041:
	or	%r0, 0, %r8
cmpui_163040:
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
	sethi	%hi(PosixPrimIOFn_verifyPos_code_157542), %r8
	or	%r8, %lo(PosixPrimIOFn_verifyPos_code_157542), %r8
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
code_163054:
	mov	%r8, %r10
code_163043:
	! done making normal call
	! making closure call
	sethi	%hi(_149034), %r8
	or	%r8, %lo(_149034), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163057:
code_163046:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163047
	nop
code_163048:
	call	GCFromML ! delay slot empty
	nop
needgc_163047:
	! allocating 5-record
	sethi	%hi(gctag_148904), %r8
	ld	[%r8+%lo(gctag_148904)], %r8
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
after_zeroone_162275:
code_163052:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_posFns_code_157510,(.-PosixPrimIOFn_posFns_code_157510)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163053
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
	.long code_163054
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long needgc_162991
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
	.long type_148636
		! -------- label,sizes,reg
	.long code_163055
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163001
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163007
	.word 0x00200007
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_163056
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
	.long needgc_163023
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
	.long needgc_163029
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
	.long code_163057
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long needgc_163047
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long code_163058
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
	.long code_163059
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_157563
 ! arguments : [$157565,$8] [$157566,$9] [$144582,$10] 
 ! results    : [$162192,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_157563:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163102
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163102:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_163061:
funtop_162149:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163062
	nop
code_163063:
	call	GCFromML ! delay slot empty
	nop
needgc_163062:
	! Proj_c at label type_155060_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_155054_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label type_150210_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_150201_INT
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
	bgu	cmpui_163065
	nop
code_163066:
	or	%r0, 0, %r8
cmpui_163065:
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
	sethi	%hi(exn_handler_162168), %r8
	or	%r8, %lo(exn_handler_162168), %r11
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
code_163100:
	mov	%r8, %r10
code_163069:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163070
	nop
code_163071:
	call	GCFromML ! delay slot empty
	nop
needgc_163070:
	ld	[%sp+108], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_162196
	nop
code_163073:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_162197
	nop
code_163074:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_162196
	nop
code_163075:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_162196
	nop
code_163076:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_162196
	nop
dynamic_nobox_162197:
	ba	xinject_sum_dyn_after_162193
	mov	%r10, %r8
dynamic_box_162196:
	or	%r0, 9, %r9
	ld	[%sp+104], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_163079
	nop
code_163080:
	or	%r0, 0, %r8
cmpui_163079:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1-record
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
xinject_sum_dyn_after_162193:
	ba	exn_handler_after_162169
	ld	[%r1+12], %r1
exn_handler_162168:
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
exnarm_162208:
	sethi	%hi(stamp_149397), %r8
	ld	[%r8+%lo(stamp_149397)], %r8
	cmp	%r9, %r8
	bne	exnarm_162211
	nop
code_163085:
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%r8+4], %r9
sumarm_162225:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	ble	nomatch_sum_162223
	nop
code_163086:
	ld	[%r9], %r10
	sethi	%hi(again_149412), %r8
	ld	[%r8+%lo(again_149412)], %r11
	! making closure call
	sethi	%hi(PLUSEsyserror_149409), %r8
	or	%r8, %lo(PLUSEsyserror_149409), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_163101:
code_163090:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_162250
	nop
zero_case_162249:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_zeroone_162251
	or	%r0, 0, %r8
one_case_162250:
	ld	[%sp+112], %r8
after_zeroone_162251:
	ba	after_sum_162222 ! delay slot empty
	nop
sumarm_162226:
nomatch_sum_162223:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_162222:
	ba	afterPLUSexncase_162207 ! delay slot empty
	nop
exnarm_162211:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
afterPLUSexncase_162207:
exn_handler_after_162169:
code_163099:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_anonfun_code_157563,(.-PosixPrimIOFn_anonfun_code_157563)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163100
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
		! -------- label,sizes,reg
	.long needgc_163062
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
	.long needgc_163070
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
	.long code_163101
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
	.global PosixPrimIOFn_handleBlock_inner_code_157556
 ! arguments : [$157558,$8] [$157559,$9] [$144580,$10] 
 ! results    : [$162148,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_handleBlock_inner_code_157556:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163120
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163120:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+104]
	mov	%r10, %r12
code_163103:
funtop_162087:
	! Proj_c at label type_155060_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+108]
	! Proj_c at label type_155054_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_150210_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+116]
	! Proj_c at label type_150201_INT
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
code_163119:
	mov	%r8, %r11
code_163106:
	! done making normal call
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163107
	nop
code_163108:
	call	GCFromML ! delay slot empty
	nop
needgc_163107:
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
	bgu	cmpui_163110
	nop
code_163111:
	or	%r0, 0, %r8
cmpui_163110:
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
	sethi	%hi(PosixPrimIOFn_anonfun_code_157563), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_157563), %r8
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
code_163118:
code_163115:
	! done making normal call
code_163117:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_handleBlock_inner_code_157556,(.-PosixPrimIOFn_handleBlock_inner_code_157556)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163118
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163107
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
	.long code_163119
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
	.global PosixPrimIOFn_handleBlock_r_code_157551
 ! arguments : [$157553,$8] [$144575,$9] [$157554,$10] [$144576,$11] 
 ! results    : [$162081,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_handleBlock_r_code_157551:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163136
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163136:
	st	%r15, [%sp+92]
code_163121:
funtop_162044:
	! Proj_c at label 'a_TYV
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	! Proj_c at label 'b_TYV
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	! start making constructor call
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+100], %r9
code_163135:
	st	%r8, [%sp+96]
code_163124:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_148852), %r8
	or	%r8, %lo(type_148852), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+100], %r9
code_163134:
	mov	%r8, %r9
code_163127:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163128
	nop
code_163129:
	call	GCFromML ! delay slot empty
	nop
needgc_163128:
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
	sethi	%hi(PosixPrimIOFn_handleBlock_inner_code_157556), %r8
	or	%r8, %lo(PosixPrimIOFn_handleBlock_inner_code_157556), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 0, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_163133:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_handleBlock_r_code_157551,(.-PosixPrimIOFn_handleBlock_r_code_157551)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163134
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_163128
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_163135
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
	.text
	.align 8
	.global PosixPrimIOFn_readVec_code_157595
 ! arguments : [$157597,$8] [$157598,$9] [$144181,$10] 
 ! results    : [$162008,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_readVec_code_157595:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163174
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163174:
	st	%r15, [%sp+92]
	st	%r10, [%sp+100]
code_163137:
funtop_161976:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r16
	st	%r16, [%sp+96]
	sethi	%hi(string_158493), %r8
	or	%r8, %lo(string_158493), %r10
	! making closure call
	sethi	%hi(_149285), %r8
	or	%r8, %lo(_149285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163173:
	mov	%r8, %r12
code_163141:
	! done making normal call
	sethi	%hi(strbindvar_r_readVec_149288), %r8
	or	%r8, %lo(strbindvar_r_readVec_149288), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163168:
	mov	%r8, %r9
code_163144:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_163169:
	st	%r8, [%sp+100]
code_163145:
	! done making normal call
	! making closure call
	sethi	%hi(strbindvar_r_length_149296), %r8
	or	%r8, %lo(strbindvar_r_length_149296), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_163170:
	mov	%r8, %r10
code_163148:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+96]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163171:
	mov	%r8, %r11
code_163151:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_149255), %r8
	or	%r8, %lo(PLUS_149255), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_163172:
	mov	%r8, %r11
code_163154:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_163158
	nop
code_163159:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_163158:
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
code_163167:
	ld	[%sp+100], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_readVec_code_157595,(.-PosixPrimIOFn_readVec_code_157595)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163168
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
	.long code_163169
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163170
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_163171
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_163172
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long afterMutateCheck_163158
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_163173
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
	.global PosixPrimIOFn_readArr_code_157604
 ! arguments : [$157606,$8] [$157607,$9] [$154402,$10] [$154403,$11] [$154404,$12] 
 ! results    : [$161949,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_readArr_code_157604:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163212
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163212:
	st	%r15, [%sp+92]
code_163175:
funtop_161910:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163176
	nop
code_163177:
	call	GCFromML ! delay slot empty
	nop
needgc_163176:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	! allocating 3-record
	sethi	%hi(record_gctag_154406), %r8
	ld	[%r8+%lo(record_gctag_154406)], %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	sethi	%hi(string_158571), %r8
	or	%r8, %lo(string_158571), %r10
	! making closure call
	sethi	%hi(_149312), %r8
	or	%r8, %lo(_149312), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163211:
	mov	%r8, %r12
code_163183:
	! done making normal call
	sethi	%hi(strbindvar_r_readArr_149315), %r8
	or	%r8, %lo(strbindvar_r_readArr_149315), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163207:
	mov	%r8, %r9
code_163186:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r12, %r15
	ld	[%sp+96], %r11
code_163208:
	st	%r8, [%sp+96]
code_163187:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_163209:
	mov	%r8, %r11
code_163190:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_149255), %r8
	or	%r8, %lo(PLUS_149255), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_163210:
	mov	%r8, %r11
code_163193:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_163197
	nop
code_163198:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_163197:
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
code_163206:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_readArr_code_157604,(.-PosixPrimIOFn_readArr_code_157604)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_163176
	.word 0x001c0009
	.word 0x00170000
	.word 0x00001200
	.word 0x00000800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! worddata
	.word 0x80000000
	.long type_148640
		! -------- label,sizes,reg
	.long code_163207
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
	.long code_163208
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163209
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163210
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_163197
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163211
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
	.global PosixPrimIOFn_anonfun_code_157625
 ! arguments : [$157627,$8] [$157628,$9] [$144221,$10] 
 ! results    : [$161909,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_157625:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163248
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163248:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+116]
code_163213:
funtop_161815:
	! Proj_c at label type_154459_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label type_149329_INT
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
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163246:
code_163217:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_161844
	nop
zero_case_161843:
	ba	after_zeroone_161845
	or	%r0, 256, %r8
one_case_161844:
	sethi	%hi(mk_148921), %r8
	or	%r8, %lo(mk_148921), %r9
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
after_zeroone_161845:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163242:
code_163226:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_161866
	nop
zero_case_161865:
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(_154336), %r8
	or	%r8, %lo(_154336), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163247:
	mov	%r8, %r11
code_163230:
	! done making normal call
	! making closure call
	sethi	%hi(strbindvar_r_setfl_149187), %r8
	or	%r8, %lo(strbindvar_r_setfl_149187), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r10
code_163244:
code_163233:
	! done making normal call
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163245:
code_163237:
	! done making normal call
	ba	after_zeroone_161867 ! delay slot empty
	nop
one_case_161866:
	or	%r0, 256, %r8
after_zeroone_161867:
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
code_163239:
	! done making tail call
code_163241:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_anonfun_code_157625,(.-PosixPrimIOFn_anonfun_code_157625)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163242
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
	.long code_163244
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
	.long code_163245
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
	.long code_163246
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
	.long code_163247
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
	.global PosixPrimIOFn_blockWrap_inner_code_157618
 ! arguments : [$157620,$8] [$157621,$9] [$144219,$10] 
 ! results    : [$161814,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_blockWrap_inner_code_157618:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163268
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163268:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_163249:
funtop_161748:
	! Proj_c at label type_154459_INT
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_149329_INT
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
code_163267:
	mov	%r8, %r9
code_163252:
	! done making normal call
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163253
	nop
code_163254:
	call	GCFromML ! delay slot empty
	nop
needgc_163253:
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
	bgu	cmpui_163258
	nop
code_163259:
	or	%r0, 0, %r10
cmpui_163258:
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
	sethi	%hi(PosixPrimIOFn_anonfun_code_157625), %r9
	or	%r9, %lo(PosixPrimIOFn_anonfun_code_157625), %r9
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
code_163266:
code_163263:
	! done making normal call
code_163265:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_blockWrap_inner_code_157618,(.-PosixPrimIOFn_blockWrap_inner_code_157618)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163266
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163253
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
	.long code_163267
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
	.global PosixPrimIOFn_blockWrap_r_code_157613
 ! arguments : [$157615,$8] [$144214,$9] [$157616,$10] [$144215,$11] 
 ! results    : [$161743,$8] 
 ! destroys   :  $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_blockWrap_r_code_157613:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163280
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_163280:
	st	%r15, [%sp+92]
	mov	%r9, %r13
code_163269:
funtop_161707:
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163270
	nop
code_163271:
	call	GCFromML ! delay slot empty
	nop
needgc_163270:
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
	bgu	cmpui_163275
	nop
code_163276:
	or	%r0, 0, %r10
cmpui_163275:
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
	sethi	%hi(PosixPrimIOFn_blockWrap_inner_code_157618), %r8
	or	%r8, %lo(PosixPrimIOFn_blockWrap_inner_code_157618), %r8
	st	%r8, [%r4+4]
	st	%r13, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_163279:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_blockWrap_r_code_157613,(.-PosixPrimIOFn_blockWrap_r_code_157613)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_163270
	.word 0x00180007
	.word 0x00170000
	.word 0xbff82400
	.word 0xbff80000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_anonfun_code_157662
 ! arguments : [$157664,$8] [$157665,$9] [$144244,$10] 
 ! results    : [$161633,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_157662:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163360
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_163360:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+128]
code_163281:
funtop_161512:
	! Proj_c at label type_154491_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_154485_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label type_149389_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+124]
	! Proj_c at label type_149362_INT
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
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163359:
code_163285:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_161549
	nop
zero_case_161548:
	ba	after_zeroone_161550
	or	%r0, 256, %r8
one_case_161549:
	sethi	%hi(mk_148921), %r8
	or	%r8, %lo(mk_148921), %r9
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
after_zeroone_161550:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163355:
code_163294:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163295
	nop
code_163296:
	call	GCFromML ! delay slot empty
	nop
needgc_163295:
	cmp	%r8, 0
	bne,pn	%icc,one_case_161571
	nop
zero_case_161570:
	ba	after_zeroone_161572
	or	%r0, 256, %r8
one_case_161571:
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_163302
	nop
code_163303:
	or	%r0, 0, %r8
cmpui_163302:
	cmp	%r8, 0
	be,pn	%icc,else_case_161587
	nop
code_163304:
	sethi	%hi(nonblock_149231), %r8
	or	%r8, %lo(nonblock_149231), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ba	after_ite_161588
	ld	[%r8], %r11
else_case_161587:
	sethi	%hi(nonblock_149231), %r8
	ld	[%r8+%lo(nonblock_149231)], %r11
after_ite_161588:
	! making closure call
	sethi	%hi(strbindvar_r_setfl_149187), %r8
	or	%r8, %lo(strbindvar_r_setfl_149187), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_163354:
code_163311:
	! done making normal call
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163357:
code_163315:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163316
	nop
code_163317:
	call	GCFromML ! delay slot empty
	nop
needgc_163316:
after_zeroone_161572:
	or	%r0, 793, %r9
	ld	[%sp+100], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_163319
	nop
code_163320:
	or	%r0, 0, %r8
cmpui_163319:
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
	sethi	%hi(exn_handler_161609), %r8
	or	%r8, %lo(exn_handler_161609), %r10
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
code_163356:
	mov	%r8, %r10
code_163323:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163324
	nop
code_163325:
	call	GCFromML ! delay slot empty
	nop
needgc_163324:
	ld	[%sp+124], %r17
	ld	[%r17+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_161637
	nop
code_163327:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_161638
	nop
code_163328:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_161637
	nop
code_163329:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_161637
	nop
code_163330:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_161637
	nop
dynamic_nobox_161638:
	ba	xinject_sum_dyn_after_161634
	mov	%r10, %r8
dynamic_box_161637:
	or	%r0, 9, %r9
	ld	[%sp+104], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_163333
	nop
code_163334:
	or	%r0, 0, %r8
cmpui_163333:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1-record
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
xinject_sum_dyn_after_161634:
	ba	exn_handler_after_161610
	ld	[%r1+12], %r1
exn_handler_161609:
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
exnarm_161649:
	sethi	%hi(stamp_149397), %r8
	ld	[%r8+%lo(stamp_149397)], %r8
	cmp	%r9, %r8
	bne	exnarm_161652
	nop
code_163339:
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%r8+4], %r9
sumarm_161666:
	or	%r0, 255, %r8
	cmp	%r9, %r8
	ble	nomatch_sum_161664
	nop
code_163340:
	ld	[%r9], %r10
	sethi	%hi(again_149412), %r8
	ld	[%r8+%lo(again_149412)], %r11
	! making closure call
	sethi	%hi(PLUSEsyserror_149409), %r8
	or	%r8, %lo(PLUSEsyserror_149409), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_163358:
code_163344:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_161695
	nop
zero_case_161694:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_zeroone_161696
	or	%r0, 0, %r8
one_case_161695:
	ld	[%sp+116], %r8
after_zeroone_161696:
	ba	after_sum_161663 ! delay slot empty
	nop
sumarm_161667:
nomatch_sum_161664:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
after_sum_161663:
	ba	afterPLUSexncase_161648 ! delay slot empty
	nop
exnarm_161652:
	ld	[%sp+108], %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	or	%r0, 0, %r8
afterPLUSexncase_161648:
exn_handler_after_161610:
code_163353:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size PosixPrimIOFn_anonfun_code_157662,(.-PosixPrimIOFn_anonfun_code_157662)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163354
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
	.long code_163355
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
	.long code_163356
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40100000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163295
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
	.long type_148636
		! -------- label,sizes,reg
	.long code_163357
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
	.long needgc_163316
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
	.long needgc_163324
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
	.long code_163358
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
	.long code_163359
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
	.global PosixPrimIOFn_noBlockWrap_inner_code_157655
 ! arguments : [$157657,$8] [$157658,$9] [$144242,$10] 
 ! results    : [$161511,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_noBlockWrap_inner_code_157655:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163382
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163382:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_163361:
funtop_161432:
	! Proj_c at label type_154491_INT
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! Proj_c at label type_154485_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
	! Proj_c at label type_149389_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+116]
	! Proj_c at label type_149362_INT
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
code_163381:
	mov	%r8, %r9
code_163364:
	! done making normal call
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163365
	nop
code_163366:
	call	GCFromML ! delay slot empty
	nop
needgc_163365:
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
	bgu	cmpui_163368
	nop
code_163369:
	or	%r0, 0, %r10
cmpui_163368:
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
	bgu	cmpui_163372
	nop
code_163373:
	or	%r0, 0, %r10
cmpui_163372:
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
	sethi	%hi(PosixPrimIOFn_anonfun_code_157662), %r9
	or	%r9, %lo(PosixPrimIOFn_anonfun_code_157662), %r9
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
code_163380:
code_163377:
	! done making normal call
code_163379:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_noBlockWrap_inner_code_157655,(.-PosixPrimIOFn_noBlockWrap_inner_code_157655)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163380
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163365
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
	.long code_163381
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
	.global PosixPrimIOFn_noBlockWrap_r_code_157650
 ! arguments : [$157652,$8] [$144237,$9] [$157653,$10] [$144238,$11] 
 ! results    : [$161427,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_noBlockWrap_r_code_157650:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163404
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163404:
	st	%r15, [%sp+92]
code_163383:
funtop_161365:
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+116], %r9
code_163403:
	st	%r8, [%sp+96]
code_163386:
	! done making constructor call
	! start making constructor call
	sethi	%hi(type_148852), %r8
	or	%r8, %lo(type_148852), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+116], %r9
code_163402:
	mov	%r8, %r9
code_163389:
	add	%r4, 56, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163390
	nop
code_163391:
	call	GCFromML ! delay slot empty
	nop
needgc_163390:
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
	bgu	cmpui_163393
	nop
code_163394:
	or	%r0, 0, %r9
cmpui_163393:
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
	bgu	cmpui_163397
	nop
code_163398:
	or	%r0, 0, %r9
cmpui_163397:
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
	sethi	%hi(PosixPrimIOFn_noBlockWrap_inner_code_157655), %r9
	or	%r9, %lo(PosixPrimIOFn_noBlockWrap_inner_code_157655), %r9
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_163401:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_noBlockWrap_r_code_157650,(.-PosixPrimIOFn_noBlockWrap_r_code_157650)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163402
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
	.long needgc_163390
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
	.long code_163403
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
	.global PosixPrimIOFn_close_code_157696
 ! arguments : [$157698,$8] [$157699,$9] 
 ! results    : [$161362,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_close_code_157696:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163438
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163438:
	st	%r15, [%sp+92]
code_163405:
funtop_161287:
	ld	[%r9], %r16
	st	%r16, [%sp+96]
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163436:
code_163409:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_161306
	nop
zero_case_161305:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163437:
code_163414:
	! done making normal call
	sethi	%hi(string_158749), %r8
	or	%r8, %lo(string_158749), %r10
	! making closure call
	sethi	%hi(_149483), %r8
	or	%r8, %lo(_149483), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163432:
	mov	%r8, %r12
code_163418:
	! done making normal call
	sethi	%hi(strbindvar_r_close_149486), %r8
	or	%r8, %lo(strbindvar_r_close_149486), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163433:
	mov	%r8, %r12
code_163421:
	! done making normal call
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	sethi	%hi(record_158239), %r8
	or	%r8, %lo(record_158239), %r10
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
code_163434:
	mov	%r8, %r9
code_163427:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_163428:
	! done making tail call
	ba	after_zeroone_161307 ! delay slot empty
	nop
one_case_161306:
	or	%r0, 256, %r8
after_zeroone_161307:
code_163431:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_close_code_157696,(.-PosixPrimIOFn_close_code_157696)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163432
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
	.long code_163433
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
	.long code_163434
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
	.long code_163436
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
	.long code_163437
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
	.global PosixPrimIOFn_avail_code_157705
 ! arguments : [$157707,$8] [$157708,$9] 
 ! results    : [$161284,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_avail_code_157705:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163467
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163467:
	st	%r15, [%sp+92]
code_163439:
funtop_161208:
	ld	[%r9], %r12
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	ld	[%r9+8], %r16
	st	%r16, [%sp+96]
	ld	[%r9+12], %r16
	st	%r16, [%sp+104]
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163465:
code_163443:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_161231
	nop
zero_case_161230:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_161236
	nop
zero_case_161235:
	ba	after_zeroone_161237
	or	%r0, 0, %r8
one_case_161236:
	! making closure call
	sethi	%hi(_154116), %r8
	or	%r8, %lo(_154116), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_163466:
	mov	%r8, %r10
code_163449:
	! done making normal call
	! making closure call
	sethi	%hi(_154260), %r8
	or	%r8, %lo(_154260), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163463:
	mov	%r8, %r10
code_163452:
	! done making normal call
	! int sub start
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	! int sub end
	! making closure call
	sethi	%hi(MINUS_149514), %r8
	or	%r8, %lo(MINUS_149514), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_163464:
	mov	%r8, %r9
code_163455:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163456
	nop
code_163457:
	call	GCFromML ! delay slot empty
	nop
needgc_163456:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
after_zeroone_161237:
	ba	after_zeroone_161232 ! delay slot empty
	nop
one_case_161231:
	sethi	%hi(record_158806), %r8
	or	%r8, %lo(record_158806), %r8
after_zeroone_161232:
code_163462:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_avail_code_157705,(.-PosixPrimIOFn_avail_code_157705)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163463
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_163464
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163456
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_163465
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
	.long code_163466
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global PosixPrimIOFn_mkReader_code_157590
 ! arguments : [$157592,$8] [$157593,$9] [$154284,$10] [$154285,$11] [$154286,$12] 
 ! results    : [$161200,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_mkReader_code_157590:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163567
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_163567:
	st	%r15, [%sp+92]
	st	%r10, [%sp+148]
	st	%r11, [%sp+96]
	st	%r12, [%sp+144]
code_163468:
funtop_160855:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163566:
	st	%r8, [%sp+108]
code_163472:
	! done making normal call
	! making closure call
	sethi	%hi(posFns_143991), %r8
	or	%r8, %lo(posFns_143991), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r12, %r15
	ld	[%sp+148], %r11
code_163556:
code_163474:
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
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163557:
	mov	%r8, %r9
code_163478:
	! done making normal call
	add	%r4, 148, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163479
	nop
code_163480:
	call	GCFromML ! delay slot empty
	nop
needgc_163479:
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
	bgu	cmpui_163484
	nop
code_163485:
	or	%r0, 0, %r8
cmpui_163484:
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
	sethi	%hi(PosixPrimIOFn_readVec_code_157595), %r8
	or	%r8, %lo(PosixPrimIOFn_readVec_code_157595), %r8
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
	bgu	cmpui_163489
	nop
code_163490:
	or	%r0, 0, %r10
cmpui_163489:
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
	sethi	%hi(PosixPrimIOFn_readArr_code_157604), %r8
	or	%r8, %lo(PosixPrimIOFn_readArr_code_157604), %r8
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
	bgu	cmpui_163494
	nop
code_163495:
	or	%r0, 0, %r10
cmpui_163494:
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
	sethi	%hi(PosixPrimIOFn_blockWrap_r_code_157613), %r8
	or	%r8, %lo(PosixPrimIOFn_blockWrap_r_code_157613), %r8
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
	bgu	cmpui_163499
	nop
code_163500:
	or	%r0, 0, %r10
cmpui_163499:
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
	sethi	%hi(PosixPrimIOFn_noBlockWrap_r_code_157650), %r8
	or	%r8, %lo(PosixPrimIOFn_noBlockWrap_r_code_157650), %r8
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
	bgu	cmpui_163504
	nop
code_163505:
	or	%r0, 0, %r9
cmpui_163504:
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
	sethi	%hi(PosixPrimIOFn_close_code_157696), %r8
	or	%r8, %lo(PosixPrimIOFn_close_code_157696), %r8
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
	sethi	%hi(isRegFile_154100), %r8
	or	%r8, %lo(isRegFile_154100), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+148], %r10
code_163554:
	mov	%r8, %r9
code_163508:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163509
	nop
code_163510:
	call	GCFromML ! delay slot empty
	nop
needgc_163509:
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
	bgu	cmpui_163514
	nop
code_163515:
	or	%r0, 0, %r10
cmpui_163514:
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
	sethi	%hi(PosixPrimIOFn_avail_code_157705), %r8
	or	%r8, %lo(PosixPrimIOFn_avail_code_157705), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_149547), %r8
	or	%r8, %lo(type_149547), %r9
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
code_163555:
	mov	%r8, %r9
code_163519:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_163558:
	st	%r8, [%sp+108]
code_163520:
	! done making normal call
	sethi	%hi(type_149565), %r8
	or	%r8, %lo(type_149565), %r9
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
code_163559:
	mov	%r8, %r9
code_163523:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_163560:
	st	%r8, [%sp+104]
code_163524:
	! done making normal call
	sethi	%hi(type_149547), %r8
	or	%r8, %lo(type_149547), %r9
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
code_163561:
	mov	%r8, %r9
code_163527:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_163562:
	st	%r8, [%sp+100]
code_163528:
	! done making normal call
	sethi	%hi(type_149565), %r8
	or	%r8, %lo(type_149565), %r9
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
code_163563:
	mov	%r8, %r9
code_163531:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_163564:
	st	%r8, [%sp+96]
code_163532:
	! done making normal call
	! making closure call
	sethi	%hi(_154695), %r8
	or	%r8, %lo(_154695), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+148], %r10
code_163565:
	mov	%r8, %r9
code_163535:
	! done making normal call
	add	%r4, 72, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163536
	nop
code_163537:
	call	GCFromML ! delay slot empty
	nop
needgc_163536:
	sethi	%hi(type_149662), %r8
	or	%r8, %lo(type_149662), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_161174
	nop
code_163541:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_161175
	nop
code_163542:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_161174
	nop
code_163543:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_161174
	nop
code_163544:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_161174
	nop
dynamic_nobox_161175:
	ba	xinject_sum_dyn_after_161168 ! delay slot empty
	nop
dynamic_box_161174:
	or	%r0, 9, %r8
	sethi	%hi(type_154694), %r10
	or	%r10, %lo(type_154694), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_163549
	nop
code_163550:
	or	%r0, 0, %r10
cmpui_163549:
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
xinject_sum_dyn_after_161168:
	! allocating 15-record
	sethi	%hi(gctag_149725), %r8
	ld	[%r8+%lo(gctag_149725)], %r8
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
code_163553:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size PosixPrimIOFn_mkReader_code_157590,(.-PosixPrimIOFn_mkReader_code_157590)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163554
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163555
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163556
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
	.long type_148636
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163557
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_163479
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_163509
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_148636
		! -------- label,sizes,reg
	.long code_163558
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163559
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163560
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163561
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163562
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163563
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163564
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163565
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
		! -------- label,sizes,reg
	.long needgc_163536
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000000
	.long type_154694
		! -------- label,sizes,reg
	.long code_163566
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
	.long type_148636
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_openRd_code_157720
 ! arguments : [$157722,$8] [$157723,$9] [$144387,$10] 
 ! results    : [$160854,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_openRd_code_157720:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163588
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163588:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+108]
	st	%r10, [%sp+104]
code_163568:
funtop_160803:
	sethi	%hi(string_159096), %r8
	or	%r8, %lo(string_159096), %r10
	! making closure call
	sethi	%hi(_149751), %r8
	or	%r8, %lo(_149751), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163587:
	mov	%r8, %r12
code_163572:
	! done making normal call
	sethi	%hi(strbindvar_r_openf_149754), %r8
	or	%r8, %lo(strbindvar_r_openf_149754), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163583:
	st	%r8, [%sp+100]
code_163575:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(_154752), %r8
	or	%r8, %lo(_154752), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163584:
	mov	%r8, %r12
code_163578:
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
code_163585:
	mov	%r8, %r10
code_163579:
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
code_163580:
	! done making tail call
code_163582:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_openRd_code_157720,(.-PosixPrimIOFn_openRd_code_157720)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163583
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_163584
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_163585
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_163587
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
	.text
	.align 8
	.global PosixPrimIOFn_putV_code_157734
 ! arguments : [$157736,$8] [$157737,$9] [$154913,$10] [$154914,$11] 
 ! results    : [$160776,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_putV_code_157734:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163622
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163622:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
	st	%r10, [%sp+96]
	st	%r11, [%sp+100]
code_163589:
funtop_160748:
	sethi	%hi(string_159282), %r8
	or	%r8, %lo(string_159282), %r10
	! making closure call
	sethi	%hi(_150134), %r8
	or	%r8, %lo(_150134), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163621:
	mov	%r8, %r12
code_163593:
	! done making normal call
	sethi	%hi(strbindvar_r_writeVec_150137), %r8
	or	%r8, %lo(strbindvar_r_writeVec_150137), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163617:
	mov	%r8, %r9
code_163596:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_163618:
	st	%r8, [%sp+96]
code_163597:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_163619:
	mov	%r8, %r11
code_163600:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_149255), %r8
	or	%r8, %lo(PLUS_149255), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_163620:
	mov	%r8, %r11
code_163603:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_163607
	nop
code_163608:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_163607:
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
code_163616:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_putV_code_157734,(.-PosixPrimIOFn_putV_code_157734)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163617
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
	.long code_163618
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163619
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163620
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_163607
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163621
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
	.global PosixPrimIOFn_putA_code_157741
 ! arguments : [$157743,$8] [$157744,$9] [$154963,$10] [$154964,$11] 
 ! results    : [$160721,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_putA_code_157741:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163656
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163656:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
	st	%r10, [%sp+96]
	st	%r11, [%sp+100]
code_163623:
funtop_160693:
	sethi	%hi(string_159299), %r8
	or	%r8, %lo(string_159299), %r10
	! making closure call
	sethi	%hi(_149312), %r8
	or	%r8, %lo(_149312), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163655:
	mov	%r8, %r12
code_163627:
	! done making normal call
	sethi	%hi(strbindvar_r_writeArr_150159), %r8
	or	%r8, %lo(strbindvar_r_writeArr_150159), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163651:
	mov	%r8, %r9
code_163630:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r11
code_163652:
	st	%r8, [%sp+96]
code_163631:
	! done making normal call
	! int sub start
	ld	[%sp+104], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! int sub end
	! making closure call
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_163653:
	mov	%r8, %r11
code_163634:
	! done making normal call
	! making closure call
	sethi	%hi(PLUS_149255), %r8
	or	%r8, %lo(PLUS_149255), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_163654:
	mov	%r8, %r11
code_163637:
	! done making normal call
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_163641
	nop
code_163642:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_163641:
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
code_163650:
	ld	[%sp+96], %r8
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_putA_code_157741,(.-PosixPrimIOFn_putA_code_157741)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163651
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
	.long code_163652
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163653
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163654
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long afterMutateCheck_163641
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
		! -------- label,sizes,reg
	.long code_163655
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
	.global PosixPrimIOFn_anonfun_code_157758
 ! arguments : [$157760,$8] [$157761,$9] [$144563,$10] 
 ! results    : [$160692,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_anonfun_code_157758:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163710
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_163710:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+128]
code_163657:
funtop_160550:
	! Proj_c at label type_155024_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r16
	st	%r16, [%sp+100]
	! Proj_c at label type_150178_INT
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
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163707:
code_163661:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_160585
	nop
zero_case_160584:
	ba	after_zeroone_160586
	or	%r0, 256, %r8
one_case_160585:
	sethi	%hi(mk_148921), %r8
	or	%r8, %lo(mk_148921), %r9
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
after_zeroone_160586:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163700:
	mov	%r8, %r10
code_163670:
	! done making normal call
	! making closure call
	sethi	%hi(PLUSEbool_150095), %r8
	or	%r8, %lo(PLUSEbool_150095), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_163703:
code_163673:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_160617
	nop
zero_case_160616:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163708:
code_163678:
	! done making normal call
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163704:
code_163682:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_160643
	nop
zero_case_160642:
	! making closure call
	sethi	%hi(_154336), %r8
	or	%r8, %lo(_154336), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_163709:
code_163686:
	! done making normal call
	ba	after_zeroone_160644
	st	%r8, [%sp+104]
one_case_160643:
	ld	[%sp+112], %r16
	st	%r16, [%sp+104]
after_zeroone_160644:
	sethi	%hi(string_159231), %r8
	or	%r8, %lo(string_159231), %r10
	! making closure call
	sethi	%hi(_150071), %r8
	or	%r8, %lo(_150071), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163702:
	mov	%r8, %r12
code_163691:
	! done making normal call
	sethi	%hi(strbindvar_r_setfl_149187), %r8
	or	%r8, %lo(strbindvar_r_setfl_149187), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163705:
	mov	%r8, %r9
code_163694:
	! done making normal call
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+116], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r11
code_163706:
code_163695:
	! done making normal call
	ba	after_zeroone_160618 ! delay slot empty
	nop
one_case_160617:
	or	%r0, 256, %r8
after_zeroone_160618:
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
code_163697:
	! done making tail call
code_163699:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size PosixPrimIOFn_anonfun_code_157758,(.-PosixPrimIOFn_anonfun_code_157758)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163700
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
	.long type_148636
	.word 0x80000000
	.long type_149194
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_156025
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_163702
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
	.long type_149194
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_163703
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
	.long type_148636
	.word 0x80000000
	.long type_149194
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_156025
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_163704
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
	.long type_149194
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_156025
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_163705
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
	.long type_149194
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_163706
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
	.long code_163707
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
	.long type_148636
	.word 0x80000000
	.long type_149194
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_156025
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_163708
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
	.long type_149194
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_156025
	.word 0x00000002
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_163709
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
	.global PosixPrimIOFn_write_inner_code_157753
 ! arguments : [$157755,$8] [$157756,$9] [$155022,$10] [$155023,$11] 
 ! results    : [$160549,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_write_inner_code_157753:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163738
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_163738:
	st	%r15, [%sp+92]
	mov	%r10, %r22
	mov	%r11, %r21
code_163711:
funtop_160473:
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163712
	nop
code_163713:
	call	GCFromML ! delay slot empty
	nop
needgc_163712:
	! Proj_c at label type_155024_INT
	ld	[%r8], %r20
	! Proj_c at label type_150178_INT
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
	sethi	%hi(type_156025), %r23
	or	%r23, %lo(type_156025), %r24
	ld	[%r2+804], %r23
	add	%r24, %r23, %r23
	ld	[%r23], %r23
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_163717
	nop
code_163718:
	or	%r0, 0, %r23
cmpui_163717:
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
	bgu	cmpui_163721
	nop
code_163722:
	or	%r0, 0, %r23
cmpui_163721:
	sll	%r23, 11, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	sethi	%hi(type_149194), %r23
	or	%r23, %lo(type_149194), %r24
	ld	[%r2+804], %r23
	add	%r24, %r23, %r23
	ld	[%r23], %r23
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_163725
	nop
code_163726:
	or	%r0, 0, %r23
cmpui_163725:
	sll	%r23, 12, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	sethi	%hi(type_148636), %r23
	or	%r23, %lo(type_148636), %r24
	ld	[%r2+804], %r23
	add	%r24, %r23, %r23
	ld	[%r23], %r23
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_163729
	nop
code_163730:
	or	%r0, 0, %r23
cmpui_163729:
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
	sethi	%hi(PosixPrimIOFn_anonfun_code_157758), %r8
	or	%r8, %lo(PosixPrimIOFn_anonfun_code_157758), %r8
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
code_163737:
code_163734:
	! done making normal call
code_163736:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_write_inner_code_157753,(.-PosixPrimIOFn_write_inner_code_157753)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163737
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_163712
	.word 0x00180009
	.word 0x00170000
	.word 0x00400300
	.word 0x00200000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! worddata
	.word 0x80000000
	.long type_148636
	.text
	.align 8
	.global PosixPrimIOFn_write_r_code_157748
 ! arguments : [$157750,$8] [$144556,$9] [$157751,$10] [$144557,$11] 
 ! results    : [$160468,$8] 
 ! destroys   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_write_r_code_157748:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163758
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_163758:
	st	%r15, [%sp+92]
code_163739:
funtop_160416:
	add	%r4, 52, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163740
	nop
code_163741:
	call	GCFromML ! delay slot empty
	nop
needgc_163740:
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
	sethi	%hi(type_156025), %r19
	or	%r19, %lo(type_156025), %r20
	ld	[%r2+804], %r19
	add	%r20, %r19, %r19
	ld	[%r19], %r19
	cmp	%r19, 3
	or	%r0, 1, %r19
	bgu	cmpui_163745
	nop
code_163746:
	or	%r0, 0, %r19
cmpui_163745:
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
	bgu	cmpui_163749
	nop
code_163750:
	or	%r0, 0, %r19
cmpui_163749:
	sll	%r19, 11, %r19
	add	%r19, %r0, %r19
	or	%r19, %r8, %r8
	sethi	%hi(type_149194), %r19
	or	%r19, %lo(type_149194), %r20
	ld	[%r2+804], %r19
	add	%r20, %r19, %r19
	ld	[%r19], %r19
	cmp	%r19, 3
	or	%r0, 1, %r19
	bgu	cmpui_163753
	nop
code_163754:
	or	%r0, 0, %r19
cmpui_163753:
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
	sethi	%hi(PosixPrimIOFn_write_inner_code_157753), %r8
	or	%r8, %lo(PosixPrimIOFn_write_inner_code_157753), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_163757:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size PosixPrimIOFn_write_r_code_157748,(.-PosixPrimIOFn_write_r_code_157748)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_163740
	.word 0x00180007
	.word 0x00170000
	.word 0xbfe00600
	.word 0xbfe00000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global PosixPrimIOFn_close_code_157793
 ! arguments : [$157795,$8] [$157796,$9] 
 ! results    : [$160413,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_close_code_157793:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163792
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163792:
	st	%r15, [%sp+92]
code_163759:
funtop_160338:
	ld	[%r9], %r16
	st	%r16, [%sp+96]
	ld	[%r9+4], %r16
	st	%r16, [%sp+100]
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163790:
code_163763:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_160357
	nop
zero_case_160356:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163791:
code_163768:
	! done making normal call
	sethi	%hi(string_158749), %r8
	or	%r8, %lo(string_158749), %r10
	! making closure call
	sethi	%hi(_149483), %r8
	or	%r8, %lo(_149483), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163786:
	mov	%r8, %r12
code_163772:
	! done making normal call
	sethi	%hi(strbindvar_r_close_149486), %r8
	or	%r8, %lo(strbindvar_r_close_149486), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163787:
	mov	%r8, %r12
code_163775:
	! done making normal call
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	sethi	%hi(record_158239), %r8
	or	%r8, %lo(record_158239), %r10
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
code_163788:
	mov	%r8, %r9
code_163781:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+100], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_163782:
	! done making tail call
	ba	after_zeroone_160358 ! delay slot empty
	nop
one_case_160357:
	or	%r0, 256, %r8
after_zeroone_160358:
code_163785:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_close_code_157793,(.-PosixPrimIOFn_close_code_157793)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163786
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
	.long code_163787
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
	.long code_163788
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
	.long code_163790
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
	.long code_163791
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
	.global PosixPrimIOFn_mkWriter_code_157729
 ! arguments : [$157731,$8] [$157732,$9] [$154771,$10] [$154772,$11] [$154773,$12] [$154774,$13] [$154775,$18] 
 ! results    : [$160331,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_mkWriter_code_157729:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163895
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_163895:
	st	%r15, [%sp+92]
	st	%r10, [%sp+148]
	st	%r11, [%sp+96]
	st	%r12, [%sp+100]
	st	%r13, [%sp+144]
	st	%r18, [%sp+140]
code_163793:
funtop_159992:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163894:
	st	%r8, [%sp+108]
code_163797:
	! done making normal call
	! making closure call
	sethi	%hi(posFns_143991), %r8
	or	%r8, %lo(posFns_143991), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r12, %r15
	ld	[%sp+148], %r11
code_163884:
code_163799:
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
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_163885:
	st	%r8, [%sp+96]
code_163803:
	! done making normal call
	ld	[%sp+100], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_160040
	nop
zero_case_160039:
	or	%r0, 0, %r8
	ba	after_zeroone_160041
	mov	%r8, %r10
one_case_160040:
	sethi	%hi(_149971), %r8
	or	%r8, %lo(_149971), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	mov	%r8, %r10
after_zeroone_160041:
	! making closure call
	sethi	%hi(_154336), %r8
	or	%r8, %lo(_154336), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163882:
	mov	%r8, %r10
code_163810:
	! done making normal call
	add	%r4, 124, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163811
	nop
code_163812:
	call	GCFromML ! delay slot empty
	nop
needgc_163811:
	! allocating 2-record
	sethi	%hi(gctag_149955), %r8
	ld	[%r8+%lo(gctag_149955)], %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 2-record
	sethi	%hi(gctag_149955), %r9
	ld	[%r9+%lo(gctag_149955)], %r9
	st	%r9, [%r4]
	sethi	%hi(strbindvar_c_143962), %r9
	or	%r9, %lo(strbindvar_c_143962), %r11
	ld	[%r2+804], %r9
	add	%r11, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_163818
	nop
code_163819:
	or	%r0, 0, %r9
cmpui_163818:
	cmp	%r9, 0
	be,pn	%icc,else_case_160086
	nop
code_163820:
	sethi	%hi(nonblock_149231), %r9
	or	%r9, %lo(nonblock_149231), %r11
	ld	[%r2+804], %r9
	add	%r11, %r9, %r9
	ba	after_ite_160087
	ld	[%r9], %r9
else_case_160086:
	sethi	%hi(nonblock_149231), %r9
	ld	[%r9+%lo(nonblock_149231)], %r9
after_ite_160087:
	st	%r9, [%r4+4]
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_putV_code_157734), %r8
	or	%r8, %lo(PosixPrimIOFn_putV_code_157734), %r8
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
	sethi	%hi(PosixPrimIOFn_putA_code_157741), %r8
	or	%r8, %lo(PosixPrimIOFn_putA_code_157741), %r8
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
	sethi	%hi(type_156025), %r11
	or	%r11, %lo(type_156025), %r12
	ld	[%r2+804], %r11
	add	%r12, %r11, %r11
	ld	[%r11], %r11
	cmp	%r11, 3
	or	%r0, 1, %r11
	bgu	cmpui_163829
	nop
code_163830:
	or	%r0, 0, %r11
cmpui_163829:
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
	bgu	cmpui_163833
	nop
code_163834:
	or	%r0, 0, %r11
cmpui_163833:
	sll	%r11, 11, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r8
	sethi	%hi(type_149194), %r11
	or	%r11, %lo(type_149194), %r12
	ld	[%r2+804], %r11
	add	%r12, %r11, %r11
	ld	[%r11], %r11
	cmp	%r11, 3
	or	%r0, 1, %r11
	bgu	cmpui_163837
	nop
code_163838:
	or	%r0, 0, %r11
cmpui_163837:
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
	sethi	%hi(PosixPrimIOFn_write_r_code_157748), %r8
	or	%r8, %lo(PosixPrimIOFn_write_r_code_157748), %r8
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
	bgu	cmpui_163842
	nop
code_163843:
	or	%r0, 0, %r9
cmpui_163842:
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
	sethi	%hi(PosixPrimIOFn_close_code_157793), %r8
	or	%r8, %lo(PosixPrimIOFn_close_code_157793), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r16
	st	%r16, [%sp+112]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	sethi	%hi(type_150317), %r8
	or	%r8, %lo(type_150317), %r9
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
code_163883:
	st	%r8, [%sp+100]
code_163847:
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
code_163886:
	st	%r8, [%sp+108]
code_163848:
	! done making normal call
	sethi	%hi(type_150361), %r8
	or	%r8, %lo(type_150361), %r9
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
code_163887:
	st	%r8, [%sp+96]
code_163851:
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
code_163888:
	st	%r8, [%sp+104]
code_163852:
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
code_163889:
	mov	%r8, %r10
code_163853:
	! done making normal call
	! making closure call
	sethi	%hi(_150408), %r8
	or	%r8, %lo(_150408), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163890:
	st	%r8, [%sp+100]
code_163856:
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
code_163891:
	mov	%r8, %r10
code_163857:
	! done making normal call
	! making closure call
	sethi	%hi(_150466), %r8
	or	%r8, %lo(_150466), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163892:
	st	%r8, [%sp+96]
code_163860:
	! done making normal call
	! making closure call
	sethi	%hi(_154695), %r8
	or	%r8, %lo(_154695), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+148], %r10
code_163893:
	mov	%r8, %r9
code_163863:
	! done making normal call
	add	%r4, 68, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163864
	nop
code_163865:
	call	GCFromML ! delay slot empty
	nop
needgc_163864:
	sethi	%hi(type_149662), %r8
	or	%r8, %lo(type_149662), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	cmp	%r8, 4
	ble,pn	%icc,dynamic_box_160306
	nop
code_163869:
	cmp	%r8, 255
	ble,pn	%icc,dynamic_nobox_160307
	nop
code_163870:
	ld	[%r8], %r8
	cmp	%r8, 12
	be,pn	%icc,dynamic_box_160306
	nop
code_163871:
	cmp	%r8, 4
	be,pn	%icc,dynamic_box_160306
	nop
code_163872:
	cmp	%r8, 8
	be,pn	%icc,dynamic_box_160306
	nop
dynamic_nobox_160307:
	ba	xinject_sum_dyn_after_160300 ! delay slot empty
	nop
dynamic_box_160306:
	or	%r0, 9, %r8
	sethi	%hi(type_154694), %r10
	or	%r10, %lo(type_154694), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_163877
	nop
code_163878:
	or	%r0, 0, %r10
cmpui_163877:
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
xinject_sum_dyn_after_160300:
	! allocating 14-record
	sethi	%hi(gctag_150615), %r8
	ld	[%r8+%lo(gctag_150615)], %r8
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
code_163881:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size PosixPrimIOFn_mkWriter_code_157729,(.-PosixPrimIOFn_mkWriter_code_157729)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163882
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163883
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163884
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
	.long type_148636
	.word 0x80000000
	.long type_148636
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163885
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
	.long type_148636
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long needgc_163811
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
	.word 0x80000000
	.long type_149194
		! -------- label,sizes,reg
	.long code_163886
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163887
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163888
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163889
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163890
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163891
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163892
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000084
	.long Posix_STR_c_INT
		! -------- label,sizes,reg
	.long code_163893
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
		! -------- label,sizes,reg
	.long needgc_163864
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
	.long reify_156222
	.word 0x80000000
	.long reify_156222
	.word 0x80000000
	.long reify_156224
	.word 0x80000000
	.long reify_156226
	.word 0x80000000
	.long type_154694
		! -------- label,sizes,reg
	.long code_163894
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
	.long type_148636
	.word 0x80000000
	.long type_148636
	.word 0x80000084
	.long Posix_STR_c_INT
	.text
	.align 8
	.global PosixPrimIOFn_createFile_code_157804
 ! arguments : [$157806,$8] [$157807,$9] [$155349,$10] [$155350,$11] [$155351,$12] 
 ! results    : [$159991,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_createFile_code_157804:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163910
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163910:
	st	%r15, [%sp+92]
	st	%r9, [%sp+96]
	st	%r10, [%sp+100]
	st	%r11, [%sp+104]
	st	%r12, [%sp+108]
code_163896:
funtop_159961:
	sethi	%hi(string_159700), %r8
	or	%r8, %lo(string_159700), %r10
	! making closure call
	sethi	%hi(_150846), %r8
	or	%r8, %lo(_150846), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163909:
	mov	%r8, %r12
code_163900:
	! done making normal call
	sethi	%hi(strbindvar_r_createf_150849), %r8
	or	%r8, %lo(strbindvar_r_createf_150849), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_163907:
	mov	%r8, %r9
code_163903:
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
code_163904:
	! done making tail call
code_163906:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_createFile_code_157804,(.-PosixPrimIOFn_createFile_code_157804)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163907
	.word 0x001c000d
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00f70000
		! worddata
	.word 0x80000000
	.long type_155341
	.word 0x80000007
	.long strbindvar_c_143962
	.word 0x80000006
	.long strbindvar_c_143962
		! -------- label,sizes,reg
	.long code_163909
	.word 0x001c000d
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00f70000
		! worddata
	.word 0x80000000
	.long type_155341
	.word 0x80000007
	.long strbindvar_c_143962
	.word 0x80000006
	.long strbindvar_c_143962
	.text
	.align 8
	.global PosixPrimIOFn_openWr_code_157811
 ! arguments : [$157813,$8] [$157814,$9] [$144781,$10] 
 ! results    : [$159960,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_openWr_code_157811:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163927
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163927:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+100]
code_163911:
funtop_159920:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r10
	or	%r0, 2, %r11
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_163914
	nop
code_163915:
	or	%r0, 0, %r8
cmpui_163914:
	cmp	%r8, 0
	be,pn	%icc,else_case_159941
	nop
code_163916:
	sethi	%hi(trunc_150879), %r8
	or	%r8, %lo(trunc_150879), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ba	after_ite_159942
	ld	[%r8], %r12
else_case_159941:
	sethi	%hi(trunc_150879), %r8
	ld	[%r8+%lo(trunc_150879)], %r12
after_ite_159942:
	! making closure call
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_163925:
	mov	%r8, %r10
code_163921:
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
code_163922:
	! done making tail call
code_163924:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_openWr_code_157811,(.-PosixPrimIOFn_openWr_code_157811)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163925
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
	.align 8
	.global PosixPrimIOFn_openApp_code_157822
 ! arguments : [$157824,$8] [$157825,$9] [$144805,$10] 
 ! results    : [$159919,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_openApp_code_157822:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163944
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_163944:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+100]
code_163928:
funtop_159879:
	ld	[%r9], %r16
	st	%r16, [%sp+104]
	ld	[%r9+4], %r10
	or	%r0, 2, %r11
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_163931
	nop
code_163932:
	or	%r0, 0, %r8
cmpui_163931:
	cmp	%r8, 0
	be,pn	%icc,else_case_159900
	nop
code_163933:
	sethi	%hi(append_150921), %r8
	or	%r8, %lo(append_150921), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ba	after_ite_159901
	ld	[%r8], %r12
else_case_159900:
	sethi	%hi(append_150921), %r8
	ld	[%r8+%lo(append_150921)], %r12
after_ite_159901:
	! making closure call
	ld	[%r10], %r13
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	jmpl	%r13, %r15
	ld	[%sp+100], %r10
code_163942:
	mov	%r8, %r10
code_163938:
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
code_163939:
	! done making tail call
code_163941:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size PosixPrimIOFn_openApp_code_157822,(.-PosixPrimIOFn_openApp_code_157822)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163942
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
	.align 8
	.global PosixPrimIOFn_functor_var_r_code_157585
 ! arguments : [$157587,$8] [$148715,$9] [$157588,$10] [$143492,$11] 
 ! results    : [$159856,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_functor_var_r_code_157585:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_163988
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_163988:
	st	%r15, [%sp+92]
	st	%r9, [%sp+116]
code_163945:
funtop_159741:
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163946
	nop
code_163947:
	call	GCFromML ! delay slot empty
	nop
needgc_163946:
	ld	[%r11], %r16
	st	%r16, [%sp+112]
	ld	[%sp+112], %r17
	ld	[%r17+8], %r8
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(PosixPrimIOFn_mkReader_code_157590), %r8
	or	%r8, %lo(PosixPrimIOFn_mkReader_code_157590), %r8
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
	sethi	%hi(PosixPrimIOFn_openRd_code_157720), %r8
	or	%r8, %lo(PosixPrimIOFn_openRd_code_157720), %r8
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
	sethi	%hi(PosixPrimIOFn_mkWriter_code_157729), %r8
	or	%r8, %lo(PosixPrimIOFn_mkWriter_code_157729), %r8
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
	sethi	%hi(type_155341), %r8
	or	%r8, %lo(type_155341), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_163954
	nop
code_163955:
	or	%r0, 0, %r8
cmpui_163954:
	sll	%r8, 21, %r8
	add	%r8, %r0, %r8
	ld	[%sp+96], %r17
	or	%r8, %r17, %r16
	st	%r16, [%sp+96]
	sethi	%hi(_150814), %r8
	or	%r8, %lo(_150814), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(_155346), %r8
	or	%r8, %lo(_155346), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_163987:
	mov	%r8, %r11
code_163960:
	! done making normal call
	add	%r4, 144, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_163961
	nop
code_163962:
	call	GCFromML ! delay slot empty
	nop
needgc_163961:
	! allocating 1 closures
	or	%r0, 537, %r10
	sethi	%hi(type_155341), %r8
	or	%r8, %lo(type_155341), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_163966
	nop
code_163967:
	or	%r0, 0, %r8
cmpui_163966:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 3-record
	st	%r10, [%r4]
	sethi	%hi(PosixPrimIOFn_createFile_code_157804), %r8
	or	%r8, %lo(PosixPrimIOFn_createFile_code_157804), %r8
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
	sethi	%hi(PosixPrimIOFn_openWr_code_157811), %r8
	or	%r8, %lo(PosixPrimIOFn_openWr_code_157811), %r8
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
	sethi	%hi(PosixPrimIOFn_openApp_code_157822), %r8
	or	%r8, %lo(PosixPrimIOFn_openApp_code_157822), %r8
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
	sethi	%hi(FileSys_148765), %r12
	or	%r12, %lo(FileSys_148765), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+12]
	sethi	%hi(IO_148766), %r12
	or	%r12, %lo(IO_148766), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+16]
	sethi	%hi(strbindvar_r_PLUSEfile_desc_148767), %r12
	or	%r12, %lo(strbindvar_r_PLUSEfile_desc_148767), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+20]
	sethi	%hi(fromInt_148768), %r12
	or	%r12, %lo(fromInt_148768), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+24]
	sethi	%hi(announce_r_143983), %r12
	or	%r12, %lo(announce_r_143983), %r12
	st	%r12, [%r4+28]
	sethi	%hi(4096), %r12
	st	%r12, [%r4+32]
	sethi	%hi(isRegFile_143985), %r12
	or	%r12, %lo(isRegFile_143985), %r13
	ld	[%r2+804], %r12
	add	%r13, %r12, %r12
	ld	[%r12], %r12
	st	%r12, [%r4+36]
	sethi	%hi(posFns_143991), %r12
	or	%r12, %lo(posFns_143991), %r12
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
code_163986:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size PosixPrimIOFn_functor_var_r_code_157585,(.-PosixPrimIOFn_functor_var_r_code_157585)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_163987
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! -------- label,sizes,reg
	.long needgc_163946
	.word 0x00200007
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04000000
		! -------- label,sizes,reg
	.long needgc_163961
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000800
		! stacktrace
	.word 0x00000000
	.word 0x05540000
		! worddata
	.word 0x80000000
	.long type_155341
	.text
	.align 8
	.global PosixPrimIOFn_main
 ! arguments : 
 ! results    : [$159740,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
PosixPrimIOFn_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_165790
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_165790:
	st	%r15, [%sp+92]
code_163989:
funtop_157926:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_165780:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_163990:
	sethi	%hi(PLUSO_bool_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label bool_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_148636+-4), %r8
	st	%r9, [%r8+%lo(type_148636+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 24, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_163997
	nop
code_163998:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_163997:
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r12
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
	sethi	%hi(type_148639+-4), %r8
	st	%r9, [%r8+%lo(type_148639+-4)]
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r12
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	or	%r0, 2, %r9
	ld	[%r8], %r10
	jmpl	%r10, %r15
	ld	[%r8+4], %r8
code_165741:
	mov	%r8, %r13
code_164025:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(type_148640+-4), %r8
	st	%r9, [%r8+%lo(type_148640+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 120, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_164030
	nop
code_164031:
	sub	%r4, 120, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164030:
	sethi	%hi(type_148640), %r8
	or	%r8, %lo(type_148640), %r12
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
	sethi	%hi(strbindvar_c_143962+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_c_143962+-4)]
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r12
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
	sethi	%hi(strbindvar_c_143964+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_c_143964+-4)]
	sethi	%hi(strbindvar_c_143964), %r8
	or	%r8, %lo(strbindvar_c_143964), %r12
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
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label file_desc_TYC
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(file_desc_143966+-4), %r8
	st	%r9, [%r8+%lo(file_desc_143966+-4)]
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r12
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
	sethi	%hi(FileSys_148765+-4), %r8
	st	%r9, [%r8+%lo(FileSys_148765+-4)]
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r12
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
	sethi	%hi(IO_148766+-4), %r8
	st	%r9, [%r8+%lo(IO_148766+-4)]
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r12
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
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_PLUSEfile_desc_148767+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_PLUSEfile_desc_148767+-4)]
	sethi	%hi(strbindvar_r_PLUSEfile_desc_148767), %r8
	or	%r8, %lo(strbindvar_r_PLUSEfile_desc_148767), %r12
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
	sethi	%hi(fromInt_148768+-4), %r8
	st	%r9, [%r8+%lo(fromInt_148768+-4)]
	sethi	%hi(fromInt_148768), %r8
	or	%r8, %lo(fromInt_148768), %r12
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
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r8
	! done allocating 1 closures
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+68], %r16
	st	%r16, [%sp+100]
	ld	[%sp+100], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+96]
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+80], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_fstat_148797+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_fstat_148797+-4)]
	sethi	%hi(strbindvar_r_fstat_148797), %r8
	or	%r8, %lo(strbindvar_r_fstat_148797), %r12
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
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label ST_STR
	ld	[%r8+44], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_154115+-4), %r8
	st	%r9, [%r8+%lo(type_154115+-4)]
	sethi	%hi(type_154115), %r8
	or	%r8, %lo(type_154115), %r12
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
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_154115), %r8
	or	%r8, %lo(type_154115), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	sethi	%hi(strbindvar_r_fstat_148797), %r8
	or	%r8, %lo(strbindvar_r_fstat_148797), %r11
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
code_165742:
code_164161:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_154116+-4), %r9
	st	%r10, [%r9+%lo(_154116+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164166
	nop
code_164167:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164166:
	sethi	%hi(_154116), %r9
	or	%r9, %lo(_154116), %r13
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
	sethi	%hi(type_154115), %r8
	or	%r8, %lo(type_154115), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r12
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
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
code_165743:
code_164184:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_154128+-4), %r9
	st	%r10, [%r9+%lo(_154128+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164189
	nop
code_164190:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164189:
	sethi	%hi(_154128), %r9
	or	%r9, %lo(_154128), %r13
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
	sethi	%hi(isRegFile_154100), %r8
	or	%r8, %lo(isRegFile_154100), %r8
	! done allocating 1 closures
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label file_desc_TYC
	ld	[%r8+8], %r18
	sethi	%hi(type_148636), %r8
	or	%r8, %lo(type_148636), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r10
	sethi	%hi(isRegFile_154100), %r8
	or	%r8, %lo(isRegFile_154100), %r12
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
code_165744:
code_164209:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(isRegFile_143985+-4), %r9
	st	%r10, [%r9+%lo(isRegFile_143985+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164214
	nop
code_164215:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164214:
	sethi	%hi(isRegFile_143985), %r9
	or	%r9, %lo(isRegFile_143985), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158141), %r8
	or	%r8, %lo(record_158141), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165745:
code_164229:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_148827+-4), %r9
	st	%r10, [%r9+%lo(type_148827+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164234
	nop
code_164235:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164234:
	sethi	%hi(type_148827), %r9
	or	%r9, %lo(type_148827), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158156), %r8
	or	%r8, %lo(record_158156), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165746:
code_164249:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_148831+-4), %r9
	st	%r10, [%r9+%lo(type_148831+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164254
	nop
code_164255:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164254:
	sethi	%hi(type_148831), %r9
	or	%r9, %lo(type_148831), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158170), %r8
	or	%r8, %lo(record_158170), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165747:
code_164269:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_148835+-4), %r9
	st	%r10, [%r9+%lo(type_148835+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 48, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164274
	nop
code_164275:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164274:
	sethi	%hi(type_148835), %r9
	or	%r9, %lo(type_148835), %r13
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
	sethi	%hi(PLUSNbool_out_148843), %r8
	st	%r9, [%r8+%lo(PLUSNbool_out_148843)]
	sethi	%hi(PLUSO_option_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r9
	sethi	%hi(PLUSNoption_in_148849), %r8
	st	%r9, [%r8+%lo(PLUSNoption_in_148849)]
	sethi	%hi(PLUSO_option_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label option_sum_INT
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_148852+-4), %r8
	st	%r9, [%r8+%lo(type_148852+-4)]
	sethi	%hi(type_148852), %r8
	or	%r8, %lo(type_148852), %r12
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
	sethi	%hi(type_148827), %r9
	or	%r9, %lo(type_148827), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_164306
	nop
code_164307:
	or	%r0, 0, %r9
cmpui_164306:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148831), %r9
	or	%r9, %lo(type_148831), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_164310
	nop
code_164311:
	or	%r0, 0, %r9
cmpui_164310:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148835), %r9
	or	%r9, %lo(type_148835), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_164314
	nop
code_164315:
	or	%r0, 0, %r9
cmpui_164314:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148827), %r9
	or	%r9, %lo(type_148827), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_164318
	nop
code_164319:
	or	%r0, 0, %r9
cmpui_164318:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_148904), %r9
	st	%r8, [%r9+%lo(gctag_148904)]
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
	sethi	%hi(mk_148921+-4), %r8
	st	%r9, [%r8+%lo(mk_148921+-4)]
	sethi	%hi(mk_148921), %r8
	or	%r8, %lo(mk_148921), %r12
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
	sethi	%hi(strbindvar_c_143964), %r8
	or	%r8, %lo(strbindvar_c_143964), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label file_desc_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_148926+-4), %r8
	st	%r9, [%r8+%lo(type_148926+-4)]
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r12
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
	sethi	%hi(strbindvar_c_143964), %r8
	or	%r8, %lo(strbindvar_c_143964), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label whence_TYC
	ld	[%r8+8], %r10
	! allocating 5-record
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_164347
	nop
code_164348:
	call	GCFromML ! delay slot empty
	nop
needgc_164347:
	sethi	%hi(7209), %r8
	or	%r8, %lo(7209), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 3, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
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
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
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
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165781:
code_164358:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_148935+-4), %r9
	st	%r10, [%r9+%lo(_148935+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164363
	nop
code_164364:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164363:
	sethi	%hi(_148935), %r9
	or	%r9, %lo(_148935), %r13
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_lseek_148938+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_lseek_148938+-4)]
	sethi	%hi(strbindvar_r_lseek_148938), %r8
	or	%r8, %lo(strbindvar_r_lseek_148938), %r12
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
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
	ble	needgc_164389
	nop
code_164390:
	call	GCFromML ! delay slot empty
	nop
needgc_164389:
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_154115), %r8
	or	%r8, %lo(type_154115), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165782:
code_164398:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_148990+-4), %r9
	st	%r10, [%r9+%lo(_148990+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164403
	nop
code_164404:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164403:
	sethi	%hi(_148990), %r9
	or	%r9, %lo(_148990), %r13
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
	sethi	%hi(type_154115), %r8
	or	%r8, %lo(type_154115), %r9
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
code_165748:
	mov	%r8, %r13
code_164421:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_154260+-4), %r8
	st	%r9, [%r8+%lo(_154260+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_164426
	nop
code_164427:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164426:
	sethi	%hi(_154260), %r8
	or	%r8, %lo(_154260), %r12
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
code_165749:
	mov	%r8, %r13
code_164442:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_149034+-4), %r8
	st	%r9, [%r8+%lo(_149034+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_164447
	nop
code_164448:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164447:
	sethi	%hi(_149034), %r8
	or	%r8, %lo(_149034), %r12
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
	sethi	%hi(posFns_143991), %r8
	or	%r8, %lo(posFns_143991), %r8
	! done allocating 1 closures
	sethi	%hi(PLUSO_bool_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	! start making constructor call
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_158410), %r8
	or	%r8, %lo(record_158410), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165750:
	mov	%r8, %r13
code_164465:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(reify_156226+-4), %r8
	st	%r9, [%r8+%lo(reify_156226+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_164470
	nop
code_164471:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164470:
	sethi	%hi(reify_156226), %r8
	or	%r8, %lo(reify_156226), %r12
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_158424), %r8
	or	%r8, %lo(record_158424), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165751:
	mov	%r8, %r13
code_164485:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(reify_156224+-4), %r8
	st	%r9, [%r8+%lo(reify_156224+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_164490
	nop
code_164491:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164490:
	sethi	%hi(reify_156224), %r8
	or	%r8, %lo(reify_156224), %r12
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(record_158438), %r8
	or	%r8, %lo(record_158438), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165752:
	mov	%r8, %r13
code_164505:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(reify_156222+-4), %r8
	st	%r9, [%r8+%lo(reify_156222+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 24, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_164510
	nop
code_164511:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164510:
	sethi	%hi(reify_156222), %r8
	or	%r8, %lo(reify_156222), %r12
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
	sethi	%hi(PLUS_149255+-4), %r8
	st	%r9, [%r8+%lo(PLUS_149255+-4)]
	sethi	%hi(PLUS_149255), %r8
	or	%r8, %lo(PLUS_149255), %r12
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
	ble	needgc_164534
	nop
code_164535:
	call	GCFromML ! delay slot empty
	nop
needgc_164534:
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
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
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165783:
code_164543:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_149285+-4), %r9
	st	%r10, [%r9+%lo(_149285+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164548
	nop
code_164549:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164548:
	sethi	%hi(_149285), %r9
	or	%r9, %lo(_149285), %r13
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_readVec_149288+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_readVec_149288+-4)]
	sethi	%hi(strbindvar_r_readVec_149288), %r8
	or	%r8, %lo(strbindvar_r_readVec_149288), %r12
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
	sethi	%hi(strbindvar_r_length_149296+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_length_149296+-4)]
	sethi	%hi(strbindvar_r_length_149296), %r8
	or	%r8, %lo(strbindvar_r_length_149296), %r12
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
	sethi	%hi(type_148640), %r8
	or	%r8, %lo(type_148640), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_164586
	nop
code_164587:
	or	%r0, 0, %r8
cmpui_164586:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	sethi	%hi(record_gctag_154406), %r8
	st	%r10, [%r8+%lo(record_gctag_154406)]
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
	ble	needgc_164591
	nop
code_164592:
	call	GCFromML ! delay slot empty
	nop
needgc_164591:
	sethi	%hi(7209), %r8
	or	%r8, %lo(7209), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 3, %r8
	st	%r8, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_148640), %r8
	or	%r8, %lo(type_148640), %r9
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
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
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
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165784:
code_164600:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_149312+-4), %r9
	st	%r10, [%r9+%lo(_149312+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 48, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164605
	nop
code_164606:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164605:
	sethi	%hi(_149312), %r9
	or	%r9, %lo(_149312), %r13
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+28], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_readArr_149315+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_readArr_149315+-4)]
	sethi	%hi(strbindvar_r_readArr_149315), %r8
	or	%r8, %lo(strbindvar_r_readArr_149315), %r12
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_setfl_149187+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_setfl_149187+-4)]
	sethi	%hi(strbindvar_r_setfl_149187), %r8
	or	%r8, %lo(strbindvar_r_setfl_149187), %r12
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
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
	sethi	%hi(PLUSNlist_in_149191), %r8
	st	%r9, [%r8+%lo(PLUSNlist_in_149191)]
	sethi	%hi(strbindvar_c_143964), %r8
	or	%r8, %lo(strbindvar_c_143964), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label O_STR
	ld	[%r8+24], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_149194+-4), %r8
	st	%r9, [%r8+%lo(type_149194+-4)]
	sethi	%hi(type_149194), %r8
	or	%r8, %lo(type_149194), %r12
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
	sethi	%hi(type_149194), %r8
	or	%r8, %lo(type_149194), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_165753:
code_164662:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156025+-4), %r9
	st	%r10, [%r9+%lo(type_156025+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164667
	nop
code_164668:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164667:
	sethi	%hi(type_156025), %r9
	or	%r9, %lo(type_156025), %r13
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
	sethi	%hi(type_156025), %r8
	or	%r8, %lo(type_156025), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_149194), %r8
	or	%r8, %lo(type_149194), %r10
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
code_165754:
code_164685:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_154336+-4), %r9
	st	%r10, [%r9+%lo(_154336+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164690
	nop
code_164691:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164690:
	sethi	%hi(_154336), %r9
	or	%r9, %lo(_154336), %r13
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
	sethi	%hi(strbindvar_c_143962), %r9
	or	%r9, %lo(strbindvar_c_143962), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_164704
	nop
code_164705:
	or	%r0, 0, %r9
cmpui_164704:
	cmp	%r9, 0
	be,pn	%icc,else_case_158665
	nop
code_164706:
	or	%r0, 111, %r10
	sethi	%hi(nonblock_149231+-4), %r9
	st	%r10, [%r9+%lo(nonblock_149231+-4)]
	sethi	%hi(nonblock_149231), %r9
	or	%r9, %lo(nonblock_149231), %r13
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
	ba	after_ite_158666
	st	%r8, [%r9]
else_case_158665:
	or	%r0, 9, %r10
	sethi	%hi(nonblock_149231+-4), %r9
	st	%r10, [%r9+%lo(nonblock_149231+-4)]
	or	%r0, 23, %r10
	sethi	%hi(nonblock_149231+4), %r9
	st	%r10, [%r9+%lo(nonblock_149231+4)]
	sethi	%hi(nonblock_149231), %r9
	st	%r8, [%r9+%lo(nonblock_149231)]
after_ite_158666:
	sethi	%hi(OS_STR_r_INT), %r8
	or	%r8, %lo(OS_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r8
	ld	[%r8], %r9
	sethi	%hi(stamp_149397), %r8
	st	%r9, [%r8+%lo(stamp_149397)]
	! start making constructor call
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
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
code_165775:
code_164728:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(reify_156047+-4), %r9
	st	%r10, [%r9+%lo(reify_156047+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164733
	nop
code_164734:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164733:
	sethi	%hi(reify_156047), %r9
	or	%r9, %lo(reify_156047), %r13
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
	sethi	%hi(PLUSNoption_out_149400), %r8
	st	%r9, [%r8+%lo(PLUSNoption_out_149400)]
	sethi	%hi(OS_STR_c_INT), %r8
	or	%r8, %lo(OS_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label syserror_TYC
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_149401+-4), %r8
	st	%r9, [%r8+%lo(type_149401+-4)]
	sethi	%hi(type_149401), %r8
	or	%r8, %lo(type_149401), %r12
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
	sethi	%hi(PLUSEsyserror_149409+-4), %r8
	st	%r9, [%r8+%lo(PLUSEsyserror_149409+-4)]
	sethi	%hi(PLUSEsyserror_149409), %r8
	or	%r8, %lo(PLUSEsyserror_149409), %r12
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
	sethi	%hi(again_149412), %r8
	st	%r9, [%r8+%lo(again_149412)]
	! allocating 3-record
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_164775
	nop
code_164776:
	call	GCFromML ! delay slot empty
	nop
needgc_164775:
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(record_158239), %r8
	or	%r8, %lo(record_158239), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165785:
code_164783:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_149483+-4), %r9
	st	%r10, [%r9+%lo(_149483+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164788
	nop
code_164789:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164788:
	sethi	%hi(_149483), %r9
	or	%r9, %lo(_149483), %r13
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_close_149486+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_close_149486+-4)]
	sethi	%hi(strbindvar_r_close_149486), %r8
	or	%r8, %lo(strbindvar_r_close_149486), %r12
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
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
code_165755:
code_164816:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_149499+-4), %r9
	st	%r10, [%r9+%lo(type_149499+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164821
	nop
code_164822:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164821:
	sethi	%hi(type_149499), %r9
	or	%r9, %lo(type_149499), %r13
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
	sethi	%hi(MINUS_149514+-4), %r8
	st	%r9, [%r8+%lo(MINUS_149514+-4)]
	sethi	%hi(MINUS_149514), %r8
	or	%r8, %lo(MINUS_149514), %r12
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
	ble	needgc_164845
	nop
code_164846:
	call	GCFromML ! delay slot empty
	nop
needgc_164845:
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
	sethi	%hi(type_149547+-4), %r9
	st	%r10, [%r9+%lo(type_149547+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164852
	nop
code_164853:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164852:
	sethi	%hi(type_149547), %r9
	or	%r9, %lo(type_149547), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158817), %r8
	or	%r8, %lo(record_158817), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165756:
code_164867:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156210+-4), %r9
	st	%r10, [%r9+%lo(type_156210+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164872
	nop
code_164873:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164872:
	sethi	%hi(type_156210), %r9
	or	%r9, %lo(type_156210), %r13
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
	ble	needgc_164884
	nop
code_164885:
	call	GCFromML ! delay slot empty
	nop
needgc_164884:
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
	sethi	%hi(type_149565+-4), %r9
	st	%r10, [%r9+%lo(type_149565+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164891
	nop
code_164892:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164891:
	sethi	%hi(type_149565), %r9
	or	%r9, %lo(type_149565), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158840), %r8
	or	%r8, %lo(record_158840), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165757:
code_164906:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156199+-4), %r9
	st	%r10, [%r9+%lo(type_156199+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164911
	nop
code_164912:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164911:
	sethi	%hi(type_156199), %r9
	or	%r9, %lo(type_156199), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r10
	ld	[%r8+4], %r8
	jmpl	%r10, %r15
	ld	[%sp+108], %r9
code_165758:
code_164925:
	! done making constructor call
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158862), %r8
	or	%r8, %lo(record_158862), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165777:
code_164929:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156184+-4), %r9
	st	%r10, [%r9+%lo(type_156184+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164934
	nop
code_164935:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164934:
	sethi	%hi(type_156184), %r9
	or	%r9, %lo(type_156184), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158876), %r8
	or	%r8, %lo(record_158876), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165759:
code_164949:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156169+-4), %r9
	st	%r10, [%r9+%lo(type_156169+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164954
	nop
code_164955:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164954:
	sethi	%hi(type_156169), %r9
	or	%r9, %lo(type_156169), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158890), %r8
	or	%r8, %lo(record_158890), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165760:
code_164969:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156166+-4), %r9
	st	%r10, [%r9+%lo(type_156166+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164974
	nop
code_164975:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164974:
	sethi	%hi(type_156166), %r9
	or	%r9, %lo(type_156166), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_158908), %r8
	or	%r8, %lo(record_158908), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165761:
code_164989:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156163+-4), %r9
	st	%r10, [%r9+%lo(type_156163+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_164994
	nop
code_164995:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_164994:
	sethi	%hi(type_156163), %r9
	or	%r9, %lo(type_156163), %r13
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
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r12
	! Proj_c at label iodesc_TYC
	ld	[%sp+112], %r17
	ld	[%r17], %r18
	or	%r0, 111, %r9
	sethi	%hi(type_154694+-4), %r8
	st	%r9, [%r8+%lo(type_154694+-4)]
	sethi	%hi(type_154694), %r8
	or	%r8, %lo(type_154694), %r13
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
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_154694), %r8
	or	%r8, %lo(type_154694), %r10
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
code_165762:
code_165024:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_154695+-4), %r9
	st	%r10, [%r9+%lo(_154695+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165029
	nop
code_165030:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165029:
	sethi	%hi(_154695), %r9
	or	%r9, %lo(_154695), %r13
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
	sethi	%hi(type_148852), %r8
	or	%r8, %lo(type_148852), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(type_154694), %r8
	or	%r8, %lo(type_154694), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165763:
code_165045:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_149662+-4), %r9
	st	%r10, [%r9+%lo(type_149662+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165050
	nop
code_165051:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165050:
	sethi	%hi(type_149662), %r9
	or	%r9, %lo(type_149662), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(type_154694), %r8
	or	%r8, %lo(type_154694), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165764:
code_165066:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156159+-4), %r9
	st	%r10, [%r9+%lo(type_156159+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165071
	nop
code_165072:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165071:
	sethi	%hi(type_156159), %r9
	or	%r9, %lo(type_156159), %r13
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
	sethi	%hi(type_156163), %r9
	or	%r9, %lo(type_156163), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165085
	nop
code_165086:
	or	%r0, 0, %r9
cmpui_165085:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148827), %r9
	or	%r9, %lo(type_148827), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165089
	nop
code_165090:
	or	%r0, 0, %r9
cmpui_165089:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148831), %r9
	or	%r9, %lo(type_148831), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165093
	nop
code_165094:
	or	%r0, 0, %r9
cmpui_165093:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148835), %r9
	or	%r9, %lo(type_148835), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165097
	nop
code_165098:
	or	%r0, 0, %r9
cmpui_165097:
	sll	%r9, 11, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156159), %r9
	or	%r9, %lo(type_156159), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165101
	nop
code_165102:
	or	%r0, 0, %r9
cmpui_165101:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148827), %r9
	or	%r9, %lo(type_148827), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165105
	nop
code_165106:
	or	%r0, 0, %r9
cmpui_165105:
	sll	%r9, 15, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156184), %r9
	or	%r9, %lo(type_156184), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165109
	nop
code_165110:
	or	%r0, 0, %r9
cmpui_165109:
	sll	%r9, 16, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156169), %r9
	or	%r9, %lo(type_156169), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165113
	nop
code_165114:
	or	%r0, 0, %r9
cmpui_165113:
	sll	%r9, 18, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156210), %r9
	or	%r9, %lo(type_156210), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165117
	nop
code_165118:
	or	%r0, 0, %r9
cmpui_165117:
	sll	%r9, 19, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156199), %r9
	or	%r9, %lo(type_156199), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165121
	nop
code_165122:
	or	%r0, 0, %r9
cmpui_165121:
	sll	%r9, 20, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156166), %r9
	or	%r9, %lo(type_156166), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165125
	nop
code_165126:
	or	%r0, 0, %r9
cmpui_165125:
	sll	%r9, 22, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_149725), %r9
	st	%r8, [%r9+%lo(gctag_149725)]
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label open_mode_TYC
	ld	[%r8+24], %r16
	st	%r16, [%sp+136]
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label O_STR
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_149743+-4), %r8
	st	%r9, [%r8+%lo(type_149743+-4)]
	sethi	%hi(type_149743), %r8
	or	%r8, %lo(type_149743), %r12
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
	ble	needgc_165142
	nop
code_165143:
	call	GCFromML ! delay slot empty
	nop
needgc_165142:
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
	sethi	%hi(type_149743), %r8
	or	%r8, %lo(type_149743), %r9
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
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165786:
code_165153:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_149751+-4), %r9
	st	%r10, [%r9+%lo(_149751+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165158
	nop
code_165159:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165158:
	sethi	%hi(_149751), %r9
	or	%r9, %lo(_149751), %r13
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
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+84], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_openf_149754+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_openf_149754+-4)]
	sethi	%hi(strbindvar_r_openf_149754), %r8
	or	%r8, %lo(strbindvar_r_openf_149754), %r12
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+60], %r8
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+68], %r16
	st	%r16, [%sp+132]
	ld	[%sp+132], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+96]
	! start making constructor call
	sethi	%hi(type_149743), %r8
	or	%r8, %lo(type_149743), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_165765:
code_165188:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156276+-4), %r9
	st	%r10, [%r9+%lo(type_156276+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165193
	nop
code_165194:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165193:
	sethi	%hi(type_156276), %r9
	or	%r9, %lo(type_156276), %r13
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
	sethi	%hi(type_156276), %r8
	or	%r8, %lo(type_156276), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_149743), %r8
	or	%r8, %lo(type_149743), %r10
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
code_165766:
code_165211:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_154752+-4), %r9
	st	%r10, [%r9+%lo(_154752+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165216
	nop
code_165217:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165216:
	sethi	%hi(_154752), %r9
	or	%r9, %lo(_154752), %r13
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
	sethi	%hi(type_149194), %r10
	or	%r10, %lo(type_149194), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_165230
	nop
code_165231:
	or	%r0, 0, %r10
cmpui_165230:
	sll	%r10, 8, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	sethi	%hi(type_156025), %r10
	or	%r10, %lo(type_156025), %r11
	ld	[%r2+804], %r10
	add	%r11, %r10, %r10
	ld	[%r10], %r10
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_165234
	nop
code_165235:
	or	%r0, 0, %r10
cmpui_165234:
	sll	%r10, 9, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	sethi	%hi(gctag_149955), %r10
	st	%r8, [%r10+%lo(gctag_149955)]
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_165237
	nop
code_165238:
	call	GCFromML ! delay slot empty
	nop
needgc_165237:
	sethi	%hi(gctag_149955), %r8
	ld	[%r8+%lo(gctag_149955)], %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	or	%r0, 0, %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	or	%r0, 111, %r10
	sethi	%hi(_149971+-4), %r9
	st	%r10, [%r9+%lo(_149971+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165245
	nop
code_165246:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165245:
	sethi	%hi(_149971), %r9
	or	%r9, %lo(_149971), %r13
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
	sethi	%hi(PLUSEbool_150095+-4), %r8
	st	%r9, [%r8+%lo(PLUSEbool_150095+-4)]
	sethi	%hi(PLUSEbool_150095), %r8
	or	%r8, %lo(PLUSEbool_150095), %r12
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
	ble	needgc_165269
	nop
code_165270:
	call	GCFromML ! delay slot empty
	nop
needgc_165269:
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_149194), %r8
	or	%r8, %lo(type_149194), %r9
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
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(record_158239), %r8
	or	%r8, %lo(record_158239), %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165787:
code_165279:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_150071+-4), %r9
	st	%r10, [%r9+%lo(_150071+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165284
	nop
code_165285:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165284:
	sethi	%hi(_150071), %r9
	or	%r9, %lo(_150071), %r13
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
	ble	needgc_165296
	nop
code_165297:
	call	GCFromML ! delay slot empty
	nop
needgc_165296:
	sethi	%hi(7209), %r8
	or	%r8, %lo(7209), %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 3, %r8
	st	%r8, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_148640), %r8
	or	%r8, %lo(type_148640), %r9
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
	sethi	%hi(type_148926), %r8
	or	%r8, %lo(type_148926), %r9
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
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 2, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165788:
code_165305:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_150134+-4), %r9
	st	%r10, [%r9+%lo(_150134+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 36, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165310
	nop
code_165311:
	sub	%r4, 36, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165310:
	sethi	%hi(_150134), %r9
	or	%r9, %lo(_150134), %r13
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_writeVec_150137+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_writeVec_150137+-4)]
	sethi	%hi(strbindvar_r_writeVec_150137), %r8
	or	%r8, %lo(strbindvar_r_writeVec_150137), %r12
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
	sethi	%hi(IO_148766), %r8
	or	%r8, %lo(IO_148766), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+36], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_writeArr_150159+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_writeArr_150159+-4)]
	sethi	%hi(strbindvar_r_writeArr_150159), %r8
	or	%r8, %lo(strbindvar_r_writeArr_150159), %r12
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(type_149401), %r8
	or	%r8, %lo(type_149401), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165767:
code_165350:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(reify_156512+-4), %r9
	st	%r10, [%r9+%lo(reify_156512+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165355
	nop
code_165356:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165355:
	sethi	%hi(reify_156512), %r9
	or	%r9, %lo(reify_156512), %r13
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
	sethi	%hi(handleBlock_r_144612), %r8
	or	%r8, %lo(handleBlock_r_144612), %r8
	! done allocating 1 closures
	! allocating 2-record
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_165368
	nop
code_165369:
	call	GCFromML ! delay slot empty
	nop
needgc_165368:
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
	sethi	%hi(type_150317+-4), %r9
	st	%r10, [%r9+%lo(type_150317+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165377
	nop
code_165378:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165377:
	sethi	%hi(type_150317), %r9
	or	%r9, %lo(type_150317), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_159339), %r8
	or	%r8, %lo(record_159339), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165768:
code_165392:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156692+-4), %r9
	st	%r10, [%r9+%lo(type_156692+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165397
	nop
code_165398:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165397:
	sethi	%hi(type_156692), %r9
	or	%r9, %lo(type_156692), %r13
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
	ble	needgc_165409
	nop
code_165410:
	call	GCFromML ! delay slot empty
	nop
needgc_165409:
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
	sethi	%hi(type_150361+-4), %r9
	st	%r10, [%r9+%lo(type_150361+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165418
	nop
code_165419:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165418:
	sethi	%hi(type_150361), %r9
	or	%r9, %lo(type_150361), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_159364), %r8
	or	%r8, %lo(record_159364), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165769:
code_165433:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156675+-4), %r9
	st	%r10, [%r9+%lo(type_156675+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165438
	nop
code_165439:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165438:
	sethi	%hi(type_156675), %r9
	or	%r9, %lo(type_156675), %r13
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
	sethi	%hi(type_150317), %r8
	or	%r8, %lo(type_150317), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r13
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(handleBlock_r_144612), %r8
	or	%r8, %lo(handleBlock_r_144612), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165770:
code_165453:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_150408+-4), %r9
	st	%r10, [%r9+%lo(_150408+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165458
	nop
code_165459:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165458:
	sethi	%hi(_150408), %r9
	or	%r9, %lo(_150408), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
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
code_165771:
code_165474:
	! done making constructor call
	! allocating 1-record
	! done allocating 1 record
	! start making constructor call
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_159404), %r8
	or	%r8, %lo(record_159404), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165778:
code_165478:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156646+-4), %r9
	st	%r10, [%r9+%lo(type_156646+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165483
	nop
code_165484:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165483:
	sethi	%hi(type_156646), %r9
	or	%r9, %lo(type_156646), %r13
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
	sethi	%hi(type_150361), %r8
	or	%r8, %lo(type_150361), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r13
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(handleBlock_r_144612), %r8
	or	%r8, %lo(handleBlock_r_144612), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165772:
code_165498:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_150466+-4), %r9
	st	%r10, [%r9+%lo(_150466+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165503
	nop
code_165504:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165503:
	sethi	%hi(_150466), %r9
	or	%r9, %lo(_150466), %r13
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
	sethi	%hi(type_148639), %r8
	or	%r8, %lo(type_148639), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r11
	sethi	%hi(record_159433), %r8
	or	%r8, %lo(record_159433), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_165773:
code_165518:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_156617+-4), %r9
	st	%r10, [%r9+%lo(type_156617+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165523
	nop
code_165524:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165523:
	sethi	%hi(type_156617), %r9
	or	%r9, %lo(type_156617), %r13
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
	sethi	%hi(type_156163), %r9
	or	%r9, %lo(type_156163), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165537
	nop
code_165538:
	or	%r0, 0, %r9
cmpui_165537:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148827), %r9
	or	%r9, %lo(type_148827), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165541
	nop
code_165542:
	or	%r0, 0, %r9
cmpui_165541:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148831), %r9
	or	%r9, %lo(type_148831), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165545
	nop
code_165546:
	or	%r0, 0, %r9
cmpui_165545:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148835), %r9
	or	%r9, %lo(type_148835), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165549
	nop
code_165550:
	or	%r0, 0, %r9
cmpui_165549:
	sll	%r9, 11, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156159), %r9
	or	%r9, %lo(type_156159), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165553
	nop
code_165554:
	or	%r0, 0, %r9
cmpui_165553:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_148827), %r9
	or	%r9, %lo(type_148827), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165557
	nop
code_165558:
	or	%r0, 0, %r9
cmpui_165557:
	sll	%r9, 14, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156692), %r9
	or	%r9, %lo(type_156692), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165561
	nop
code_165562:
	or	%r0, 0, %r9
cmpui_165561:
	sll	%r9, 15, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156675), %r9
	or	%r9, %lo(type_156675), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165565
	nop
code_165566:
	or	%r0, 0, %r9
cmpui_165565:
	sll	%r9, 17, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156646), %r9
	or	%r9, %lo(type_156646), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165569
	nop
code_165570:
	or	%r0, 0, %r9
cmpui_165569:
	sll	%r9, 18, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156617), %r9
	or	%r9, %lo(type_156617), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165573
	nop
code_165574:
	or	%r0, 0, %r9
cmpui_165573:
	sll	%r9, 19, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156166), %r9
	or	%r9, %lo(type_156166), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165577
	nop
code_165578:
	or	%r0, 0, %r9
cmpui_165577:
	sll	%r9, 21, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_150615), %r9
	st	%r8, [%r9+%lo(gctag_150615)]
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
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
	sethi	%hi(strbindvar_c_143962), %r8
	or	%r8, %lo(strbindvar_c_143962), %r9
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
code_165776:
	mov	%r8, %r11
code_165584:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(type_156844+-4), %r8
	st	%r9, [%r8+%lo(type_156844+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_165589
	nop
code_165590:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165589:
	sethi	%hi(type_156844), %r8
	or	%r8, %lo(type_156844), %r10
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
	bgu	cmpui_165601
	nop
code_165602:
	or	%r0, 0, %r9
cmpui_165601:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_156844), %r9
	or	%r9, %lo(type_156844), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165605
	nop
code_165606:
	or	%r0, 0, %r9
cmpui_165605:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	! allocating 2-record
	add	%r4, 72, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_165607
	nop
code_165608:
	call	GCFromML ! delay slot empty
	nop
needgc_165607:
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
	sethi	%hi(_150814+-4), %r9
	st	%r10, [%r9+%lo(_150814+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165614
	nop
code_165615:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165614:
	sethi	%hi(_150814), %r9
	or	%r9, %lo(_150814), %r13
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
	sethi	%hi(type_155341+-4), %r8
	st	%r9, [%r8+%lo(type_155341+-4)]
	sethi	%hi(type_155341), %r8
	or	%r8, %lo(type_155341), %r12
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
	sethi	%hi(type_155341), %r8
	or	%r8, %lo(type_155341), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+140], %r17
	ld	[%r17], %r10
	ld	[%sp+140], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_165774:
	mov	%r8, %r12
code_165638:
	! done making constructor call
	sethi	%hi(type_155341), %r8
	or	%r8, %lo(type_155341), %r9
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
code_165779:
code_165643:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_155346+-4), %r9
	st	%r10, [%r9+%lo(_155346+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_165648
	nop
code_165649:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165648:
	sethi	%hi(_155346), %r9
	or	%r9, %lo(_155346), %r13
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
	ble	needgc_165660
	nop
code_165661:
	call	GCFromML ! delay slot empty
	nop
needgc_165660:
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
	sethi	%hi(type_149743), %r8
	or	%r8, %lo(type_149743), %r9
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
	sethi	%hi(record_158252), %r8
	or	%r8, %lo(record_158252), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	sethi	%hi(file_desc_143966), %r8
	or	%r8, %lo(file_desc_143966), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(announce_r_143983), %r8
	or	%r8, %lo(announce_r_143983), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_165789:
	mov	%r8, %r13
code_165671:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_150846+-4), %r8
	st	%r9, [%r8+%lo(_150846+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 48, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_165676
	nop
code_165677:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_165676:
	sethi	%hi(_150846), %r8
	or	%r8, %lo(_150846), %r12
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
	sethi	%hi(FileSys_148765), %r8
	or	%r8, %lo(FileSys_148765), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8], %r13
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_createf_150849+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_createf_150849+-4)]
	sethi	%hi(strbindvar_r_createf_150849), %r8
	or	%r8, %lo(strbindvar_r_createf_150849), %r12
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
	sethi	%hi(strbindvar_c_143962), %r9
	or	%r9, %lo(strbindvar_c_143962), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165702
	nop
code_165703:
	or	%r0, 0, %r9
cmpui_165702:
	cmp	%r9, 0
	be,pn	%icc,else_case_159720
	nop
code_165704:
	or	%r0, 111, %r10
	sethi	%hi(trunc_150879+-4), %r9
	st	%r10, [%r9+%lo(trunc_150879+-4)]
	sethi	%hi(trunc_150879), %r9
	or	%r9, %lo(trunc_150879), %r13
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
	ba	after_ite_159721
	st	%r8, [%r9]
else_case_159720:
	or	%r0, 9, %r10
	sethi	%hi(trunc_150879+-4), %r9
	st	%r10, [%r9+%lo(trunc_150879+-4)]
	or	%r0, 23, %r10
	sethi	%hi(trunc_150879+4), %r9
	st	%r10, [%r9+%lo(trunc_150879+4)]
	sethi	%hi(trunc_150879), %r9
	st	%r8, [%r9+%lo(trunc_150879)]
after_ite_159721:
	ld	[%sp+132], %r17
	ld	[%r17+24], %r8
	sethi	%hi(strbindvar_c_143962), %r9
	or	%r9, %lo(strbindvar_c_143962), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+20], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_165721
	nop
code_165722:
	or	%r0, 0, %r9
cmpui_165721:
	cmp	%r9, 0
	be,pn	%icc,else_case_159732
	nop
code_165723:
	or	%r0, 111, %r10
	sethi	%hi(append_150921+-4), %r9
	st	%r10, [%r9+%lo(append_150921+-4)]
	sethi	%hi(append_150921), %r9
	or	%r9, %lo(append_150921), %r13
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
	ba	after_ite_159733
	st	%r8, [%r9]
else_case_159732:
	or	%r0, 9, %r10
	sethi	%hi(append_150921+-4), %r9
	st	%r10, [%r9+%lo(append_150921+-4)]
	or	%r0, 23, %r10
	sethi	%hi(append_150921+4), %r9
	st	%r10, [%r9+%lo(append_150921+4)]
	sethi	%hi(append_150921), %r9
	st	%r8, [%r9+%lo(append_150921)]
after_ite_159733:
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(functor_var_r_143490), %r8
	or	%r8, %lo(functor_var_r_143490), %r8
	! done allocating 1 closures
	or	%r0, 256, %r8
code_165740:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size PosixPrimIOFn_main,(.-PosixPrimIOFn_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_165741
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165742
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165743
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165744
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165745
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165746
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165747
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165748
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165749
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165750
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165751
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165752
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165753
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165754
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165755
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165756
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165757
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165758
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165759
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165760
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165761
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165762
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165763
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165764
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165765
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165766
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165767
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165768
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165769
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165770
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165771
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165772
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165773
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165774
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000015
		! -------- label,sizes,reg
	.long code_165775
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165776
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
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
		! -------- label,sizes,reg
	.long afterMutateCheck_163997
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164030
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164166
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164189
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164214
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164234
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164254
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164274
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_164347
	.word 0x00240008
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164363
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_164389
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164403
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164426
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164447
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164470
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164490
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164510
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_164534
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164548
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_164591
	.word 0x00240008
	.word 0x00170000
	.word 0x00000400
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164605
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01500000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_164667
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164690
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164733
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_164775
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164788
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164821
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_164845
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164852
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164872
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_164884
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164891
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164911
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165777
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164934
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164954
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164974
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_164994
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_165029
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_165050
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_165071
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long needgc_165142
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000050
		! -------- label,sizes,reg
	.long afterMutateCheck_165158
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000050
		! -------- label,sizes,reg
	.long afterMutateCheck_165193
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165216
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_165237
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
	.long strbindvar_c_143962
		! -------- label,sizes,reg
	.long afterMutateCheck_165245
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_165269
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165284
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_165296
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165310
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165355
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_165368
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165377
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165397
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long needgc_165409
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165418
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165438
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165458
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165778
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165483
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165503
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165523
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000054
		! -------- label,sizes,reg
	.long afterMutateCheck_165589
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
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
		! -------- label,sizes,reg
	.long needgc_165607
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
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
	.word 0x80000025
	.long strbindvar_c_143962
		! -------- label,sizes,reg
	.long afterMutateCheck_165614
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_165779
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000014
		! -------- label,sizes,reg
	.long afterMutateCheck_165648
	.word 0x00240008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000014
		! -------- label,sizes,reg
	.long needgc_165660
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10000000
	.word 0x00000014
		! -------- label,sizes,reg
	.long afterMutateCheck_165676
	.word 0x00240008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000004
		! -------- label,sizes,reg
	.long code_165780
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165781
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165782
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165783
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01400000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165784
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01500000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_165785
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01540000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_165786
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.word 0x00000050
		! -------- label,sizes,reg
	.long code_165787
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00500000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165788
	.word 0x00240008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00110000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_165789
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
type_148636:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_148639:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_148640:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_c_143962:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_c_143964:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
file_desc_143966:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000211
record_158001:
	.long PosixPrimIOFn_functor_var_c_code_157466
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl PosixPrimIO_FCT_c_INT
PosixPrimIO_FCT_c_INT:
	.long record_158001
	.long record_158001
		! Global
	.word 0x00000037
FileSys_148765:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
IO_148766:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_PLUSEfile_desc_148767:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
fromInt_148768:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
announce_r_143983:
	.long PosixPrimIOFn_announce_r_code_157471
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
strbindvar_r_fstat_148797:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_154115:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_154116:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_154128:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
isRegFile_154100:
	.long PosixPrimIOFn_isRegFile_code_157505
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
isRegFile_143985:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158141:
	.word 0x00000009
		! Global
	.word 0x00000037
type_148827:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158156:
	.word 0x00000009
		! Global
	.word 0x00000037
type_148831:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158170:
	.word 0x00000009
		! Global
	.word 0x00000037
type_148835:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNbool_out_148843:
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNoption_in_148849:
	.word 0x00000102
		! Global
	.word 0x00000037
type_148852:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_148904:
	.word 0x00000102
		! static record tag
	.word 0x00000011
record_158239:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x00000037
mk_148921:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000211
record_158252:
	.word 0x00000001
	.word 0x00000000
		! Global
	.word 0x00000037
type_148926:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_148935:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_158307:
		! string size = 5
	.ascii "lseek"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_lseek_148938:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_148990:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_158354:
		! string size = 5
	.ascii "fstat"
.align 4
		! Global
	.word 0x00000037
_154260:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_149034:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
posFns_143991:
	.long PosixPrimIOFn_posFns_code_157510
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000009
record_158410:
	.word 0x00000009
		! Global
	.word 0x00000037
reify_156226:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158424:
	.word 0x00000009
		! Global
	.word 0x00000037
reify_156224:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158438:
	.word 0x00000009
		! Global
	.word 0x00000037
reify_156222:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
PLUS_149255:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_149285:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000022
string_158493:
		! string size = 4
	.ascii "read"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_readVec_149288:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_length_149296:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
record_gctag_154406:
	.word 0x00000102
		! Global
	.word 0x00000037
_149312:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000003a
string_158571:
		! string size = 7
	.ascii "readBuf"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_readArr_149315:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
strbindvar_r_setfl_149187:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNlist_in_149191:
	.word 0x00000102
		! Global
	.word 0x00000037
type_149194:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_156025:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_154336:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
nonblock_149231:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
stamp_149397:
	.word 0x00000102
		! Global
	.word 0x00000037
reify_156047:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNoption_out_149400:
	.word 0x00000102
		! Global
	.word 0x00000037
type_149401:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
PLUSEsyserror_149409:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
again_149412:
	.word 0x00000102
		! Global
	.word 0x00000037
_149483:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_158749:
		! string size = 5
	.ascii "close"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_close_149486:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_149499:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
MINUS_149514:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158806:
	.word 0x00000000
		! Global
	.word 0x00000037
type_149547:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158817:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156210:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_149565:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158840:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156199:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158862:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156184:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158876:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156169:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158890:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156166:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_158908:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156163:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_154694:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_154695:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_149662:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_156159:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_149725:
	.word 0x00000102
		! Global
	.word 0x00000037
type_149743:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_149751:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_159096:
		! string size = 5
	.ascii "openf"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_openf_149754:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_156276:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_154752:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_149955:
	.word 0x00000102
		! Global
	.word 0x00000037
_149971:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
PLUSEbool_150095:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_150071:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000002a
string_159231:
		! string size = 5
	.ascii "setfl"
.align 4
		! Global
	.word 0x00000037
_150134:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000042
string_159282:
		! string size = 8
	.ascii "writeVec"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_writeVec_150137:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000042
string_159299:
		! string size = 8
	.ascii "writeArr"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_writeArr_150159:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
reify_156512:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
handleBlock_r_144612:
	.long PosixPrimIOFn_handleBlock_r_code_157551
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
type_150317:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_159339:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156692:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_150361:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_159364:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156675:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_150408:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_159404:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156646:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_150466:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_159433:
	.word 0x00000009
		! Global
	.word 0x00000037
type_156617:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_150615:
	.word 0x00000102
		! Global
	.word 0x00000037
type_156844:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_150814:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_155341:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_155346:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_150846:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000003a
string_159700:
		! string size = 7
	.ascii "createf"
.align 4
		! Global
	.word 0x00000037
strbindvar_r_createf_150849:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
trunc_150879:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
append_150921:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x0000006f
	.globl PosixPrimIO_FCT_r_INT
PosixPrimIO_FCT_r_INT:
	.long functor_var_r_143490
	.long functor_var_r_143490
		! static record tag
	.word 0x00000619
functor_var_r_143490:
	.long PosixPrimIOFn_functor_var_r_code_157585
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000009
record_162688:
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
	.long append_150921
	.long trunc_150879
	.long strbindvar_r_createf_150849
	.long _150846
	.long _155346
	.long type_155341
	.long _150814
	.long type_156844
	.long type_156617
	.long _150466
	.long type_156646
	.long _150408
	.long type_156675
	.long type_150361
	.long type_156692
	.long type_150317
	.long reify_156512
	.long strbindvar_r_writeArr_150159
	.long strbindvar_r_writeVec_150137
	.long _150134
	.long _150071
	.long PLUSEbool_150095
	.long _149971
	.long _154752
	.long type_156276
	.long strbindvar_r_openf_149754
	.long _149751
	.long type_149743
	.long type_156159
	.long type_149662
	.long _154695
	.long type_154694
	.long type_156163
	.long type_156166
	.long type_156169
	.long type_156184
	.long type_156199
	.long type_149565
	.long type_156210
	.long type_149547
	.long MINUS_149514
	.long type_149499
	.long strbindvar_r_close_149486
	.long _149483
	.long PLUSEsyserror_149409
	.long type_149401
	.long reify_156047
	.long nonblock_149231
	.long _154336
	.long type_156025
	.long type_149194
	.long strbindvar_r_setfl_149187
	.long strbindvar_r_readArr_149315
	.long _149312
	.long strbindvar_r_length_149296
	.long strbindvar_r_readVec_149288
	.long _149285
	.long PLUS_149255
	.long reify_156222
	.long reify_156224
	.long reify_156226
	.long _149034
	.long _154260
	.long _148990
	.long strbindvar_r_lseek_148938
	.long _148935
	.long type_148926
	.long mk_148921
	.long type_148852
	.long type_148835
	.long type_148831
	.long type_148827
	.long isRegFile_143985
	.long _154128
	.long _154116
	.long type_154115
	.long strbindvar_r_fstat_148797
	.long fromInt_148768
	.long strbindvar_r_PLUSEfile_desc_148767
	.long IO_148766
	.long FileSys_148765
	.long file_desc_143966
	.long strbindvar_c_143964
	.long strbindvar_c_143962
	.long type_148640
	.long type_148639
	.long type_148636
	.globl PosixPrimIOFn_unit_TRACE_GLOBALS_END_VAL
PosixPrimIOFn_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
