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
	.globl LINKUNIT_unit_GCTABLE_BEGIN_VAL
LINKUNIT_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl LINKUNIT_unit_CODE_END_VAL
	.globl LINKUNIT_unit_CODE_BEGIN_VAL
LINKUNIT_unit_CODE_BEGIN_VAL:
	.text
	.align 8
	.global LINKUNIT_polyLen_303
 ! arguments : [$278,$8] [$279,$9] 
 ! results    : [$3051,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
LINKUNIT_polyLen_303:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3075
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_3075:
	st	%r15, [%sp+92]
	mov	%r8, %r10
	mov	%r9, %r8
code_3063:
funtop_3041:
load_nonstall_tag_3048:
	ld	[%r8+-4], %r9
	cmp	%r9, 15
	be	load_nonstall_tag_3048
	nop
load_true_tag_3049:
	and	%r9, 3, %r8
	cmp	%r8, 0
	bne	loaded_tag_3050
	nop
code_3065:
	ba	load_true_tag_3049
	ld	[%r9+-4], %r9
loaded_tag_3050:
	cmp	%r10, 11
	be,pn	%icc,length_float_3054
	nop
code_3067:
	cmp	%r10, 2
	be,pn	%icc,length_word_3056
	nop
code_3068:
	cmp	%r10, 0
	be,pn	%icc,length_char_3055
	nop
length_ptr_3057:
	ba	length_after_3053
	srl	%r9, 5, %r8
length_word_3056:
	ba	length_after_3053
	srl	%r9, 5, %r8
length_float_3054:
	ba	length_after_3053
	srl	%r9, 6, %r8
length_char_3055:
	srl	%r9, 3, %r8
length_after_3053:
code_3074:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size LINKUNIT_polyLen_303,(.-LINKUNIT_polyLen_303)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_polyVlen_304
 ! arguments : [$281,$8] [$282,$9] 
 ! results    : [$3034,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
LINKUNIT_polyVlen_304:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3088
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_3088:
	st	%r15, [%sp+92]
	mov	%r8, %r10
	mov	%r9, %r8
code_3076:
funtop_3024:
load_nonstall_tag_3031:
	ld	[%r8+-4], %r9
	cmp	%r9, 15
	be	load_nonstall_tag_3031
	nop
load_true_tag_3032:
	and	%r9, 3, %r8
	cmp	%r8, 0
	bne	loaded_tag_3033
	nop
code_3078:
	ba	load_true_tag_3032
	ld	[%r9+-4], %r9
loaded_tag_3033:
	cmp	%r10, 11
	be,pn	%icc,length_float_3037
	nop
code_3080:
	cmp	%r10, 2
	be,pn	%icc,length_word_3039
	nop
code_3081:
	cmp	%r10, 0
	be,pn	%icc,length_char_3038
	nop
length_ptr_3040:
	ba	length_after_3036
	srl	%r9, 5, %r8
length_word_3039:
	ba	length_after_3036
	srl	%r9, 5, %r8
length_float_3037:
	ba	length_after_3036
	srl	%r9, 6, %r8
length_char_3038:
	srl	%r9, 3, %r8
length_after_3036:
code_3087:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size LINKUNIT_polyVlen_304,(.-LINKUNIT_polyVlen_304)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_polySub_305
 ! arguments : [$284,$8] [$285,$9] [$286,$10] 
 ! results    : [$3005,$8] 
 ! destroys   :  $f0 $10 $9 $8
 ! modifies   :  $f0 $10 $9 $8
LINKUNIT_polySub_305:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3103
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3103:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_3089:
funtop_2998:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3090
	nop
code_3091:
	call	GCFromML ! delay slot empty
	nop
needgc_3090:
	ld	[%sp+96], %r17
	cmp	%r17, 11
	be,pn	%icc,sub_float_3007
	nop
code_3093:
	ld	[%sp+96], %r17
	cmp	%r17, 2
	be,pn	%icc,sub_word_3009
	nop
code_3094:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	be,pn	%icc,sub_char_3008
	nop
sub_ptr_3010:
	! ptr sub start
	sll	%r10, 2, %r16
	add	%r16, %r9, %r8
	ld	[%r8], %r8
	! ptr sub end
	ba	sub_after_3006 ! delay slot empty
	nop
sub_word_3009:
	! int sub start
	sll	%r10, 2, %r16
	add	%r16, %r9, %r8
	ld	[%r8], %r8
	! int sub end
	ba	sub_after_3006 ! delay slot empty
	nop
sub_char_3008:
	! int sub start
	add	%r10, %r9, %r8
	ldub	[%r8], %r8
	! int sub end
	ba	sub_after_3006 ! delay slot empty
	nop
sub_float_3007:
	sll	%r10, 3, %r16
	add	%r16, %r9, %r8
	ldd	[%r8], %f0
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_3099
	nop
code_3100:
	nop
	or	%r0, %r8, %r4
cmv_3099:
	or	%r0, 67, %r8
	st	%r8, [%r4]
	std	%f0, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
sub_after_3006:
code_3102:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_polySub_305,(.-LINKUNIT_polySub_305)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3090
	.word 0x001c0007
	.word 0x00170000
	.word 0xbffc3a00
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
	.align 8
	.global LINKUNIT_polyVsub_306
 ! arguments : [$288,$8] [$289,$9] [$290,$10] 
 ! results    : [$2979,$8] 
 ! destroys   :  $f0 $10 $9 $8
 ! modifies   :  $f0 $10 $9 $8
LINKUNIT_polyVsub_306:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3118
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3118:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_3104:
funtop_2972:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3105
	nop
code_3106:
	call	GCFromML ! delay slot empty
	nop
needgc_3105:
	ld	[%sp+96], %r17
	cmp	%r17, 11
	be,pn	%icc,sub_float_2981
	nop
code_3108:
	ld	[%sp+96], %r17
	cmp	%r17, 2
	be,pn	%icc,sub_word_2983
	nop
code_3109:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	be,pn	%icc,sub_char_2982
	nop
sub_ptr_2984:
	! ptr sub start
	sll	%r10, 2, %r16
	add	%r16, %r9, %r8
	ld	[%r8], %r8
	! ptr sub end
	ba	sub_after_2980 ! delay slot empty
	nop
sub_word_2983:
	! int sub start
	sll	%r10, 2, %r16
	add	%r16, %r9, %r8
	ld	[%r8], %r8
	! int sub end
	ba	sub_after_2980 ! delay slot empty
	nop
sub_char_2982:
	! int sub start
	add	%r10, %r9, %r8
	ldub	[%r8], %r8
	! int sub end
	ba	sub_after_2980 ! delay slot empty
	nop
sub_float_2981:
	sll	%r10, 3, %r16
	add	%r16, %r9, %r8
	ldd	[%r8], %f0
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_3114
	nop
code_3115:
	nop
	or	%r0, %r8, %r4
cmv_3114:
	or	%r0, 67, %r8
	st	%r8, [%r4]
	std	%f0, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
sub_after_2980:
code_3117:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_polyVsub_306,(.-LINKUNIT_polyVsub_306)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3105
	.word 0x001c0007
	.word 0x00170000
	.word 0xbffc3a00
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.text
	.align 8
	.global LINKUNIT_polyUpdate_307
 ! arguments : [$292,$8] [$293,$9] [$294,$10] [$295,$11] 
 ! results    : [$2971,$8] 
 ! destroys   :  $f0 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f0 $13 $12 $11 $10 $9 $8
LINKUNIT_polyUpdate_307:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3178
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3178:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_3119:
funtop_2954:
	ld	[%sp+96], %r17
	cmp	%r17, 11
	be,pn	%icc,update_float_2963
	nop
code_3120:
	ld	[%sp+96], %r17
	cmp	%r17, 2
	be,pn	%icc,update_int_2964
	nop
code_3121:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	be,pn	%icc,update_char_2965
	nop
update_ptr_2966:
	ld	[%r2+792], %r8
	ld	[%r2+796], %r12
	add	%r8, 12, %r8
	cmp	%r8, %r12
	ble	afterMutateCheck_3126
	nop
code_3127:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_3126:
	sll	%r10, 2, %r13
	add	%r13, %r0, %r13
	ld	[%r2+792], %r12
	mov	%r9, %r10
	mov	%r13, %r8
	st	%r10, [%r12]
	st	%r8, [%r12+4]
	add	%r9, %r13, %r8
	ld	[%r8], %r8
	st	%r8, [%r12+8]
	add	%r12, 12, %r8
	st	%r8, [%r2+792]
	add	%r9, %r13, %r8
	ba	update_after_2962
	st	%r11, [%r8]
update_int_2964:
	ld	[%r2+792], %r8
	ld	[%r2+796], %r12
	add	%r8, 12, %r8
	cmp	%r8, %r12
	ble	afterMutateCheck_3141
	nop
code_3142:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_3141:
	sll	%r10, 2, %r13
	add	%r13, %r0, %r13
	ld	[%r2+792], %r12
	mov	%r9, %r10
	mov	%r13, %r8
	st	%r10, [%r12]
	st	%r8, [%r12+4]
	add	%r12, 12, %r8
	st	%r8, [%r2+792]
	add	%r9, %r13, %r8
	ba	update_after_2962
	st	%r11, [%r8]
update_char_2965:
	ld	[%r2+792], %r8
	ld	[%r2+796], %r12
	add	%r8, 12, %r8
	cmp	%r8, %r12
	ble	afterMutateCheck_3154
	nop
code_3155:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_3154:
	ld	[%r2+792], %r13
	mov	%r9, %r12
	mov	%r10, %r8
	st	%r12, [%r13]
	st	%r8, [%r13+4]
	add	%r13, 12, %r8
	st	%r8, [%r2+792]
	add	%r9, %r10, %r8
	ba	update_after_2962
	stub	%r11, [%r8]
update_float_2963:
	ldd	[%r11], %f0
	ld	[%r2+792], %r8
	ld	[%r2+796], %r11
	add	%r8, 12, %r8
	cmp	%r8, %r11
	ble	afterMutateCheck_3167
	nop
code_3168:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_3167:
	sll	%r10, 3, %r12
	add	%r12, %r0, %r12
	ld	[%r2+792], %r11
	mov	%r9, %r10
	mov	%r12, %r8
	st	%r10, [%r11]
	st	%r8, [%r11+4]
	add	%r11, 12, %r8
	st	%r8, [%r2+792]
	add	%r9, %r12, %r8
	std	%f0, [%r8]
update_after_2962:
	or	%r0, 256, %r8
code_3177:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_polyUpdate_307,(.-LINKUNIT_polyUpdate_307)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_3126
	.word 0x001c0009
	.word 0x00170000
	.word 0xbffc0200
	.word 0xbffc0800
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long afterMutateCheck_3141
	.word 0x001c0009
	.word 0x00170000
	.word 0xbffc0200
	.word 0xbffc0800
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long afterMutateCheck_3154
	.word 0x001c0009
	.word 0x00170000
	.word 0xbffc0200
	.word 0xbffc0800
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long afterMutateCheck_3167
	.word 0x001c0007
	.word 0x00170000
	.word 0xbffc0200
	.word 0xbffc0000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global LINKUNIT_polyArray_308
 ! arguments : [$297,$8] [$298,$9] [$299,$10] 
 ! results    : [$2890,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_polyArray_308:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3229
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3229:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
	mov	%r10, %r9
code_3179:
funtop_2884:
	ld	[%sp+96], %r17
	cmp	%r17, 11
	be,pn	%icc,array_float_2893
	nop
code_3180:
	ld	[%sp+96], %r17
	cmp	%r17, 2
	be,pn	%icc,array_int_2894
	nop
code_3181:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	be,pn	%icc,array_char_2895
	nop
code_3182:
	or	%r0, 510, %r10
	cmp	%r8, %r10
	ble	array_ptr_alloc_2902
	nop
code_3183:
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigptrarray ! delay slot empty
	nop
code_3225:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_3184:
	ba	array_ptr_aftert_2901 ! delay slot empty
	nop
array_ptr_alloc_2902:
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
	ble	needgc_3186
	nop
code_3187:
	call	GCFromML ! delay slot empty
	nop
needgc_3186:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_2908
	add	%r10, %r0, %r10
array_init_loopto_2909:
	add	%r11, %r10, %r8
	st	%r9, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_2908:
	cmp	%r10, 0
	bge	array_init_loopto_2909
	nop
array_ptr_aftert_2901:
	ba	array_after_2892
	mov	%r11, %r8
array_int_2894:
	! initializing int/ptr array start
	sll	%r8, 2, %r11
	add	%r11, %r0, %r11
	or	%r0, 510, %r10
	cmp	%r8, %r10
	ble	array_int_small_2914
	nop
code_3193:
	call	save_regs_MLtoC
	mov	%r11, %r8
	call	alloc_bigintarray ! delay slot empty
	nop
code_3226:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_3194:
	ba	array_int_after_2913 ! delay slot empty
	nop
array_int_small_2914:
	sll	%r11, 3, %r11
	add	%r11, %r0, %r11
	or	%r11, 2, %r11
	or	%r0, 1, %r10
	add	%r10, %r8, %r10
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3196
	nop
code_3197:
	call	GCFromML ! delay slot empty
	nop
needgc_3196:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_2921
	add	%r10, %r0, %r10
array_init_loopto_2922:
	add	%r11, %r10, %r8
	st	%r9, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_2921:
	cmp	%r10, 0
	bge	array_init_loopto_2922
	nop
array_int_after_2913:
	ba	array_after_2892
	mov	%r11, %r8
array_char_2895:
	! initializing int/ptr array start
	add	%r8, 3, %r10
	srl	%r10, 2, %r11
	sll	%r9, 8, %r10
	add	%r10, %r0, %r10
	or	%r10, %r9, %r10
	sll	%r10, 16, %r12
	add	%r12, %r0, %r12
	or	%r12, %r10, %r12
	or	%r0, 510, %r9
	cmp	%r11, %r9
	ble	array_int_small_2930
	nop
code_3203:
	call	save_regs_MLtoC
	mov	%r12, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_3227:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_3204:
	ba	array_int_after_2929 ! delay slot empty
	nop
array_int_small_2930:
	sll	%r8, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r11, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3206
	nop
code_3207:
	call	GCFromML ! delay slot empty
	nop
needgc_3206:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r10
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r11, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_2937
	add	%r9, %r0, %r9
array_init_loopto_2938:
	add	%r10, %r9, %r8
	st	%r12, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_2937:
	cmp	%r9, 0
	bge	array_init_loopto_2938
	nop
array_int_after_2929:
	ba	array_after_2892
	mov	%r10, %r8
array_float_2893:
	ldd	[%r9], %f0
	add	%r8, %r8, %r10
	add	%r10, 2, %r10
	or	%r0, 510, %r9
	cmp	%r10, %r9
	ble	array_float_smallalloc_2947
	nop
code_3213:
	std	%f0, [%r2+392]
	ld	[%r2+392], %r9
	ld	[%r2+396], %r10
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_3228:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_3214:
	ba	array_float_after_2948 ! delay slot empty
	nop
array_float_smallalloc_2947:
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3216
	nop
code_3217:
	call	GCFromML ! delay slot empty
	nop
needgc_3216:
	or	%r0, 23, %r10
	st	%r10, [%r4]
	and	%r4, 4, %r10
	add	%r4, 4, %r9
	cmp	%r10, 0
	bne	cmv_3219
	nop
code_3220:
	nop
	or	%r0, %r9, %r4
cmv_3219:
	sll	%r8, 3, %r9
	add	%r9, %r0, %r9
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 3, %r9
	st	%r9, [%r4]
	add	%r4, 4, %r10
	sll	%r8, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r9
	st	%r9, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_2949
	sub	%r8, 1, %r9
array_float_top_2950:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	std	%f0, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_2949:
	cmp	%r9, 0
	bge	array_float_top_2950
	nop
array_float_after_2948:
	mov	%r10, %r8
array_after_2892:
code_3224:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_polyArray_308,(.-LINKUNIT_polyArray_308)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_3225
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3186
	.word 0x001c0009
	.word 0x00170000
	.word 0x80000000
	.word 0x80000200
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_3226
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3196
	.word 0x001c0009
	.word 0x00170000
	.word 0x80000000
	.word 0x80000200
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_3227
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3206
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3228
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3216
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global LINKUNIT_polyVector_309
 ! arguments : [$300,$8] [$301,$9] [$302,$10] 
 ! results    : [$2820,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_polyVector_309:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3280
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3280:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
	mov	%r10, %r9
code_3230:
funtop_2814:
	ld	[%sp+96], %r17
	cmp	%r17, 11
	be,pn	%icc,array_float_2823
	nop
code_3231:
	ld	[%sp+96], %r17
	cmp	%r17, 2
	be,pn	%icc,array_int_2824
	nop
code_3232:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	be,pn	%icc,array_char_2825
	nop
code_3233:
	or	%r0, 510, %r10
	cmp	%r8, %r10
	ble	array_ptr_alloc_2832
	nop
code_3234:
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigptrarray ! delay slot empty
	nop
code_3276:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_3235:
	ba	array_ptr_aftert_2831 ! delay slot empty
	nop
array_ptr_alloc_2832:
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
	ble	needgc_3237
	nop
code_3238:
	call	GCFromML ! delay slot empty
	nop
needgc_3237:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_2838
	add	%r10, %r0, %r10
array_init_loopto_2839:
	add	%r11, %r10, %r8
	st	%r9, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_2838:
	cmp	%r10, 0
	bge	array_init_loopto_2839
	nop
array_ptr_aftert_2831:
	ba	array_after_2822
	mov	%r11, %r8
array_int_2824:
	! initializing int/ptr array start
	sll	%r8, 2, %r11
	add	%r11, %r0, %r11
	or	%r0, 510, %r10
	cmp	%r8, %r10
	ble	array_int_small_2844
	nop
code_3244:
	call	save_regs_MLtoC
	mov	%r11, %r8
	call	alloc_bigintarray ! delay slot empty
	nop
code_3277:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_3245:
	ba	array_int_after_2843 ! delay slot empty
	nop
array_int_small_2844:
	sll	%r11, 3, %r11
	add	%r11, %r0, %r11
	or	%r11, 2, %r11
	or	%r0, 1, %r10
	add	%r10, %r8, %r10
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3247
	nop
code_3248:
	call	GCFromML ! delay slot empty
	nop
needgc_3247:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r10, 2, %r10
	add	%r10, %r0, %r10
	add	%r4, %r10, %r4
	sub	%r8, 1, %r10
	sll	%r10, 2, %r10
	ba	array_init_loopcheck_2851
	add	%r10, %r0, %r10
array_init_loopto_2852:
	add	%r11, %r10, %r8
	st	%r9, [%r8]
	sub	%r10, 4, %r10
array_init_loopcheck_2851:
	cmp	%r10, 0
	bge	array_init_loopto_2852
	nop
array_int_after_2843:
	ba	array_after_2822
	mov	%r11, %r8
array_char_2825:
	! initializing int/ptr array start
	add	%r8, 3, %r10
	srl	%r10, 2, %r11
	sll	%r9, 8, %r10
	add	%r10, %r0, %r10
	or	%r10, %r9, %r10
	sll	%r10, 16, %r12
	add	%r12, %r0, %r12
	or	%r12, %r10, %r12
	or	%r0, 510, %r9
	cmp	%r11, %r9
	ble	array_int_small_2860
	nop
code_3254:
	call	save_regs_MLtoC
	mov	%r12, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_3278:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_3255:
	ba	array_int_after_2859 ! delay slot empty
	nop
array_int_small_2860:
	sll	%r8, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r11, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3257
	nop
code_3258:
	call	GCFromML ! delay slot empty
	nop
needgc_3257:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r10
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r11, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_2867
	add	%r9, %r0, %r9
array_init_loopto_2868:
	add	%r10, %r9, %r8
	st	%r12, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_2867:
	cmp	%r9, 0
	bge	array_init_loopto_2868
	nop
array_int_after_2859:
	ba	array_after_2822
	mov	%r10, %r8
array_float_2823:
	ldd	[%r9], %f0
	add	%r8, %r8, %r10
	add	%r10, 2, %r10
	or	%r0, 510, %r9
	cmp	%r10, %r9
	ble	array_float_smallalloc_2877
	nop
code_3264:
	std	%f0, [%r2+392]
	ld	[%r2+392], %r9
	ld	[%r2+396], %r10
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_3279:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_3265:
	ba	array_float_after_2878 ! delay slot empty
	nop
array_float_smallalloc_2877:
	sll	%r10, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3267
	nop
code_3268:
	call	GCFromML ! delay slot empty
	nop
needgc_3267:
	or	%r0, 23, %r10
	st	%r10, [%r4]
	and	%r4, 4, %r10
	add	%r4, 4, %r9
	cmp	%r10, 0
	bne	cmv_3270
	nop
code_3271:
	nop
	or	%r0, %r9, %r4
cmv_3270:
	sll	%r8, 3, %r9
	add	%r9, %r0, %r9
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 3, %r9
	st	%r9, [%r4]
	add	%r4, 4, %r10
	sll	%r8, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r9
	st	%r9, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_2879
	sub	%r8, 1, %r9
array_float_top_2880:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	std	%f0, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_2879:
	cmp	%r9, 0
	bge	array_float_top_2880
	nop
array_float_after_2878:
	mov	%r10, %r8
array_after_2822:
code_3275:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_polyVector_309,(.-LINKUNIT_polyVector_309)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_3276
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3237
	.word 0x001c0009
	.word 0x00170000
	.word 0x80000000
	.word 0x80000200
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_3277
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3247
	.word 0x001c0009
	.word 0x00170000
	.word 0x80000000
	.word 0x80000200
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_3278
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3257
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3279
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_3267
	.word 0x001c0007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global LINKUNIT_onearg0_code_507
 ! arguments : [$509,$8] [$510,$9] [$324,$10] 
 ! results    : [$2813,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg0_code_507:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3286
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3286:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_3281:
funtop_2804:
	! making closure polycall
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r10, %r0
	add	%sp, 112, %sp
code_3282:
	! done making tail call
code_3284:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_onearg0_code_507,(.-LINKUNIT_onearg0_code_507)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_onearg1_code_516
 ! arguments : [$518,$8] [$519,$9] [$327,$10] 
 ! results    : [$2803,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg1_code_516:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3292
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3292:
	st	%r15, [%sp+92]
code_3287:
funtop_2787:
	! Proj_c at label _326_INT
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! Proj_c at label range_322_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+100]
	ld	[%r10], %r10
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3288:
	! done making tail call
code_3290:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_onearg1_code_516,(.-LINKUNIT_onearg1_code_516)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_onearg2_code_527
 ! arguments : [$529,$8] [$530,$9] [$331,$10] 
 ! results    : [$2786,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg2_code_527:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3298
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3298:
	st	%r15, [%sp+92]
	mov	%r10, %r11
code_3293:
funtop_2764:
	! Proj_c at label _330_INT
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label _329_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+96]
	! Proj_c at label range_322_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+104]
	ld	[%r11], %r10
	ld	[%r11+4], %r11
	! making closure call
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_3294:
	! done making tail call
code_3296:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_onearg2_code_527,(.-LINKUNIT_onearg2_code_527)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_onearg3_code_540
 ! arguments : [$542,$8] [$543,$9] [$336,$10] 
 ! results    : [$2763,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg3_code_540:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3304
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3304:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_3299:
funtop_2735:
	! Proj_c at label _335_INT
	ld	[%r8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label _334_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+100]
	! Proj_c at label _333_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+96]
	! Proj_c at label range_322_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+108]
	ld	[%r12], %r10
	ld	[%r12+4], %r11
	ld	[%r12+8], %r12
	! making closure call
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 112, %sp
code_3300:
	! done making tail call
code_3302:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_onearg3_code_540,(.-LINKUNIT_onearg3_code_540)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_onearg4_code_555
 ! arguments : [$557,$8] [$558,$9] [$342,$10] 
 ! results    : [$2734,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg4_code_555:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3310
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_3310:
	st	%r15, [%sp+92]
	mov	%r10, %r13
code_3305:
funtop_2700:
	! Proj_c at label _341_INT
	ld	[%r8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label _340_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+104]
	! Proj_c at label _339_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+100]
	! Proj_c at label _338_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+96]
	! Proj_c at label range_322_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+112]
	ld	[%r13], %r10
	ld	[%r13+4], %r11
	ld	[%r13+8], %r12
	ld	[%r13+12], %r13
	! making closure call
	ld	[%r9], %r18
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r18, %r0
	add	%sp, 128, %sp
code_3306:
	! done making tail call
code_3308:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size LINKUNIT_onearg4_code_555,(.-LINKUNIT_onearg4_code_555)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_onearg5_code_572
 ! arguments : [$574,$8] [$575,$9] [$349,$10] 
 ! results    : [$2699,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg5_code_572:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3316
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_3316:
	st	%r15, [%sp+92]
	mov	%r10, %r18
code_3311:
funtop_2659:
	! Proj_c at label _348_INT
	ld	[%r8], %r16
	st	%r16, [%sp+112]
	! Proj_c at label _347_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+108]
	! Proj_c at label _346_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+104]
	! Proj_c at label _345_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+100]
	! Proj_c at label _344_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+96]
	! Proj_c at label range_322_INT
	ld	[%r8+20], %r16
	st	%r16, [%sp+116]
	ld	[%r18], %r10
	ld	[%r18+4], %r11
	ld	[%r18+8], %r12
	ld	[%r18+12], %r13
	ld	[%r18+16], %r18
	! making closure call
	ld	[%r9], %r19
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r19, %r0
	add	%sp, 128, %sp
code_3312:
	! done making tail call
code_3314:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size LINKUNIT_onearg5_code_572,(.-LINKUNIT_onearg5_code_572)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_onearg6_code_591
 ! arguments : [$593,$8] [$594,$9] [$357,$10] 
 ! results    : [$2658,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg6_code_591:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3322
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_3322:
	st	%r15, [%sp+92]
	mov	%r10, %r19
code_3317:
funtop_2612:
	! Proj_c at label _356_INT
	ld	[%r8], %r16
	st	%r16, [%sp+116]
	! Proj_c at label _355_INT
	ld	[%r8+4], %r16
	st	%r16, [%sp+112]
	! Proj_c at label _354_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+108]
	! Proj_c at label _353_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+104]
	! Proj_c at label _352_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+100]
	! Proj_c at label _351_INT
	ld	[%r8+20], %r16
	st	%r16, [%sp+96]
	! Proj_c at label range_322_INT
	ld	[%r8+24], %r16
	st	%r16, [%sp+120]
	ld	[%r19], %r10
	ld	[%r19+4], %r11
	ld	[%r19+8], %r12
	ld	[%r19+12], %r13
	ld	[%r19+16], %r18
	ld	[%r19+20], %r19
	! making closure call
	ld	[%r9], %r20
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r20, %r0
	add	%sp, 128, %sp
code_3318:
	! done making tail call
code_3320:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size LINKUNIT_onearg6_code_591,(.-LINKUNIT_onearg6_code_591)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_onearg_code_502
 ! arguments : [$504,$8] [$321,$9] [$322,$10] [$505,$11] [$323,$12] 
 ! results    : [$2484,$8] 
 ! destroys   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_onearg_code_502:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3352
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_3352:
	st	%r15, [%sp+92]
code_3323:
funtop_2464:
	add	%r4, 244, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3324
	nop
code_3325:
	call	GCFromML ! delay slot empty
	nop
needgc_3324:
	cmp	%r9, 255
	ble,pn	%icc,defaultTypecase_2469
	nop
code_3327:
	ld	[%r9], %r8
	cmp	%r8, 5
	bne,pn	%icc,defaultTypecase_2469
	nop
code_3328:
	ld	[%r9+4], %r8
typecasearm_2472:
	cmp	%r8, 0
	bne	exnarm_2473
	nop
code_3329:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_onearg0_code_507), %r8
	or	%r8, %lo(LINKUNIT_onearg0_code_507), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2468 ! delay slot empty
	nop
exnarm_2473:
	cmp	%r8, 1
	bne	exnarm_2485
	nop
code_3332:
	ld	[%r9+8], %r9
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_onearg1_code_516), %r8
	or	%r8, %lo(LINKUNIT_onearg1_code_516), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2468 ! delay slot empty
	nop
exnarm_2485:
	cmp	%r8, 2
	bne	exnarm_2501
	nop
code_3335:
	ld	[%r9+8], %r11
	ld	[%r9+12], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_onearg2_code_527), %r8
	or	%r8, %lo(LINKUNIT_onearg2_code_527), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2468 ! delay slot empty
	nop
exnarm_2501:
	cmp	%r8, 3
	bne	exnarm_2519
	nop
code_3338:
	ld	[%r9+8], %r13
	ld	[%r9+12], %r11
	ld	[%r9+16], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r10, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_onearg3_code_540), %r8
	or	%r8, %lo(LINKUNIT_onearg3_code_540), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2468 ! delay slot empty
	nop
exnarm_2519:
	cmp	%r8, 4
	bne	exnarm_2539
	nop
code_3341:
	ld	[%r9+8], %r18
	ld	[%r9+12], %r13
	ld	[%r9+16], %r11
	ld	[%r9+20], %r9
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r18, [%r4+16]
	st	%r10, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_onearg4_code_555), %r8
	or	%r8, %lo(LINKUNIT_onearg4_code_555), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2468 ! delay slot empty
	nop
exnarm_2539:
	cmp	%r8, 5
	bne	exnarm_2561
	nop
code_3344:
	ld	[%r9+8], %r19
	ld	[%r9+12], %r18
	ld	[%r9+16], %r13
	ld	[%r9+20], %r11
	ld	[%r9+24], %r9
	! allocating 1 closures
	! allocating 6-record
	sethi	%hi(16177), %r8
	or	%r8, %lo(16177), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r18, [%r4+16]
	st	%r19, [%r4+20]
	st	%r10, [%r4+24]
	add	%r4, 4, %r9
	add	%r4, 28, %r4
	! done allocating 6 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_onearg5_code_572), %r8
	or	%r8, %lo(LINKUNIT_onearg5_code_572), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2468 ! delay slot empty
	nop
exnarm_2561:
	cmp	%r8, 6
	bne	exnarm_2585
	nop
code_3347:
	ld	[%r9+8], %r20
	ld	[%r9+12], %r19
	ld	[%r9+16], %r18
	ld	[%r9+20], %r13
	ld	[%r9+24], %r11
	ld	[%r9+28], %r9
	! allocating 1 closures
	! allocating 7-record
	sethi	%hi(32569), %r8
	or	%r8, %lo(32569), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r18, [%r4+16]
	st	%r19, [%r4+20]
	st	%r20, [%r4+24]
	st	%r10, [%r4+28]
	add	%r4, 4, %r9
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_onearg6_code_591), %r8
	or	%r8, %lo(LINKUNIT_onearg6_code_591), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2468 ! delay slot empty
	nop
exnarm_2585:
defaultTypecase_2469:
	mov	%r12, %r8
afterTypecase_2468:
code_3351:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size LINKUNIT_onearg_code_502,(.-LINKUNIT_onearg_code_502)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3324
	.word 0x00180007
	.word 0x00170000
	.word 0xbfe01600
	.word 0xbfe00000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global LINKUNIT_vararg0_code_617
 ! arguments : [$619,$8] [$620,$9] 
 ! results    : [$2463,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg0_code_617:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3358
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3358:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_3353:
funtop_2452:
	or	%r0, 256, %r10
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3354:
	! done making tail call
code_3356:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_vararg0_code_617,(.-LINKUNIT_vararg0_code_617)

	.section	".rodata"
	.text
	.align 8
	.global LINKUNIT_vararg1_code_626
 ! arguments : [$628,$8] [$629,$9] [$365,$10] 
 ! results    : [$2451,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg1_code_626:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3369
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3369:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r12
code_3359:
funtop_2430:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3360
	nop
code_3361:
	call	GCFromML ! delay slot empty
	nop
needgc_3360:
	! Proj_c at label _364_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	! Proj_c at label range_360_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+100]
	or	%r0, 9, %r9
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_3363
	nop
code_3364:
	or	%r0, 0, %r8
cmpui_3363:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 1-record
	st	%r9, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r10
	add	%r4, 8, %r4
	! done allocating 1 record
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3365:
	! done making tail call
code_3367:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_vararg1_code_626,(.-LINKUNIT_vararg1_code_626)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3360
	.word 0x001c0009
	.word 0x00170000
	.word 0x00001000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global LINKUNIT_vararg2_code_637
 ! arguments : [$639,$8] [$640,$9] [$372,$10] [$373,$11] 
 ! results    : [$2429,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg2_code_637:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3382
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3382:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r12
	mov	%r10, %r13
code_3370:
funtop_2402:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3371
	nop
code_3372:
	call	GCFromML ! delay slot empty
	nop
needgc_3371:
	! Proj_c at label _371_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	! Proj_c at label _370_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! Proj_c at label range_360_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r16
	st	%r16, [%sp+100]
	or	%r0, 17, %r9
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_3374
	nop
code_3375:
	or	%r0, 0, %r8
cmpui_3374:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	cmp	%r10, 3
	or	%r0, 1, %r8
	bgu	cmpui_3376
	nop
code_3377:
	or	%r0, 0, %r8
cmpui_3376:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 2-record
	st	%r9, [%r4]
	st	%r13, [%r4+4]
	st	%r11, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! making closure call
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	ld	[%r12+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3378:
	! done making tail call
code_3380:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_vararg2_code_637,(.-LINKUNIT_vararg2_code_637)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3371
	.word 0x001c000b
	.word 0x00170000
	.word 0x00001000
	.word 0x00002800
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.word 0x00000002
	.word 0x00000060
	.text
	.align 8
	.global LINKUNIT_vararg3_code_650
 ! arguments : [$652,$8] [$653,$9] [$381,$10] [$382,$11] [$383,$12] 
 ! results    : [$2401,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg3_code_650:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3397
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3397:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r19
	mov	%r11, %r18
	mov	%r12, %r13
code_3383:
funtop_2368:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3384
	nop
code_3385:
	call	GCFromML ! delay slot empty
	nop
needgc_3384:
	! Proj_c at label _380_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	! Proj_c at label _379_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r11
	! Proj_c at label _378_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r8
	! Proj_c at label range_360_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r16
	st	%r16, [%sp+100]
	or	%r0, 25, %r10
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_3387
	nop
code_3388:
	or	%r0, 0, %r8
cmpui_3387:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	cmp	%r11, 3
	or	%r0, 1, %r8
	bgu	cmpui_3389
	nop
code_3390:
	or	%r0, 0, %r8
cmpui_3389:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	cmp	%r12, 3
	or	%r0, 1, %r8
	bgu	cmpui_3391
	nop
code_3392:
	or	%r0, 0, %r8
cmpui_3391:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r10, %r10
	! allocating 3-record
	st	%r10, [%r4]
	st	%r19, [%r4+4]
	st	%r18, [%r4+8]
	st	%r13, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3393:
	! done making tail call
code_3395:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_vararg3_code_650,(.-LINKUNIT_vararg3_code_650)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3384
	.word 0x001c000d
	.word 0x00170000
	.word 0x00000200
	.word 0x000c2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000001
	.word 0x00000060
	.word 0x00000002
	.word 0x00000060
	.word 0x00000003
	.word 0x00000060
	.text
	.align 8
	.global LINKUNIT_vararg4_code_665
 ! arguments : [$667,$8] [$668,$9] [$392,$10] [$393,$11] [$394,$12] [$395,$13] 
 ! results    : [$2367,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg4_code_665:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3414
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3414:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
code_3398:
funtop_2328:
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3399
	nop
code_3400:
	call	GCFromML ! delay slot empty
	nop
needgc_3399:
	! Proj_c at label _391_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r21
	! Proj_c at label _390_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r20
	! Proj_c at label _389_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r19
	! Proj_c at label _388_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r8
	! Proj_c at label range_360_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r16
	st	%r16, [%sp+100]
	or	%r0, 33, %r18
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_3402
	nop
code_3403:
	or	%r0, 0, %r8
cmpui_3402:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r18, %r18
	cmp	%r19, 3
	or	%r0, 1, %r8
	bgu	cmpui_3404
	nop
code_3405:
	or	%r0, 0, %r8
cmpui_3404:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r18, %r18
	cmp	%r20, 3
	or	%r0, 1, %r8
	bgu	cmpui_3406
	nop
code_3407:
	or	%r0, 0, %r8
cmpui_3406:
	sll	%r8, 10, %r8
	add	%r8, %r0, %r8
	or	%r8, %r18, %r18
	cmp	%r21, 3
	or	%r0, 1, %r8
	bgu	cmpui_3408
	nop
code_3409:
	or	%r0, 0, %r8
cmpui_3408:
	sll	%r8, 11, %r8
	add	%r8, %r0, %r8
	or	%r8, %r18, %r18
	! allocating 4-record
	st	%r18, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	st	%r13, [%r4+16]
	add	%r4, 4, %r10
	add	%r4, 20, %r4
	! done allocating 4 record
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3410:
	! done making tail call
code_3412:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_vararg4_code_665,(.-LINKUNIT_vararg4_code_665)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3399
	.word 0x001c000f
	.word 0x00170000
	.word 0x00000200
	.word 0x00003c00
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000004
	.word 0x00000060
	.word 0x00000003
	.word 0x00000060
	.word 0x00000002
	.word 0x00000060
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global LINKUNIT_vararg5_code_682
 ! arguments : [$684,$8] [$685,$9] [$405,$10] [$406,$11] [$407,$12] [$408,$13] [$409,$18] 
 ! results    : [$2327,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg5_code_682:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3433
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3433:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r19
code_3415:
funtop_2282:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3416
	nop
code_3417:
	call	GCFromML ! delay slot empty
	nop
needgc_3416:
	! Proj_c at label _404_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r9
	! Proj_c at label _403_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r23
	! Proj_c at label _402_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r22
	! Proj_c at label _401_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r21
	! Proj_c at label _400_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r20
	! Proj_c at label range_360_INT
	ld	[%sp+96], %r17
	ld	[%r17+20], %r16
	st	%r16, [%sp+100]
	or	%r0, 41, %r8
	cmp	%r20, 3
	or	%r0, 1, %r20
	bgu	cmpui_3419
	nop
code_3420:
	or	%r0, 0, %r20
cmpui_3419:
	sll	%r20, 8, %r20
	add	%r20, %r0, %r20
	or	%r20, %r8, %r8
	cmp	%r21, 3
	or	%r0, 1, %r20
	bgu	cmpui_3421
	nop
code_3422:
	or	%r0, 0, %r20
cmpui_3421:
	sll	%r20, 9, %r20
	add	%r20, %r0, %r20
	or	%r20, %r8, %r8
	cmp	%r22, 3
	or	%r0, 1, %r20
	bgu	cmpui_3423
	nop
code_3424:
	or	%r0, 0, %r20
cmpui_3423:
	sll	%r20, 10, %r20
	add	%r20, %r0, %r20
	or	%r20, %r8, %r8
	cmp	%r23, 3
	or	%r0, 1, %r20
	bgu	cmpui_3425
	nop
code_3426:
	or	%r0, 0, %r20
cmpui_3425:
	sll	%r20, 11, %r20
	add	%r20, %r0, %r20
	or	%r20, %r8, %r8
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_3427
	nop
code_3428:
	or	%r0, 0, %r9
cmpui_3427:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	! allocating 5-record
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	st	%r13, [%r4+16]
	st	%r18, [%r4+20]
	add	%r4, 4, %r10
	add	%r4, 24, %r4
	! done allocating 5 record
	! making closure call
	ld	[%r19], %r11
	ld	[%r19+4], %r8
	ld	[%r19+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3429:
	! done making tail call
code_3431:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_vararg5_code_682,(.-LINKUNIT_vararg5_code_682)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3416
	.word 0x001c0011
	.word 0x00170000
	.word 0x00080000
	.word 0x00043c00
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000005
	.word 0x00000060
	.word 0x00000004
	.word 0x00000060
	.word 0x00000003
	.word 0x00000060
	.word 0x00000002
	.word 0x00000060
	.word 0x00000001
	.word 0x00000060
	.text
	.align 8
	.global LINKUNIT_vararg6_code_701
 ! arguments : [$703,$8] [$704,$9] [$420,$10] [$421,$11] [$422,$12] [$423,$13] [$424,$18] [$425,$19] 
 ! results    : [$2281,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg6_code_701:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3454
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_3454:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r22
	mov	%r11, %r21
	mov	%r12, %r20
code_3434:
funtop_2230:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3435
	nop
code_3436:
	call	GCFromML ! delay slot empty
	nop
needgc_3435:
	! Proj_c at label _419_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	! Proj_c at label _418_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r11
	! Proj_c at label _417_INT
	ld	[%sp+96], %r17
	ld	[%r17+8], %r10
	! Proj_c at label _416_INT
	ld	[%sp+96], %r17
	ld	[%r17+12], %r25
	! Proj_c at label _415_INT
	ld	[%sp+96], %r17
	ld	[%r17+16], %r24
	! Proj_c at label _414_INT
	ld	[%sp+96], %r17
	ld	[%r17+20], %r23
	! Proj_c at label range_360_INT
	ld	[%sp+96], %r17
	ld	[%r17+24], %r16
	st	%r16, [%sp+100]
	or	%r0, 49, %r8
	cmp	%r23, 3
	or	%r0, 1, %r23
	bgu	cmpui_3438
	nop
code_3439:
	or	%r0, 0, %r23
cmpui_3438:
	sll	%r23, 8, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	cmp	%r24, 3
	or	%r0, 1, %r23
	bgu	cmpui_3440
	nop
code_3441:
	or	%r0, 0, %r23
cmpui_3440:
	sll	%r23, 9, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	cmp	%r25, 3
	or	%r0, 1, %r23
	bgu	cmpui_3442
	nop
code_3443:
	or	%r0, 0, %r23
cmpui_3442:
	sll	%r23, 10, %r23
	add	%r23, %r0, %r23
	or	%r23, %r8, %r8
	cmp	%r10, 3
	or	%r0, 1, %r10
	bgu	cmpui_3444
	nop
code_3445:
	or	%r0, 0, %r10
cmpui_3444:
	sll	%r10, 11, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	cmp	%r11, 3
	or	%r0, 1, %r10
	bgu	cmpui_3446
	nop
code_3447:
	or	%r0, 0, %r10
cmpui_3446:
	sll	%r10, 12, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	cmp	%r12, 3
	or	%r0, 1, %r10
	bgu	cmpui_3448
	nop
code_3449:
	or	%r0, 0, %r10
cmpui_3448:
	sll	%r10, 13, %r10
	add	%r10, %r0, %r10
	or	%r10, %r8, %r8
	! allocating 6-record
	st	%r8, [%r4]
	st	%r22, [%r4+4]
	st	%r21, [%r4+8]
	st	%r20, [%r4+12]
	st	%r13, [%r4+16]
	st	%r18, [%r4+20]
	st	%r19, [%r4+24]
	add	%r4, 4, %r10
	add	%r4, 28, %r4
	! done allocating 6 record
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_3450:
	! done making tail call
code_3452:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size LINKUNIT_vararg6_code_701,(.-LINKUNIT_vararg6_code_701)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3435
	.word 0x001c0013
	.word 0x00170000
	.word 0x00000200
	.word 0x007c2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000003
	.word 0x00000060
	.word 0x00000002
	.word 0x00000060
	.word 0x00000001
	.word 0x00000060
	.word 0x00000004
	.word 0x00000060
	.word 0x00000005
	.word 0x00000060
	.word 0x00000006
	.word 0x00000060
	.text
	.align 8
	.global LINKUNIT_vararg_code_612
 ! arguments : [$614,$8] [$359,$9] [$360,$10] [$615,$11] [$361,$12] 
 ! results    : [$2102,$8] 
 ! destroys   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_vararg_code_612:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3484
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_3484:
	st	%r15, [%sp+92]
code_3455:
funtop_2082:
	add	%r4, 244, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_3456
	nop
code_3457:
	call	GCFromML ! delay slot empty
	nop
needgc_3456:
	cmp	%r9, 255
	ble,pn	%icc,defaultTypecase_2087
	nop
code_3459:
	ld	[%r9], %r8
	cmp	%r8, 5
	bne,pn	%icc,defaultTypecase_2087
	nop
code_3460:
	ld	[%r9+4], %r8
typecasearm_2090:
	cmp	%r8, 0
	bne	exnarm_2091
	nop
code_3461:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_vararg0_code_617), %r8
	or	%r8, %lo(LINKUNIT_vararg0_code_617), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2086 ! delay slot empty
	nop
exnarm_2091:
	cmp	%r8, 1
	bne	exnarm_2103
	nop
code_3464:
	ld	[%r9+8], %r9
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_vararg1_code_626), %r8
	or	%r8, %lo(LINKUNIT_vararg1_code_626), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2086 ! delay slot empty
	nop
exnarm_2103:
	cmp	%r8, 2
	bne	exnarm_2119
	nop
code_3467:
	ld	[%r9+8], %r11
	ld	[%r9+12], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_vararg2_code_637), %r8
	or	%r8, %lo(LINKUNIT_vararg2_code_637), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2086 ! delay slot empty
	nop
exnarm_2119:
	cmp	%r8, 3
	bne	exnarm_2137
	nop
code_3470:
	ld	[%r9+8], %r13
	ld	[%r9+12], %r11
	ld	[%r9+16], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r10, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_vararg3_code_650), %r8
	or	%r8, %lo(LINKUNIT_vararg3_code_650), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2086 ! delay slot empty
	nop
exnarm_2137:
	cmp	%r8, 4
	bne	exnarm_2157
	nop
code_3473:
	ld	[%r9+8], %r18
	ld	[%r9+12], %r13
	ld	[%r9+16], %r11
	ld	[%r9+20], %r9
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7977), %r8
	or	%r8, %lo(7977), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r18, [%r4+16]
	st	%r10, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_vararg4_code_665), %r8
	or	%r8, %lo(LINKUNIT_vararg4_code_665), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2086 ! delay slot empty
	nop
exnarm_2157:
	cmp	%r8, 5
	bne	exnarm_2179
	nop
code_3476:
	ld	[%r9+8], %r19
	ld	[%r9+12], %r18
	ld	[%r9+16], %r13
	ld	[%r9+20], %r11
	ld	[%r9+24], %r9
	! allocating 1 closures
	! allocating 6-record
	sethi	%hi(16177), %r8
	or	%r8, %lo(16177), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r18, [%r4+16]
	st	%r19, [%r4+20]
	st	%r10, [%r4+24]
	add	%r4, 4, %r9
	add	%r4, 28, %r4
	! done allocating 6 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_vararg5_code_682), %r8
	or	%r8, %lo(LINKUNIT_vararg5_code_682), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2086 ! delay slot empty
	nop
exnarm_2179:
	cmp	%r8, 6
	bne	exnarm_2203
	nop
code_3479:
	ld	[%r9+8], %r20
	ld	[%r9+12], %r19
	ld	[%r9+16], %r18
	ld	[%r9+20], %r13
	ld	[%r9+24], %r11
	ld	[%r9+28], %r9
	! allocating 1 closures
	! allocating 7-record
	sethi	%hi(32569), %r8
	or	%r8, %lo(32569), %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r11, [%r4+8]
	st	%r13, [%r4+12]
	st	%r18, [%r4+16]
	st	%r19, [%r4+20]
	st	%r20, [%r4+24]
	st	%r10, [%r4+28]
	add	%r4, 4, %r9
	add	%r4, 32, %r4
	! done allocating 7 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(LINKUNIT_vararg6_code_701), %r8
	or	%r8, %lo(LINKUNIT_vararg6_code_701), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	st	%r12, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ba	afterTypecase_2086 ! delay slot empty
	nop
exnarm_2203:
defaultTypecase_2087:
	mov	%r12, %r8
afterTypecase_2086:
code_3483:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size LINKUNIT_vararg_code_612,(.-LINKUNIT_vararg_code_612)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_3456
	.word 0x00180007
	.word 0x00170000
	.word 0xbfe01600
	.word 0xbfe00000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global LINKUNIT_main
 ! arguments : 
 ! results    : [$2081,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
LINKUNIT_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_3972
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_3972:
	st	%r15, [%sp+92]
code_3485:
funtop_1106:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_3971:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_3486:
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(onearg_319), %r8
	or	%r8, %lo(onearg_319), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(vararg_320), %r8
	or	%r8, %lo(vararg_320), %r8
	! done allocating 1 closures
	! making closure polycall
	sethi	%hi(Firstlude_unit), %r8
	or	%r8, %lo(Firstlude_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3851:
code_3491:
	! done making normal call
	! making closure polycall
	sethi	%hi(Prelude_unit), %r8
	or	%r8, %lo(Prelude_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3852:
code_3494:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_SYS_DB_unit), %r8
	or	%r8, %lo(POSIX_SYS_DB_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3853:
code_3497:
	! done making normal call
	! making closure polycall
	sethi	%hi(PreWord_unit), %r8
	or	%r8, %lo(PreWord_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3854:
code_3500:
	! done making normal call
	! making closure polycall
	sethi	%hi(PreInt_unit), %r8
	or	%r8, %lo(PreInt_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3855:
code_3503:
	! done making normal call
	! making closure polycall
	sethi	%hi(OPTION_unit), %r8
	or	%r8, %lo(OPTION_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3856:
code_3506:
	! done making normal call
	! making closure polycall
	sethi	%hi(STRING_CVT_unit), %r8
	or	%r8, %lo(STRING_CVT_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3857:
code_3509:
	! done making normal call
	! making closure polycall
	sethi	%hi(PreString_unit), %r8
	or	%r8, %lo(PreString_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3858:
code_3512:
	! done making normal call
	! making closure polycall
	sethi	%hi(StringCvt_unit), %r8
	or	%r8, %lo(StringCvt_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3859:
code_3515:
	! done making normal call
	! making closure polycall
	sethi	%hi(WORD_unit), %r8
	or	%r8, %lo(WORD_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3860:
code_3518:
	! done making normal call
	! making closure polycall
	sethi	%hi(PreVector_unit), %r8
	or	%r8, %lo(PreVector_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3861:
code_3521:
	! done making normal call
	! making closure polycall
	sethi	%hi(PreReal_unit), %r8
	or	%r8, %lo(PreReal_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3862:
code_3524:
	! done making normal call
	! making closure polycall
	sethi	%hi(CHAR_unit), %r8
	or	%r8, %lo(CHAR_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3863:
code_3527:
	! done making normal call
	! making closure polycall
	sethi	%hi(STRING_unit), %r8
	or	%r8, %lo(STRING_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3864:
code_3530:
	! done making normal call
	! making closure polycall
	sethi	%hi(General_extern_unit), %r8
	or	%r8, %lo(General_extern_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3865:
code_3533:
	! done making normal call
	! making closure polycall
	sethi	%hi(GENERAL_unit), %r8
	or	%r8, %lo(GENERAL_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3866:
code_3536:
	! done making normal call
	! making closure polycall
	sethi	%hi(General_unit), %r8
	or	%r8, %lo(General_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3867:
code_3539:
	! done making normal call
	! making closure polycall
	sethi	%hi(NumFormat_unit), %r8
	or	%r8, %lo(NumFormat_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3868:
code_3542:
	! done making normal call
	! making closure polycall
	sethi	%hi(Char_unit), %r8
	or	%r8, %lo(Char_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3869:
code_3545:
	! done making normal call
	! making closure polycall
	sethi	%hi(String_unit), %r8
	or	%r8, %lo(String_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3870:
code_3548:
	! done making normal call
	! making closure polycall
	sethi	%hi(Option_unit), %r8
	or	%r8, %lo(Option_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3871:
code_3551:
	! done making normal call
	! making closure polycall
	sethi	%hi(NumScan_unit), %r8
	or	%r8, %lo(NumScan_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3872:
code_3554:
	! done making normal call
	! making closure polycall
	sethi	%hi(Word32_unit), %r8
	or	%r8, %lo(Word32_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3873:
code_3557:
	! done making normal call
	! making closure polycall
	sethi	%hi(SysWord_unit), %r8
	or	%r8, %lo(SysWord_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3874:
code_3560:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_FLAGS_unit), %r8
	or	%r8, %lo(POSIX_FLAGS_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3875:
code_3563:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_TTY_unit), %r8
	or	%r8, %lo(POSIX_TTY_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3876:
code_3566:
	! done making normal call
	! making closure polycall
	sethi	%hi(PreTime_unit), %r8
	or	%r8, %lo(PreTime_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3877:
code_3569:
	! done making normal call
	! making closure polycall
	sethi	%hi(TIME_unit), %r8
	or	%r8, %lo(TIME_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3878:
code_3572:
	! done making normal call
	! making closure polycall
	sethi	%hi(LIST_unit), %r8
	or	%r8, %lo(LIST_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3879:
code_3575:
	! done making normal call
	! making closure polycall
	sethi	%hi(List_unit), %r8
	or	%r8, %lo(List_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3880:
code_3578:
	! done making normal call
	! making closure polycall
	sethi	%hi(RealFormat_unit), %r8
	or	%r8, %lo(RealFormat_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3881:
code_3581:
	! done making normal call
	! making closure polycall
	sethi	%hi(IEEE_REAL_unit), %r8
	or	%r8, %lo(IEEE_REAL_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3882:
code_3584:
	! done making normal call
	! making closure polycall
	sethi	%hi(INTEGER_unit), %r8
	or	%r8, %lo(INTEGER_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3883:
code_3587:
	! done making normal call
	! making closure polycall
	sethi	%hi(Int32_unit), %r8
	or	%r8, %lo(Int32_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3884:
code_3590:
	! done making normal call
	! making closure polycall
	sethi	%hi(Int_unit), %r8
	or	%r8, %lo(Int_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3885:
code_3593:
	! done making normal call
	! making closure polycall
	sethi	%hi(IEEEReal_unit), %r8
	or	%r8, %lo(IEEEReal_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3886:
code_3596:
	! done making normal call
	! making closure polycall
	sethi	%hi(MATH_unit), %r8
	or	%r8, %lo(MATH_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3887:
code_3599:
	! done making normal call
	! making closure polycall
	sethi	%hi(REAL_unit), %r8
	or	%r8, %lo(REAL_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3888:
code_3602:
	! done making normal call
	! making closure polycall
	sethi	%hi(Math64_unit), %r8
	or	%r8, %lo(Math64_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3889:
code_3605:
	! done making normal call
	! making closure polycall
	sethi	%hi(BOOL_unit), %r8
	or	%r8, %lo(BOOL_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3890:
code_3608:
	! done making normal call
	! making closure polycall
	sethi	%hi(Bool_unit), %r8
	or	%r8, %lo(Bool_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3891:
code_3611:
	! done making normal call
	! making closure polycall
	sethi	%hi(Real64_unit), %r8
	or	%r8, %lo(Real64_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3892:
code_3614:
	! done making normal call
	! making closure polycall
	sethi	%hi(Real_unit), %r8
	or	%r8, %lo(Real_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3893:
code_3617:
	! done making normal call
	! making closure polycall
	sethi	%hi(Time_unit), %r8
	or	%r8, %lo(Time_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3894:
code_3620:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_PROC_ENV_unit), %r8
	or	%r8, %lo(POSIX_PROC_ENV_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3895:
code_3623:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_SIGNAL_unit), %r8
	or	%r8, %lo(POSIX_SIGNAL_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3896:
code_3626:
	! done making normal call
	! making closure polycall
	sethi	%hi(Word8_unit), %r8
	or	%r8, %lo(Word8_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3897:
code_3629:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_PROCESS_unit), %r8
	or	%r8, %lo(POSIX_PROCESS_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3898:
code_3632:
	! done making normal call
	! making closure polycall
	sethi	%hi(PreOS_unit), %r8
	or	%r8, %lo(PreOS_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3899:
code_3635:
	! done making normal call
	! making closure polycall
	sethi	%hi(Position_unit), %r8
	or	%r8, %lo(Position_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3900:
code_3638:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_FILE_SYS_unit), %r8
	or	%r8, %lo(POSIX_FILE_SYS_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3901:
code_3641:
	! done making normal call
	! making closure polycall
	sethi	%hi(MONO_VECTOR_unit), %r8
	or	%r8, %lo(MONO_VECTOR_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3902:
code_3644:
	! done making normal call
	! making closure polycall
	sethi	%hi(VECTOR_unit), %r8
	or	%r8, %lo(VECTOR_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3903:
code_3647:
	! done making normal call
	! making closure polycall
	sethi	%hi(ARRAY_unit), %r8
	or	%r8, %lo(ARRAY_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3904:
code_3650:
	! done making normal call
	! making closure polycall
	sethi	%hi(Array_unit), %r8
	or	%r8, %lo(Array_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3905:
code_3653:
	! done making normal call
	! making closure polycall
	sethi	%hi(Vector_unit), %r8
	or	%r8, %lo(Vector_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3906:
code_3656:
	! done making normal call
	! making closure polycall
	sethi	%hi(Word8Vector_unit), %r8
	or	%r8, %lo(Word8Vector_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3907:
code_3659:
	! done making normal call
	! making closure polycall
	sethi	%hi(MONO_ARRAY_unit), %r8
	or	%r8, %lo(MONO_ARRAY_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3908:
code_3662:
	! done making normal call
	! making closure polycall
	sethi	%hi(Word8Array_unit), %r8
	or	%r8, %lo(Word8Array_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3909:
code_3665:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_IO_unit), %r8
	or	%r8, %lo(POSIX_IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3910:
code_3668:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_ERROR_unit), %r8
	or	%r8, %lo(POSIX_ERROR_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3911:
code_3671:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_unit), %r8
	or	%r8, %lo(POSIX_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3912:
code_3674:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_extern_unit), %r8
	or	%r8, %lo(POSIX_extern_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3913:
code_3677:
	! done making normal call
	! making closure polycall
	sethi	%hi(PrePosix_unit), %r8
	or	%r8, %lo(PrePosix_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3914:
code_3680:
	! done making normal call
	! making closure polycall
	sethi	%hi(Word_unit), %r8
	or	%r8, %lo(Word_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3915:
code_3683:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_FileSys_unit), %r8
	or	%r8, %lo(POSIX_FileSys_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3916:
code_3686:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_Sys_DB_unit), %r8
	or	%r8, %lo(POSIX_Sys_DB_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3917:
code_3689:
	! done making normal call
	! making closure polycall
	sethi	%hi(SysInt_unit), %r8
	or	%r8, %lo(SysInt_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3918:
code_3692:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_Signal_unit), %r8
	or	%r8, %lo(POSIX_Signal_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3919:
code_3695:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_Process_unit), %r8
	or	%r8, %lo(POSIX_Process_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3920:
code_3698:
	! done making normal call
	! making closure polycall
	sethi	%hi(SUBSTRING_unit), %r8
	or	%r8, %lo(SUBSTRING_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3921:
code_3701:
	! done making normal call
	! making closure polycall
	sethi	%hi(Substring_unit), %r8
	or	%r8, %lo(Substring_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3922:
code_3704:
	! done making normal call
	! making closure polycall
	sethi	%hi(BYTE_unit), %r8
	or	%r8, %lo(BYTE_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3923:
code_3707:
	! done making normal call
	! making closure polycall
	sethi	%hi(Byte_unit), %r8
	or	%r8, %lo(Byte_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3924:
code_3710:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_Tty_unit), %r8
	or	%r8, %lo(POSIX_Tty_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3925:
code_3713:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_ProcEnv_unit), %r8
	or	%r8, %lo(POSIX_ProcEnv_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3926:
code_3716:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_IO_Str_unit), %r8
	or	%r8, %lo(POSIX_IO_Str_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3927:
code_3719:
	! done making normal call
	! making closure polycall
	sethi	%hi(POSIX_Error_unit), %r8
	or	%r8, %lo(POSIX_Error_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3928:
code_3722:
	! done making normal call
	! making closure polycall
	sethi	%hi(Posix_unit), %r8
	or	%r8, %lo(Posix_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3929:
code_3725:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_IO_unit), %r8
	or	%r8, %lo(OS_IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3930:
code_3728:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_IO_Str_unit), %r8
	or	%r8, %lo(OS_IO_Str_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3931:
code_3731:
	! done making normal call
	! making closure polycall
	sethi	%hi(CLEAN_UP_unit), %r8
	or	%r8, %lo(CLEAN_UP_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3932:
code_3734:
	! done making normal call
	! making closure polycall
	sethi	%hi(CleanUp_unit), %r8
	or	%r8, %lo(CleanUp_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3933:
code_3737:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_PROCESS_unit), %r8
	or	%r8, %lo(OS_PROCESS_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3934:
code_3740:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_Process_unit), %r8
	or	%r8, %lo(OS_Process_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3935:
code_3743:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_PATH_unit), %r8
	or	%r8, %lo(OS_PATH_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3936:
code_3746:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_PathFn_unit), %r8
	or	%r8, %lo(OS_PathFn_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3937:
code_3749:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_Path_unit), %r8
	or	%r8, %lo(OS_Path_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3938:
code_3752:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_FILE_SYS_unit), %r8
	or	%r8, %lo(OS_FILE_SYS_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3939:
code_3755:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_FileSys_unit), %r8
	or	%r8, %lo(OS_FileSys_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3940:
code_3758:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_SIG_unit), %r8
	or	%r8, %lo(OS_SIG_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3941:
code_3761:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_unit), %r8
	or	%r8, %lo(OS_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3942:
code_3764:
	! done making normal call
	! making closure polycall
	sethi	%hi(PRIM_IO_unit), %r8
	or	%r8, %lo(PRIM_IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3943:
code_3767:
	! done making normal call
	! making closure polycall
	sethi	%hi(PrimIOFn_unit), %r8
	or	%r8, %lo(PrimIOFn_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3944:
code_3770:
	! done making normal call
	! making closure polycall
	sethi	%hi(BinPrimIO_unit), %r8
	or	%r8, %lo(BinPrimIO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3945:
code_3773:
	! done making normal call
	! making closure polycall
	sethi	%hi(OS_PRIM_IO_unit), %r8
	or	%r8, %lo(OS_PRIM_IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3946:
code_3776:
	! done making normal call
	! making closure polycall
	sethi	%hi(IO_SIG_unit), %r8
	or	%r8, %lo(IO_SIG_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3947:
code_3779:
	! done making normal call
	! making closure polycall
	sethi	%hi(IO_unit), %r8
	or	%r8, %lo(IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3948:
code_3782:
	! done making normal call
	! making closure polycall
	sethi	%hi(PosixPrimIOFn_unit), %r8
	or	%r8, %lo(PosixPrimIOFn_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3949:
code_3785:
	! done making normal call
	! making closure polycall
	sethi	%hi(PosixBinPrimIO_unit), %r8
	or	%r8, %lo(PosixBinPrimIO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3950:
code_3788:
	! done making normal call
	! making closure polycall
	sethi	%hi(CharArray_unit), %r8
	or	%r8, %lo(CharArray_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3951:
code_3791:
	! done making normal call
	! making closure polycall
	sethi	%hi(CharVector_unit), %r8
	or	%r8, %lo(CharVector_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3952:
code_3794:
	! done making normal call
	! making closure polycall
	sethi	%hi(TextPrimIO_unit), %r8
	or	%r8, %lo(TextPrimIO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3953:
code_3797:
	! done making normal call
	! making closure polycall
	sethi	%hi(PosixTextPrimIO_unit), %r8
	or	%r8, %lo(PosixTextPrimIO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3954:
code_3800:
	! done making normal call
	! making closure polycall
	sethi	%hi(CleanIO_unit), %r8
	or	%r8, %lo(CleanIO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3955:
code_3803:
	! done making normal call
	! making closure polycall
	sethi	%hi(STREAM_IO_unit), %r8
	or	%r8, %lo(STREAM_IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3956:
code_3806:
	! done making normal call
	! making closure polycall
	sethi	%hi(TEXT_STREAM_IO_unit), %r8
	or	%r8, %lo(TEXT_STREAM_IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3957:
code_3809:
	! done making normal call
	! making closure polycall
	sethi	%hi(TEXT_IO_unit), %r8
	or	%r8, %lo(TEXT_IO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3958:
code_3812:
	! done making normal call
	! making closure polycall
	sethi	%hi(TextIOFn_unit), %r8
	or	%r8, %lo(TextIOFn_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3959:
code_3815:
	! done making normal call
	! making closure polycall
	sethi	%hi(TextIO_unit), %r8
	or	%r8, %lo(TextIO_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3960:
code_3818:
	! done making normal call
	! making closure polycall
	sethi	%hi(Word31_unit), %r8
	or	%r8, %lo(Word31_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3961:
code_3821:
	! done making normal call
	! making closure polycall
	sethi	%hi(TopLevel_unit), %r8
	or	%r8, %lo(TopLevel_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3962:
code_3824:
	! done making normal call
	! making closure polycall
	sethi	%hi(CommandLineHelp_unit), %r8
	or	%r8, %lo(CommandLineHelp_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3963:
code_3827:
	! done making normal call
	! making closure polycall
	sethi	%hi(COMMAND_LINE_unit), %r8
	or	%r8, %lo(COMMAND_LINE_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3964:
code_3830:
	! done making normal call
	! making closure polycall
	sethi	%hi(CommandLine_unit), %r8
	or	%r8, %lo(CommandLine_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3965:
code_3833:
	! done making normal call
	! making closure polycall
	sethi	%hi(Help_unit), %r8
	or	%r8, %lo(Help_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3966:
code_3836:
	! done making normal call
	! making closure polycall
	sethi	%hi(DATE_unit), %r8
	or	%r8, %lo(DATE_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3967:
code_3839:
	! done making normal call
	! making closure polycall
	sethi	%hi(Date_unit), %r8
	or	%r8, %lo(Date_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3968:
code_3842:
	! done making normal call
	! making closure polycall
	sethi	%hi(Config_unit), %r8
	or	%r8, %lo(Config_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3969:
code_3845:
	! done making normal call
	! making closure polycall
	sethi	%hi(MakeRelease_unit), %r8
	or	%r8, %lo(MakeRelease_unit), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r10
	ld	[%r9+4], %r8
	jmpl	%r10, %r15
	ld	[%r9+8], %r9
code_3970:
code_3848:
	! done making normal call
	or	%r0, 256, %r8
code_3850:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size LINKUNIT_main,(.-LINKUNIT_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_3851
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3852
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3853
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3854
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3855
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3856
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3857
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3858
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3859
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3860
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3861
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3862
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3863
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3864
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3865
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3866
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3867
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3868
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3869
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3870
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3871
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3872
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3873
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3874
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3875
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3876
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3877
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3878
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3879
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3880
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3881
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3882
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3883
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3884
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3885
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3886
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3887
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3888
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3889
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3890
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3891
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3892
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3893
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3894
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3895
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3896
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3897
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3898
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3899
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3900
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3901
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3902
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3903
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3904
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3905
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3906
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3907
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3908
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3909
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3910
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3911
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3912
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3913
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3914
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3915
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3916
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3917
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3918
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3919
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3920
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3921
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3922
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3923
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3924
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3925
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3926
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3927
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3928
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3929
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3930
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3931
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3932
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3933
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3934
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3935
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3936
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3937
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3938
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3939
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3940
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3941
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3942
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3943
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3944
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3945
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3946
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3947
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3948
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3949
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3950
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3951
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3952
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3953
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3954
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3955
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3956
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3957
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3958
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3959
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3960
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3961
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3962
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3963
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3964
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3965
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3966
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3967
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3968
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3969
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3970
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_3971
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
LINKUNIT_unit_CODE_END_VAL:
	.section	".rodata"
		! endgcinfo with filler for alignment
	.globl LINKUNIT_unit_GCTABLE_END_VAL
LINKUNIT_unit_GCTABLE_END_VAL:
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl LINKUNIT_unit_GLOBALS_BEGIN_VAL
LINKUNIT_unit_GLOBALS_BEGIN_VAL:
		! Global
	.word 0x00000009
	.globl polyLen_INT
polyLen_INT:
	.long LINKUNIT_polyLen_303
		! Global
	.word 0x00000009
	.globl polyVlen_INT
polyVlen_INT:
	.long LINKUNIT_polyVlen_304
		! Global
	.word 0x00000009
	.globl polySub_INT
polySub_INT:
	.long LINKUNIT_polySub_305
		! Global
	.word 0x00000009
	.globl polyVsub_INT
polyVsub_INT:
	.long LINKUNIT_polyVsub_306
		! Global
	.word 0x00000009
	.globl polyUpdate_INT
polyUpdate_INT:
	.long LINKUNIT_polyUpdate_307
		! Global
	.word 0x00000009
	.globl polyArray_INT
polyArray_INT:
	.long LINKUNIT_polyArray_308
		! Global
	.word 0x00000009
	.globl polyVector_INT
polyVector_INT:
	.long LINKUNIT_polyVector_309
		! Global
	.word 0x0000006f
	.globl onearg_INT
onearg_INT:
	.long onearg_319
	.long onearg_319
		! static record tag
	.word 0x00000619
onearg_319:
	.long LINKUNIT_onearg_code_502
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x0000006f
	.globl vararg_INT
vararg_INT:
	.long vararg_320
	.long vararg_320
		! static record tag
	.word 0x00000619
vararg_320:
	.long LINKUNIT_vararg_code_612
	.word 0x00000100
	.word 0x00000100
		! Module closure
	.word 0x00000619
	.globl LINKUNIT_unit_closure
LINKUNIT_unit_closure:
	.long LINKUNIT_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl LINKUNIT_unit
LINKUNIT_unit:
	.long LINKUNIT_unit_closure
	.long LINKUNIT_unit_closure
	.globl LINKUNIT_unit_GLOBALS_END_VAL
LINKUNIT_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.globl module_count
module_count:
	.word 0x00000079
	.globl GCTABLE_BEGIN_VAL
GCTABLE_BEGIN_VAL:
	.long LINKUNIT_unit_GCTABLE_BEGIN_VAL
	.long Firstlude_unit_GCTABLE_BEGIN_VAL
	.long Prelude_unit_GCTABLE_BEGIN_VAL
	.long POSIX_SYS_DB_unit_GCTABLE_BEGIN_VAL
	.long PreWord_unit_GCTABLE_BEGIN_VAL
	.long PreInt_unit_GCTABLE_BEGIN_VAL
	.long OPTION_unit_GCTABLE_BEGIN_VAL
	.long STRING_CVT_unit_GCTABLE_BEGIN_VAL
	.long PreString_unit_GCTABLE_BEGIN_VAL
	.long StringCvt_unit_GCTABLE_BEGIN_VAL
	.long WORD_unit_GCTABLE_BEGIN_VAL
	.long PreVector_unit_GCTABLE_BEGIN_VAL
	.long PreReal_unit_GCTABLE_BEGIN_VAL
	.long CHAR_unit_GCTABLE_BEGIN_VAL
	.long STRING_unit_GCTABLE_BEGIN_VAL
	.long General_extern_unit_GCTABLE_BEGIN_VAL
	.long GENERAL_unit_GCTABLE_BEGIN_VAL
	.long General_unit_GCTABLE_BEGIN_VAL
	.long NumFormat_unit_GCTABLE_BEGIN_VAL
	.long Char_unit_GCTABLE_BEGIN_VAL
	.long String_unit_GCTABLE_BEGIN_VAL
	.long Option_unit_GCTABLE_BEGIN_VAL
	.long NumScan_unit_GCTABLE_BEGIN_VAL
	.long Word32_unit_GCTABLE_BEGIN_VAL
	.long SysWord_unit_GCTABLE_BEGIN_VAL
	.long POSIX_FLAGS_unit_GCTABLE_BEGIN_VAL
	.long POSIX_TTY_unit_GCTABLE_BEGIN_VAL
	.long PreTime_unit_GCTABLE_BEGIN_VAL
	.long TIME_unit_GCTABLE_BEGIN_VAL
	.long LIST_unit_GCTABLE_BEGIN_VAL
	.long List_unit_GCTABLE_BEGIN_VAL
	.long RealFormat_unit_GCTABLE_BEGIN_VAL
	.long IEEE_REAL_unit_GCTABLE_BEGIN_VAL
	.long INTEGER_unit_GCTABLE_BEGIN_VAL
	.long Int32_unit_GCTABLE_BEGIN_VAL
	.long Int_unit_GCTABLE_BEGIN_VAL
	.long IEEEReal_unit_GCTABLE_BEGIN_VAL
	.long MATH_unit_GCTABLE_BEGIN_VAL
	.long REAL_unit_GCTABLE_BEGIN_VAL
	.long Math64_unit_GCTABLE_BEGIN_VAL
	.long BOOL_unit_GCTABLE_BEGIN_VAL
	.long Bool_unit_GCTABLE_BEGIN_VAL
	.long Real64_unit_GCTABLE_BEGIN_VAL
	.long Real_unit_GCTABLE_BEGIN_VAL
	.long Time_unit_GCTABLE_BEGIN_VAL
	.long POSIX_PROC_ENV_unit_GCTABLE_BEGIN_VAL
	.long POSIX_SIGNAL_unit_GCTABLE_BEGIN_VAL
	.long Word8_unit_GCTABLE_BEGIN_VAL
	.long POSIX_PROCESS_unit_GCTABLE_BEGIN_VAL
	.long PreOS_unit_GCTABLE_BEGIN_VAL
	.long Position_unit_GCTABLE_BEGIN_VAL
	.long POSIX_FILE_SYS_unit_GCTABLE_BEGIN_VAL
	.long MONO_VECTOR_unit_GCTABLE_BEGIN_VAL
	.long VECTOR_unit_GCTABLE_BEGIN_VAL
	.long ARRAY_unit_GCTABLE_BEGIN_VAL
	.long Array_unit_GCTABLE_BEGIN_VAL
	.long Vector_unit_GCTABLE_BEGIN_VAL
	.long Word8Vector_unit_GCTABLE_BEGIN_VAL
	.long MONO_ARRAY_unit_GCTABLE_BEGIN_VAL
	.long Word8Array_unit_GCTABLE_BEGIN_VAL
	.long POSIX_IO_unit_GCTABLE_BEGIN_VAL
	.long POSIX_ERROR_unit_GCTABLE_BEGIN_VAL
	.long POSIX_unit_GCTABLE_BEGIN_VAL
	.long POSIX_extern_unit_GCTABLE_BEGIN_VAL
	.long PrePosix_unit_GCTABLE_BEGIN_VAL
	.long Word_unit_GCTABLE_BEGIN_VAL
	.long POSIX_FileSys_unit_GCTABLE_BEGIN_VAL
	.long POSIX_Sys_DB_unit_GCTABLE_BEGIN_VAL
	.long SysInt_unit_GCTABLE_BEGIN_VAL
	.long POSIX_Signal_unit_GCTABLE_BEGIN_VAL
	.long POSIX_Process_unit_GCTABLE_BEGIN_VAL
	.long SUBSTRING_unit_GCTABLE_BEGIN_VAL
	.long Substring_unit_GCTABLE_BEGIN_VAL
	.long BYTE_unit_GCTABLE_BEGIN_VAL
	.long Byte_unit_GCTABLE_BEGIN_VAL
	.long POSIX_Tty_unit_GCTABLE_BEGIN_VAL
	.long POSIX_ProcEnv_unit_GCTABLE_BEGIN_VAL
	.long POSIX_IO_Str_unit_GCTABLE_BEGIN_VAL
	.long POSIX_Error_unit_GCTABLE_BEGIN_VAL
	.long Posix_unit_GCTABLE_BEGIN_VAL
	.long OS_IO_unit_GCTABLE_BEGIN_VAL
	.long OS_IO_Str_unit_GCTABLE_BEGIN_VAL
	.long CLEAN_UP_unit_GCTABLE_BEGIN_VAL
	.long CleanUp_unit_GCTABLE_BEGIN_VAL
	.long OS_PROCESS_unit_GCTABLE_BEGIN_VAL
	.long OS_Process_unit_GCTABLE_BEGIN_VAL
	.long OS_PATH_unit_GCTABLE_BEGIN_VAL
	.long OS_PathFn_unit_GCTABLE_BEGIN_VAL
	.long OS_Path_unit_GCTABLE_BEGIN_VAL
	.long OS_FILE_SYS_unit_GCTABLE_BEGIN_VAL
	.long OS_FileSys_unit_GCTABLE_BEGIN_VAL
	.long OS_SIG_unit_GCTABLE_BEGIN_VAL
	.long OS_unit_GCTABLE_BEGIN_VAL
	.long PRIM_IO_unit_GCTABLE_BEGIN_VAL
	.long PrimIOFn_unit_GCTABLE_BEGIN_VAL
	.long BinPrimIO_unit_GCTABLE_BEGIN_VAL
	.long OS_PRIM_IO_unit_GCTABLE_BEGIN_VAL
	.long IO_SIG_unit_GCTABLE_BEGIN_VAL
	.long IO_unit_GCTABLE_BEGIN_VAL
	.long PosixPrimIOFn_unit_GCTABLE_BEGIN_VAL
	.long PosixBinPrimIO_unit_GCTABLE_BEGIN_VAL
	.long CharArray_unit_GCTABLE_BEGIN_VAL
	.long CharVector_unit_GCTABLE_BEGIN_VAL
	.long TextPrimIO_unit_GCTABLE_BEGIN_VAL
	.long PosixTextPrimIO_unit_GCTABLE_BEGIN_VAL
	.long CleanIO_unit_GCTABLE_BEGIN_VAL
	.long STREAM_IO_unit_GCTABLE_BEGIN_VAL
	.long TEXT_STREAM_IO_unit_GCTABLE_BEGIN_VAL
	.long TEXT_IO_unit_GCTABLE_BEGIN_VAL
	.long TextIOFn_unit_GCTABLE_BEGIN_VAL
	.long TextIO_unit_GCTABLE_BEGIN_VAL
	.long Word31_unit_GCTABLE_BEGIN_VAL
	.long TopLevel_unit_GCTABLE_BEGIN_VAL
	.long CommandLineHelp_unit_GCTABLE_BEGIN_VAL
	.long COMMAND_LINE_unit_GCTABLE_BEGIN_VAL
	.long CommandLine_unit_GCTABLE_BEGIN_VAL
	.long Help_unit_GCTABLE_BEGIN_VAL
	.long DATE_unit_GCTABLE_BEGIN_VAL
	.long Date_unit_GCTABLE_BEGIN_VAL
	.long Config_unit_GCTABLE_BEGIN_VAL
	.long MakeRelease_unit_GCTABLE_BEGIN_VAL
	.globl GCTABLE_END_VAL
GCTABLE_END_VAL:
	.long LINKUNIT_unit_GCTABLE_END_VAL
	.long Firstlude_unit_GCTABLE_END_VAL
	.long Prelude_unit_GCTABLE_END_VAL
	.long POSIX_SYS_DB_unit_GCTABLE_END_VAL
	.long PreWord_unit_GCTABLE_END_VAL
	.long PreInt_unit_GCTABLE_END_VAL
	.long OPTION_unit_GCTABLE_END_VAL
	.long STRING_CVT_unit_GCTABLE_END_VAL
	.long PreString_unit_GCTABLE_END_VAL
	.long StringCvt_unit_GCTABLE_END_VAL
	.long WORD_unit_GCTABLE_END_VAL
	.long PreVector_unit_GCTABLE_END_VAL
	.long PreReal_unit_GCTABLE_END_VAL
	.long CHAR_unit_GCTABLE_END_VAL
	.long STRING_unit_GCTABLE_END_VAL
	.long General_extern_unit_GCTABLE_END_VAL
	.long GENERAL_unit_GCTABLE_END_VAL
	.long General_unit_GCTABLE_END_VAL
	.long NumFormat_unit_GCTABLE_END_VAL
	.long Char_unit_GCTABLE_END_VAL
	.long String_unit_GCTABLE_END_VAL
	.long Option_unit_GCTABLE_END_VAL
	.long NumScan_unit_GCTABLE_END_VAL
	.long Word32_unit_GCTABLE_END_VAL
	.long SysWord_unit_GCTABLE_END_VAL
	.long POSIX_FLAGS_unit_GCTABLE_END_VAL
	.long POSIX_TTY_unit_GCTABLE_END_VAL
	.long PreTime_unit_GCTABLE_END_VAL
	.long TIME_unit_GCTABLE_END_VAL
	.long LIST_unit_GCTABLE_END_VAL
	.long List_unit_GCTABLE_END_VAL
	.long RealFormat_unit_GCTABLE_END_VAL
	.long IEEE_REAL_unit_GCTABLE_END_VAL
	.long INTEGER_unit_GCTABLE_END_VAL
	.long Int32_unit_GCTABLE_END_VAL
	.long Int_unit_GCTABLE_END_VAL
	.long IEEEReal_unit_GCTABLE_END_VAL
	.long MATH_unit_GCTABLE_END_VAL
	.long REAL_unit_GCTABLE_END_VAL
	.long Math64_unit_GCTABLE_END_VAL
	.long BOOL_unit_GCTABLE_END_VAL
	.long Bool_unit_GCTABLE_END_VAL
	.long Real64_unit_GCTABLE_END_VAL
	.long Real_unit_GCTABLE_END_VAL
	.long Time_unit_GCTABLE_END_VAL
	.long POSIX_PROC_ENV_unit_GCTABLE_END_VAL
	.long POSIX_SIGNAL_unit_GCTABLE_END_VAL
	.long Word8_unit_GCTABLE_END_VAL
	.long POSIX_PROCESS_unit_GCTABLE_END_VAL
	.long PreOS_unit_GCTABLE_END_VAL
	.long Position_unit_GCTABLE_END_VAL
	.long POSIX_FILE_SYS_unit_GCTABLE_END_VAL
	.long MONO_VECTOR_unit_GCTABLE_END_VAL
	.long VECTOR_unit_GCTABLE_END_VAL
	.long ARRAY_unit_GCTABLE_END_VAL
	.long Array_unit_GCTABLE_END_VAL
	.long Vector_unit_GCTABLE_END_VAL
	.long Word8Vector_unit_GCTABLE_END_VAL
	.long MONO_ARRAY_unit_GCTABLE_END_VAL
	.long Word8Array_unit_GCTABLE_END_VAL
	.long POSIX_IO_unit_GCTABLE_END_VAL
	.long POSIX_ERROR_unit_GCTABLE_END_VAL
	.long POSIX_unit_GCTABLE_END_VAL
	.long POSIX_extern_unit_GCTABLE_END_VAL
	.long PrePosix_unit_GCTABLE_END_VAL
	.long Word_unit_GCTABLE_END_VAL
	.long POSIX_FileSys_unit_GCTABLE_END_VAL
	.long POSIX_Sys_DB_unit_GCTABLE_END_VAL
	.long SysInt_unit_GCTABLE_END_VAL
	.long POSIX_Signal_unit_GCTABLE_END_VAL
	.long POSIX_Process_unit_GCTABLE_END_VAL
	.long SUBSTRING_unit_GCTABLE_END_VAL
	.long Substring_unit_GCTABLE_END_VAL
	.long BYTE_unit_GCTABLE_END_VAL
	.long Byte_unit_GCTABLE_END_VAL
	.long POSIX_Tty_unit_GCTABLE_END_VAL
	.long POSIX_ProcEnv_unit_GCTABLE_END_VAL
	.long POSIX_IO_Str_unit_GCTABLE_END_VAL
	.long POSIX_Error_unit_GCTABLE_END_VAL
	.long Posix_unit_GCTABLE_END_VAL
	.long OS_IO_unit_GCTABLE_END_VAL
	.long OS_IO_Str_unit_GCTABLE_END_VAL
	.long CLEAN_UP_unit_GCTABLE_END_VAL
	.long CleanUp_unit_GCTABLE_END_VAL
	.long OS_PROCESS_unit_GCTABLE_END_VAL
	.long OS_Process_unit_GCTABLE_END_VAL
	.long OS_PATH_unit_GCTABLE_END_VAL
	.long OS_PathFn_unit_GCTABLE_END_VAL
	.long OS_Path_unit_GCTABLE_END_VAL
	.long OS_FILE_SYS_unit_GCTABLE_END_VAL
	.long OS_FileSys_unit_GCTABLE_END_VAL
	.long OS_SIG_unit_GCTABLE_END_VAL
	.long OS_unit_GCTABLE_END_VAL
	.long PRIM_IO_unit_GCTABLE_END_VAL
	.long PrimIOFn_unit_GCTABLE_END_VAL
	.long BinPrimIO_unit_GCTABLE_END_VAL
	.long OS_PRIM_IO_unit_GCTABLE_END_VAL
	.long IO_SIG_unit_GCTABLE_END_VAL
	.long IO_unit_GCTABLE_END_VAL
	.long PosixPrimIOFn_unit_GCTABLE_END_VAL
	.long PosixBinPrimIO_unit_GCTABLE_END_VAL
	.long CharArray_unit_GCTABLE_END_VAL
	.long CharVector_unit_GCTABLE_END_VAL
	.long TextPrimIO_unit_GCTABLE_END_VAL
	.long PosixTextPrimIO_unit_GCTABLE_END_VAL
	.long CleanIO_unit_GCTABLE_END_VAL
	.long STREAM_IO_unit_GCTABLE_END_VAL
	.long TEXT_STREAM_IO_unit_GCTABLE_END_VAL
	.long TEXT_IO_unit_GCTABLE_END_VAL
	.long TextIOFn_unit_GCTABLE_END_VAL
	.long TextIO_unit_GCTABLE_END_VAL
	.long Word31_unit_GCTABLE_END_VAL
	.long TopLevel_unit_GCTABLE_END_VAL
	.long CommandLineHelp_unit_GCTABLE_END_VAL
	.long COMMAND_LINE_unit_GCTABLE_END_VAL
	.long CommandLine_unit_GCTABLE_END_VAL
	.long Help_unit_GCTABLE_END_VAL
	.long DATE_unit_GCTABLE_END_VAL
	.long Date_unit_GCTABLE_END_VAL
	.long Config_unit_GCTABLE_END_VAL
	.long MakeRelease_unit_GCTABLE_END_VAL
	.globl GLOBALS_BEGIN_VAL
GLOBALS_BEGIN_VAL:
	.long LINKUNIT_unit_GLOBALS_BEGIN_VAL
	.long Firstlude_unit_GLOBALS_BEGIN_VAL
	.long Prelude_unit_GLOBALS_BEGIN_VAL
	.long POSIX_SYS_DB_unit_GLOBALS_BEGIN_VAL
	.long PreWord_unit_GLOBALS_BEGIN_VAL
	.long PreInt_unit_GLOBALS_BEGIN_VAL
	.long OPTION_unit_GLOBALS_BEGIN_VAL
	.long STRING_CVT_unit_GLOBALS_BEGIN_VAL
	.long PreString_unit_GLOBALS_BEGIN_VAL
	.long StringCvt_unit_GLOBALS_BEGIN_VAL
	.long WORD_unit_GLOBALS_BEGIN_VAL
	.long PreVector_unit_GLOBALS_BEGIN_VAL
	.long PreReal_unit_GLOBALS_BEGIN_VAL
	.long CHAR_unit_GLOBALS_BEGIN_VAL
	.long STRING_unit_GLOBALS_BEGIN_VAL
	.long General_extern_unit_GLOBALS_BEGIN_VAL
	.long GENERAL_unit_GLOBALS_BEGIN_VAL
	.long General_unit_GLOBALS_BEGIN_VAL
	.long NumFormat_unit_GLOBALS_BEGIN_VAL
	.long Char_unit_GLOBALS_BEGIN_VAL
	.long String_unit_GLOBALS_BEGIN_VAL
	.long Option_unit_GLOBALS_BEGIN_VAL
	.long NumScan_unit_GLOBALS_BEGIN_VAL
	.long Word32_unit_GLOBALS_BEGIN_VAL
	.long SysWord_unit_GLOBALS_BEGIN_VAL
	.long POSIX_FLAGS_unit_GLOBALS_BEGIN_VAL
	.long POSIX_TTY_unit_GLOBALS_BEGIN_VAL
	.long PreTime_unit_GLOBALS_BEGIN_VAL
	.long TIME_unit_GLOBALS_BEGIN_VAL
	.long LIST_unit_GLOBALS_BEGIN_VAL
	.long List_unit_GLOBALS_BEGIN_VAL
	.long RealFormat_unit_GLOBALS_BEGIN_VAL
	.long IEEE_REAL_unit_GLOBALS_BEGIN_VAL
	.long INTEGER_unit_GLOBALS_BEGIN_VAL
	.long Int32_unit_GLOBALS_BEGIN_VAL
	.long Int_unit_GLOBALS_BEGIN_VAL
	.long IEEEReal_unit_GLOBALS_BEGIN_VAL
	.long MATH_unit_GLOBALS_BEGIN_VAL
	.long REAL_unit_GLOBALS_BEGIN_VAL
	.long Math64_unit_GLOBALS_BEGIN_VAL
	.long BOOL_unit_GLOBALS_BEGIN_VAL
	.long Bool_unit_GLOBALS_BEGIN_VAL
	.long Real64_unit_GLOBALS_BEGIN_VAL
	.long Real_unit_GLOBALS_BEGIN_VAL
	.long Time_unit_GLOBALS_BEGIN_VAL
	.long POSIX_PROC_ENV_unit_GLOBALS_BEGIN_VAL
	.long POSIX_SIGNAL_unit_GLOBALS_BEGIN_VAL
	.long Word8_unit_GLOBALS_BEGIN_VAL
	.long POSIX_PROCESS_unit_GLOBALS_BEGIN_VAL
	.long PreOS_unit_GLOBALS_BEGIN_VAL
	.long Position_unit_GLOBALS_BEGIN_VAL
	.long POSIX_FILE_SYS_unit_GLOBALS_BEGIN_VAL
	.long MONO_VECTOR_unit_GLOBALS_BEGIN_VAL
	.long VECTOR_unit_GLOBALS_BEGIN_VAL
	.long ARRAY_unit_GLOBALS_BEGIN_VAL
	.long Array_unit_GLOBALS_BEGIN_VAL
	.long Vector_unit_GLOBALS_BEGIN_VAL
	.long Word8Vector_unit_GLOBALS_BEGIN_VAL
	.long MONO_ARRAY_unit_GLOBALS_BEGIN_VAL
	.long Word8Array_unit_GLOBALS_BEGIN_VAL
	.long POSIX_IO_unit_GLOBALS_BEGIN_VAL
	.long POSIX_ERROR_unit_GLOBALS_BEGIN_VAL
	.long POSIX_unit_GLOBALS_BEGIN_VAL
	.long POSIX_extern_unit_GLOBALS_BEGIN_VAL
	.long PrePosix_unit_GLOBALS_BEGIN_VAL
	.long Word_unit_GLOBALS_BEGIN_VAL
	.long POSIX_FileSys_unit_GLOBALS_BEGIN_VAL
	.long POSIX_Sys_DB_unit_GLOBALS_BEGIN_VAL
	.long SysInt_unit_GLOBALS_BEGIN_VAL
	.long POSIX_Signal_unit_GLOBALS_BEGIN_VAL
	.long POSIX_Process_unit_GLOBALS_BEGIN_VAL
	.long SUBSTRING_unit_GLOBALS_BEGIN_VAL
	.long Substring_unit_GLOBALS_BEGIN_VAL
	.long BYTE_unit_GLOBALS_BEGIN_VAL
	.long Byte_unit_GLOBALS_BEGIN_VAL
	.long POSIX_Tty_unit_GLOBALS_BEGIN_VAL
	.long POSIX_ProcEnv_unit_GLOBALS_BEGIN_VAL
	.long POSIX_IO_Str_unit_GLOBALS_BEGIN_VAL
	.long POSIX_Error_unit_GLOBALS_BEGIN_VAL
	.long Posix_unit_GLOBALS_BEGIN_VAL
	.long OS_IO_unit_GLOBALS_BEGIN_VAL
	.long OS_IO_Str_unit_GLOBALS_BEGIN_VAL
	.long CLEAN_UP_unit_GLOBALS_BEGIN_VAL
	.long CleanUp_unit_GLOBALS_BEGIN_VAL
	.long OS_PROCESS_unit_GLOBALS_BEGIN_VAL
	.long OS_Process_unit_GLOBALS_BEGIN_VAL
	.long OS_PATH_unit_GLOBALS_BEGIN_VAL
	.long OS_PathFn_unit_GLOBALS_BEGIN_VAL
	.long OS_Path_unit_GLOBALS_BEGIN_VAL
	.long OS_FILE_SYS_unit_GLOBALS_BEGIN_VAL
	.long OS_FileSys_unit_GLOBALS_BEGIN_VAL
	.long OS_SIG_unit_GLOBALS_BEGIN_VAL
	.long OS_unit_GLOBALS_BEGIN_VAL
	.long PRIM_IO_unit_GLOBALS_BEGIN_VAL
	.long PrimIOFn_unit_GLOBALS_BEGIN_VAL
	.long BinPrimIO_unit_GLOBALS_BEGIN_VAL
	.long OS_PRIM_IO_unit_GLOBALS_BEGIN_VAL
	.long IO_SIG_unit_GLOBALS_BEGIN_VAL
	.long IO_unit_GLOBALS_BEGIN_VAL
	.long PosixPrimIOFn_unit_GLOBALS_BEGIN_VAL
	.long PosixBinPrimIO_unit_GLOBALS_BEGIN_VAL
	.long CharArray_unit_GLOBALS_BEGIN_VAL
	.long CharVector_unit_GLOBALS_BEGIN_VAL
	.long TextPrimIO_unit_GLOBALS_BEGIN_VAL
	.long PosixTextPrimIO_unit_GLOBALS_BEGIN_VAL
	.long CleanIO_unit_GLOBALS_BEGIN_VAL
	.long STREAM_IO_unit_GLOBALS_BEGIN_VAL
	.long TEXT_STREAM_IO_unit_GLOBALS_BEGIN_VAL
	.long TEXT_IO_unit_GLOBALS_BEGIN_VAL
	.long TextIOFn_unit_GLOBALS_BEGIN_VAL
	.long TextIO_unit_GLOBALS_BEGIN_VAL
	.long Word31_unit_GLOBALS_BEGIN_VAL
	.long TopLevel_unit_GLOBALS_BEGIN_VAL
	.long CommandLineHelp_unit_GLOBALS_BEGIN_VAL
	.long COMMAND_LINE_unit_GLOBALS_BEGIN_VAL
	.long CommandLine_unit_GLOBALS_BEGIN_VAL
	.long Help_unit_GLOBALS_BEGIN_VAL
	.long DATE_unit_GLOBALS_BEGIN_VAL
	.long Date_unit_GLOBALS_BEGIN_VAL
	.long Config_unit_GLOBALS_BEGIN_VAL
	.long MakeRelease_unit_GLOBALS_BEGIN_VAL
	.globl GLOBALS_END_VAL
GLOBALS_END_VAL:
	.long LINKUNIT_unit_GLOBALS_END_VAL
	.long Firstlude_unit_GLOBALS_END_VAL
	.long Prelude_unit_GLOBALS_END_VAL
	.long POSIX_SYS_DB_unit_GLOBALS_END_VAL
	.long PreWord_unit_GLOBALS_END_VAL
	.long PreInt_unit_GLOBALS_END_VAL
	.long OPTION_unit_GLOBALS_END_VAL
	.long STRING_CVT_unit_GLOBALS_END_VAL
	.long PreString_unit_GLOBALS_END_VAL
	.long StringCvt_unit_GLOBALS_END_VAL
	.long WORD_unit_GLOBALS_END_VAL
	.long PreVector_unit_GLOBALS_END_VAL
	.long PreReal_unit_GLOBALS_END_VAL
	.long CHAR_unit_GLOBALS_END_VAL
	.long STRING_unit_GLOBALS_END_VAL
	.long General_extern_unit_GLOBALS_END_VAL
	.long GENERAL_unit_GLOBALS_END_VAL
	.long General_unit_GLOBALS_END_VAL
	.long NumFormat_unit_GLOBALS_END_VAL
	.long Char_unit_GLOBALS_END_VAL
	.long String_unit_GLOBALS_END_VAL
	.long Option_unit_GLOBALS_END_VAL
	.long NumScan_unit_GLOBALS_END_VAL
	.long Word32_unit_GLOBALS_END_VAL
	.long SysWord_unit_GLOBALS_END_VAL
	.long POSIX_FLAGS_unit_GLOBALS_END_VAL
	.long POSIX_TTY_unit_GLOBALS_END_VAL
	.long PreTime_unit_GLOBALS_END_VAL
	.long TIME_unit_GLOBALS_END_VAL
	.long LIST_unit_GLOBALS_END_VAL
	.long List_unit_GLOBALS_END_VAL
	.long RealFormat_unit_GLOBALS_END_VAL
	.long IEEE_REAL_unit_GLOBALS_END_VAL
	.long INTEGER_unit_GLOBALS_END_VAL
	.long Int32_unit_GLOBALS_END_VAL
	.long Int_unit_GLOBALS_END_VAL
	.long IEEEReal_unit_GLOBALS_END_VAL
	.long MATH_unit_GLOBALS_END_VAL
	.long REAL_unit_GLOBALS_END_VAL
	.long Math64_unit_GLOBALS_END_VAL
	.long BOOL_unit_GLOBALS_END_VAL
	.long Bool_unit_GLOBALS_END_VAL
	.long Real64_unit_GLOBALS_END_VAL
	.long Real_unit_GLOBALS_END_VAL
	.long Time_unit_GLOBALS_END_VAL
	.long POSIX_PROC_ENV_unit_GLOBALS_END_VAL
	.long POSIX_SIGNAL_unit_GLOBALS_END_VAL
	.long Word8_unit_GLOBALS_END_VAL
	.long POSIX_PROCESS_unit_GLOBALS_END_VAL
	.long PreOS_unit_GLOBALS_END_VAL
	.long Position_unit_GLOBALS_END_VAL
	.long POSIX_FILE_SYS_unit_GLOBALS_END_VAL
	.long MONO_VECTOR_unit_GLOBALS_END_VAL
	.long VECTOR_unit_GLOBALS_END_VAL
	.long ARRAY_unit_GLOBALS_END_VAL
	.long Array_unit_GLOBALS_END_VAL
	.long Vector_unit_GLOBALS_END_VAL
	.long Word8Vector_unit_GLOBALS_END_VAL
	.long MONO_ARRAY_unit_GLOBALS_END_VAL
	.long Word8Array_unit_GLOBALS_END_VAL
	.long POSIX_IO_unit_GLOBALS_END_VAL
	.long POSIX_ERROR_unit_GLOBALS_END_VAL
	.long POSIX_unit_GLOBALS_END_VAL
	.long POSIX_extern_unit_GLOBALS_END_VAL
	.long PrePosix_unit_GLOBALS_END_VAL
	.long Word_unit_GLOBALS_END_VAL
	.long POSIX_FileSys_unit_GLOBALS_END_VAL
	.long POSIX_Sys_DB_unit_GLOBALS_END_VAL
	.long SysInt_unit_GLOBALS_END_VAL
	.long POSIX_Signal_unit_GLOBALS_END_VAL
	.long POSIX_Process_unit_GLOBALS_END_VAL
	.long SUBSTRING_unit_GLOBALS_END_VAL
	.long Substring_unit_GLOBALS_END_VAL
	.long BYTE_unit_GLOBALS_END_VAL
	.long Byte_unit_GLOBALS_END_VAL
	.long POSIX_Tty_unit_GLOBALS_END_VAL
	.long POSIX_ProcEnv_unit_GLOBALS_END_VAL
	.long POSIX_IO_Str_unit_GLOBALS_END_VAL
	.long POSIX_Error_unit_GLOBALS_END_VAL
	.long Posix_unit_GLOBALS_END_VAL
	.long OS_IO_unit_GLOBALS_END_VAL
	.long OS_IO_Str_unit_GLOBALS_END_VAL
	.long CLEAN_UP_unit_GLOBALS_END_VAL
	.long CleanUp_unit_GLOBALS_END_VAL
	.long OS_PROCESS_unit_GLOBALS_END_VAL
	.long OS_Process_unit_GLOBALS_END_VAL
	.long OS_PATH_unit_GLOBALS_END_VAL
	.long OS_PathFn_unit_GLOBALS_END_VAL
	.long OS_Path_unit_GLOBALS_END_VAL
	.long OS_FILE_SYS_unit_GLOBALS_END_VAL
	.long OS_FileSys_unit_GLOBALS_END_VAL
	.long OS_SIG_unit_GLOBALS_END_VAL
	.long OS_unit_GLOBALS_END_VAL
	.long PRIM_IO_unit_GLOBALS_END_VAL
	.long PrimIOFn_unit_GLOBALS_END_VAL
	.long BinPrimIO_unit_GLOBALS_END_VAL
	.long OS_PRIM_IO_unit_GLOBALS_END_VAL
	.long IO_SIG_unit_GLOBALS_END_VAL
	.long IO_unit_GLOBALS_END_VAL
	.long PosixPrimIOFn_unit_GLOBALS_END_VAL
	.long PosixBinPrimIO_unit_GLOBALS_END_VAL
	.long CharArray_unit_GLOBALS_END_VAL
	.long CharVector_unit_GLOBALS_END_VAL
	.long TextPrimIO_unit_GLOBALS_END_VAL
	.long PosixTextPrimIO_unit_GLOBALS_END_VAL
	.long CleanIO_unit_GLOBALS_END_VAL
	.long STREAM_IO_unit_GLOBALS_END_VAL
	.long TEXT_STREAM_IO_unit_GLOBALS_END_VAL
	.long TEXT_IO_unit_GLOBALS_END_VAL
	.long TextIOFn_unit_GLOBALS_END_VAL
	.long TextIO_unit_GLOBALS_END_VAL
	.long Word31_unit_GLOBALS_END_VAL
	.long TopLevel_unit_GLOBALS_END_VAL
	.long CommandLineHelp_unit_GLOBALS_END_VAL
	.long COMMAND_LINE_unit_GLOBALS_END_VAL
	.long CommandLine_unit_GLOBALS_END_VAL
	.long Help_unit_GLOBALS_END_VAL
	.long DATE_unit_GLOBALS_END_VAL
	.long Date_unit_GLOBALS_END_VAL
	.long Config_unit_GLOBALS_END_VAL
	.long MakeRelease_unit_GLOBALS_END_VAL
	.globl TRACE_GLOBALS_BEGIN_VAL
TRACE_GLOBALS_BEGIN_VAL:
	.long LINKUNIT_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Firstlude_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Prelude_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_SYS_DB_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PreWord_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PreInt_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OPTION_unit_TRACE_GLOBALS_BEGIN_VAL
	.long STRING_CVT_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PreString_unit_TRACE_GLOBALS_BEGIN_VAL
	.long StringCvt_unit_TRACE_GLOBALS_BEGIN_VAL
	.long WORD_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PreVector_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PreReal_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CHAR_unit_TRACE_GLOBALS_BEGIN_VAL
	.long STRING_unit_TRACE_GLOBALS_BEGIN_VAL
	.long General_extern_unit_TRACE_GLOBALS_BEGIN_VAL
	.long GENERAL_unit_TRACE_GLOBALS_BEGIN_VAL
	.long General_unit_TRACE_GLOBALS_BEGIN_VAL
	.long NumFormat_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Char_unit_TRACE_GLOBALS_BEGIN_VAL
	.long String_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Option_unit_TRACE_GLOBALS_BEGIN_VAL
	.long NumScan_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Word32_unit_TRACE_GLOBALS_BEGIN_VAL
	.long SysWord_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_FLAGS_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_TTY_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PreTime_unit_TRACE_GLOBALS_BEGIN_VAL
	.long TIME_unit_TRACE_GLOBALS_BEGIN_VAL
	.long LIST_unit_TRACE_GLOBALS_BEGIN_VAL
	.long List_unit_TRACE_GLOBALS_BEGIN_VAL
	.long RealFormat_unit_TRACE_GLOBALS_BEGIN_VAL
	.long IEEE_REAL_unit_TRACE_GLOBALS_BEGIN_VAL
	.long INTEGER_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Int32_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Int_unit_TRACE_GLOBALS_BEGIN_VAL
	.long IEEEReal_unit_TRACE_GLOBALS_BEGIN_VAL
	.long MATH_unit_TRACE_GLOBALS_BEGIN_VAL
	.long REAL_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Math64_unit_TRACE_GLOBALS_BEGIN_VAL
	.long BOOL_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Bool_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Real64_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Real_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Time_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_PROC_ENV_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_SIGNAL_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Word8_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_PROCESS_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PreOS_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Position_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_FILE_SYS_unit_TRACE_GLOBALS_BEGIN_VAL
	.long MONO_VECTOR_unit_TRACE_GLOBALS_BEGIN_VAL
	.long VECTOR_unit_TRACE_GLOBALS_BEGIN_VAL
	.long ARRAY_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Array_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Vector_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Word8Vector_unit_TRACE_GLOBALS_BEGIN_VAL
	.long MONO_ARRAY_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Word8Array_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_ERROR_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_extern_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PrePosix_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Word_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_FileSys_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_Sys_DB_unit_TRACE_GLOBALS_BEGIN_VAL
	.long SysInt_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_Signal_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_Process_unit_TRACE_GLOBALS_BEGIN_VAL
	.long SUBSTRING_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Substring_unit_TRACE_GLOBALS_BEGIN_VAL
	.long BYTE_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Byte_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_Tty_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_ProcEnv_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_IO_Str_unit_TRACE_GLOBALS_BEGIN_VAL
	.long POSIX_Error_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Posix_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_IO_Str_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CLEAN_UP_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CleanUp_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_PROCESS_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_Process_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_PATH_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_PathFn_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_Path_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_FILE_SYS_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_FileSys_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_SIG_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PRIM_IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PrimIOFn_unit_TRACE_GLOBALS_BEGIN_VAL
	.long BinPrimIO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long OS_PRIM_IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long IO_SIG_unit_TRACE_GLOBALS_BEGIN_VAL
	.long IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PosixPrimIOFn_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PosixBinPrimIO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CharArray_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CharVector_unit_TRACE_GLOBALS_BEGIN_VAL
	.long TextPrimIO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long PosixTextPrimIO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CleanIO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long STREAM_IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long TEXT_STREAM_IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long TEXT_IO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long TextIOFn_unit_TRACE_GLOBALS_BEGIN_VAL
	.long TextIO_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Word31_unit_TRACE_GLOBALS_BEGIN_VAL
	.long TopLevel_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CommandLineHelp_unit_TRACE_GLOBALS_BEGIN_VAL
	.long COMMAND_LINE_unit_TRACE_GLOBALS_BEGIN_VAL
	.long CommandLine_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Help_unit_TRACE_GLOBALS_BEGIN_VAL
	.long DATE_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Date_unit_TRACE_GLOBALS_BEGIN_VAL
	.long Config_unit_TRACE_GLOBALS_BEGIN_VAL
	.long MakeRelease_unit_TRACE_GLOBALS_BEGIN_VAL
	.globl TRACE_GLOBALS_END_VAL
TRACE_GLOBALS_END_VAL:
	.long LINKUNIT_unit_TRACE_GLOBALS_END_VAL
	.long Firstlude_unit_TRACE_GLOBALS_END_VAL
	.long Prelude_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_SYS_DB_unit_TRACE_GLOBALS_END_VAL
	.long PreWord_unit_TRACE_GLOBALS_END_VAL
	.long PreInt_unit_TRACE_GLOBALS_END_VAL
	.long OPTION_unit_TRACE_GLOBALS_END_VAL
	.long STRING_CVT_unit_TRACE_GLOBALS_END_VAL
	.long PreString_unit_TRACE_GLOBALS_END_VAL
	.long StringCvt_unit_TRACE_GLOBALS_END_VAL
	.long WORD_unit_TRACE_GLOBALS_END_VAL
	.long PreVector_unit_TRACE_GLOBALS_END_VAL
	.long PreReal_unit_TRACE_GLOBALS_END_VAL
	.long CHAR_unit_TRACE_GLOBALS_END_VAL
	.long STRING_unit_TRACE_GLOBALS_END_VAL
	.long General_extern_unit_TRACE_GLOBALS_END_VAL
	.long GENERAL_unit_TRACE_GLOBALS_END_VAL
	.long General_unit_TRACE_GLOBALS_END_VAL
	.long NumFormat_unit_TRACE_GLOBALS_END_VAL
	.long Char_unit_TRACE_GLOBALS_END_VAL
	.long String_unit_TRACE_GLOBALS_END_VAL
	.long Option_unit_TRACE_GLOBALS_END_VAL
	.long NumScan_unit_TRACE_GLOBALS_END_VAL
	.long Word32_unit_TRACE_GLOBALS_END_VAL
	.long SysWord_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_FLAGS_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_TTY_unit_TRACE_GLOBALS_END_VAL
	.long PreTime_unit_TRACE_GLOBALS_END_VAL
	.long TIME_unit_TRACE_GLOBALS_END_VAL
	.long LIST_unit_TRACE_GLOBALS_END_VAL
	.long List_unit_TRACE_GLOBALS_END_VAL
	.long RealFormat_unit_TRACE_GLOBALS_END_VAL
	.long IEEE_REAL_unit_TRACE_GLOBALS_END_VAL
	.long INTEGER_unit_TRACE_GLOBALS_END_VAL
	.long Int32_unit_TRACE_GLOBALS_END_VAL
	.long Int_unit_TRACE_GLOBALS_END_VAL
	.long IEEEReal_unit_TRACE_GLOBALS_END_VAL
	.long MATH_unit_TRACE_GLOBALS_END_VAL
	.long REAL_unit_TRACE_GLOBALS_END_VAL
	.long Math64_unit_TRACE_GLOBALS_END_VAL
	.long BOOL_unit_TRACE_GLOBALS_END_VAL
	.long Bool_unit_TRACE_GLOBALS_END_VAL
	.long Real64_unit_TRACE_GLOBALS_END_VAL
	.long Real_unit_TRACE_GLOBALS_END_VAL
	.long Time_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_PROC_ENV_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_SIGNAL_unit_TRACE_GLOBALS_END_VAL
	.long Word8_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_PROCESS_unit_TRACE_GLOBALS_END_VAL
	.long PreOS_unit_TRACE_GLOBALS_END_VAL
	.long Position_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_FILE_SYS_unit_TRACE_GLOBALS_END_VAL
	.long MONO_VECTOR_unit_TRACE_GLOBALS_END_VAL
	.long VECTOR_unit_TRACE_GLOBALS_END_VAL
	.long ARRAY_unit_TRACE_GLOBALS_END_VAL
	.long Array_unit_TRACE_GLOBALS_END_VAL
	.long Vector_unit_TRACE_GLOBALS_END_VAL
	.long Word8Vector_unit_TRACE_GLOBALS_END_VAL
	.long MONO_ARRAY_unit_TRACE_GLOBALS_END_VAL
	.long Word8Array_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_IO_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_ERROR_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_extern_unit_TRACE_GLOBALS_END_VAL
	.long PrePosix_unit_TRACE_GLOBALS_END_VAL
	.long Word_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_FileSys_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_Sys_DB_unit_TRACE_GLOBALS_END_VAL
	.long SysInt_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_Signal_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_Process_unit_TRACE_GLOBALS_END_VAL
	.long SUBSTRING_unit_TRACE_GLOBALS_END_VAL
	.long Substring_unit_TRACE_GLOBALS_END_VAL
	.long BYTE_unit_TRACE_GLOBALS_END_VAL
	.long Byte_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_Tty_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_ProcEnv_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_IO_Str_unit_TRACE_GLOBALS_END_VAL
	.long POSIX_Error_unit_TRACE_GLOBALS_END_VAL
	.long Posix_unit_TRACE_GLOBALS_END_VAL
	.long OS_IO_unit_TRACE_GLOBALS_END_VAL
	.long OS_IO_Str_unit_TRACE_GLOBALS_END_VAL
	.long CLEAN_UP_unit_TRACE_GLOBALS_END_VAL
	.long CleanUp_unit_TRACE_GLOBALS_END_VAL
	.long OS_PROCESS_unit_TRACE_GLOBALS_END_VAL
	.long OS_Process_unit_TRACE_GLOBALS_END_VAL
	.long OS_PATH_unit_TRACE_GLOBALS_END_VAL
	.long OS_PathFn_unit_TRACE_GLOBALS_END_VAL
	.long OS_Path_unit_TRACE_GLOBALS_END_VAL
	.long OS_FILE_SYS_unit_TRACE_GLOBALS_END_VAL
	.long OS_FileSys_unit_TRACE_GLOBALS_END_VAL
	.long OS_SIG_unit_TRACE_GLOBALS_END_VAL
	.long OS_unit_TRACE_GLOBALS_END_VAL
	.long PRIM_IO_unit_TRACE_GLOBALS_END_VAL
	.long PrimIOFn_unit_TRACE_GLOBALS_END_VAL
	.long BinPrimIO_unit_TRACE_GLOBALS_END_VAL
	.long OS_PRIM_IO_unit_TRACE_GLOBALS_END_VAL
	.long IO_SIG_unit_TRACE_GLOBALS_END_VAL
	.long IO_unit_TRACE_GLOBALS_END_VAL
	.long PosixPrimIOFn_unit_TRACE_GLOBALS_END_VAL
	.long PosixBinPrimIO_unit_TRACE_GLOBALS_END_VAL
	.long CharArray_unit_TRACE_GLOBALS_END_VAL
	.long CharVector_unit_TRACE_GLOBALS_END_VAL
	.long TextPrimIO_unit_TRACE_GLOBALS_END_VAL
	.long PosixTextPrimIO_unit_TRACE_GLOBALS_END_VAL
	.long CleanIO_unit_TRACE_GLOBALS_END_VAL
	.long STREAM_IO_unit_TRACE_GLOBALS_END_VAL
	.long TEXT_STREAM_IO_unit_TRACE_GLOBALS_END_VAL
	.long TEXT_IO_unit_TRACE_GLOBALS_END_VAL
	.long TextIOFn_unit_TRACE_GLOBALS_END_VAL
	.long TextIO_unit_TRACE_GLOBALS_END_VAL
	.long Word31_unit_TRACE_GLOBALS_END_VAL
	.long TopLevel_unit_TRACE_GLOBALS_END_VAL
	.long CommandLineHelp_unit_TRACE_GLOBALS_END_VAL
	.long COMMAND_LINE_unit_TRACE_GLOBALS_END_VAL
	.long CommandLine_unit_TRACE_GLOBALS_END_VAL
	.long Help_unit_TRACE_GLOBALS_END_VAL
	.long DATE_unit_TRACE_GLOBALS_END_VAL
	.long Date_unit_TRACE_GLOBALS_END_VAL
	.long Config_unit_TRACE_GLOBALS_END_VAL
	.long MakeRelease_unit_TRACE_GLOBALS_END_VAL
	.long 0 !filler

	.data
	.align 8
	.globl LINKUNIT_unit_TRACE_GLOBALS_BEGIN_VAL
LINKUNIT_unit_TRACE_GLOBALS_BEGIN_VAL:
	.globl LINKUNIT_unit_TRACE_GLOBALS_END_VAL
LINKUNIT_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
