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
	.globl Array_unit_GCTABLE_BEGIN_VAL
Array_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl Array_unit_CODE_END_VAL
	.globl Array_unit_CODE_BEGIN_VAL
Array_unit_CODE_BEGIN_VAL:
	.text
	.align 8
	.global Array__code_61729
 ! arguments : [$61731,$8] [$55076,$9] 
 ! results    : [$67202,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
Array__code_61729:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67217
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67217:
	st	%r15, [%sp+92]
code_67211:
funtop_67197:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67212
	nop
code_67213:
	call	GCFromML ! delay slot empty
	nop
needgc_67212:
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
code_67216:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array__code_61729,(.-Array__code_61729)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67212
	.word 0xb8003006
	.word 0xbffc3e00
	.word 0xbffc3c00
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array__code_61734
 ! arguments : [$61736,$8] [$55098,$9] 
 ! results    : [$67193,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
Array__code_61734:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67224
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67224:
	st	%r15, [%sp+92]
code_67218:
funtop_67188:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67219
	nop
code_67220:
	call	GCFromML ! delay slot empty
	nop
needgc_67219:
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	or	%r0, 1, %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
code_67223:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array__code_61734,(.-Array__code_61734)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67219
	.word 0xb8003006
	.word 0xbffc3e00
	.word 0xbffc3c00
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_anonfun_code_61739
 ! arguments : [$61741,$8] [$61742,$9] [$54810,$10] 
 ! results    : [$54810,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
Array_anonfun_code_61739:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67228
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67228:
	st	%r15, [%sp+92]
	mov	%r10, %r8
code_67225:
funtop_67184:
code_67227:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_anonfun_code_61739,(.-Array_anonfun_code_61739)

	.section	".rodata"
	.text
	.align 8
	.global Array_anonfun_code_61744
 ! arguments : [$61746,$8] [$61747,$9] [$54816,$10] 
 ! results    : [$54816,$8] 
 ! destroys   :  $9 $8
 ! modifies   :  $9 $8
Array_anonfun_code_61744:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67232
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67232:
	st	%r15, [%sp+92]
	mov	%r10, %r8
code_67229:
funtop_67180:
code_67231:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_anonfun_code_61744,(.-Array_anonfun_code_61744)

	.section	".rodata"
	.text
	.align 8
	.global Array_array_length_code_61754
 ! arguments : [$61756,$8] [$61757,$9] [$54824,$10] 
 ! results    : [$67179,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_array_length_code_61754:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67239
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67239:
	st	%r15, [%sp+92]
	mov	%r8, %r11
	mov	%r10, %r9
code_67233:
funtop_67170:
	! making direct call 
	sethi	%hi(polyLen_INT), %r8
	ld	[%r8+%lo(polyLen_INT)], %r10
	mov	%r11, %r8
	ld	[%sp+92], %r15
	jmpl	%r10, %r0
	add	%sp, 96, %sp
code_67235:
	! done making tail call
code_67237:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_array_length_code_61754,(.-Array_array_length_code_61754)

	.section	".rodata"
	.text
	.align 8
	.global Array_outer_valbind_r_code_61749
 ! arguments : [$61751,$8] [$54833,$9] [$61752,$10] [$54834,$11] 
 ! results    : [$67164,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_outer_valbind_r_code_61749:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67247
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67247:
	st	%r15, [%sp+92]
code_67240:
funtop_67156:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67241
	nop
code_67242:
	call	GCFromML ! delay slot empty
	nop
needgc_67241:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_array_length_code_61754), %r8
	or	%r8, %lo(Array_array_length_code_61754), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67246:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_outer_valbind_r_code_61749,(.-Array_outer_valbind_r_code_61749)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67241
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_unsafe_sub_code_61766
 ! arguments : [$61768,$8] [$61769,$9] [$58307,$10] [$58308,$11] 
 ! results    : [$67155,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_unsafe_sub_code_61766:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67254
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67254:
	st	%r15, [%sp+92]
	mov	%r10, %r12
	mov	%r11, %r10
code_67248:
funtop_67141:
	! Proj_c at label type_56482_INT
	ld	[%r8], %r9
	! Proj_c at label varpoly_c_54925_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	! making direct call 
	sethi	%hi(polySub_INT), %r8
	ld	[%r8+%lo(polySub_INT)], %r11
	mov	%r9, %r8
	mov	%r12, %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_67250:
	! done making tail call
code_67252:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_unsafe_sub_code_61766,(.-Array_unsafe_sub_code_61766)

	.section	".rodata"
	.text
	.align 8
	.global Array_outer_valbind_r_code_61761
 ! arguments : [$61763,$8] [$54925,$9] [$61764,$10] [$54926,$11] 
 ! results    : [$67135,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_outer_valbind_r_code_61761:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67262
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67262:
	st	%r15, [%sp+92]
code_67255:
funtop_67123:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67256
	nop
code_67257:
	call	GCFromML ! delay slot empty
	nop
needgc_67256:
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
	sethi	%hi(Array_unsafe_sub_code_61766), %r8
	or	%r8, %lo(Array_unsafe_sub_code_61766), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67261:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_outer_valbind_r_code_61761,(.-Array_outer_valbind_r_code_61761)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67256
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_unsafe_update_code_61780
 ! arguments : [$61782,$8] [$61783,$9] [$58318,$10] [$58319,$11] [$58320,$12] 
 ! results    : [$67122,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_unsafe_update_code_61780:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67269
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67269:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r13
	mov	%r11, %r10
	mov	%r12, %r11
code_67263:
funtop_67107:
	! Proj_c at label type_56500_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r9
	! Proj_c at label varpoly_c_54949_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	! making direct call 
	sethi	%hi(polyUpdate_INT), %r8
	ld	[%r8+%lo(polyUpdate_INT)], %r12
	mov	%r9, %r8
	mov	%r13, %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_67265:
	! done making tail call
code_67267:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_unsafe_update_code_61780,(.-Array_unsafe_update_code_61780)

	.section	".rodata"
	.text
	.align 8
	.global Array_outer_valbind_r_code_61775
 ! arguments : [$61777,$8] [$54949,$9] [$61778,$10] [$54950,$11] 
 ! results    : [$67101,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_outer_valbind_r_code_61775:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67277
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67277:
	st	%r15, [%sp+92]
code_67270:
funtop_67089:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67271
	nop
code_67272:
	call	GCFromML ! delay slot empty
	nop
needgc_67271:
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
	sethi	%hi(Array_unsafe_update_code_61780), %r8
	or	%r8, %lo(Array_unsafe_update_code_61780), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67276:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_outer_valbind_r_code_61775,(.-Array_outer_valbind_r_code_61775)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67271
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_anonfun_code_61789
 ! arguments : [$61791,$8] [$61792,$9] [$58362,$10] [$58363,$11] 
 ! results    : [$67088,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_anonfun_code_61789:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67283
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67283:
	st	%r15, [%sp+92]
code_67278:
funtop_67083:
	cmp	%r10, %r11
	or	%r0, 1, %r8
	bcs	cmpui_67279
	nop
code_67280:
	or	%r0, 0, %r8
cmpui_67279:
code_67282:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_anonfun_code_61789,(.-Array_anonfun_code_61789)

	.section	".rodata"
	.text
	.align 8
	.global Array_anonfun_code_61794
 ! arguments : [$61796,$8] [$61797,$9] [$58368,$10] [$58369,$11] 
 ! results    : [$67082,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_anonfun_code_61794:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67287
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67287:
	st	%r15, [%sp+92]
code_67284:
funtop_67077:
	sub	%r10, %r11, %r8
code_67286:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_anonfun_code_61794,(.-Array_anonfun_code_61794)

	.section	".rodata"
	.text
	.align 8
	.global Array_anonfun_code_61799
 ! arguments : [$61801,$8] [$61802,$9] [$58371,$10] [$58372,$11] 
 ! results    : [$67076,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_anonfun_code_61799:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67291
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67291:
	st	%r15, [%sp+92]
code_67288:
funtop_67071:
	add	%r10, %r11, %r8
code_67290:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_anonfun_code_61799,(.-Array_anonfun_code_61799)

	.section	".rodata"
	.text
	.align 8
	.global Array_anonfun_code_61809
 ! arguments : [$61811,$8] [$61812,$9] [$58375,$10] [$58376,$11] 
 ! results    : [$67070,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_anonfun_code_61809:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67297
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67297:
	st	%r15, [%sp+92]
code_67292:
funtop_67063:
	cmp	%r10, %r11
	or	%r0, 1, %r8
	be	cmpui_67293
	nop
code_67294:
	or	%r0, 0, %r8
cmpui_67293:
code_67296:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_anonfun_code_61809,(.-Array_anonfun_code_61809)

	.section	".rodata"
	.text
	.align 8
	.global Array__r_code_61804
 ! arguments : [$61806,$8] [$55081,$9] [$61807,$10] [$55082,$11] 
 ! results    : [$67057,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array__r_code_61804:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67305
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67305:
	st	%r15, [%sp+92]
code_67298:
funtop_67049:
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67299
	nop
code_67300:
	call	GCFromML ! delay slot empty
	nop
needgc_67299:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_61809), %r8
	or	%r8, %lo(Array_anonfun_code_61809), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67304:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array__r_code_61804,(.-Array__r_code_61804)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67299
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_array_inner_code_61821
 ! arguments : [$61823,$8] [$61824,$9] [$58389,$10] [$58390,$11] 
 ! results    : [$66975,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_array_inner_code_61821:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67373
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67373:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r11, [%sp+104]
code_67306:
funtop_66928:
	! Proj_c at label type_56653_INT
	ld	[%sp+96], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+100]
	! Proj_c at label var_poly_c_55172_INT
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r10, 0
	or	%r0, 1, %r8
	be	cmpui_67307
	nop
code_67308:
	or	%r0, 0, %r8
cmpui_67307:
	cmp	%r8, 0
	bne,pn	%icc,one_case_66943
	nop
zero_case_66942:
	sethi	%hi(1048576), %r8
	cmp	%r8, %r10
	or	%r0, 1, %r8
	bl	cmpsi_67310
	nop
code_67311:
	or	%r0, 0, %r8
cmpsi_67310:
	cmp	%r8, 0
	bne,pn	%icc,one_case_66953
	nop
zero_case_66952:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_67372:
	mov	%r8, %r9
code_67314:
	! done making normal call
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	ld	[%sp+100], %r8
	ld	[%sp+104], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_67316:
	! done making tail call
	ba	after_zeroone_66954 ! delay slot empty
	nop
one_case_66953:
	sethi	%hi(mk_56675), %r8
	or	%r8, %lo(mk_56675), %r9
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
after_zeroone_66954:
	ba	after_zeroone_66944 ! delay slot empty
	nop
one_case_66943:
	ld	[%sp+100], %r17
	cmp	%r17, 11
	be,pn	%icc,array_float_66981
	nop
code_67322:
	ld	[%sp+100], %r17
	cmp	%r17, 2
	be,pn	%icc,array_int_66982
	nop
code_67323:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	be,pn	%icc,array_char_66983
	nop
code_67324:
	or	%r0, 0, %r9
	or	%r0, 0, %r10
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_66992
	nop
code_67325:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r10, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_67368:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_67326:
	ba	array_ptr_aftert_66991 ! delay slot empty
	nop
array_ptr_alloc_66992:
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
	ble	needgc_67328
	nop
code_67329:
	call	GCFromML ! delay slot empty
	nop
needgc_67328:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_66998
	add	%r9, %r0, %r9
array_init_loopto_66999:
	add	%r11, %r9, %r8
	st	%r10, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_66998:
	cmp	%r9, 0
	bge	array_init_loopto_66999
	nop
array_ptr_aftert_66991:
	ba	array_after_66980
	mov	%r11, %r8
array_int_66982:
	or	%r0, 0, %r11
	or	%r0, 0, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_67006
	nop
code_67335:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67369:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67336:
	ba	array_int_after_67005 ! delay slot empty
	nop
array_int_small_67006:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67338
	nop
code_67339:
	call	GCFromML ! delay slot empty
	nop
needgc_67338:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_67013
	add	%r9, %r0, %r9
array_init_loopto_67014:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_67013:
	cmp	%r9, 0
	bge	array_init_loopto_67014
	nop
array_int_after_67005:
	ba	array_after_66980
	mov	%r12, %r8
array_char_66983:
	or	%r0, 0, %r11
	or	%r0, 0, %r9
	! initializing int/ptr array start
	add	%r9, 3, %r8
	srl	%r8, 2, %r10
	sll	%r11, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r8
	sll	%r8, 16, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r11
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_67024
	nop
code_67345:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67370:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67346:
	ba	array_int_after_67023 ! delay slot empty
	nop
array_int_small_67024:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67348
	nop
code_67349:
	call	GCFromML ! delay slot empty
	nop
needgc_67348:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_67031
	add	%r9, %r0, %r9
array_init_loopto_67032:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_67031:
	cmp	%r9, 0
	bge	array_init_loopto_67032
	nop
array_int_after_67023:
	ba	array_after_66980
	mov	%r12, %r8
array_float_66981:
	or	%r0, 0, %r11
	add	%r11, %r11, %r9
	add	%r9, 2, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_float_smallalloc_67041
	nop
code_67355:
	mov	%r11, %r8
	std	%f0, [%r2+392]
	ld	[%r2+392], %r9
	ld	[%r2+396], %r10
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_67371:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_67356:
	ba	array_float_after_67042 ! delay slot empty
	nop
array_float_smallalloc_67041:
	sll	%r9, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67358
	nop
code_67359:
	call	GCFromML ! delay slot empty
	nop
needgc_67358:
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_67361
	nop
code_67362:
	nop
	or	%r0, %r8, %r4
cmv_67361:
	sll	%r11, 3, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r8
	add	%r8, %r0, %r8
	or	%r8, 3, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r10
	sll	%r11, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_67043
	sub	%r11, 1, %r9
array_float_top_67044:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	std	%f0, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_67043:
	cmp	%r9, 0
	bge	array_float_top_67044
	nop
array_float_after_67042:
	mov	%r10, %r8
array_after_66980:
after_zeroone_66944:
code_67366:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_array_inner_code_61821,(.-Array_array_inner_code_61821)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67368
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67328
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67369
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67338
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67370
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67348
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67371
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67358
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67372
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00350000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_array_r_code_61816
 ! arguments : [$61818,$8] [$55172,$9] [$61819,$10] [$55173,$11] 
 ! results    : [$66922,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_array_r_code_61816:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67381
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67381:
	st	%r15, [%sp+92]
code_67374:
funtop_66910:
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67375
	nop
code_67376:
	call	GCFromML ! delay slot empty
	nop
needgc_67375:
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
	sethi	%hi(Array_array_inner_code_61821), %r8
	or	%r8, %lo(Array_array_inner_code_61821), %r8
	st	%r8, [%r4+4]
	st	%r9, [%r4+8]
	or	%r0, 256, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67380:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_array_r_code_61816,(.-Array_array_r_code_61816)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67375
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc3000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_anonfun_code_61848
 ! arguments : [$61850,$8] [$61851,$9] [$55254,$10] 
 ! results    : [$66852,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_61848:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67397
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_67397:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r10, [%sp+120]
code_67382:
funtop_66826:
	ld	[%r9], %r17
	st	%r17, [%sp+116]
	ld	[%r9+4], %r17
	st	%r17, [%sp+100]
	ld	[%r9+8], %r17
	st	%r17, [%sp+112]
	ld	[%r9+12], %r17
	st	%r17, [%sp+108]
sumarm_66847:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	bne	sumarm_66848
	nop
code_67383:
	ba	after_sum_66844
	or	%r0, 256, %r8
sumarm_66848:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_67396:
	st	%r8, [%sp+104]
code_67386:
	! done making normal call
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+100]
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r13
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r13, %r15
	ld	[%sp+120], %r11
code_67393:
code_67387:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r11
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_67394:
	mov	%r8, %r9
code_67388:
	! done making normal call
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+104], %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_67389:
	! done making tail call
	ba	after_sum_66844 ! delay slot empty
	nop
sumarm_66853:
after_sum_66844:
code_67392:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_anonfun_code_61848,(.-Array_anonfun_code_61848)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67393
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04040000
		! -------- label,sizes,reg
	.long code_67394
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67396
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05440000
	.text
	.align 8
	.global Array_loop_code_61843
 ! arguments : [$61845,$8] [$61846,$9] [$55252,$10] 
 ! results    : [$66821,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
Array_loop_code_61843:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67405
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67405:
	st	%r15, [%sp+92]
	mov	%r8, %r12
	mov	%r10, %r13
code_67398:
funtop_66798:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67399
	nop
code_67400:
	call	GCFromML ! delay slot empty
	nop
needgc_67399:
	ld	[%r9], %r11
	ld	[%r9+4], %r10
	ld	[%r9+8], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r13, [%r4+8]
	st	%r10, [%r4+12]
	st	%r9, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_61848), %r8
	or	%r8, %lo(Array_anonfun_code_61848), %r8
	st	%r8, [%r4+4]
	st	%r12, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67404:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_loop_code_61843,(.-Array_loop_code_61843)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67399
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc0000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_fromListPRIME_inner_code_61835
 ! arguments : [$61837,$8] [$61838,$9] [$58423,$10] [$58424,$11] 
 ! results    : [$66724,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fromListPRIME_inner_code_61835:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67483
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_67483:
	st	%r15, [%sp+92]
	st	%r11, [%sp+116]
code_67406:
funtop_66637:
	! Proj_c at label type_56739_INT
	ld	[%r8], %r17
	st	%r17, [%sp+112]
	! Proj_c at label var_poly_c_55228_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	ld	[%r9], %r17
	st	%r17, [%sp+108]
	ld	[%r9+4], %r17
	st	%r17, [%sp+104]
	sethi	%hi(1048576), %r8
	cmp	%r8, %r10
	or	%r0, 1, %r8
	bl	cmpsi_67407
	nop
code_67408:
	or	%r0, 0, %r8
cmpsi_67407:
	cmp	%r10, 0
	or	%r0, 1, %r11
	be	cmpui_67409
	nop
code_67410:
	or	%r0, 0, %r11
cmpui_67409:
	cmp	%r8, 0
	bne,pn	%icc,one_case_66661
	nop
zero_case_66660:
	ba	after_zeroone_66662
	or	%r0, 256, %r8
one_case_66661:
	sethi	%hi(mk_56675), %r8
	or	%r8, %lo(mk_56675), %r9
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
after_zeroone_66662:
	cmp	%r11, 0
	bne,pn	%icc,one_case_66673
	nop
zero_case_66672:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_67482:
	st	%r8, [%sp+100]
code_67418:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_67474:
	mov	%r8, %r10
code_67419:
	! done making normal call
	! making direct call 
	sethi	%hi(polyArray_INT), %r8
	ld	[%r8+%lo(polyArray_INT)], %r11
	ld	[%sp+112], %r8
	jmpl	%r11, %r15
	ld	[%sp+100], %r9
code_67475:
	st	%r8, [%sp+100]
code_67421:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67422
	nop
code_67423:
	call	GCFromML ! delay slot empty
	nop
needgc_67422:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_loop_code_61843), %r8
	or	%r8, %lo(Array_loop_code_61843), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	or	%r0, 258, %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	st	%r8, [%r9+8]
	! done allocating 1 closures
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_67481:
	mov	%r8, %r9
code_67426:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_67476:
code_67427:
	! done making normal call
	ba	after_zeroone_66674
	ld	[%sp+100], %r8
one_case_66673:
	ld	[%sp+112], %r17
	cmp	%r17, 11
	be,pn	%icc,array_float_66730
	nop
code_67429:
	ld	[%sp+112], %r17
	cmp	%r17, 2
	be,pn	%icc,array_int_66731
	nop
code_67430:
	ld	[%sp+112], %r17
	cmp	%r17, 0
	be,pn	%icc,array_char_66732
	nop
code_67431:
	or	%r0, 0, %r9
	or	%r0, 0, %r10
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_66741
	nop
code_67432:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r10, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_67477:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_67433:
	ba	array_ptr_aftert_66740 ! delay slot empty
	nop
array_ptr_alloc_66741:
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
	ble	needgc_67435
	nop
code_67436:
	call	GCFromML ! delay slot empty
	nop
needgc_67435:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_66747
	add	%r9, %r0, %r9
array_init_loopto_66748:
	add	%r11, %r9, %r8
	st	%r10, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_66747:
	cmp	%r9, 0
	bge	array_init_loopto_66748
	nop
array_ptr_aftert_66740:
	ba	array_after_66729
	mov	%r11, %r8
array_int_66731:
	or	%r0, 0, %r11
	or	%r0, 0, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_66755
	nop
code_67442:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67478:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67443:
	ba	array_int_after_66754 ! delay slot empty
	nop
array_int_small_66755:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67445
	nop
code_67446:
	call	GCFromML ! delay slot empty
	nop
needgc_67445:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_66762
	add	%r9, %r0, %r9
array_init_loopto_66763:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_66762:
	cmp	%r9, 0
	bge	array_init_loopto_66763
	nop
array_int_after_66754:
	ba	array_after_66729
	mov	%r12, %r8
array_char_66732:
	or	%r0, 0, %r11
	or	%r0, 0, %r9
	! initializing int/ptr array start
	add	%r9, 3, %r8
	srl	%r8, 2, %r10
	sll	%r11, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r8
	sll	%r8, 16, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r11
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_66773
	nop
code_67452:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67479:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67453:
	ba	array_int_after_66772 ! delay slot empty
	nop
array_int_small_66773:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67455
	nop
code_67456:
	call	GCFromML ! delay slot empty
	nop
needgc_67455:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_66780
	add	%r9, %r0, %r9
array_init_loopto_66781:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_66780:
	cmp	%r9, 0
	bge	array_init_loopto_66781
	nop
array_int_after_66772:
	ba	array_after_66729
	mov	%r12, %r8
array_float_66730:
	or	%r0, 0, %r11
	add	%r11, %r11, %r9
	add	%r9, 2, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_float_smallalloc_66790
	nop
code_67462:
	mov	%r11, %r8
	std	%f0, [%r2+392]
	ld	[%r2+392], %r9
	ld	[%r2+396], %r10
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_67480:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_67463:
	ba	array_float_after_66791 ! delay slot empty
	nop
array_float_smallalloc_66790:
	sll	%r9, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67465
	nop
code_67466:
	call	GCFromML ! delay slot empty
	nop
needgc_67465:
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_67468
	nop
code_67469:
	nop
	or	%r0, %r8, %r4
cmv_67468:
	sll	%r11, 3, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r8
	add	%r8, %r0, %r8
	or	%r8, 3, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r10
	sll	%r11, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_66792
	sub	%r11, 1, %r9
array_float_top_66793:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	std	%f0, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_66792:
	cmp	%r9, 0
	bge	array_float_top_66793
	nop
array_float_after_66791:
	mov	%r10, %r8
array_after_66729:
after_zeroone_66674:
code_67473:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_fromListPRIME_inner_code_61835,(.-Array_fromListPRIME_inner_code_61835)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67474
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05110000
		! -------- label,sizes,reg
	.long code_67475
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04110000
		! -------- label,sizes,reg
	.long needgc_67422
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04150000
		! -------- label,sizes,reg
	.long code_67476
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_67477
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67435
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67478
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67445
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67479
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67455
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67480
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67465
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67481
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
		! -------- label,sizes,reg
	.long code_67482
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05510000
	.text
	.align 8
	.global Array_fromListPRIME_r_code_61830
 ! arguments : [$61832,$8] [$55228,$9] [$61833,$10] [$55229,$11] 
 ! results    : [$66632,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fromListPRIME_r_code_61830:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67498
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67498:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
code_67484:
funtop_66594:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(hd_56750), %r8
	or	%r8, %lo(hd_56750), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67497:
	st	%r8, [%sp+96]
code_67487:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67496:
	mov	%r8, %r9
code_67489:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67490
	nop
code_67491:
	call	GCFromML ! delay slot empty
	nop
needgc_67490:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
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
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_fromListPRIME_inner_code_61835), %r8
	or	%r8, %lo(Array_fromListPRIME_inner_code_61835), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67495:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_fromListPRIME_r_code_61830,(.-Array_fromListPRIME_r_code_61830)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67496
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_67490
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_67497
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global Array_len_inner_code_61876
 ! arguments : [$61878,$8] [$61879,$9] [$58483,$10] [$58484,$11] 
 ! results    : [$66527,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
Array_len_inner_code_61876:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67511
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67511:
	st	%r15, [%sp+92]
code_67499:
funtop_66511:
sumarm_66523:
	cmp	%r10, 0
	bne	sumarm_66524
	nop
code_67500:
	ba	after_sum_66520
	mov	%r11, %r8
sumarm_66524:
	ld	[%r10+4], %r8
sumarm_66559:
	cmp	%r8, 0
	bne	sumarm_66560
	nop
code_67502:
	addcc	%r11, 1, %r8
	bvs,pn	%icc,localOverflowCheck
	nop
code_67503:
	ba	after_sum_66556 ! delay slot empty
	nop
sumarm_66560:
	ld	[%r8+4], %r9
	addcc	%r11, 2, %r8
	bvs,pn	%icc,localOverflowCheck
	nop
code_67505:
	! making direct call 
	mov	%r9, %r10
	ba	funtop_66511
	mov	%r8, %r11
code_67506:
	! done making self tail call
	ba	after_sum_66556
	or	%r0, 0, %r8
sumarm_66567:
after_sum_66556:
	ba	after_sum_66520 ! delay slot empty
	nop
sumarm_66528:
after_sum_66520:
code_67510:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_len_inner_code_61876,(.-Array_len_inner_code_61876)

	.section	".rodata"
	.text
	.align 8
	.global Array_fromList_inner_code_61881
 ! arguments : [$61883,$8] [$61884,$9] [$55285,$10] 
 ! results    : [$66510,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fromList_inner_code_61881:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67519
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67519:
	st	%r15, [%sp+92]
	mov	%r9, %r8
	st	%r10, [%sp+100]
code_67512:
funtop_66489:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+96]
	or	%r0, 0, %r11
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_67518:
	mov	%r8, %r10
code_67513:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_67514:
	! done making tail call
code_67516:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_fromList_inner_code_61881,(.-Array_fromList_inner_code_61881)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67518
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.text
	.align 8
	.global Array_fromList_r_code_61871
 ! arguments : [$61873,$8] [$55281,$9] [$61874,$10] [$55282,$11] 
 ! results    : [$66483,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fromList_r_code_61871:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67531
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67531:
	st	%r15, [%sp+92]
	mov	%r9, %r13
code_67520:
funtop_66455:
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(record_66464), %r8
	or	%r8, %lo(record_66464), %r17
	st	%r17, [%sp+96]
	! done allocating 1 closures
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(fromListPRIME_r_55280), %r8
	or	%r8, %lo(fromListPRIME_r_55280), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_67530:
	mov	%r8, %r9
code_67523:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67524
	nop
code_67525:
	call	GCFromML ! delay slot empty
	nop
needgc_67524:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 529, %r8
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
	sethi	%hi(Array_fromList_inner_code_61881), %r8
	or	%r8, %lo(Array_fromList_inner_code_61881), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67529:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_fromList_r_code_61871,(.-Array_fromList_r_code_61871)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67524
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67530
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_tab_code_61902
 ! arguments : [$61904,$8] [$61905,$9] [$55343,$10] 
 ! results    : [$66418,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_tab_code_61902:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67551
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_67551:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+116]
	st	%r10, [%sp+120]
code_67532:
funtop_66391:
	ld	[%sp+116], %r17
	ld	[%r17], %r11
	ld	[%sp+116], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+112]
	ld	[%sp+116], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+108]
	ld	[%sp+116], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+104]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_67549:
code_67534:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_66415
	nop
zero_case_66414:
	ba	after_zeroone_66416
	ld	[%sp+104], %r8
one_case_66415:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_67550:
	st	%r8, [%sp+100]
code_67538:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_67546:
	mov	%r8, %r10
code_67540:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_67547:
	mov	%r8, %r12
code_67541:
	! done making normal call
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r13
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r10
	jmpl	%r13, %r15
	ld	[%sp+120], %r11
code_67548:
code_67542:
	! done making normal call
	! making direct call 
	ld	[%sp+100], %r16
	ba	funtop_66391
	st	%r16, [%sp+120]
code_67543:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_66416:
code_67545:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_tab_code_61902,(.-Array_tab_code_61902)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67546
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05510000
		! -------- label,sizes,reg
	.long code_67547
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05110000
		! -------- label,sizes,reg
	.long code_67548
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04010000
		! -------- label,sizes,reg
	.long code_67549
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05510000
		! -------- label,sizes,reg
	.long code_67550
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05510000
	.text
	.align 8
	.global Array_tabulate_inner_code_61895
 ! arguments : [$61897,$8] [$61898,$9] [$58525,$10] [$58526,$11] 
 ! results    : [$66317,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_tabulate_inner_code_61895:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67619
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_67619:
	st	%r15, [%sp+92]
	mov	%r8, %r12
	mov	%r9, %r8
	st	%r10, [%sp+112]
	st	%r11, [%sp+108]
code_67552:
funtop_66252:
	! Proj_c at label type_56848_INT
	ld	[%r12], %r9
	! Proj_c at label var_poly_c_55326_INT
	ld	[%r12+4], %r17
	st	%r17, [%sp+96]
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	ld	[%sp+112], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	be	cmpui_67553
	nop
code_67554:
	or	%r0, 0, %r8
cmpui_67553:
	cmp	%r8, 0
	bne,pn	%icc,one_case_66271
	nop
zero_case_66270:
	or	%r0, 0, %r10
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r11
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_67618:
	mov	%r8, %r11
code_67556:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_67611:
	st	%r8, [%sp+100]
code_67557:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_67612:
	mov	%r8, %r9
code_67559:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67560
	nop
code_67561:
	call	GCFromML ! delay slot empty
	nop
needgc_67560:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3617, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_tab_code_61902), %r8
	or	%r8, %lo(Array_tab_code_61902), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 1, %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_67564:
	! done making tail call
	ba	after_zeroone_66272 ! delay slot empty
	nop
one_case_66271:
	cmp	%r9, 11
	be,pn	%icc,array_float_66323
	nop
code_67566:
	cmp	%r9, 2
	be,pn	%icc,array_int_66324
	nop
code_67567:
	cmp	%r9, 0
	be,pn	%icc,array_char_66325
	nop
code_67568:
	or	%r0, 0, %r9
	or	%r0, 0, %r10
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_66334
	nop
code_67569:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r10, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_67613:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_67570:
	ba	array_ptr_aftert_66333 ! delay slot empty
	nop
array_ptr_alloc_66334:
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
	ble	needgc_67572
	nop
code_67573:
	call	GCFromML ! delay slot empty
	nop
needgc_67572:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_66340
	add	%r9, %r0, %r9
array_init_loopto_66341:
	add	%r11, %r9, %r8
	st	%r10, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_66340:
	cmp	%r9, 0
	bge	array_init_loopto_66341
	nop
array_ptr_aftert_66333:
	ba	array_after_66322
	mov	%r11, %r8
array_int_66324:
	or	%r0, 0, %r11
	or	%r0, 0, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_66348
	nop
code_67579:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67614:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67580:
	ba	array_int_after_66347 ! delay slot empty
	nop
array_int_small_66348:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67582
	nop
code_67583:
	call	GCFromML ! delay slot empty
	nop
needgc_67582:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_66355
	add	%r9, %r0, %r9
array_init_loopto_66356:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_66355:
	cmp	%r9, 0
	bge	array_init_loopto_66356
	nop
array_int_after_66347:
	ba	array_after_66322
	mov	%r12, %r8
array_char_66325:
	or	%r0, 0, %r11
	or	%r0, 0, %r9
	! initializing int/ptr array start
	add	%r9, 3, %r8
	srl	%r8, 2, %r10
	sll	%r11, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r8
	sll	%r8, 16, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r11
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_66366
	nop
code_67589:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67615:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67590:
	ba	array_int_after_66365 ! delay slot empty
	nop
array_int_small_66366:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67592
	nop
code_67593:
	call	GCFromML ! delay slot empty
	nop
needgc_67592:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_66373
	add	%r9, %r0, %r9
array_init_loopto_66374:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_66373:
	cmp	%r9, 0
	bge	array_init_loopto_66374
	nop
array_int_after_66365:
	ba	array_after_66322
	mov	%r12, %r8
array_float_66323:
	or	%r0, 0, %r11
	add	%r11, %r11, %r9
	add	%r9, 2, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_float_smallalloc_66383
	nop
code_67599:
	mov	%r11, %r8
	std	%f0, [%r2+392]
	ld	[%r2+392], %r9
	ld	[%r2+396], %r10
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_67616:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_67600:
	ba	array_float_after_66384 ! delay slot empty
	nop
array_float_smallalloc_66383:
	sll	%r9, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67602
	nop
code_67603:
	call	GCFromML ! delay slot empty
	nop
needgc_67602:
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_67605
	nop
code_67606:
	nop
	or	%r0, %r8, %r4
cmv_67605:
	sll	%r11, 3, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r8
	add	%r8, %r0, %r8
	or	%r8, 3, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r10
	sll	%r11, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_66385
	sub	%r11, 1, %r9
array_float_top_66386:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	std	%f0, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_66385:
	cmp	%r9, 0
	bge	array_float_top_66386
	nop
array_float_after_66384:
	mov	%r10, %r8
array_after_66322:
after_zeroone_66272:
code_67610:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_tabulate_inner_code_61895,(.-Array_tabulate_inner_code_61895)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67611
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_67612
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long needgc_67560
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_67613
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67572
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67614
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67582
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67615
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67592
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67616
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67602
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67618
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.text
	.align 8
	.global Array_tabulate_r_code_61890
 ! arguments : [$61892,$8] [$55326,$9] [$61893,$10] [$55327,$11] 
 ! results    : [$66247,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_tabulate_r_code_61890:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67633
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67633:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
code_67620:
funtop_66211:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(array_r_55198), %r8
	or	%r8, %lo(array_r_55198), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67632:
	st	%r8, [%sp+96]
code_67622:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67631:
	mov	%r8, %r9
code_67624:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67625
	nop
code_67626:
	call	GCFromML ! delay slot empty
	nop
needgc_67625:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
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
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_tabulate_inner_code_61895), %r8
	or	%r8, %lo(Array_tabulate_inner_code_61895), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67630:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_tabulate_r_code_61890,(.-Array_tabulate_r_code_61890)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67631
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_67625
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_67632
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global Array_length_inner_code_61927
 ! arguments : [$61929,$8] [$61930,$9] [$55366,$10] 
 ! results    : [$66210,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_length_inner_code_61927:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67642
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67642:
	st	%r15, [%sp+92]
code_67634:
funtop_66195:
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_67641:
	mov	%r8, %r10
code_67635:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_67637:
	! done making tail call
code_67639:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_length_inner_code_61927,(.-Array_length_inner_code_61927)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67641
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_length_r_code_61922
 ! arguments : [$61924,$8] [$55362,$9] [$61925,$10] [$55363,$11] 
 ! results    : [$66189,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_length_r_code_61922:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67653
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67653:
	st	%r15, [%sp+92]
	mov	%r9, %r13
code_67643:
funtop_66172:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54832), %r8
	or	%r8, %lo(outer_valbind_r_54832), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_67652:
	mov	%r8, %r9
code_67645:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67646
	nop
code_67647:
	call	GCFromML ! delay slot empty
	nop
needgc_67646:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_length_inner_code_61927), %r8
	or	%r8, %lo(Array_length_inner_code_61927), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67651:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_length_r_code_61922,(.-Array_length_r_code_61922)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67646
	.word 0xb8003006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67652
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_sub_inner_code_61939
 ! arguments : [$61941,$8] [$61942,$9] [$58587,$10] [$58588,$11] 
 ! results    : [$66166,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_sub_inner_code_61939:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67671
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_67671:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
	st	%r10, [%sp+112]
	st	%r11, [%sp+108]
code_67654:
funtop_66128:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_67669:
	st	%r8, [%sp+100]
code_67655:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_67668:
	mov	%r8, %r11
code_67657:
	! done making normal call
	ld	[%sp+100], %r17
	cmp	%r11, %r17
	or	%r0, 1, %r8
	bcc	cmpui_67658
	nop
code_67659:
	or	%r0, 0, %r8
cmpui_67658:
	cmp	%r8, 0
	bne,pn	%icc,one_case_66156
	nop
zero_case_66155:
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_67661:
	! done making tail call
	ba	after_zeroone_66157 ! delay slot empty
	nop
one_case_66156:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_66157:
code_67667:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_sub_inner_code_61939,(.-Array_sub_inner_code_61939)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67668
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01110000
		! -------- label,sizes,reg
	.long code_67669
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01110000
	.text
	.align 8
	.global Array_sub_r_code_61934
 ! arguments : [$61936,$8] [$55375,$9] [$61937,$10] [$55376,$11] 
 ! results    : [$66123,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_sub_r_code_61934:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67685
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67685:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
code_67672:
funtop_66091:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54832), %r8
	or	%r8, %lo(outer_valbind_r_54832), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67684:
	st	%r8, [%sp+96]
code_67674:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67683:
	mov	%r8, %r9
code_67676:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67677
	nop
code_67678:
	call	GCFromML ! delay slot empty
	nop
needgc_67677:
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
	sethi	%hi(Array_sub_inner_code_61939), %r8
	or	%r8, %lo(Array_sub_inner_code_61939), %r8
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67682:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_sub_r_code_61934,(.-Array_sub_r_code_61934)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67683
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_67677
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_67684
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global Array_update_inner_code_61955
 ! arguments : [$61957,$8] [$61958,$9] [$58618,$10] [$58619,$11] [$58620,$12] 
 ! results    : [$66085,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_update_inner_code_61955:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67703
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_67703:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
	st	%r10, [%sp+116]
	st	%r11, [%sp+112]
	st	%r12, [%sp+108]
code_67686:
funtop_66046:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_67701:
	st	%r8, [%sp+100]
code_67687:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_67700:
	mov	%r8, %r11
code_67689:
	! done making normal call
	ld	[%sp+100], %r17
	cmp	%r11, %r17
	or	%r0, 1, %r8
	bcc	cmpui_67690
	nop
code_67691:
	or	%r0, 0, %r8
cmpui_67690:
	cmp	%r8, 0
	bne,pn	%icc,one_case_66074
	nop
zero_case_66073:
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r13
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	ld	[%sp+108], %r12
	ld	[%sp+92], %r15
	jmpl	%r13, %r0
	add	%sp, 128, %sp
code_67693:
	! done making tail call
	ba	after_zeroone_66075 ! delay slot empty
	nop
one_case_66074:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_66075:
code_67699:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_update_inner_code_61955,(.-Array_update_inner_code_61955)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67700
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04d10000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_67701
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04d10000
		! worddata
	.word 0x00000000
	.word 0x00000060
	.text
	.align 8
	.global Array_update_r_code_61950
 ! arguments : [$61952,$8] [$55398,$9] [$61953,$10] [$55399,$11] 
 ! results    : [$66041,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_update_r_code_61950:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67717
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67717:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
code_67704:
funtop_66009:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54832), %r8
	or	%r8, %lo(outer_valbind_r_54832), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67716:
	st	%r8, [%sp+96]
code_67706:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_67715:
	mov	%r8, %r9
code_67708:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67709
	nop
code_67710:
	call	GCFromML ! delay slot empty
	nop
needgc_67709:
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
	sethi	%hi(Array_update_inner_code_61955), %r8
	or	%r8, %lo(Array_update_inner_code_61955), %r8
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67714:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_update_r_code_61950,(.-Array_update_r_code_61950)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67715
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_67709
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_67716
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global Array_tab_code_61981
 ! arguments : [$61983,$8] [$61984,$9] [$58662,$10] [$58663,$11] 
 ! results    : [$65999,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_tab_code_61981:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67742
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_67742:
	st	%r15, [%sp+92]
	st	%r8, [%sp+116]
	st	%r9, [%sp+112]
	mov	%r10, %r12
	st	%r11, [%sp+120]
code_67718:
funtop_65929:
	! Proj_c at label type_56969_INT
	ld	[%sp+116], %r17
	ld	[%r17], %r8
	! Proj_c at label var_poly_c_55422_INT
	ld	[%sp+116], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+96]
	ld	[%sp+112], %r17
	ld	[%r17], %r11
	ld	[%sp+112], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+108]
	ld	[%sp+112], %r17
	ld	[%r17+8], %r10
	ld	[%sp+112], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+104]
	ld	[%sp+112], %r17
	ld	[%r17+16], %r9
	or	%r0, -1, %r8
	cmp	%r12, %r8
	or	%r0, 1, %r8
	be	cmpui_67719
	nop
code_67720:
	or	%r0, 0, %r8
cmpui_67719:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65955
	nop
zero_case_65954:
	subcc	%r12, 1, %r17
	st	%r17, [%sp+100]
	bvs,pn	%icc,localOverflowCheck
	nop
code_67722:
	addcc	%r9, %r12, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_67723:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_67739:
	mov	%r8, %r11
code_67725:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+104], %r10
code_67740:
	mov	%r8, %r10
code_67726:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67727
	nop
code_67728:
	call	GCFromML ! delay slot empty
	nop
needgc_67727:
	! allocating 2-record
	or	%r0, 529, %r9
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_67730
	nop
code_67731:
	or	%r0, 0, %r8
cmpui_67730:
	sll	%r8, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	ld	[%sp+96], %r17
	cmp	%r17, 3
	or	%r0, 1, %r8
	bgu	cmpui_67732
	nop
code_67733:
	or	%r0, 0, %r8
cmpui_67732:
	st	%r10, [%r4+4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! making direct call 
	ld	[%sp+100], %r12
	ba	funtop_65929
	st	%r8, [%sp+120]
code_67734:
	! done making self tail call
	ba	after_zeroone_65956
	or	%r0, 0, %r8
one_case_65955:
	! making closure call 
	ld	[%r10], %r12
	ld	[%r10+4], %r8
	ld	[%r10+8], %r9
	mov	%r11, %r10
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_67741:
code_67736:
	! done making normal call
after_zeroone_65956:
code_67738:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_tab_code_61981,(.-Array_tab_code_61981)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67739
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15510000
		! -------- label,sizes,reg
	.long code_67740
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15010000
		! -------- label,sizes,reg
	.long needgc_67727
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000400
		! stacktrace
	.word 0x00000000
	.word 0x15010000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_67741
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_newVec_code_61976
 ! arguments : [$61978,$8] [$61979,$9] [$55437,$10] 
 ! results    : [$65928,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_newVec_code_61976:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67753
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_67753:
	st	%r15, [%sp+92]
code_67743:
funtop_65880:
	add	%r4, 52, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67744
	nop
code_67745:
	call	GCFromML ! delay slot empty
	nop
needgc_67744:
	! Proj_c at label type_56969_INT
	ld	[%r8], %r20
	! Proj_c at label var_poly_c_55422_INT
	ld	[%r8+4], %r12
	ld	[%r9], %r19
	ld	[%r9+4], %r18
	ld	[%r9+8], %r11
	ld	[%r9+12], %r13
	ld	[%r9+16], %r9
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	st	%r20, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r12
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 5-record
	or	%r0, 3625, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r19, [%r4+8]
	st	%r18, [%r4+12]
	st	%r13, [%r4+16]
	st	%r9, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_tab_code_61981), %r8
	or	%r8, %lo(Array_tab_code_61981), %r8
	st	%r8, [%r4+4]
	st	%r12, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	subcc	%r10, 1, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_67748:
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_67749:
	! done making tail call
code_67751:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_newVec_code_61976,(.-Array_newVec_code_61976)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67744
	.word 0xb8003006
	.word 0x00000300
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_extract_inner_code_61971
 ! arguments : [$61973,$8] [$61974,$9] [$58651,$10] [$58652,$11] [$58653,$12] 
 ! results    : [$65712,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_extract_inner_code_61971:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67968
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_67968:
	st	%r15, [%sp+92]
	st	%r11, [%sp+108]
	st	%r12, [%sp+104]
code_67754:
funtop_65437:
	add	%r4, 52, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67755
	nop
code_67756:
	call	GCFromML ! delay slot empty
	nop
needgc_67755:
	! Proj_c at label type_56969_INT
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	! Proj_c at label var_poly_c_55422_INT
	ld	[%r8+4], %r12
	ld	[%r9], %r8
	ld	[%r9+4], %r18
	ld	[%r9+8], %r13
	ld	[%r9+12], %r11
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r9
	st	%r9, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r12, [%r4+8]
	add	%r4, 4, %r12
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 5-record
	or	%r0, 3881, %r9
	st	%r9, [%r4]
	st	%r18, [%r4+4]
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
	st	%r10, [%r4+16]
	ld	[%sp+108], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r11
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r9
	st	%r9, [%r4]
	sethi	%hi(Array_newVec_code_61976), %r9
	or	%r9, %lo(Array_newVec_code_61976), %r9
	st	%r9, [%r4+4]
	st	%r12, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	ld	[%r8+8], %r9
	jmpl	%r12, %r15
	mov	%r11, %r8
code_67965:
	mov	%r8, %r10
code_67759:
	! done making normal call
sumarm_65489:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	bne	sumarm_65490
	nop
code_67760:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	be	cmpui_67761
	nop
code_67762:
	or	%r0, 0, %r8
cmpui_67761:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65499
	nop
zero_case_65498:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bl	cmpsi_67764
	nop
code_67765:
	or	%r0, 0, %r8
cmpsi_67764:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65508
	nop
zero_case_65507:
	ld	[%sp+108], %r17
	cmp	%r10, %r17
	or	%r0, 1, %r8
	bl	cmpsi_67767
	nop
code_67768:
	or	%r0, 0, %r8
cmpsi_67767:
	ba	after_zeroone_65509 ! delay slot empty
	nop
one_case_65508:
	or	%r0, 1, %r8
after_zeroone_65509:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65520
	nop
zero_case_65519:
	ld	[%sp+108], %r17
	cmp	%r10, %r17
	or	%r0, 1, %r8
	be	cmpui_67771
	nop
code_67772:
	or	%r0, 0, %r8
cmpui_67771:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65529
	nop
zero_case_65528:
	ld	[%sp+108], %r17
	subcc	%r10, %r17, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_67774:
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 144, %sp
code_67775:
	! done making tail call
	ba	after_zeroone_65530 ! delay slot empty
	nop
one_case_65529:
	ld	[%sp+100], %r17
	cmp	%r17, 11
	be,pn	%icc,array_float_65547
	nop
code_67777:
	ld	[%sp+100], %r17
	cmp	%r17, 2
	be,pn	%icc,array_int_65548
	nop
code_67778:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	be,pn	%icc,array_char_65549
	nop
code_67779:
	or	%r0, 0, %r9
	or	%r0, 0, %r10
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_65558
	nop
code_67780:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r10, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_67953:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_67781:
	ba	array_ptr_aftert_65557 ! delay slot empty
	nop
array_ptr_alloc_65558:
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
	ble	needgc_67783
	nop
code_67784:
	call	GCFromML ! delay slot empty
	nop
needgc_67783:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65564
	add	%r9, %r0, %r9
array_init_loopto_65565:
	add	%r11, %r9, %r8
	st	%r10, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65564:
	cmp	%r9, 0
	bge	array_init_loopto_65565
	nop
array_ptr_aftert_65557:
	ba	array_after_65546
	mov	%r11, %r8
array_int_65548:
	or	%r0, 0, %r11
	or	%r0, 0, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_65572
	nop
code_67790:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67954:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67791:
	ba	array_int_after_65571 ! delay slot empty
	nop
array_int_small_65572:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67793
	nop
code_67794:
	call	GCFromML ! delay slot empty
	nop
needgc_67793:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65579
	add	%r9, %r0, %r9
array_init_loopto_65580:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65579:
	cmp	%r9, 0
	bge	array_init_loopto_65580
	nop
array_int_after_65571:
	ba	array_after_65546
	mov	%r12, %r8
array_char_65549:
	or	%r0, 0, %r11
	or	%r0, 0, %r9
	! initializing int/ptr array start
	add	%r9, 3, %r8
	srl	%r8, 2, %r10
	sll	%r11, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r8
	sll	%r8, 16, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r11
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_65590
	nop
code_67800:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67955:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67801:
	ba	array_int_after_65589 ! delay slot empty
	nop
array_int_small_65590:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67803
	nop
code_67804:
	call	GCFromML ! delay slot empty
	nop
needgc_67803:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65597
	add	%r9, %r0, %r9
array_init_loopto_65598:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65597:
	cmp	%r9, 0
	bge	array_init_loopto_65598
	nop
array_int_after_65589:
	ba	array_after_65546
	mov	%r12, %r8
array_float_65547:
	or	%r0, 0, %r11
	add	%r11, %r11, %r9
	add	%r9, 2, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_float_smallalloc_65607
	nop
code_67810:
	mov	%r11, %r8
	call	save_regs_MLtoC
	ldd	[%sp+128], %r9
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_67956:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_67811:
	ba	array_float_after_65608 ! delay slot empty
	nop
array_float_smallalloc_65607:
	sll	%r9, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67813
	nop
code_67814:
	call	GCFromML ! delay slot empty
	nop
needgc_67813:
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_67816
	nop
code_67817:
	nop
	or	%r0, %r8, %r4
cmv_67816:
	sll	%r11, 3, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r8
	add	%r8, %r0, %r8
	or	%r8, 3, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r10
	sll	%r11, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_65609
	sub	%r11, 1, %r9
array_float_top_65610:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	ldd	[%sp+128], %f60
	std	%f60, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_65609:
	cmp	%r9, 0
	bge	array_float_top_65610
	nop
array_float_after_65608:
	mov	%r10, %r8
array_after_65546:
after_zeroone_65530:
	ba	after_zeroone_65521 ! delay slot empty
	nop
one_case_65520:
	sethi	%hi(mk_57067), %r8
	or	%r8, %lo(mk_57067), %r9
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
after_zeroone_65521:
	ba	after_zeroone_65500 ! delay slot empty
	nop
one_case_65499:
	cmp	%r10, 0
	or	%r0, 1, %r8
	bg	cmpsi_67825
	nop
code_67826:
	or	%r0, 0, %r8
cmpsi_67825:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65629
	nop
zero_case_65628:
	ld	[%sp+100], %r17
	cmp	%r17, 11
	be,pn	%icc,array_float_65636
	nop
code_67828:
	ld	[%sp+100], %r17
	cmp	%r17, 2
	be,pn	%icc,array_int_65637
	nop
code_67829:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	be,pn	%icc,array_char_65638
	nop
code_67830:
	or	%r0, 0, %r9
	or	%r0, 0, %r10
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_65647
	nop
code_67831:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r10, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_67957:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_67832:
	ba	array_ptr_aftert_65646 ! delay slot empty
	nop
array_ptr_alloc_65647:
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
	ble	needgc_67834
	nop
code_67835:
	call	GCFromML ! delay slot empty
	nop
needgc_67834:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65653
	add	%r9, %r0, %r9
array_init_loopto_65654:
	add	%r11, %r9, %r8
	st	%r10, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65653:
	cmp	%r9, 0
	bge	array_init_loopto_65654
	nop
array_ptr_aftert_65646:
	ba	array_after_65635
	mov	%r11, %r8
array_int_65637:
	or	%r0, 0, %r11
	or	%r0, 0, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_65661
	nop
code_67841:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67958:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67842:
	ba	array_int_after_65660 ! delay slot empty
	nop
array_int_small_65661:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67844
	nop
code_67845:
	call	GCFromML ! delay slot empty
	nop
needgc_67844:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65668
	add	%r9, %r0, %r9
array_init_loopto_65669:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65668:
	cmp	%r9, 0
	bge	array_init_loopto_65669
	nop
array_int_after_65660:
	ba	array_after_65635
	mov	%r12, %r8
array_char_65638:
	or	%r0, 0, %r11
	or	%r0, 0, %r9
	! initializing int/ptr array start
	add	%r9, 3, %r8
	srl	%r8, 2, %r10
	sll	%r11, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r8
	sll	%r8, 16, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r11
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_65679
	nop
code_67851:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67959:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67852:
	ba	array_int_after_65678 ! delay slot empty
	nop
array_int_small_65679:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67854
	nop
code_67855:
	call	GCFromML ! delay slot empty
	nop
needgc_67854:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65686
	add	%r9, %r0, %r9
array_init_loopto_65687:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65686:
	cmp	%r9, 0
	bge	array_init_loopto_65687
	nop
array_int_after_65678:
	ba	array_after_65635
	mov	%r12, %r8
array_float_65636:
	or	%r0, 0, %r11
	add	%r11, %r11, %r9
	add	%r9, 2, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_float_smallalloc_65696
	nop
code_67861:
	mov	%r11, %r8
	call	save_regs_MLtoC
	ldd	[%sp+120], %r9
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_67960:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_67862:
	ba	array_float_after_65697 ! delay slot empty
	nop
array_float_smallalloc_65696:
	sll	%r9, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67864
	nop
code_67865:
	call	GCFromML ! delay slot empty
	nop
needgc_67864:
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_67867
	nop
code_67868:
	nop
	or	%r0, %r8, %r4
cmv_67867:
	sll	%r11, 3, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r8
	add	%r8, %r0, %r8
	or	%r8, 3, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r10
	sll	%r11, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_65698
	sub	%r11, 1, %r9
array_float_top_65699:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	ldd	[%sp+120], %f60
	std	%f60, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_65698:
	cmp	%r9, 0
	bge	array_float_top_65699
	nop
array_float_after_65697:
	mov	%r10, %r8
array_after_65635:
	ba	after_zeroone_65630 ! delay slot empty
	nop
one_case_65629:
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 144, %sp
code_67872:
	! done making tail call
after_zeroone_65630:
after_zeroone_65500:
	ba	after_sum_65486 ! delay slot empty
	nop
sumarm_65490:
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	cmp	%r12, 0
	or	%r0, 1, %r8
	be	cmpui_67874
	nop
code_67875:
	or	%r0, 0, %r8
cmpui_67874:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65728
	nop
zero_case_65727:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bl	cmpsi_67877
	nop
code_67878:
	or	%r0, 0, %r8
cmpsi_67877:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65737
	nop
zero_case_65736:
	cmp	%r12, 0
	or	%r0, 1, %r8
	bl	cmpsi_67880
	nop
code_67881:
	or	%r0, 0, %r8
cmpsi_67880:
	ba	after_zeroone_65738 ! delay slot empty
	nop
one_case_65737:
	or	%r0, 1, %r8
after_zeroone_65738:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65749
	nop
zero_case_65748:
	ld	[%sp+108], %r17
	addcc	%r17, %r12, %r8
	bvs,pn	%icc,localOverflowCheck
	nop
code_67884:
	cmp	%r10, %r8
	or	%r0, 1, %r8
	bl	cmpsi_67885
	nop
code_67886:
	or	%r0, 0, %r8
cmpsi_67885:
	ba	after_zeroone_65750 ! delay slot empty
	nop
one_case_65749:
	or	%r0, 1, %r8
after_zeroone_65750:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65764
	nop
zero_case_65763:
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r11
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	mov	%r12, %r10
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 144, %sp
code_67889:
	! done making tail call
	ba	after_zeroone_65765 ! delay slot empty
	nop
one_case_65764:
	sethi	%hi(mk_57067), %r8
	or	%r8, %lo(mk_57067), %r9
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
after_zeroone_65765:
	ba	after_zeroone_65729 ! delay slot empty
	nop
one_case_65728:
	ld	[%sp+108], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bl	cmpsi_67895
	nop
code_67896:
	or	%r0, 0, %r8
cmpsi_67895:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65787
	nop
zero_case_65786:
	ld	[%sp+108], %r17
	cmp	%r10, %r17
	or	%r0, 1, %r8
	bl	cmpsi_67898
	nop
code_67899:
	or	%r0, 0, %r8
cmpsi_67898:
	ba	after_zeroone_65788 ! delay slot empty
	nop
one_case_65787:
	or	%r0, 1, %r8
after_zeroone_65788:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65799
	nop
zero_case_65798:
	ld	[%sp+100], %r17
	cmp	%r17, 11
	be,pn	%icc,array_float_65806
	nop
code_67902:
	ld	[%sp+100], %r17
	cmp	%r17, 2
	be,pn	%icc,array_int_65807
	nop
code_67903:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	be,pn	%icc,array_char_65808
	nop
code_67904:
	or	%r0, 0, %r9
	or	%r0, 0, %r10
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_ptr_alloc_65817
	nop
code_67905:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r10, %r9
	call	alloc_bigptrarray ! delay slot empty
	nop
code_67961:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_67906:
	ba	array_ptr_aftert_65816 ! delay slot empty
	nop
array_ptr_alloc_65817:
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
	ble	needgc_67908
	nop
code_67909:
	call	GCFromML ! delay slot empty
	nop
needgc_67908:
	! storing tag
	st	%r11, [%r4]
	add	%r4, 4, %r11
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r9, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65823
	add	%r9, %r0, %r9
array_init_loopto_65824:
	add	%r11, %r9, %r8
	st	%r10, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65823:
	cmp	%r9, 0
	bge	array_init_loopto_65824
	nop
array_ptr_aftert_65816:
	ba	array_after_65805
	mov	%r11, %r8
array_int_65807:
	or	%r0, 0, %r11
	or	%r0, 0, %r10
	! initializing int/ptr array start
	sll	%r10, 2, %r9
	add	%r9, %r0, %r9
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_65831
	nop
code_67915:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67962:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67916:
	ba	array_int_after_65830 ! delay slot empty
	nop
array_int_small_65831:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67918
	nop
code_67919:
	call	GCFromML ! delay slot empty
	nop
needgc_67918:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65838
	add	%r9, %r0, %r9
array_init_loopto_65839:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65838:
	cmp	%r9, 0
	bge	array_init_loopto_65839
	nop
array_int_after_65830:
	ba	array_after_65805
	mov	%r12, %r8
array_char_65808:
	or	%r0, 0, %r11
	or	%r0, 0, %r9
	! initializing int/ptr array start
	add	%r9, 3, %r8
	srl	%r8, 2, %r10
	sll	%r11, 8, %r8
	add	%r8, %r0, %r8
	or	%r8, %r11, %r8
	sll	%r8, 16, %r11
	add	%r11, %r0, %r11
	or	%r11, %r8, %r11
	or	%r0, 510, %r8
	cmp	%r10, %r8
	ble	array_int_small_65849
	nop
code_67925:
	mov	%r9, %r8
	call	save_regs_MLtoC
	mov	%r11, %r9
	call	alloc_bigintarray ! delay slot empty
	nop
code_67963:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r12
code_67926:
	ba	array_int_after_65848 ! delay slot empty
	nop
array_int_small_65849:
	sll	%r9, 3, %r9
	add	%r9, %r0, %r9
	or	%r9, 2, %r9
	or	%r0, 1, %r8
	add	%r8, %r10, %r8
	sll	%r8, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67928
	nop
code_67929:
	call	GCFromML ! delay slot empty
	nop
needgc_67928:
	! storing tag
	st	%r9, [%r4]
	add	%r4, 4, %r12
	sll	%r8, 2, %r8
	add	%r8, %r0, %r8
	add	%r4, %r8, %r4
	sub	%r10, 1, %r9
	sll	%r9, 2, %r9
	ba	array_init_loopcheck_65856
	add	%r9, %r0, %r9
array_init_loopto_65857:
	add	%r12, %r9, %r8
	st	%r11, [%r8]
	sub	%r9, 4, %r9
array_init_loopcheck_65856:
	cmp	%r9, 0
	bge	array_init_loopto_65857
	nop
array_int_after_65848:
	ba	array_after_65805
	mov	%r12, %r8
array_float_65806:
	or	%r0, 0, %r11
	add	%r11, %r11, %r9
	add	%r9, 2, %r9
	or	%r0, 510, %r8
	cmp	%r9, %r8
	ble	array_float_smallalloc_65866
	nop
code_67935:
	mov	%r11, %r8
	call	save_regs_MLtoC
	ldd	[%sp+112], %r9
	call	alloc_bigfloatarray ! delay slot empty
	nop
code_67964:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_67936:
	ba	array_float_after_65867 ! delay slot empty
	nop
array_float_smallalloc_65866:
	sll	%r9, 2, %r16
	add	%r16, %r4, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67938
	nop
code_67939:
	call	GCFromML ! delay slot empty
	nop
needgc_67938:
	or	%r0, 23, %r9
	st	%r9, [%r4]
	and	%r4, 4, %r9
	add	%r4, 4, %r8
	cmp	%r9, 0
	bne	cmv_67941
	nop
code_67942:
	nop
	or	%r0, %r8, %r4
cmv_67941:
	sll	%r11, 3, %r8
	add	%r8, %r0, %r8
	sll	%r8, 3, %r8
	add	%r8, %r0, %r8
	or	%r8, 3, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r10
	sll	%r11, 3, %r16
	add	%r16, %r10, %r4
	or	%r0, 23, %r8
	st	%r8, [%r4]
	add	%r4, 4, %r4
	ba	array_float_bottom_65868
	sub	%r11, 1, %r9
array_float_top_65869:
	sll	%r9, 3, %r16
	add	%r16, %r10, %r8
	ldd	[%sp+112], %f60
	std	%f60, [%r8]
	sub	%r9, 1, %r9
array_float_bottom_65868:
	cmp	%r9, 0
	bge	array_float_top_65869
	nop
array_float_after_65867:
	mov	%r10, %r8
array_after_65805:
	ba	after_zeroone_65800 ! delay slot empty
	nop
one_case_65799:
	sethi	%hi(mk_57067), %r8
	or	%r8, %lo(mk_57067), %r9
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
after_zeroone_65800:
after_zeroone_65729:
	ba	after_sum_65486 ! delay slot empty
	nop
sumarm_65713:
after_sum_65486:
code_67951:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_extract_inner_code_61971,(.-Array_extract_inner_code_61971)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_67755
	.word 0xb8004807
	.word 0x00000700
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67953
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67783
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67954
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67793
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67955
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67803
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67956
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67813
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67957
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67834
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67958
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67844
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67959
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67854
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67960
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67864
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67961
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67908
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67962
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67918
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67963
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67928
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67964
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_67938
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_67965
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.word 0x00000000
	.text
	.align 8
	.global Array_extract_r_code_61966
 ! arguments : [$61968,$8] [$55422,$9] [$61969,$10] [$55423,$11] 
 ! results    : [$65432,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_extract_r_code_61966:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_67985
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_67985:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
code_67969:
funtop_65379:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(length_r_55374), %r8
	or	%r8, %lo(length_r_55374), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_67984:
	st	%r8, [%sp+100]
code_67971:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_67982:
	st	%r8, [%sp+96]
code_67973:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(fromListPRIME_r_55280), %r8
	or	%r8, %lo(fromListPRIME_r_55280), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_67983:
	mov	%r8, %r9
code_67975:
	! done making normal call
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_67976
	nop
code_67977:
	call	GCFromML ! delay slot empty
	nop
needgc_67976:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	or	%r0, 0, %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_extract_inner_code_61971), %r8
	or	%r8, %lo(Array_extract_inner_code_61971), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_67981:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_extract_r_code_61966,(.-Array_extract_r_code_61966)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_67982
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_67983
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_67976
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_67984
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.text
	.align 8
	.global Array_copyUp_code_62026
 ! arguments : [$62028,$8] [$62029,$9] [$58759,$10] [$58760,$11] 
 ! results    : [$65338,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copyUp_code_62026:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68005
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_68005:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+124]
	st	%r10, [%sp+132]
	st	%r11, [%sp+128]
code_67986:
funtop_65308:
	ld	[%sp+124], %r17
	ld	[%r17], %r11
	ld	[%sp+124], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+120]
	ld	[%sp+124], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+116]
	ld	[%sp+124], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+112]
	ld	[%sp+124], %r17
	ld	[%r17+16], %r17
	st	%r17, [%sp+108]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_68003:
code_67988:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_65334
	nop
zero_case_65333:
	ba	after_zeroone_65335
	or	%r0, 256, %r8
one_case_65334:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_68004:
	st	%r8, [%sp+104]
code_67992:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68000:
	st	%r8, [%sp+100]
code_67994:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r12, %r15
	ld	[%sp+132], %r11
code_68001:
	mov	%r8, %r12
code_67995:
	! done making normal call
	! making closure call 
	ld	[%sp+120], %r17
	ld	[%r17], %r13
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r13, %r15
	ld	[%sp+128], %r11
code_68002:
code_67996:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r16
	st	%r16, [%sp+132]
	ld	[%sp+100], %r16
	ba	funtop_65308
	st	%r16, [%sp+128]
code_67997:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_65335:
code_67999:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_copyUp_code_62026,(.-Array_copyUp_code_62026)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68000
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68001
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50410000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68002
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68003
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68004
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
	.text
	.align 8
	.global Array_copyDown_code_62043
 ! arguments : [$62045,$8] [$62046,$9] [$58812,$10] [$58813,$11] 
 ! results    : [$65267,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copyDown_code_62043:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68024
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_68024:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+124]
	st	%r10, [%sp+132]
	st	%r11, [%sp+128]
code_68006:
funtop_65242:
	ld	[%sp+124], %r17
	ld	[%r17], %r8
	ld	[%sp+124], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+120]
	ld	[%sp+124], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+116]
	ld	[%sp+124], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+112]
	ld	[%sp+124], %r17
	ld	[%r17+16], %r17
	st	%r17, [%sp+108]
	ld	[%sp+132], %r17
	cmp	%r8, %r17
	or	%r0, 1, %r8
	bleu	cmpui_68007
	nop
code_68008:
	or	%r0, 0, %r8
cmpui_68007:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65263
	nop
zero_case_65262:
	ba	after_zeroone_65264
	or	%r0, 256, %r8
one_case_65263:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55066), %r8
	or	%r8, %lo(anonfun_55066), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_68023:
	st	%r8, [%sp+104]
code_68012:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55066), %r8
	or	%r8, %lo(anonfun_55066), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68020:
	st	%r8, [%sp+100]
code_68014:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r12
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r12, %r15
	ld	[%sp+132], %r11
code_68021:
	mov	%r8, %r12
code_68015:
	! done making normal call
	! making closure call 
	ld	[%sp+120], %r17
	ld	[%r17], %r13
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r13, %r15
	ld	[%sp+128], %r11
code_68022:
code_68016:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r16
	st	%r16, [%sp+132]
	ld	[%sp+100], %r16
	ba	funtop_65242
	st	%r16, [%sp+128]
code_68017:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_65264:
code_68019:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_copyDown_code_62043,(.-Array_copyDown_code_62043)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68020
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68021
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50410000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68022
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68023
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
	.text
	.align 8
	.global Array_copy_inner_code_62020
 ! arguments : [$62022,$8] [$62023,$9] [$58738,$10] [$58739,$11] [$58740,$12] [$58741,$13] [$58742,$18] 
 ! results    : [$65236,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copy_inner_code_62020:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68111
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_68111:
	st	%r15, [%sp+92]
	st	%r8, [%sp+132]
	st	%r10, [%sp+148]
	st	%r11, [%sp+144]
	st	%r12, [%sp+140]
	st	%r13, [%sp+96]
	st	%r18, [%sp+136]
code_68025:
funtop_64971:
	ld	[%r9], %r17
	st	%r17, [%sp+128]
	ld	[%r9+4], %r17
	st	%r17, [%sp+124]
	ld	[%r9+8], %r17
	st	%r17, [%sp+100]
	ld	[%sp+148], %r17
	cmp	%r17, 0
	or	%r0, 1, %r17
	st	%r17, [%sp+120]
	bl	cmpsi_68026
	nop
code_68027:
	or	%r0, 0, %r17
	st	%r17, [%sp+120]
cmpsi_68026:
	! making closure call 
	ld	[%sp+128], %r17
	ld	[%r17], %r11
	ld	[%sp+128], %r17
	ld	[%r17+4], %r8
	ld	[%sp+128], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+140], %r10
code_68102:
	mov	%r8, %r9
code_68028:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68029
	nop
code_68030:
	call	GCFromML ! delay slot empty
	nop
needgc_68029:
sumarm_65001:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne	sumarm_65002
	nop
code_68032:
	ld	[%sp+144], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bl	cmpsi_68033
	nop
code_68034:
	or	%r0, 0, %r8
cmpsi_68033:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65011
	nop
zero_case_65010:
	ld	[%sp+144], %r17
	cmp	%r9, %r17
	or	%r0, 1, %r8
	bl	cmpsi_68036
	nop
code_68037:
	or	%r0, 0, %r8
cmpsi_68036:
	ba	after_zeroone_65012 ! delay slot empty
	nop
one_case_65011:
	or	%r0, 1, %r8
after_zeroone_65012:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65023
	nop
zero_case_65022:
	ld	[%sp+148], %r17
	addcc	%r17, %r9, %r8
	bvs,pn	%icc,localOverflowCheck
	nop
code_68040:
	ld	[%sp+144], %r17
	subcc	%r8, %r17, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68041:
	! allocating 2-record
	or	%r0, 17, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_65024 ! delay slot empty
	nop
one_case_65023:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_65024:
	ba	after_sum_64998 ! delay slot empty
	nop
sumarm_65002:
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	cmp	%r8, 0
	or	%r0, 1, %r10
	bl	cmpsi_68047
	nop
code_68048:
	or	%r0, 0, %r10
cmpsi_68047:
	cmp	%r10, 0
	bne,pn	%icc,one_case_65059
	nop
zero_case_65058:
	ld	[%sp+144], %r17
	cmp	%r17, 0
	or	%r0, 1, %r10
	bl	cmpsi_68050
	nop
code_68051:
	or	%r0, 0, %r10
cmpsi_68050:
	ba	after_zeroone_65060 ! delay slot empty
	nop
one_case_65059:
	or	%r0, 1, %r10
after_zeroone_65060:
	cmp	%r10, 0
	bne,pn	%icc,one_case_65071
	nop
zero_case_65070:
	ld	[%sp+144], %r17
	addcc	%r17, %r8, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68054:
	cmp	%r9, %r10
	or	%r0, 1, %r9
	bl	cmpsi_68055
	nop
code_68056:
	or	%r0, 0, %r9
cmpsi_68055:
	ba	after_zeroone_65072 ! delay slot empty
	nop
one_case_65071:
	or	%r0, 1, %r9
after_zeroone_65072:
	cmp	%r9, 0
	bne,pn	%icc,one_case_65086
	nop
zero_case_65085:
	ld	[%sp+144], %r17
	addcc	%r17, %r8, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68059:
	ld	[%sp+148], %r17
	addcc	%r17, %r8, %r9
	bvs,pn	%icc,localOverflowCheck
	nop
code_68060:
	! allocating 2-record
	or	%r0, 17, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_65087 ! delay slot empty
	nop
one_case_65086:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_65087:
	ba	after_sum_64998 ! delay slot empty
	nop
sumarm_65044:
after_sum_64998:
	ld	[%r8], %r10
	ld	[%r8+4], %r17
	st	%r17, [%sp+116]
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_68101:
	st	%r8, [%sp+112]
code_68067:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_68103:
	st	%r8, [%sp+108]
code_68069:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+144], %r10
code_68104:
	st	%r8, [%sp+104]
code_68071:
	! done making normal call
	add	%r4, 80, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68072
	nop
code_68073:
	call	GCFromML ! delay slot empty
	nop
needgc_68072:
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7721), %r8
	or	%r8, %lo(7721), %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+140], %r17
	st	%r17, [%r4+16]
	ld	[%sp+136], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_copyUp_code_62026), %r8
	or	%r8, %lo(Array_copyUp_code_62026), %r8
	st	%r8, [%r4+4]
	ld	[%sp+132], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+96]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7721), %r8
	or	%r8, %lo(7721), %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+124], %r17
	st	%r17, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+140], %r17
	st	%r17, [%r4+16]
	ld	[%sp+136], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_copyDown_code_62043), %r8
	or	%r8, %lo(Array_copyDown_code_62043), %r8
	st	%r8, [%r4+4]
	ld	[%sp+132], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+120], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_65165
	nop
zero_case_65164:
	! making closure call 
	ld	[%sp+128], %r17
	ld	[%r17], %r11
	ld	[%sp+128], %r17
	ld	[%r17+4], %r8
	ld	[%sp+128], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+136], %r10
code_68109:
code_68078:
	! done making normal call
	ld	[%sp+116], %r17
	cmp	%r8, %r17
	or	%r0, 1, %r8
	bl	cmpsi_68079
	nop
code_68080:
	or	%r0, 0, %r8
cmpsi_68079:
	ba	after_zeroone_65166 ! delay slot empty
	nop
one_case_65165:
	or	%r0, 1, %r8
after_zeroone_65166:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65183
	nop
zero_case_65182:
	ld	[%sp+148], %r16
	ld	[%sp+144], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	bl	cmpsi_68083
	nop
code_68084:
	or	%r0, 0, %r8
cmpsi_68083:
	cmp	%r8, 0
	bne,pn	%icc,one_case_65192
	nop
zero_case_65191:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+148], %r10
code_68110:
	mov	%r8, %r11
code_68087:
	! done making normal call
	! making closure call 
	ld	[%sp+96], %r17
	ld	[%r17], %r12
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	ld	[%sp+96], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 160, %sp
code_68088:
	! done making tail call
	ba	after_zeroone_65193 ! delay slot empty
	nop
one_case_65192:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55066), %r8
	or	%r8, %lo(anonfun_55066), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r10
code_68108:
	st	%r8, [%sp+96]
code_68091:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55066), %r8
	or	%r8, %lo(anonfun_55066), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+108], %r10
code_68106:
	mov	%r8, %r11
code_68093:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 160, %sp
code_68094:
	! done making tail call
after_zeroone_65193:
	ba	after_zeroone_65184 ! delay slot empty
	nop
one_case_65183:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_65184:
code_68100:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size Array_copy_inner_code_62020,(.-Array_copy_inner_code_62020)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68101
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_68102
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40050000
	.word 0x00000055
		! -------- label,sizes,reg
	.long needgc_68029
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40050000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_68103
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_68104
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long needgc_68072
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_68106
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68108
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68109
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68110
	.word 0xb8005007
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000000
	.text
	.align 8
	.global Array_copy_r_code_62015
 ! arguments : [$62017,$8] [$55532,$9] [$62018,$10] [$55533,$11] 
 ! results    : [$64966,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copy_r_code_62015:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68128
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68128:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
code_68112:
funtop_64924:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(length_r_55374), %r8
	or	%r8, %lo(length_r_55374), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68127:
	st	%r8, [%sp+100]
code_68114:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68125:
	st	%r8, [%sp+96]
code_68116:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68126:
	mov	%r8, %r9
code_68118:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68119
	nop
code_68120:
	call	GCFromML ! delay slot empty
	nop
needgc_68119:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_copy_inner_code_62020), %r8
	or	%r8, %lo(Array_copy_inner_code_62020), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68124:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_copy_r_code_62015,(.-Array_copy_r_code_62015)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68125
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_68126
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_68119
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_68127
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.text
	.align 8
	.global Array_copyUp_code_62075
 ! arguments : [$62077,$8] [$62078,$9] [$58917,$10] [$58918,$11] 
 ! results    : [$64884,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copyUp_code_62075:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68149
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_68149:
	st	%r15, [%sp+92]
	st	%r8, [%sp+124]
	st	%r9, [%sp+120]
	st	%r10, [%sp+132]
	st	%r11, [%sp+128]
code_68129:
funtop_64852:
	! Proj_c at label type_57305_INT
	ld	[%sp+124], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+104]
	! Proj_c at label var_poly_c_55634_INT
	ld	[%sp+124], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+96]
	ld	[%sp+120], %r17
	ld	[%r17], %r11
	ld	[%sp+120], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+116]
	ld	[%sp+120], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+100]
	ld	[%sp+120], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+112]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_68147:
code_68131:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_64880
	nop
zero_case_64879:
	ba	after_zeroone_64881
	or	%r0, 256, %r8
one_case_64880:
	! making direct call 
	sethi	%hi(polyVsub_INT), %r8
	ld	[%r8+%lo(polyVsub_INT)], %r11
	ld	[%sp+104], %r8
	ld	[%sp+100], %r9
	jmpl	%r11, %r15
	ld	[%sp+132], %r10
code_68148:
	st	%r8, [%sp+108]
code_68135:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+132], %r10
code_68144:
	st	%r8, [%sp+104]
code_68137:
	! done making normal call
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68145:
	st	%r8, [%sp+100]
code_68139:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r13
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	ld	[%sp+128], %r11
	jmpl	%r13, %r15
	ld	[%sp+108], %r12
code_68146:
code_68140:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r16
	st	%r16, [%sp+132]
	ld	[%sp+100], %r16
	ba	funtop_64852
	st	%r16, [%sp+128]
code_68141:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_64881:
code_68143:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_copyUp_code_62075,(.-Array_copyUp_code_62075)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68144
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55c10000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68145
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55c10000
	.word 0x00000000
		! worddata
	.word 0x00000000
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68146
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68147
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55150000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68148
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55010000
	.word 0x00000000
	.text
	.align 8
	.global Array_copyVec_inner_code_62069
 ! arguments : [$62071,$8] [$62072,$9] [$58896,$10] [$58897,$11] [$58898,$12] [$58899,$13] [$58900,$18] 
 ! results    : [$64846,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copyVec_inner_code_62069:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68227
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_68227:
	st	%r15, [%sp+92]
	st	%r10, [%sp+136]
	st	%r11, [%sp+132]
	st	%r12, [%sp+128]
	st	%r13, [%sp+96]
	st	%r18, [%sp+124]
code_68150:
funtop_64622:
	! Proj_c at label type_57305_INT
	ld	[%r8], %r17
	st	%r17, [%sp+120]
	! Proj_c at label var_poly_c_55634_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+116]
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+112]
	! making direct call 
	sethi	%hi(polyLen_INT), %r8
	ld	[%r8+%lo(polyLen_INT)], %r10
	ld	[%sp+120], %r8
	jmpl	%r10, %r15
	ld	[%sp+128], %r9
code_68224:
	mov	%r8, %r10
code_68152:
	! done making normal call
	ld	[%sp+136], %r17
	cmp	%r17, 0
	or	%r0, 1, %r17
	st	%r17, [%sp+108]
	bl	cmpsi_68153
	nop
code_68154:
	or	%r0, 0, %r17
	st	%r17, [%sp+108]
cmpsi_68153:
	! making closure call 
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_68220:
	mov	%r8, %r9
code_68156:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68157
	nop
code_68158:
	call	GCFromML ! delay slot empty
	nop
needgc_68157:
sumarm_64660:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne	sumarm_64661
	nop
code_68160:
	ld	[%sp+132], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bl	cmpsi_68161
	nop
code_68162:
	or	%r0, 0, %r8
cmpsi_68161:
	cmp	%r8, 0
	bne,pn	%icc,one_case_64670
	nop
zero_case_64669:
	ld	[%sp+132], %r17
	cmp	%r9, %r17
	or	%r0, 1, %r8
	bl	cmpsi_68164
	nop
code_68165:
	or	%r0, 0, %r8
cmpsi_68164:
	ba	after_zeroone_64671 ! delay slot empty
	nop
one_case_64670:
	or	%r0, 1, %r8
after_zeroone_64671:
	cmp	%r8, 0
	bne,pn	%icc,one_case_64682
	nop
zero_case_64681:
	ld	[%sp+136], %r17
	addcc	%r17, %r9, %r8
	bvs,pn	%icc,localOverflowCheck
	nop
code_68168:
	ld	[%sp+132], %r17
	subcc	%r8, %r17, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68169:
	! allocating 2-record
	or	%r0, 17, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	st	%r10, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_64683 ! delay slot empty
	nop
one_case_64682:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_64683:
	ba	after_sum_64657 ! delay slot empty
	nop
sumarm_64661:
	ld	[%sp+96], %r17
	ld	[%r17], %r8
	cmp	%r8, 0
	or	%r0, 1, %r10
	bl	cmpsi_68175
	nop
code_68176:
	or	%r0, 0, %r10
cmpsi_68175:
	cmp	%r10, 0
	bne,pn	%icc,one_case_64718
	nop
zero_case_64717:
	ld	[%sp+132], %r17
	cmp	%r17, 0
	or	%r0, 1, %r10
	bl	cmpsi_68178
	nop
code_68179:
	or	%r0, 0, %r10
cmpsi_68178:
	ba	after_zeroone_64719 ! delay slot empty
	nop
one_case_64718:
	or	%r0, 1, %r10
after_zeroone_64719:
	cmp	%r10, 0
	bne,pn	%icc,one_case_64730
	nop
zero_case_64729:
	ld	[%sp+132], %r17
	addcc	%r17, %r8, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68182:
	cmp	%r9, %r10
	or	%r0, 1, %r9
	bl	cmpsi_68183
	nop
code_68184:
	or	%r0, 0, %r9
cmpsi_68183:
	ba	after_zeroone_64731 ! delay slot empty
	nop
one_case_64730:
	or	%r0, 1, %r9
after_zeroone_64731:
	cmp	%r9, 0
	bne,pn	%icc,one_case_64745
	nop
zero_case_64744:
	ld	[%sp+132], %r17
	addcc	%r17, %r8, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68187:
	ld	[%sp+136], %r17
	addcc	%r17, %r8, %r9
	bvs,pn	%icc,localOverflowCheck
	nop
code_68188:
	! allocating 2-record
	or	%r0, 17, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	ba	after_zeroone_64746 ! delay slot empty
	nop
one_case_64745:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_64746:
	ba	after_sum_64657 ! delay slot empty
	nop
sumarm_64703:
after_sum_64657:
	ld	[%r8], %r10
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_68219:
	st	%r8, [%sp+96]
code_68195:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_68221:
code_68197:
	! done making normal call
	add	%r4, 48, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68198
	nop
code_68199:
	call	GCFromML ! delay slot empty
	nop
needgc_68198:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+120], %r17
	st	%r17, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 4-record
	or	%r0, 3617, %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	ld	[%sp+128], %r17
	st	%r17, [%r4+12]
	ld	[%sp+124], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_copyUp_code_62075), %r8
	or	%r8, %lo(Array_copyUp_code_62075), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	ld	[%sp+108], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_64804
	nop
zero_case_64803:
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r11
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_68225:
code_68203:
	! done making normal call
	ld	[%sp+104], %r17
	cmp	%r8, %r17
	or	%r0, 1, %r8
	bl	cmpsi_68204
	nop
code_68205:
	or	%r0, 0, %r8
cmpsi_68204:
	ba	after_zeroone_64805 ! delay slot empty
	nop
one_case_64804:
	or	%r0, 1, %r8
after_zeroone_64805:
	cmp	%r8, 0
	bne,pn	%icc,one_case_64822
	nop
zero_case_64821:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+132], %r10
code_68226:
	st	%r8, [%sp+96]
code_68209:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+136], %r10
code_68222:
	mov	%r8, %r11
code_68211:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+96], %r10
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 144, %sp
code_68212:
	! done making tail call
	ba	after_zeroone_64823 ! delay slot empty
	nop
one_case_64822:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_64823:
code_68218:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_copyVec_inner_code_62069,(.-Array_copyVec_inner_code_62069)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68219
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55040000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_68220
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55050000
	.word 0x00000001
		! -------- label,sizes,reg
	.long needgc_68157
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55050000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_68221
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55040000
	.word 0x00000001
		! -------- label,sizes,reg
	.long needgc_68198
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55040000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_68222
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68224
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55050000
	.word 0x00000001
		! -------- label,sizes,reg
	.long code_68225
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68226
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.word 0x00000000
	.text
	.align 8
	.global Array_copyVec_r_code_62064
 ! arguments : [$62066,$8] [$55634,$9] [$62067,$10] [$55635,$11] 
 ! results    : [$64617,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copyVec_r_code_62064:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68241
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68241:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
code_68228:
funtop_64581:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68240:
	st	%r8, [%sp+96]
code_68230:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(length_r_55374), %r8
	or	%r8, %lo(length_r_55374), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68239:
	mov	%r8, %r9
code_68232:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68233
	nop
code_68234:
	call	GCFromML ! delay slot empty
	nop
needgc_68233:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
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
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_copyVec_inner_code_62069), %r8
	or	%r8, %lo(Array_copyVec_inner_code_62069), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68238:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_copyVec_r_code_62064,(.-Array_copyVec_r_code_62064)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68239
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_68233
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_68240
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global Array_fold_code_62119
 ! arguments : [$62121,$8] [$62122,$9] [$59051,$10] [$59052,$11] 
 ! results    : [$64550,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fold_code_62119:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68258
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_68258:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+116]
	st	%r10, [%sp+124]
	st	%r11, [%sp+120]
code_68242:
funtop_64523:
	ld	[%sp+116], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+112]
	ld	[%sp+116], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+108]
	ld	[%sp+116], %r17
	ld	[%r17+8], %r11
	ld	[%sp+116], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+104]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+124], %r10
code_68256:
code_68244:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_64547
	nop
zero_case_64546:
	ba	after_zeroone_64548
	ld	[%sp+120], %r8
one_case_64547:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+124], %r10
code_68257:
	st	%r8, [%sp+100]
code_68248:
	! done making normal call
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r12
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	ld	[%r17+8], %r9
	ld	[%sp+108], %r10
	jmpl	%r12, %r15
	ld	[%sp+124], %r11
code_68254:
	mov	%r8, %r10
code_68249:
	! done making normal call
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r12
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_68255:
code_68250:
	! done making normal call
	! making direct call 
	ld	[%sp+100], %r16
	st	%r16, [%sp+124]
	ba	funtop_64523
	st	%r8, [%sp+120]
code_68251:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_64548:
code_68253:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_fold_code_62119,(.-Array_fold_code_62119)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68254
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35010000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68255
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04010000
		! -------- label,sizes,reg
	.long code_68256
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35510000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68257
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x35510000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62113
 ! arguments : [$62115,$8] [$62116,$9] [$55752,$10] 
 ! results    : [$64522,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62113:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68270
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_68270:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
	st	%r10, [%sp+116]
code_68259:
funtop_64480:
	ld	[%r8], %r17
	st	%r17, [%sp+112]
	ld	[%r8+4], %r17
	st	%r17, [%sp+108]
	ld	[%r8+8], %r9
	ld	[%r8+12], %r17
	st	%r17, [%sp+104]
	ld	[%r8+16], %r17
	st	%r17, [%sp+100]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+116], %r10
code_68268:
	mov	%r8, %r9
code_68260:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68261
	nop
code_68262:
	call	GCFromML ! delay slot empty
	nop
needgc_68261:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 2849, %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	ld	[%sp+116], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+104], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_fold_code_62119), %r8
	or	%r8, %lo(Array_fold_code_62119), %r8
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
	ld	[%sp+100], %r10
	ld	[%sp+108], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 128, %sp
code_68265:
	! done making tail call
code_68267:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_anonfun_code_62113,(.-Array_anonfun_code_62113)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68261
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05d10000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68268
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05d10000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62107
 ! arguments : [$62109,$8] [$62110,$9] [$55750,$10] 
 ! results    : [$64475,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
Array_anonfun_code_62107:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68282
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68282:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r13
code_68271:
funtop_64446:
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68272
	nop
code_68273:
	call	GCFromML ! delay slot empty
	nop
needgc_68272:
	ld	[%r9], %r12
	ld	[%r9+4], %r11
	ld	[%r9+8], %r10
	! allocating 1 closures
	! allocating 5-record
	or	%r0, 3369, %r9
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68275
	nop
code_68276:
	or	%r0, 0, %r8
cmpui_68275:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	st	%r12, [%r4+4]
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68277
	nop
code_68278:
	or	%r0, 0, %r8
cmpui_68277:
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
	st	%r10, [%r4+16]
	or	%r0, 0, %r8
	st	%r8, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62113), %r8
	or	%r8, %lo(Array_anonfun_code_62113), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68281:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62107,(.-Array_anonfun_code_62107)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68272
	.word 0xb8003808
	.word 0xbffc0200
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_foldl_inner_code_62102
 ! arguments : [$62104,$8] [$62105,$9] [$55748,$10] 
 ! results    : [$64445,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldl_inner_code_62102:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68294
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68294:
	st	%r15, [%sp+92]
	mov	%r8, %r11
	mov	%r9, %r8
	mov	%r10, %r18
code_68283:
funtop_64404:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68284
	nop
code_68285:
	call	GCFromML ! delay slot empty
	nop
needgc_68284:
	! Proj_c at label type_59042_INT
	ld	[%r11], %r10
	! Proj_c at label type_57456_INT
	ld	[%r11+4], %r9
	! Proj_c at label var_poly_c_55743_INT
	ld	[%r11+8], %r13
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r18, [%r4+4]
	st	%r12, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62107), %r8
	or	%r8, %lo(Array_anonfun_code_62107), %r8
	st	%r8, [%r4+4]
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
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
code_68293:
code_68290:
	! done making normal call
code_68292:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_foldl_inner_code_62102,(.-Array_foldl_inner_code_62102)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68284
	.word 0xb8003006
	.word 0x00040900
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68293
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_foldl_r_code_62097
 ! arguments : [$62099,$8] [$55743,$9] [$62100,$10] [$55744,$11] 
 ! results    : [$64399,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldl_r_code_62097:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68312
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68312:
	st	%r15, [%sp+92]
	st	%r9, [%sp+108]
code_68295:
funtop_64351:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68296
	nop
code_68297:
	call	GCFromML ! delay slot empty
	nop
needgc_68296:
	! Proj_c at label 'b_TYV
	ld	[%sp+108], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+104]
	! Proj_c at label 'a_TYV
	ld	[%sp+108], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+100]
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 1-record
	! done allocating 1 record
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54832), %r8
	or	%r8, %lo(outer_valbind_r_54832), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68311:
	st	%r8, [%sp+96]
code_68300:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68310:
	mov	%r8, %r9
code_68302:
	! done making normal call
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68303
	nop
code_68304:
	call	GCFromML ! delay slot empty
	nop
needgc_68303:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_64364), %r8
	or	%r8, %lo(record_64364), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
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
	sethi	%hi(Array_foldl_inner_code_62102), %r8
	or	%r8, %lo(Array_foldl_inner_code_62102), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68309:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_foldl_r_code_62097,(.-Array_foldl_r_code_62097)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68296
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_68310
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long needgc_68303
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_68311
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
	.align 8
	.global Array_chkSlice_inner_code_62158
 ! arguments : [$62160,$8] [$62161,$9] [$59099,$10] [$59100,$11] [$59101,$12] 
 ! results    : [$64266,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_chkSlice_inner_code_62158:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68365
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68365:
	st	%r15, [%sp+92]
	st	%r10, [%sp+108]
	st	%r11, [%sp+104]
code_68313:
funtop_64209:
sumarm_64221:
	cmp	%r12, 0
	bne	sumarm_64222
	nop
code_68314:
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_68360:
	st	%r8, [%sp+100]
code_68315:
	! done making normal call
	ld	[%sp+104], %r16
	ld	[%sp+100], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	bl	cmpsi_68316
	nop
code_68317:
	or	%r0, 0, %r8
cmpsi_68316:
	cmp	%r8, 0
	bne,pn	%icc,one_case_64237
	nop
zero_case_64236:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_68364:
	st	%r8, [%sp+96]
code_68320:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_68361:
	mov	%r8, %r9
code_68322:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68323
	nop
code_68324:
	call	GCFromML ! delay slot empty
	nop
needgc_68323:
	! allocating 3-record
	or	%r0, 281, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	ba	after_zeroone_64238 ! delay slot empty
	nop
one_case_64237:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
after_zeroone_64238:
	ba	after_sum_64218 ! delay slot empty
	nop
sumarm_64222:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r17
	st	%r17, [%sp+96]
	bge	cmpsi_68331
	nop
code_68332:
	or	%r0, 0, %r17
	st	%r17, [%sp+96]
cmpsi_68331:
	ld	[%r12], %r17
	st	%r17, [%sp+100]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_68359:
	mov	%r8, %r9
code_68333:
	! done making normal call
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_64292
	nop
zero_case_64291:
	ba	after_zeroone_64293
	or	%r0, 0, %r8
one_case_64292:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bge	cmpsi_68336
	nop
code_68337:
	or	%r0, 0, %r8
cmpsi_68336:
after_zeroone_64293:
	cmp	%r8, 0
	bne,pn	%icc,one_case_64304
	nop
zero_case_64303:
	ba	after_zeroone_64305
	or	%r0, 0, %r8
one_case_64304:
	ld	[%sp+100], %r16
	ld	[%sp+104], %r17
	addcc	%r17, %r16, %r8
	bvs,pn	%icc,localOverflowCheck
	nop
code_68340:
	cmp	%r8, %r9
	or	%r0, 1, %r8
	ble	cmpsi_68341
	nop
code_68342:
	or	%r0, 0, %r8
cmpsi_68341:
after_zeroone_64305:
	cmp	%r8, 0
	bne,pn	%icc,one_case_64319
	nop
zero_case_64318:
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r9
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
	ba	after_zeroone_64320
	or	%r0, 0, %r8
one_case_64319:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_68363:
	st	%r8, [%sp+96]
code_68349:
	! done making normal call
	ld	[%sp+100], %r16
	ld	[%sp+104], %r17
	addcc	%r17, %r16, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68350:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_68362:
	mov	%r8, %r9
code_68352:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68353
	nop
code_68354:
	call	GCFromML ! delay slot empty
	nop
needgc_68353:
	! allocating 3-record
	or	%r0, 281, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
after_zeroone_64320:
	ba	after_sum_64218 ! delay slot empty
	nop
sumarm_64267:
after_sum_64218:
code_68358:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_chkSlice_inner_code_62158,(.-Array_chkSlice_inner_code_62158)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68359
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_68360
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_68361
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long needgc_68323
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_68362
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long needgc_68353
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_68363
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_68364
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
	.align 8
	.global Array_chkSlice_r_code_62153
 ! arguments : [$62155,$8] [$55776,$9] [$62156,$10] [$55777,$11] 
 ! results    : [$64203,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_chkSlice_r_code_62153:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68376
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68376:
	st	%r15, [%sp+92]
	mov	%r9, %r13
code_68366:
funtop_64186:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(length_r_55374), %r8
	or	%r8, %lo(length_r_55374), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_68375:
	mov	%r8, %r9
code_68368:
	! done making normal call
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68369
	nop
code_68370:
	call	GCFromML ! delay slot empty
	nop
needgc_68369:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_chkSlice_inner_code_62158), %r8
	or	%r8, %lo(Array_chkSlice_inner_code_62158), %r8
	st	%r8, [%r4+4]
	or	%r0, 256, %r8
	st	%r8, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68374:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_chkSlice_r_code_62153,(.-Array_chkSlice_r_code_62153)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68369
	.word 0xb8003006
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68375
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_fold_code_62186
 ! arguments : [$62188,$8] [$62189,$9] [$59137,$10] [$59138,$11] 
 ! results    : [$64184,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fold_code_62186:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68392
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_68392:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+108]
	st	%r10, [%sp+116]
	st	%r11, [%sp+112]
code_68377:
funtop_64135:
	ld	[%sp+108], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+100]
	ld	[%sp+108], %r17
	ld	[%r17+4], %r10
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r17
	cmp	%r17, 0
	or	%r0, 1, %r17
	st	%r17, [%sp+104]
	be	cmpui_68378
	nop
code_68379:
	or	%r0, 0, %r17
	st	%r17, [%sp+104]
cmpui_68378:
	! making closure call 
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r11
code_68389:
	mov	%r8, %r10
code_68380:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+112], %r11
code_68390:
	st	%r8, [%sp+100]
code_68381:
	! done making normal call
	ld	[%sp+104], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_64166
	nop
zero_case_64165:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55066), %r8
	or	%r8, %lo(anonfun_55066), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+116], %r10
code_68391:
code_68384:
	! done making normal call
	! making direct call 
	st	%r8, [%sp+116]
	ld	[%sp+100], %r16
	ba	funtop_64135
	st	%r16, [%sp+112]
code_68385:
	! done making self tail call
	ba	after_zeroone_64167
	or	%r0, 0, %r8
one_case_64166:
	ld	[%sp+100], %r8
after_zeroone_64167:
code_68388:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_fold_code_62186,(.-Array_fold_code_62186)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68389
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03450000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68390
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00410000
		! -------- label,sizes,reg
	.long code_68391
	.word 0xb8004008
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x004d0000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62181
 ! arguments : [$62183,$8] [$62184,$9] [$55825,$10] 
 ! results    : [$64133,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62181:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68412
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68412:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_68393:
funtop_64074:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68394
	nop
code_68395:
	call	GCFromML ! delay slot empty
	nop
needgc_68394:
	ld	[%r8], %r11
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	ld	[%r8+8], %r9
	ld	[%r8+12], %r12
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r11, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_fold_code_62186), %r8
	or	%r8, %lo(Array_fold_code_62186), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r17
	st	%r17, [%sp+100]
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	! making closure call 
	ld	[%r12], %r11
	ld	[%r12+4], %r8
	jmpl	%r11, %r15
	ld	[%r12+8], %r9
code_68411:
	mov	%r8, %r9
code_68398:
	! done making normal call
	cmp	%r9, 0
	or	%r0, 1, %r8
	be	cmpui_68399
	nop
code_68400:
	or	%r0, 0, %r8
cmpui_68399:
	cmp	%r8, 0
	bne,pn	%icc,one_case_64113
	nop
zero_case_64112:
	subcc	%r9, 1, %r10
	bvs,pn	%icc,localOverflowCheck
	nop
code_68402:
	! making closure call 
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_68409:
	mov	%r8, %r10
code_68404:
	! done making normal call
	! making closure call 
	ld	[%sp+100], %r17
	ld	[%r17], %r12
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	ld	[%r17+8], %r9
	ld	[%sp+104], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_68405:
	! done making tail call
	ba	after_zeroone_64114 ! delay slot empty
	nop
one_case_64113:
	ld	[%sp+104], %r8
after_zeroone_64114:
code_68408:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62181,(.-Array_anonfun_code_62181)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68394
	.word 0xb8003806
	.word 0x00000500
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! -------- label,sizes,reg
	.long code_68409
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00350000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68411
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00350000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62175
 ! arguments : [$62177,$8] [$62178,$9] [$55823,$10] 
 ! results    : [$64069,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
Array_anonfun_code_62175:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68424
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68424:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r13
code_68413:
funtop_64042:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68414
	nop
code_68415:
	call	GCFromML ! delay slot empty
	nop
needgc_68414:
	ld	[%r9], %r12
	ld	[%r9+4], %r11
	ld	[%r9+8], %r10
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3361, %r9
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68417
	nop
code_68418:
	or	%r0, 0, %r8
cmpui_68417:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	st	%r12, [%r4+4]
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68419
	nop
code_68420:
	or	%r0, 0, %r8
cmpui_68419:
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
	st	%r10, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62181), %r8
	or	%r8, %lo(Array_anonfun_code_62181), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68423:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62175,(.-Array_anonfun_code_62175)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68414
	.word 0xb8003808
	.word 0xbffc0200
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_foldr_inner_code_62170
 ! arguments : [$62172,$8] [$62173,$9] [$55821,$10] 
 ! results    : [$64041,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldr_inner_code_62170:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68436
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68436:
	st	%r15, [%sp+92]
	mov	%r8, %r11
	mov	%r9, %r8
	mov	%r10, %r18
code_68425:
funtop_64000:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68426
	nop
code_68427:
	call	GCFromML ! delay slot empty
	nop
needgc_68426:
	! Proj_c at label type_59134_INT
	ld	[%r11], %r10
	! Proj_c at label type_57580_INT
	ld	[%r11+4], %r9
	! Proj_c at label var_poly_c_55816_INT
	ld	[%r11+8], %r13
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r18, [%r4+4]
	st	%r12, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62175), %r8
	or	%r8, %lo(Array_anonfun_code_62175), %r8
	st	%r8, [%r4+4]
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
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
code_68435:
code_68432:
	! done making normal call
code_68434:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_foldr_inner_code_62170,(.-Array_foldr_inner_code_62170)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68426
	.word 0xb8003006
	.word 0x00040900
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68435
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_foldr_r_code_62165
 ! arguments : [$62167,$8] [$55816,$9] [$62168,$10] [$55817,$11] 
 ! results    : [$63995,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldr_r_code_62165:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68454
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68454:
	st	%r15, [%sp+92]
	st	%r9, [%sp+108]
code_68437:
funtop_63947:
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68438
	nop
code_68439:
	call	GCFromML ! delay slot empty
	nop
needgc_68438:
	! Proj_c at label 'b_TYV
	ld	[%sp+108], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+104]
	! Proj_c at label 'a_TYV
	ld	[%sp+108], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+100]
	! allocating 2-record
	or	%r0, 529, %r8
	st	%r8, [%r4]
	or	%r0, 0, %r8
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 1-record
	! done allocating 1 record
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68453:
	st	%r8, [%sp+96]
code_68442:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(length_r_55374), %r8
	or	%r8, %lo(length_r_55374), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68452:
	mov	%r8, %r9
code_68444:
	! done making normal call
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68445
	nop
code_68446:
	call	GCFromML ! delay slot empty
	nop
needgc_68445:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_63960), %r8
	or	%r8, %lo(record_63960), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
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
	sethi	%hi(Array_foldr_inner_code_62170), %r8
	or	%r8, %lo(Array_foldr_inner_code_62170), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68451:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_foldr_r_code_62165,(.-Array_foldr_r_code_62165)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68438
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_68452
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long needgc_68445
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_68453
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
	.align 8
	.global Array_modifyPRIME_code_62234
 ! arguments : [$62236,$8] [$62237,$9] [$55867,$10] 
 ! results    : [$63910,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_modifyPRIME_code_62234:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68473
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_68473:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
	st	%r10, [%sp+124]
code_68455:
funtop_63880:
	ld	[%sp+120], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+116]
	ld	[%sp+120], %r17
	ld	[%r17+4], %r11
	ld	[%sp+120], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+112]
	ld	[%sp+120], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+108]
	ld	[%sp+120], %r17
	ld	[%r17+16], %r17
	st	%r17, [%sp+104]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+124], %r10
code_68471:
code_68457:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_63906
	nop
zero_case_63905:
	ba	after_zeroone_63907
	or	%r0, 256, %r8
one_case_63906:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+124], %r10
code_68472:
	st	%r8, [%sp+100]
code_68461:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	jmpl	%r12, %r15
	ld	[%sp+124], %r11
code_68468:
	mov	%r8, %r10
code_68462:
	! done making normal call
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_68469:
	mov	%r8, %r12
code_68463:
	! done making normal call
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r13
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	jmpl	%r13, %r15
	ld	[%sp+124], %r11
code_68470:
code_68464:
	! done making normal call
	! making direct call 
	ld	[%sp+100], %r16
	ba	funtop_63880
	st	%r16, [%sp+124]
code_68465:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_63907:
code_68467:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_modifyPRIME_code_62234,(.-Array_modifyPRIME_code_62234)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68468
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15110000
		! -------- label,sizes,reg
	.long code_68469
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15010000
		! -------- label,sizes,reg
	.long code_68470
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
		! -------- label,sizes,reg
	.long code_68471
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15510000
		! -------- label,sizes,reg
	.long code_68472
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15510000
	.text
	.align 8
	.global Array_anonfun_code_62228
 ! arguments : [$62230,$8] [$62231,$9] [$55859,$10] 
 ! results    : [$63879,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62228:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68485
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_68485:
	st	%r15, [%sp+92]
	st	%r8, [%sp+108]
	mov	%r9, %r8
	st	%r10, [%sp+112]
code_68474:
funtop_63838:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	ld	[%r8+8], %r17
	st	%r17, [%sp+100]
	ld	[%r8+12], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_68483:
	mov	%r8, %r9
code_68475:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68476
	nop
code_68477:
	call	GCFromML ! delay slot empty
	nop
needgc_68476:
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(7465), %r8
	or	%r8, %lo(7465), %r8
	st	%r8, [%r4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+104], %r17
	st	%r17, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	ld	[%sp+96], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_modifyPRIME_code_62234), %r8
	or	%r8, %lo(Array_modifyPRIME_code_62234), %r8
	st	%r8, [%r4+4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 0, %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 128, %sp
code_68480:
	! done making tail call
code_68482:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_anonfun_code_62228,(.-Array_anonfun_code_62228)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68476
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
		! -------- label,sizes,reg
	.long code_68483
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x01550000
	.text
	.align 8
	.global Array_modify_inner_code_62222
 ! arguments : [$62224,$8] [$62225,$9] [$55857,$10] 
 ! results    : [$63833,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_modify_inner_code_62222:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68497
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68497:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_68486:
funtop_63795:
	! Proj_c at label type_57638_INT
	ld	[%r8], %r10
	! Proj_c at label var_poly_c_55853_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+108]
	ld	[%r9], %r17
	st	%r17, [%sp+104]
	ld	[%r9+4], %r17
	st	%r17, [%sp+100]
	ld	[%r9+8], %r17
	st	%r17, [%sp+96]
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
	mov	%r10, %r9
code_68496:
	mov	%r8, %r9
code_68489:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68490
	nop
code_68491:
	call	GCFromML ! delay slot empty
	nop
needgc_68490:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	ld	[%sp+96], %r17
	st	%r17, [%r4+12]
	st	%r9, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62228), %r8
	or	%r8, %lo(Array_anonfun_code_62228), %r8
	st	%r8, [%r4+4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68495:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_modify_inner_code_62222,(.-Array_modify_inner_code_62222)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68490
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_68496
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.text
	.align 8
	.global Array_modify_r_code_62217
 ! arguments : [$62219,$8] [$55853,$9] [$62220,$10] [$55854,$11] 
 ! results    : [$63790,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_modify_r_code_62217:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68514
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68514:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
code_68498:
funtop_63744:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54832), %r8
	or	%r8, %lo(outer_valbind_r_54832), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68513:
	st	%r8, [%sp+100]
code_68500:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68511:
	st	%r8, [%sp+96]
code_68502:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68512:
	mov	%r8, %r9
code_68504:
	! done making normal call
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68505
	nop
code_68506:
	call	GCFromML ! delay slot empty
	nop
needgc_68505:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
	add	%r4, 12, %r4
	! done allocating 2 record
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_modify_inner_code_62222), %r8
	or	%r8, %lo(Array_modify_inner_code_62222), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68510:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_modify_r_code_62217,(.-Array_modify_r_code_62217)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68511
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_68512
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_68505
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_68513
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.text
	.align 8
	.global Array_fold_code_62283
 ! arguments : [$62285,$8] [$62286,$9] [$59348,$10] [$59349,$11] 
 ! results    : [$63705,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fold_code_62283:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68534
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_68534:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
	st	%r10, [%sp+128]
	st	%r11, [%sp+124]
code_68515:
funtop_63678:
	ld	[%sp+120], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+116]
	ld	[%sp+120], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+112]
	ld	[%sp+120], %r17
	ld	[%r17+8], %r11
	ld	[%sp+120], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+108]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68532:
code_68517:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_63702
	nop
zero_case_63701:
	ba	after_zeroone_63703
	ld	[%sp+124], %r8
one_case_63702:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68533:
	st	%r8, [%sp+104]
code_68521:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+128], %r10
code_68529:
	st	%r8, [%sp+100]
code_68523:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r12, %r15
	ld	[%sp+128], %r11
code_68530:
	mov	%r8, %r11
code_68524:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r13
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r13, %r15
	ld	[%sp+124], %r12
code_68531:
code_68525:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r16
	st	%r16, [%sp+128]
	ba	funtop_63678
	st	%r8, [%sp+124]
code_68526:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_63703:
code_68528:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_fold_code_62283,(.-Array_fold_code_62283)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68529
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5410000
	.word 0x00000000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68530
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd4010000
	.word 0x00000000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68531
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68532
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5410000
	.word 0x00000000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68533
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5410000
	.word 0x00000000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62277
 ! arguments : [$62279,$8] [$62280,$9] [$59323,$10] [$59324,$11] [$59325,$12] 
 ! results    : [$63677,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62277:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68546
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68546:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_68535:
funtop_63625:
	ld	[%r8], %r17
	st	%r17, [%sp+108]
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	ld	[%r8+8], %r9
	ld	[%r8+12], %r17
	st	%r17, [%sp+100]
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_68544:
code_68536:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68537
	nop
code_68538:
	call	GCFromML ! delay slot empty
	nop
needgc_68537:
	ld	[%r8], %r11
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 2849, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_fold_code_62283), %r8
	or	%r8, %lo(Array_fold_code_62283), %r8
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
	ld	[%sp+104], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_68541:
	! done making tail call
code_68543:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62277,(.-Array_anonfun_code_62277)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68537
	.word 0xb8003808
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00750000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68544
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00750000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62271
 ! arguments : [$62273,$8] [$62274,$9] [$55977,$10] 
 ! results    : [$63620,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
Array_anonfun_code_62271:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68558
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68558:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r13
code_68547:
funtop_63593:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68548
	nop
code_68549:
	call	GCFromML ! delay slot empty
	nop
needgc_68548:
	ld	[%r9], %r12
	ld	[%r9+4], %r11
	ld	[%r9+8], %r10
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3361, %r9
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68551
	nop
code_68552:
	or	%r0, 0, %r8
cmpui_68551:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	st	%r12, [%r4+4]
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68553
	nop
code_68554:
	or	%r0, 0, %r8
cmpui_68553:
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
	st	%r10, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62277), %r8
	or	%r8, %lo(Array_anonfun_code_62277), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68557:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62271,(.-Array_anonfun_code_62271)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68548
	.word 0xb8003808
	.word 0xbffc0200
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_foldli_inner_code_62266
 ! arguments : [$62268,$8] [$62269,$9] [$55975,$10] 
 ! results    : [$63592,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldli_inner_code_62266:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68570
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68570:
	st	%r15, [%sp+92]
	mov	%r8, %r11
	mov	%r9, %r8
	mov	%r10, %r18
code_68559:
funtop_63551:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68560
	nop
code_68561:
	call	GCFromML ! delay slot empty
	nop
needgc_68560:
	! Proj_c at label type_57869_INT
	ld	[%r11], %r10
	! Proj_c at label type_57829_INT
	ld	[%r11+4], %r9
	! Proj_c at label var_poly_c_55970_INT
	ld	[%r11+8], %r13
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r18, [%r4+4]
	st	%r12, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62271), %r8
	or	%r8, %lo(Array_anonfun_code_62271), %r8
	st	%r8, [%r4+4]
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
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
code_68569:
code_68566:
	! done making normal call
code_68568:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_foldli_inner_code_62266,(.-Array_foldli_inner_code_62266)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68560
	.word 0xb8003006
	.word 0x00040900
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68569
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_foldli_r_code_62261
 ! arguments : [$62263,$8] [$55970,$9] [$62264,$10] [$55971,$11] 
 ! results    : [$63546,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldli_r_code_62261:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68585
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68585:
	st	%r15, [%sp+92]
	st	%r9, [%sp+108]
code_68571:
funtop_63503:
	! Proj_c at label 'b_TYV
	ld	[%sp+108], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+104]
	! Proj_c at label 'a_TYV
	ld	[%sp+108], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+100]
	! allocating 1-record
	! done allocating 1 record
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(chkSlice_r_55815), %r8
	or	%r8, %lo(chkSlice_r_55815), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68584:
	st	%r8, [%sp+96]
code_68573:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68583:
	mov	%r8, %r9
code_68575:
	! done making normal call
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68576
	nop
code_68577:
	call	GCFromML ! delay slot empty
	nop
needgc_68576:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_63512), %r8
	or	%r8, %lo(record_63512), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
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
	sethi	%hi(Array_foldli_inner_code_62266), %r8
	or	%r8, %lo(Array_foldli_inner_code_62266), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68582:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_foldli_r_code_62261,(.-Array_foldli_r_code_62261)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68583
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long needgc_68576
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_68584
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
	.align 8
	.global Array_fold_code_62337
 ! arguments : [$62339,$8] [$62340,$9] [$59430,$10] [$59431,$11] 
 ! results    : [$63464,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_fold_code_62337:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68604
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_68604:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
	st	%r10, [%sp+128]
	st	%r11, [%sp+124]
code_68586:
funtop_63442:
	ld	[%sp+120], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+116]
	ld	[%sp+120], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+112]
	ld	[%sp+120], %r17
	ld	[%r17+8], %r8
	ld	[%sp+120], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+108]
	ld	[%sp+128], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	bgu	cmpui_68587
	nop
code_68588:
	or	%r0, 0, %r8
cmpui_68587:
	cmp	%r8, 0
	bne,pn	%icc,one_case_63461
	nop
zero_case_63460:
	ba	after_zeroone_63462
	ld	[%sp+124], %r8
one_case_63461:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55066), %r8
	or	%r8, %lo(anonfun_55066), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68603:
	st	%r8, [%sp+104]
code_68592:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+104], %r10
code_68600:
	st	%r8, [%sp+100]
code_68594:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r12, %r15
	ld	[%sp+128], %r11
code_68601:
	mov	%r8, %r11
code_68595:
	! done making normal call
	! making closure call 
	ld	[%sp+116], %r17
	ld	[%r17], %r13
	ld	[%sp+116], %r17
	ld	[%r17+4], %r8
	ld	[%sp+116], %r17
	ld	[%r17+8], %r9
	ld	[%sp+100], %r10
	jmpl	%r13, %r15
	ld	[%sp+124], %r12
code_68602:
code_68596:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r16
	st	%r16, [%sp+128]
	ba	funtop_63442
	st	%r8, [%sp+124]
code_68597:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_63462:
code_68599:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_fold_code_62337,(.-Array_fold_code_62337)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68600
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5410000
	.word 0x00000000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68601
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd4010000
	.word 0x00000000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68602
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68603
	.word 0xb8004809
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xd5410000
	.word 0x00000000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62331
 ! arguments : [$62333,$8] [$62334,$9] [$59405,$10] [$59406,$11] [$59407,$12] 
 ! results    : [$63441,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62331:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68616
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68616:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r9, %r8
code_68605:
funtop_63389:
	ld	[%r8], %r17
	st	%r17, [%sp+108]
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	ld	[%r8+8], %r9
	ld	[%r8+12], %r17
	st	%r17, [%sp+100]
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_68614:
code_68606:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68607
	nop
code_68608:
	call	GCFromML ! delay slot empty
	nop
needgc_68607:
	ld	[%r8], %r11
	ld	[%r8+4], %r9
	ld	[%r8+8], %r10
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 2849, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_fold_code_62337), %r8
	or	%r8, %lo(Array_fold_code_62337), %r8
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
	ld	[%sp+104], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_68611:
	! done making tail call
code_68613:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62331,(.-Array_anonfun_code_62331)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68607
	.word 0xb8003808
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00750000
		! worddata
	.word 0x00000008
	.word 0x00000060
		! -------- label,sizes,reg
	.long code_68614
	.word 0xb8003808
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00750000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_anonfun_code_62325
 ! arguments : [$62327,$8] [$62328,$9] [$56013,$10] 
 ! results    : [$63384,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
Array_anonfun_code_62325:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68628
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68628:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	mov	%r10, %r13
code_68617:
funtop_63357:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68618
	nop
code_68619:
	call	GCFromML ! delay slot empty
	nop
needgc_68618:
	ld	[%r9], %r12
	ld	[%r9+4], %r11
	ld	[%r9+8], %r10
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3361, %r9
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68621
	nop
code_68622:
	or	%r0, 0, %r8
cmpui_68621:
	sll	%r8, 9, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	st	%r9, [%r4]
	st	%r12, [%r4+4]
	ld	[%sp+96], %r17
	ld	[%r17+4], %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_68623
	nop
code_68624:
	or	%r0, 0, %r8
cmpui_68623:
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
	st	%r10, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62331), %r8
	or	%r8, %lo(Array_anonfun_code_62331), %r8
	st	%r8, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68627:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62325,(.-Array_anonfun_code_62325)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68618
	.word 0xb8003808
	.word 0xbffc0200
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
		! worddata
	.word 0x00000008
	.word 0x00000060
	.text
	.align 8
	.global Array_foldri_inner_code_62320
 ! arguments : [$62322,$8] [$62323,$9] [$56011,$10] 
 ! results    : [$63356,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldri_inner_code_62320:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68640
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68640:
	st	%r15, [%sp+92]
	mov	%r8, %r11
	mov	%r9, %r8
	mov	%r10, %r18
code_68629:
funtop_63315:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68630
	nop
code_68631:
	call	GCFromML ! delay slot empty
	nop
needgc_68630:
	! Proj_c at label type_57935_INT
	ld	[%r11], %r10
	! Proj_c at label type_57895_INT
	ld	[%r11+4], %r9
	! Proj_c at label var_poly_c_56006_INT
	ld	[%r11+8], %r13
	ld	[%r8], %r12
	ld	[%r8+4], %r11
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r18, [%r4+4]
	st	%r12, [%r4+8]
	st	%r11, [%r4+12]
	add	%r4, 4, %r11
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62325), %r8
	or	%r8, %lo(Array_anonfun_code_62325), %r8
	st	%r8, [%r4+4]
	st	%r13, [%r4+8]
	st	%r11, [%r4+12]
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
code_68639:
code_68636:
	! done making normal call
code_68638:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_foldri_inner_code_62320,(.-Array_foldri_inner_code_62320)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68630
	.word 0xb8003006
	.word 0x00040900
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68639
	.word 0xb8003006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_foldri_r_code_62315
 ! arguments : [$62317,$8] [$56006,$9] [$62318,$10] [$56007,$11] 
 ! results    : [$63310,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_foldri_r_code_62315:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68655
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68655:
	st	%r15, [%sp+92]
	st	%r9, [%sp+108]
code_68641:
funtop_63267:
	! Proj_c at label 'b_TYV
	ld	[%sp+108], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+104]
	! Proj_c at label 'a_TYV
	ld	[%sp+108], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+100]
	! allocating 1-record
	! done allocating 1 record
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(chkSlice_r_55815), %r8
	or	%r8, %lo(chkSlice_r_55815), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68654:
	st	%r8, [%sp+96]
code_68643:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68653:
	mov	%r8, %r9
code_68645:
	! done making normal call
	add	%r4, 44, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68646
	nop
code_68647:
	call	GCFromML ! delay slot empty
	nop
needgc_68646:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	sethi	%hi(record_63276), %r8
	or	%r8, %lo(record_63276), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	ld	[%sp+108], %r17
	st	%r17, [%r4+12]
	add	%r4, 4, %r10
	add	%r4, 16, %r4
	! done allocating 3 record
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
	sethi	%hi(Array_foldri_inner_code_62320), %r8
	or	%r8, %lo(Array_foldri_inner_code_62320), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68652:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_foldri_r_code_62315,(.-Array_foldri_r_code_62315)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68653
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long needgc_68646
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00510000
		! -------- label,sizes,reg
	.long code_68654
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00540000
	.text
	.align 8
	.global Array_modifyPRIME_code_62385
 ! arguments : [$62387,$8] [$62388,$9] [$56060,$10] 
 ! results    : [$63222,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_modifyPRIME_code_62385:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 144, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68677
	mov	%sp, %fp
	add	%sp, 144, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 144, %sp
code_68677:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+124]
	st	%r10, [%sp+128]
code_68656:
funtop_63192:
	ld	[%sp+124], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+120]
	ld	[%sp+124], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+116]
	ld	[%sp+124], %r17
	ld	[%r17+8], %r11
	ld	[%sp+124], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+112]
	ld	[%sp+124], %r17
	ld	[%r17+16], %r17
	st	%r17, [%sp+108]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68675:
code_68658:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_63218
	nop
zero_case_63217:
	ba	after_zeroone_63219
	or	%r0, 256, %r8
one_case_63218:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+128], %r10
code_68676:
	st	%r8, [%sp+104]
code_68662:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+128], %r10
code_68671:
	st	%r8, [%sp+100]
code_68664:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	jmpl	%r12, %r15
	ld	[%sp+128], %r11
code_68672:
	mov	%r8, %r11
code_68665:
	! done making normal call
	! making closure call 
	ld	[%sp+120], %r17
	ld	[%r17], %r12
	ld	[%sp+120], %r17
	ld	[%r17+4], %r8
	ld	[%sp+120], %r17
	ld	[%r17+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+100], %r10
code_68673:
	mov	%r8, %r12
code_68666:
	! done making normal call
	! making closure call 
	ld	[%sp+112], %r17
	ld	[%r17], %r13
	ld	[%sp+112], %r17
	ld	[%r17+4], %r8
	ld	[%sp+112], %r17
	ld	[%r17+8], %r9
	ld	[%sp+116], %r10
	jmpl	%r13, %r15
	ld	[%sp+128], %r11
code_68674:
code_68667:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r16
	ba	funtop_63192
	st	%r16, [%sp+128]
code_68668:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_63219:
code_68670:
	ld	[%sp+92], %r15
	retl
	add	%sp, 144, %sp
	.size Array_modifyPRIME_code_62385,(.-Array_modifyPRIME_code_62385)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68671
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68672
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68673
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68674
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68675
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68676
	.word 0xb8004807
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55410000
	.word 0x00000000
	.text
	.align 8
	.global Array_anonfun_code_62379
 ! arguments : [$62381,$8] [$62382,$9] [$59482,$10] [$59483,$11] [$59484,$12] 
 ! results    : [$63191,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62379:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68689
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68689:
	st	%r15, [%sp+92]
	st	%r8, [%sp+108]
	mov	%r9, %r8
code_68678:
funtop_63139:
	ld	[%r8], %r17
	st	%r17, [%sp+104]
	ld	[%r8+4], %r9
	ld	[%r8+8], %r17
	st	%r17, [%sp+100]
	ld	[%r8+12], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_68687:
code_68679:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68680
	nop
code_68681:
	call	GCFromML ! delay slot empty
	nop
needgc_68680:
	ld	[%r8], %r11
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	! allocating 1 closures
	! allocating 5-record
	sethi	%hi(6953), %r8
	or	%r8, %lo(6953), %r8
	st	%r8, [%r4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+100], %r17
	st	%r17, [%r4+16]
	ld	[%sp+96], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r9
	add	%r4, 24, %r4
	! done allocating 5 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_modifyPRIME_code_62385), %r8
	or	%r8, %lo(Array_modifyPRIME_code_62385), %r8
	st	%r8, [%r4+4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+8]
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
code_68684:
	! done making tail call
code_68686:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62379,(.-Array_anonfun_code_62379)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68680
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_68687
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.text
	.align 8
	.global Array_modifyi_inner_code_62374
 ! arguments : [$62376,$8] [$62377,$9] [$56047,$10] 
 ! results    : [$63134,$8] 
 ! destroys   :  $13 $12 $11 $10 $9 $8
 ! modifies   :  $13 $12 $11 $10 $9 $8
Array_modifyi_inner_code_62374:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68697
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68697:
	st	%r15, [%sp+92]
	mov	%r8, %r12
	mov	%r10, %r13
code_68690:
funtop_63112:
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68691
	nop
code_68692:
	call	GCFromML ! delay slot empty
	nop
needgc_68691:
	ld	[%r9], %r11
	ld	[%r9+4], %r10
	ld	[%r9+8], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3873, %r8
	st	%r8, [%r4]
	st	%r13, [%r4+4]
	st	%r11, [%r4+8]
	st	%r10, [%r4+12]
	st	%r9, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62379), %r8
	or	%r8, %lo(Array_anonfun_code_62379), %r8
	st	%r8, [%r4+4]
	st	%r12, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68696:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_modifyi_inner_code_62374,(.-Array_modifyi_inner_code_62374)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68691
	.word 0xb8003006
	.word 0xbffc3200
	.word 0xbffc0000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_modifyi_r_code_62369
 ! arguments : [$62371,$8] [$56043,$9] [$62372,$10] [$56044,$11] 
 ! results    : [$63107,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_modifyi_r_code_62369:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68714
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68714:
	st	%r15, [%sp+92]
	st	%r9, [%sp+104]
code_68698:
funtop_63065:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(chkSlice_r_55815), %r8
	or	%r8, %lo(chkSlice_r_55815), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68713:
	st	%r8, [%sp+100]
code_68700:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68711:
	st	%r8, [%sp+96]
code_68702:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+104], %r9
code_68712:
	mov	%r8, %r9
code_68704:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68705
	nop
code_68706:
	call	GCFromML ! delay slot empty
	nop
needgc_68705:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_modifyi_inner_code_62374), %r8
	or	%r8, %lo(Array_modifyi_inner_code_62374), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68710:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_modifyi_r_code_62369,(.-Array_modifyi_r_code_62369)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68711
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00140000
		! -------- label,sizes,reg
	.long code_68712
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long needgc_68705
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_68713
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00100000
	.text
	.align 8
	.global Array_app_code_62427
 ! arguments : [$62429,$8] [$62430,$9] [$55904,$10] 
 ! results    : [$63028,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_app_code_62427:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68734
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_68734:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+120]
	st	%r10, [%sp+124]
code_68715:
funtop_63000:
	ld	[%sp+120], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+116]
	ld	[%sp+120], %r17
	ld	[%r17+4], %r17
	st	%r17, [%sp+112]
	ld	[%sp+120], %r17
	ld	[%r17+8], %r11
	ld	[%sp+120], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+108]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+124], %r10
code_68732:
code_68717:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_63024
	nop
zero_case_63023:
	ba	after_zeroone_63025
	or	%r0, 256, %r8
one_case_63024:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+124], %r10
code_68733:
	st	%r8, [%sp+104]
code_68721:
	! done making normal call
	! making closure call 
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+124], %r10
code_68729:
	st	%r8, [%sp+100]
code_68723:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r12, %r15
	ld	[%sp+124], %r11
code_68730:
	mov	%r8, %r11
code_68724:
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
code_68731:
code_68725:
	! done making normal call
	! making direct call 
	ld	[%sp+104], %r16
	ba	funtop_63000
	st	%r16, [%sp+124]
code_68726:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_63025:
code_68728:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_app_code_62427,(.-Array_app_code_62427)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68729
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15410000
		! -------- label,sizes,reg
	.long code_68730
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x14010000
		! -------- label,sizes,reg
	.long code_68731
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x10010000
		! -------- label,sizes,reg
	.long code_68732
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15410000
		! -------- label,sizes,reg
	.long code_68733
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x15410000
	.text
	.align 8
	.global Array_anonfun_code_62421
 ! arguments : [$62423,$8] [$62424,$9] [$59254,$10] [$59255,$11] [$59256,$12] 
 ! results    : [$62999,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62421:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68746
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68746:
	st	%r15, [%sp+92]
	st	%r8, [%sp+104]
	mov	%r9, %r8
code_68735:
funtop_62950:
	ld	[%r8], %r17
	st	%r17, [%sp+100]
	ld	[%r8+4], %r9
	ld	[%r8+8], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r9], %r13
	ld	[%r9+4], %r8
	jmpl	%r13, %r15
	ld	[%r9+8], %r9
code_68744:
code_68736:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68737
	nop
code_68738:
	call	GCFromML ! delay slot empty
	nop
needgc_68737:
	ld	[%r8], %r11
	ld	[%r8+4], %r10
	ld	[%r8+8], %r9
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 2849, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_app_code_62427), %r8
	or	%r8, %lo(Array_app_code_62427), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
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
code_68741:
	! done making tail call
code_68743:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62421,(.-Array_anonfun_code_62421)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68737
	.word 0xb8003806
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_68744
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
	.align 8
	.global Array_appi_inner_code_62416
 ! arguments : [$62418,$8] [$62419,$9] [$55891,$10] 
 ! results    : [$62945,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
Array_appi_inner_code_62416:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68754
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68754:
	st	%r15, [%sp+92]
	mov	%r8, %r11
	mov	%r10, %r12
code_68747:
funtop_62926:
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68748
	nop
code_68749:
	call	GCFromML ! delay slot empty
	nop
needgc_68748:
	ld	[%r9], %r10
	ld	[%r9+4], %r9
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	st	%r12, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62421), %r8
	or	%r8, %lo(Array_anonfun_code_62421), %r8
	st	%r8, [%r4+4]
	st	%r11, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68753:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_appi_inner_code_62416,(.-Array_appi_inner_code_62416)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68748
	.word 0xb8003006
	.word 0xbffc3a00
	.word 0xbffc2000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global Array_copy_appi_r_code_62411
 ! arguments : [$62413,$8] [$56104,$9] [$62414,$10] [$56105,$11] 
 ! results    : [$62921,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copy_appi_r_code_62411:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68768
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68768:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
code_68755:
funtop_62889:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(chkSlice_r_55815), %r8
	or	%r8, %lo(chkSlice_r_55815), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68767:
	st	%r8, [%sp+96]
code_68757:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68766:
	mov	%r8, %r9
code_68759:
	! done making normal call
	add	%r4, 28, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68760
	nop
code_68761:
	call	GCFromML ! delay slot empty
	nop
needgc_68760:
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
	sethi	%hi(Array_appi_inner_code_62416), %r8
	or	%r8, %lo(Array_appi_inner_code_62416), %r8
	st	%r8, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68765:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_copy_appi_r_code_62411,(.-Array_copy_appi_r_code_62411)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68766
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_68760
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_68767
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global Array_app_code_62466
 ! arguments : [$62468,$8] [$62469,$9] [$55728,$10] 
 ! results    : [$62860,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_app_code_62466:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68785
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_68785:
	st	%r15, [%sp+92]
	st	%r8, [%sp+96]
	st	%r9, [%sp+116]
	st	%r10, [%sp+120]
code_68769:
funtop_62832:
	ld	[%sp+116], %r17
	ld	[%r17], %r17
	st	%r17, [%sp+112]
	ld	[%sp+116], %r17
	ld	[%r17+4], %r11
	ld	[%sp+116], %r17
	ld	[%r17+8], %r17
	st	%r17, [%sp+108]
	ld	[%sp+116], %r17
	ld	[%r17+12], %r17
	st	%r17, [%sp+104]
	! making closure call 
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_68783:
code_68771:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_62856
	nop
zero_case_62855:
	ba	after_zeroone_62857
	or	%r0, 256, %r8
one_case_62856:
	or	%r0, 1, %r11
	! making closure call 
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+120], %r10
code_68784:
	st	%r8, [%sp+100]
code_68775:
	! done making normal call
	! making closure call 
	ld	[%sp+108], %r17
	ld	[%r17], %r12
	ld	[%sp+108], %r17
	ld	[%r17+4], %r8
	ld	[%sp+108], %r17
	ld	[%r17+8], %r9
	ld	[%sp+112], %r10
	jmpl	%r12, %r15
	ld	[%sp+120], %r11
code_68781:
	mov	%r8, %r10
code_68776:
	! done making normal call
	! making closure call 
	ld	[%sp+104], %r17
	ld	[%r17], %r11
	ld	[%sp+104], %r17
	ld	[%r17+4], %r8
	ld	[%sp+104], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_68782:
code_68777:
	! done making normal call
	! making direct call 
	ld	[%sp+100], %r16
	ba	funtop_62832
	st	%r16, [%sp+120]
code_68778:
	! done making self tail call
	or	%r0, 0, %r8
after_zeroone_62857:
code_68780:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size Array_app_code_62466,(.-Array_app_code_62466)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68781
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04110000
		! -------- label,sizes,reg
	.long code_68782
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x04010000
		! -------- label,sizes,reg
	.long code_68783
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05510000
		! -------- label,sizes,reg
	.long code_68784
	.word 0xb8004006
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x05510000
	.text
	.align 8
	.global Array_anonfun_code_62460
 ! arguments : [$62462,$8] [$62463,$9] [$55720,$10] 
 ! results    : [$62831,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_anonfun_code_62460:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68797
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68797:
	st	%r15, [%sp+92]
	st	%r8, [%sp+104]
	mov	%r9, %r8
	st	%r10, [%sp+108]
code_68786:
funtop_62793:
	ld	[%r8], %r9
	ld	[%r8+4], %r17
	st	%r17, [%sp+100]
	ld	[%r8+8], %r17
	st	%r17, [%sp+96]
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_68795:
	mov	%r8, %r9
code_68787:
	! done making normal call
	add	%r4, 36, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68788
	nop
code_68789:
	call	GCFromML ! delay slot empty
	nop
needgc_68788:
	! allocating 1 closures
	! allocating 4-record
	or	%r0, 3361, %r8
	st	%r8, [%r4]
	ld	[%sp+108], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	ld	[%sp+100], %r17
	st	%r17, [%r4+12]
	ld	[%sp+96], %r17
	st	%r17, [%r4+16]
	add	%r4, 4, %r9
	add	%r4, 20, %r4
	! done allocating 4 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_app_code_62466), %r8
	or	%r8, %lo(Array_app_code_62466), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
	or	%r0, 0, %r10
	! making closure call 
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_68792:
	! done making tail call
code_68794:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_anonfun_code_62460,(.-Array_anonfun_code_62460)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68788
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
		! -------- label,sizes,reg
	.long code_68795
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00550000
	.text
	.align 8
	.global Array_app_inner_code_62454
 ! arguments : [$62456,$8] [$62457,$9] [$55718,$10] 
 ! results    : [$62788,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_app_inner_code_62454:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68810
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68810:
	st	%r15, [%sp+92]
	mov	%r10, %r12
code_68798:
funtop_62752:
	! Proj_c at label type_58035_INT
	ld	[%r8], %r18
	! Proj_c at label var_poly_c_56121_INT
	ld	[%r8+4], %r17
	st	%r17, [%sp+104]
	ld	[%r9], %r17
	st	%r17, [%sp+100]
	ld	[%r9+4], %r17
	st	%r17, [%sp+96]
	sethi	%hi(record_62509), %r8
	or	%r8, %lo(record_62509), %r10
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
code_68809:
	mov	%r8, %r9
code_68802:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68803
	nop
code_68804:
	call	GCFromML ! delay slot empty
	nop
needgc_68803:
	! allocating 1 closures
	! allocating 3-record
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r9
	add	%r4, 16, %r4
	! done allocating 3 record
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_anonfun_code_62460), %r8
	or	%r8, %lo(Array_anonfun_code_62460), %r8
	st	%r8, [%r4+4]
	ld	[%sp+104], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68808:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_app_inner_code_62454,(.-Array_app_inner_code_62454)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_68803
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
		! -------- label,sizes,reg
	.long code_68809
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.text
	.align 8
	.global Array_copy_app_r_code_62449
 ! arguments : [$62451,$8] [$56121,$9] [$62452,$10] [$56122,$11] 
 ! results    : [$62747,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_copy_app_r_code_62449:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68824
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_68824:
	st	%r15, [%sp+92]
	st	%r9, [%sp+100]
code_68811:
funtop_62711:
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54832), %r8
	or	%r8, %lo(outer_valbind_r_54832), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68823:
	st	%r8, [%sp+96]
code_68813:
	! done making normal call
	or	%r0, 256, %r11
	! making closure call 
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	ld	[%sp+100], %r9
code_68822:
	mov	%r8, %r9
code_68815:
	! done making normal call
	add	%r4, 40, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_68816
	nop
code_68817:
	call	GCFromML ! delay slot empty
	nop
needgc_68816:
	! allocating 1 closures
	! allocating 2-record
	or	%r0, 785, %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+8]
	add	%r4, 4, %r10
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
	! allocating 3-record
	or	%r0, 1561, %r8
	st	%r8, [%r4]
	sethi	%hi(Array_app_inner_code_62454), %r8
	or	%r8, %lo(Array_app_inner_code_62454), %r8
	st	%r8, [%r4+4]
	st	%r10, [%r4+8]
	st	%r9, [%r4+12]
	add	%r4, 4, %r8
	add	%r4, 16, %r4
	! done allocating 3 record
	! done allocating 1 closures
code_68821:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size Array_copy_app_r_code_62449,(.-Array_copy_app_r_code_62449)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_68822
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long needgc_68816
	.word 0xb8003806
	.word 0x00000200
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
		! -------- label,sizes,reg
	.long code_68823
	.word 0xb8003806
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00040000
	.text
	.align 8
	.global Array_main
 ! arguments : 
 ! results    : [$62710,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
Array_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_68912
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_68912:
	st	%r15, [%sp+92]
code_68825:
funtop_62492:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_68911:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_68826:
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
	sethi	%hi(anonfun_54809), %r8
	or	%r8, %lo(anonfun_54809), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_54815), %r8
	or	%r8, %lo(anonfun_54815), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(outer_valbind_r_54832), %r8
	or	%r8, %lo(outer_valbind_r_54832), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(outer_valbind_r_54924), %r8
	or	%r8, %lo(outer_valbind_r_54924), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(outer_valbind_r_54948), %r8
	or	%r8, %lo(outer_valbind_r_54948), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_55050), %r8
	or	%r8, %lo(anonfun_55050), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_55066), %r8
	or	%r8, %lo(anonfun_55066), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(anonfun_55073), %r8
	or	%r8, %lo(anonfun_55073), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(_r_55080), %r8
	or	%r8, %lo(_r_55080), %r8
	! done allocating 1 closures
	sethi	%hi(General_STR_r_INT), %r8
	or	%r8, %lo(General_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_56675+-4), %r8
	st	%r9, [%r8+%lo(mk_56675+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 48, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_68842
	nop
code_68843:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_68842:
	sethi	%hi(mk_56675), %r8
	or	%r8, %lo(mk_56675), %r12
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
	sethi	%hi(array_r_55198), %r8
	or	%r8, %lo(array_r_55198), %r8
	! done allocating 1 closures
	sethi	%hi(List_STR_r_INT), %r8
	or	%r8, %lo(List_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(hd_56750+-4), %r8
	st	%r9, [%r8+%lo(hd_56750+-4)]
	sethi	%hi(hd_56750), %r8
	or	%r8, %lo(hd_56750), %r12
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
	sethi	%hi(fromListPRIME_r_55280), %r8
	or	%r8, %lo(fromListPRIME_r_55280), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(fromList_r_55325), %r8
	or	%r8, %lo(fromList_r_55325), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(tabulate_r_55361), %r8
	or	%r8, %lo(tabulate_r_55361), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(length_r_55374), %r8
	or	%r8, %lo(length_r_55374), %r8
	! done allocating 1 closures
	sethi	%hi(Subscript_r_INT), %r8
	or	%r8, %lo(Subscript_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_56929+-4), %r8
	st	%r9, [%r8+%lo(mk_56929+-4)]
	sethi	%hi(mk_56929), %r8
	or	%r8, %lo(mk_56929), %r12
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
	sethi	%hi(sub_r_55397), %r8
	or	%r8, %lo(sub_r_55397), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(update_r_55421), %r8
	or	%r8, %lo(update_r_55421), %r8
	! done allocating 1 closures
	sethi	%hi(General_STR_r_INT), %r8
	or	%r8, %lo(General_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+40], %r8
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(mk_57067+-4), %r8
	st	%r9, [%r8+%lo(mk_57067+-4)]
	sethi	%hi(mk_57067), %r8
	or	%r8, %lo(mk_57067), %r12
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
	sethi	%hi(extract_r_55531), %r8
	or	%r8, %lo(extract_r_55531), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(copy_r_55633), %r8
	or	%r8, %lo(copy_r_55633), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(copyVec_r_55712), %r8
	or	%r8, %lo(copyVec_r_55712), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(foldl_r_55775), %r8
	or	%r8, %lo(foldl_r_55775), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(chkSlice_r_55815), %r8
	or	%r8, %lo(chkSlice_r_55815), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(foldr_r_55852), %r8
	or	%r8, %lo(foldr_r_55852), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(modify_r_55885), %r8
	or	%r8, %lo(modify_r_55885), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(foldli_r_56005), %r8
	or	%r8, %lo(foldli_r_56005), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(foldri_r_56042), %r8
	or	%r8, %lo(foldri_r_56042), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(modifyi_r_56078), %r8
	or	%r8, %lo(modifyi_r_56078), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(copy_appi_r_56103), %r8
	or	%r8, %lo(copy_appi_r_56103), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(copy_app_r_56120), %r8
	or	%r8, %lo(copy_app_r_56120), %r8
	! done allocating 1 closures
	! allocating 19-record
	! done allocating 19 record
	or	%r0, 256, %r8
code_68910:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size Array_main,(.-Array_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long afterMutateCheck_68842
	.word 0xb8003006
	.word 0x80002000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_68911
	.word 0xb8003006
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
Array_unit_CODE_END_VAL:
	.section	".rodata"
		! endgcinfo with filler for alignment
	.globl Array_unit_GCTABLE_END_VAL
Array_unit_GCTABLE_END_VAL:
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl Array_unit_GLOBALS_BEGIN_VAL
Array_unit_GLOBALS_BEGIN_VAL:
		! static record tag
	.word 0x00000211
record_62499:
	.long Array__code_61729
	.word 0x00000100
		! static record tag
	.word 0x00000211
record_62503:
	.long Array__code_61734
	.word 0x00000100
		! static record tag
	.word 0x00000311
record_62507:
	.long record_62499
	.long record_62503
		! Global
	.word 0x0000006f
	.globl Array_STR_c_INT
Array_STR_c_INT:
	.long record_62507
	.long record_62507
		! static record tag
	.word 0x00000011
record_62509:
	.word 0x00000005
	.word 0x00000000
		! static record tag
	.word 0x00000619
anonfun_54809:
	.long Array_anonfun_code_61739
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_54815:
	.long Array_anonfun_code_61744
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
outer_valbind_r_54832:
	.long Array_outer_valbind_r_code_61749
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
outer_valbind_r_54924:
	.long Array_outer_valbind_r_code_61761
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
outer_valbind_r_54948:
	.long Array_outer_valbind_r_code_61775
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_55050:
	.long Array_anonfun_code_61789
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_55066:
	.long Array_anonfun_code_61794
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
anonfun_55073:
	.long Array_anonfun_code_61799
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
_r_55080:
	.long Array__r_code_61804
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
mk_56675:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
array_r_55198:
	.long Array_array_r_code_61816
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
hd_56750:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
fromListPRIME_r_55280:
	.long Array_fromListPRIME_r_code_61830
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
fromList_r_55325:
	.long Array_fromList_r_code_61871
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
tabulate_r_55361:
	.long Array_tabulate_r_code_61890
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
length_r_55374:
	.long Array_length_r_code_61922
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
mk_56929:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
sub_r_55397:
	.long Array_sub_r_code_61934
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
update_r_55421:
	.long Array_update_r_code_61950
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
mk_57067:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
extract_r_55531:
	.long Array_extract_r_code_61966
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
copy_r_55633:
	.long Array_copy_r_code_62015
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
copyVec_r_55712:
	.long Array_copyVec_r_code_62064
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
foldl_r_55775:
	.long Array_foldl_r_code_62097
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
chkSlice_r_55815:
	.long Array_chkSlice_r_code_62153
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
foldr_r_55852:
	.long Array_foldr_r_code_62165
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
modify_r_55885:
	.long Array_modify_r_code_62217
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
foldli_r_56005:
	.long Array_foldli_r_code_62261
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
foldri_r_56042:
	.long Array_foldri_r_code_62315
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
modifyi_r_56078:
	.long Array_modifyi_r_code_62369
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
copy_appi_r_56103:
	.long Array_copy_appi_r_code_62411
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
copy_app_r_56120:
	.long Array_copy_app_r_code_62449
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x07fffd99
record_62708:
	.long _r_55080
	.word 0x00100000
	.long array_r_55198
	.long fromList_r_55325
	.long tabulate_r_55361
	.long length_r_55374
	.long sub_r_55397
	.long update_r_55421
	.long extract_r_55531
	.long copy_r_55633
	.long copyVec_r_55712
	.long copy_appi_r_56103
	.long copy_app_r_56120
	.long foldli_r_56005
	.long foldri_r_56042
	.long foldl_r_55775
	.long foldr_r_55852
	.long modifyi_r_56078
	.long modify_r_55885
		! Global
	.word 0x0000006f
	.globl Array_STR_r_INT
Array_STR_r_INT:
	.long record_62708
	.long record_62708
		! static record tag
	.word 0x00000009
record_63276:
	.word 0x00000009
		! static record tag
	.word 0x00000009
record_63512:
	.word 0x00000009
		! static record tag
	.word 0x00000009
record_63960:
	.word 0x00000009
		! static record tag
	.word 0x00000009
record_64364:
	.word 0x00000009
		! static record tag
	.word 0x00000619
record_66464:
	.long Array_len_inner_code_61876
	.word 0x00000100
	.word 0x00000100
		! Module closure
	.word 0x00000619
	.globl Array_unit_closure
Array_unit_closure:
	.long Array_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl Array_unit
Array_unit:
	.long Array_unit_closure
	.long Array_unit_closure
	.globl Array_unit_GLOBALS_END_VAL
Array_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.long 0 !filler

	.data
	.align 8
	.globl Array_unit_TRACE_GLOBALS_BEGIN_VAL
Array_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long mk_57067
	.long mk_56929
	.long hd_56750
	.long mk_56675
	.globl Array_unit_TRACE_GLOBALS_END_VAL
Array_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
