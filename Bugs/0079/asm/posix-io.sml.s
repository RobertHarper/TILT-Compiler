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
	.globl POSIX_IO_Str_unit_GCTABLE_BEGIN_VAL
POSIX_IO_Str_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl POSIX_IO_Str_unit_CODE_END_VAL
	.globl POSIX_IO_Str_unit_CODE_BEGIN_VAL
POSIX_IO_Str_unit_CODE_BEGIN_VAL:
	.text
	.align 8
	.global POSIX_IO_Str_osval_code_123201
 ! arguments : [$123203,$8] [$123204,$9] [$114458,$10] 
 ! results    : [$126258,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_osval_code_123201:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126269
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126269:
	st	%r15, [%sp+92]
	mov	%r10, %r8
code_126264:
funtop_126254:
	! making external call
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	posix_io_num ! delay slot empty
	nop
code_126268:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126265:
	! done making external call
code_126267:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_osval_code_123201,(.-POSIX_IO_Str_osval_code_123201)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126268
	.word 0x00180007
	.word 0x00170000
	.word 0x80000000
	.word 0x80000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_fs_intof_code_123206
 ! arguments : [$123208,$8] [$123209,$9] [$114499,$10] 
 ! results    : [$126252,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_fs_intof_code_123206:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126277
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126277:
	st	%r15, [%sp+92]
code_126270:
funtop_126241:
	! making closure call
	sethi	%hi(_121536), %r8
	or	%r8, %lo(_121536), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126276:
code_126273:
	! done making normal call
code_126275:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_fs_intof_code_123206,(.-POSIX_IO_Str_fs_intof_code_123206)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126276
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_pipe_code_123211
 ! arguments : [$123213,$8] [$123214,$9] 
 ! results    : [$126238,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_pipe_code_123211:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126295
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126295:
	st	%r15, [%sp+92]
code_126278:
funtop_126205:
	! making external call
	call	save_regs_MLtoC
	or	%r0, 256, %r8
	call	posix_io_pipe ! delay slot empty
	nop
code_126294:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126279:
	! done making external call
	ld	[%r8], %r10
	ld	[%r8+4], %r16
	st	%r16, [%sp+100]
	! making closure call
	sethi	%hi(strbindvar_r_wordToFD_118448), %r8
	or	%r8, %lo(strbindvar_r_wordToFD_118448), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126292:
	st	%r8, [%sp+96]
code_126282:
	! done making normal call
	! making closure call
	sethi	%hi(strbindvar_r_wordToFD_118448), %r8
	or	%r8, %lo(strbindvar_r_wordToFD_118448), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126293:
	mov	%r8, %r9
code_126285:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126286
	nop
code_126287:
	call	GCFromML ! delay slot empty
	nop
needgc_126286:
	! allocating 2-record
	sethi	%hi(gctag_118464), %r8
	ld	[%r8+%lo(gctag_118464)], %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
code_126291:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_pipe_code_123211,(.-POSIX_IO_Str_pipe_code_123211)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126292
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126293
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long needgc_126286
	.word 0x001c000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000000
	.long type_118285
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long code_126294
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_dup_code_123216
 ! arguments : [$123218,$8] [$123219,$9] [$114514,$10] 
 ! results    : [$126204,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_dup_code_123216:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126308
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126308:
	st	%r15, [%sp+92]
code_126296:
funtop_126183:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126307:
code_126298:
	! done making normal call
	! making external call
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	posix_io_dup ! delay slot empty
	nop
code_126305:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_126299:
	! done making external call
	! making closure call
	sethi	%hi(strbindvar_r_wordToFD_118448), %r8
	or	%r8, %lo(strbindvar_r_wordToFD_118448), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 96, %sp
code_126302:
	! done making tail call
code_126304:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_dup_code_123216,(.-POSIX_IO_Str_dup_code_123216)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126305
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126307
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_dup2_code_123221
 ! arguments : [$123223,$8] [$123224,$9] [$121556,$10] [$121557,$11] 
 ! results    : [$126182,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_dup2_code_123221:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126320
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126320:
	st	%r15, [%sp+92]
	st	%r11, [%sp+100]
code_126309:
funtop_126163:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126319:
	st	%r8, [%sp+96]
code_126311:
	! done making normal call
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126317:
	mov	%r8, %r9
code_126313:
	! done making normal call
	! making external call
	call	save_regs_MLtoC
	ld	[%sp+96], %r8
	call	posix_io_dup2 ! delay slot empty
	nop
code_126318:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126314:
	! done making external call
code_126316:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_dup2_code_123221,(.-POSIX_IO_Str_dup2_code_123221)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126317
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126318
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126319
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000000
	.long type_118285
	.text
	.align 8
	.global POSIX_IO_Str_close_code_123226
 ! arguments : [$123228,$8] [$123229,$9] [$114522,$10] 
 ! results    : [$126162,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_close_code_123226:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126329
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126329:
	st	%r15, [%sp+92]
code_126321:
funtop_126151:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126328:
code_126323:
	! done making normal call
	! making external call
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	posix_io_close ! delay slot empty
	nop
code_126327:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126324:
	! done making external call
code_126326:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_close_code_123226,(.-POSIX_IO_Str_close_code_123226)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126327
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126328
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_readArr_code_123231
 ! arguments : [$123233,$8] [$123234,$9] [$121590,$10] [$121591,$11] 
 ! results    : [$126080,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_readArr_code_123231:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126382
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_126382:
	st	%r15, [%sp+92]
	st	%r10, [%sp+112]
code_126330:
funtop_126010:
	ld	[%r11+8], %r16
	st	%r16, [%sp+108]
	ld	[%r11], %r16
	st	%r16, [%sp+104]
	ld	[%r11+4], %r8
sumarm_126027:
	cmp	%r8, 0
	bne	sumarm_126028
	nop
code_126331:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r16
	st	%r16, [%sp+100]
	bge	cmpsi_126332
	nop
code_126333:
	or	%r0, 0, %r16
	st	%r16, [%sp+100]
cmpsi_126332:
	! making closure call
	sethi	%hi(length_118500), %r8
	or	%r8, %lo(length_118500), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_126376:
	st	%r8, [%sp+96]
code_126336:
	! done making normal call
	ld	[%sp+100], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_126045
	nop
zero_case_126044:
	ba	after_zeroone_126046
	or	%r0, 0, %r8
one_case_126045:
	ld	[%sp+96], %r16
	ld	[%sp+104], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	ble	cmpsi_126339
	nop
code_126340:
	or	%r0, 0, %r8
cmpsi_126339:
after_zeroone_126046:
	cmp	%r8, 0
	bne,pn	%icc,one_case_126056
	nop
zero_case_126055:
	sethi	%hi(mk_118514), %r8
	or	%r8, %lo(mk_118514), %r9
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
	ba	after_zeroone_126057
	or	%r0, 0, %r8
one_case_126056:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_126380:
code_126347:
	! done making normal call
	ld	[%sp+104], %r16
	ld	[%sp+96], %r17
	subcc	%r17, %r16, %r10
	bvs,pn	%icc,localOverflowFromML
	nop
code_126348:
	! making external call
	ld	[%sp+108], %r9
	call	save_regs_MLtoC
	ld	[%sp+104], %r11
	call	posix_io_readbuf ! delay slot empty
	nop
code_126378:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126349:
	! done making external call
after_zeroone_126057:
	ba	after_sum_126024 ! delay slot empty
	nop
sumarm_126028:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r16
	st	%r16, [%sp+96]
	bge	cmpsi_126351
	nop
code_126352:
	or	%r0, 0, %r16
	st	%r16, [%sp+96]
cmpsi_126351:
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! making closure call
	sethi	%hi(length_118500), %r8
	or	%r8, %lo(length_118500), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_126377:
	mov	%r8, %r9
code_126355:
	! done making normal call
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_126105
	nop
zero_case_126104:
	ba	after_zeroone_126106
	or	%r0, 0, %r8
one_case_126105:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bge	cmpsi_126358
	nop
code_126359:
	or	%r0, 0, %r8
cmpsi_126358:
after_zeroone_126106:
	cmp	%r8, 0
	bne,pn	%icc,one_case_126116
	nop
zero_case_126115:
	ba	after_zeroone_126117
	or	%r0, 0, %r8
one_case_126116:
	ld	[%sp+100], %r16
	ld	[%sp+104], %r17
	addcc	%r17, %r16, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_126362:
	cmp	%r8, %r9
	or	%r0, 1, %r8
	ble	cmpsi_126363
	nop
code_126364:
	or	%r0, 0, %r8
cmpsi_126363:
after_zeroone_126117:
	cmp	%r8, 0
	bne,pn	%icc,one_case_126130
	nop
zero_case_126129:
	sethi	%hi(mk_118514), %r8
	or	%r8, %lo(mk_118514), %r9
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
	ba	after_zeroone_126131
	or	%r0, 0, %r8
one_case_126130:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_126381:
code_126371:
	! done making normal call
	! making external call
	ld	[%sp+108], %r9
	ld	[%sp+100], %r10
	call	save_regs_MLtoC
	ld	[%sp+104], %r11
	call	posix_io_readbuf ! delay slot empty
	nop
code_126379:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126372:
	! done making external call
after_zeroone_126131:
	ba	after_sum_126024 ! delay slot empty
	nop
sumarm_126081:
after_sum_126024:
code_126375:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size POSIX_IO_Str_readArr_code_123231,(.-POSIX_IO_Str_readArr_code_123231)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126376
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03400000
		! worddata
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long code_126377
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03400000
		! worddata
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long code_126378
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126379
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126380
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_126381
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
	.align 8
	.global POSIX_IO_Str_readVec_code_123236
 ! arguments : [$123238,$8] [$123239,$9] [$121623,$10] [$121624,$11] 
 ! results    : [$126004,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_readVec_code_123236:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126398
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126398:
	st	%r15, [%sp+92]
	st	%r11, [%sp+96]
code_126383:
funtop_125983:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bl	cmpsi_126384
	nop
code_126385:
	or	%r0, 0, %r8
cmpsi_126384:
	cmp	%r8, 0
	bne,pn	%icc,one_case_125991
	nop
zero_case_125990:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126397:
code_126388:
	! done making normal call
	! making external call
	call	save_regs_MLtoC
	ld	[%sp+96], %r9
	call	posix_io_read ! delay slot empty
	nop
code_126396:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126389:
	! done making external call
	ba	after_zeroone_125992 ! delay slot empty
	nop
one_case_125991:
	sethi	%hi(mk_118514), %r8
	or	%r8, %lo(mk_118514), %r9
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
after_zeroone_125992:
code_126395:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_readVec_code_123236,(.-POSIX_IO_Str_readVec_code_123236)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126396
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126397
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_writeArr_code_123241
 ! arguments : [$123243,$8] [$123244,$9] [$121669,$10] [$121670,$11] 
 ! results    : [$125912,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_writeArr_code_123241:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126451
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_126451:
	st	%r15, [%sp+92]
	st	%r10, [%sp+112]
code_126399:
funtop_125842:
	ld	[%r11+8], %r16
	st	%r16, [%sp+108]
	ld	[%r11], %r16
	st	%r16, [%sp+104]
	ld	[%r11+4], %r8
sumarm_125859:
	cmp	%r8, 0
	bne	sumarm_125860
	nop
code_126400:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r16
	st	%r16, [%sp+100]
	bge	cmpsi_126401
	nop
code_126402:
	or	%r0, 0, %r16
	st	%r16, [%sp+100]
cmpsi_126401:
	! making closure call
	sethi	%hi(length_118500), %r8
	or	%r8, %lo(length_118500), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_126445:
	st	%r8, [%sp+96]
code_126405:
	! done making normal call
	ld	[%sp+100], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_125877
	nop
zero_case_125876:
	ba	after_zeroone_125878
	or	%r0, 0, %r8
one_case_125877:
	ld	[%sp+96], %r16
	ld	[%sp+104], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	ble	cmpsi_126408
	nop
code_126409:
	or	%r0, 0, %r8
cmpsi_126408:
after_zeroone_125878:
	cmp	%r8, 0
	bne,pn	%icc,one_case_125888
	nop
zero_case_125887:
	sethi	%hi(mk_118514), %r8
	or	%r8, %lo(mk_118514), %r9
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
	ba	after_zeroone_125889
	or	%r0, 0, %r8
one_case_125888:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_126449:
code_126416:
	! done making normal call
	ld	[%sp+104], %r16
	ld	[%sp+96], %r17
	subcc	%r17, %r16, %r10
	bvs,pn	%icc,localOverflowFromML
	nop
code_126417:
	! making external call
	ld	[%sp+108], %r9
	call	save_regs_MLtoC
	ld	[%sp+104], %r11
	call	posix_io_writebuf ! delay slot empty
	nop
code_126447:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126418:
	! done making external call
after_zeroone_125889:
	ba	after_sum_125856 ! delay slot empty
	nop
sumarm_125860:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r16
	st	%r16, [%sp+96]
	bge	cmpsi_126420
	nop
code_126421:
	or	%r0, 0, %r16
	st	%r16, [%sp+96]
cmpsi_126420:
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! making closure call
	sethi	%hi(length_118500), %r8
	or	%r8, %lo(length_118500), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_126446:
	mov	%r8, %r9
code_126424:
	! done making normal call
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_125937
	nop
zero_case_125936:
	ba	after_zeroone_125938
	or	%r0, 0, %r8
one_case_125937:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bge	cmpsi_126427
	nop
code_126428:
	or	%r0, 0, %r8
cmpsi_126427:
after_zeroone_125938:
	cmp	%r8, 0
	bne,pn	%icc,one_case_125948
	nop
zero_case_125947:
	ba	after_zeroone_125949
	or	%r0, 0, %r8
one_case_125948:
	ld	[%sp+100], %r16
	ld	[%sp+104], %r17
	addcc	%r17, %r16, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_126431:
	cmp	%r8, %r9
	or	%r0, 1, %r8
	ble	cmpsi_126432
	nop
code_126433:
	or	%r0, 0, %r8
cmpsi_126432:
after_zeroone_125949:
	cmp	%r8, 0
	bne,pn	%icc,one_case_125962
	nop
zero_case_125961:
	sethi	%hi(mk_118514), %r8
	or	%r8, %lo(mk_118514), %r9
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
	ba	after_zeroone_125963
	or	%r0, 0, %r8
one_case_125962:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_126450:
code_126440:
	! done making normal call
	! making external call
	ld	[%sp+108], %r9
	ld	[%sp+100], %r10
	call	save_regs_MLtoC
	ld	[%sp+104], %r11
	call	posix_io_writebuf ! delay slot empty
	nop
code_126448:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126441:
	! done making external call
after_zeroone_125963:
	ba	after_sum_125856 ! delay slot empty
	nop
sumarm_125913:
after_sum_125856:
code_126444:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size POSIX_IO_Str_writeArr_code_123241,(.-POSIX_IO_Str_writeArr_code_123241)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126445
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03400000
		! worddata
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long code_126446
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03400000
		! worddata
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long code_126447
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126448
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126449
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_126450
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
	.align 8
	.global POSIX_IO_Str_writeVec_code_123246
 ! arguments : [$123248,$8] [$123249,$9] [$121702,$10] [$121703,$11] 
 ! results    : [$125766,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_writeVec_code_123246:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126504
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_126504:
	st	%r15, [%sp+92]
	st	%r10, [%sp+112]
code_126452:
funtop_125695:
	ld	[%r11+8], %r16
	st	%r16, [%sp+108]
	ld	[%r11], %r16
	st	%r16, [%sp+104]
	ld	[%r11+4], %r8
sumarm_125712:
	cmp	%r8, 0		pds %r8 = 0x9c23a080
	bne	sumarm_125713	pds branch taken
	nop
code_126453:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	or	%r0, 1, %r16
	st	%r16, [%sp+100]
	bge	cmpsi_126454
	nop
code_126455:
	or	%r0, 0, %r16
	st	%r16, [%sp+100]
cmpsi_126454:
	! making closure call
	sethi	%hi(length_118679), %r8
	or	%r8, %lo(length_118679), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_126498:
	st	%r8, [%sp+96]
code_126458:
	! done making normal call
	ld	[%sp+100], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_125730
	nop
zero_case_125729:
	ba	after_zeroone_125731
	or	%r0, 0, %r8
one_case_125730:
	ld	[%sp+96], %r16
	ld	[%sp+104], %r17
	cmp	%r17, %r16
	or	%r0, 1, %r8
	ble	cmpsi_126461
	nop
code_126462:
	or	%r0, 0, %r8
cmpsi_126461:
after_zeroone_125731:
	cmp	%r8, 0
	bne,pn	%icc,one_case_125741
	nop
zero_case_125740:
	sethi	%hi(mk_118514), %r8
	or	%r8, %lo(mk_118514), %r9
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
	ba	after_zeroone_125742
	or	%r0, 0, %r8
one_case_125741:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_126502:
code_126469:
	! done making normal call
	ld	[%sp+104], %r16
	ld	[%sp+96], %r17
	subcc	%r17, %r16, %r10
	bvs,pn	%icc,localOverflowFromML
	nop
code_126470:
	! making external call
	ld	[%sp+108], %r9
	call	save_regs_MLtoC
	ld	[%sp+104], %r11
	call	posix_io_writebuf ! delay slot empty
	nop
code_126500:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126471:
	! done making external call
after_zeroone_125742:
	ba	after_sum_125709 ! delay slot empty
	nop
sumarm_125713:
	ld	[%sp+104], %r17
	cmp	%r17, 0		pds %l1 = 0x91d02001= -1848631295
	or	%r0, 1, %r16
	st	%r16, [%sp+96]
	bge	cmpsi_126473
	nop
code_126474:
	or	%r0, 0, %r16
	st	%r16, [%sp+96]
cmpsi_126473:
	ld	[%r8], %r16
	st	%r16, [%sp+100]
	! making closure call
	sethi	%hi(length_118679), %r8
	or	%r8, %lo(length_118679), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_126499:
	mov	%r8, %r9
code_126477:
	! done making normal call
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_125795
	nop
zero_case_125794:
	ba	after_zeroone_125796
	or	%r0, 0, %r8
one_case_125795:
	ld	[%sp+100], %r17
	cmp	%r17, 0
	or	%r0, 1, %r8
	bge	cmpsi_126480
	nop
code_126481:
	or	%r0, 0, %r8
cmpsi_126480:
after_zeroone_125796:
	cmp	%r8, 0
	bne,pn	%icc,one_case_125806
	nop
zero_case_125805:
	ba	after_zeroone_125807
	or	%r0, 0, %r8
one_case_125806:
	ld	[%sp+100], %r16
	ld	[%sp+104], %r17
	addcc	%r17, %r16, %r8
	bvs,pn	%icc,localOverflowFromML
	nop
code_126484:
	cmp	%r8, %r9
	or	%r0, 1, %r8
	ble	cmpsi_126485
	nop
code_126486:
	or	%r0, 0, %r8
cmpsi_126485:
after_zeroone_125807:
	cmp	%r8, 0
	bne,pn	%icc,one_case_125820
	nop
zero_case_125819:
	sethi	%hi(mk_118514), %r8
	or	%r8, %lo(mk_118514), %r9
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
	ba	after_zeroone_125821
	or	%r0, 0, %r8
one_case_125820:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+112], %r10
code_126503:
code_126493:
	! done making normal call
	! making external call
	ld	[%sp+108], %r9
	ld	[%sp+100], %r10
	call	save_regs_MLtoC
	ld	[%sp+104], %r11
	call	posix_io_writebuf ! delay slot empty
	nop
code_126501:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126494:
	! done making external call
after_zeroone_125821:
	ba	after_sum_125709 ! delay slot empty
	nop
sumarm_125767:
after_sum_125709:
code_126497:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size POSIX_IO_Str_writeVec_code_123246,(.-POSIX_IO_Str_writeVec_code_123246)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126498
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03400000
		! worddata
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long code_126499
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x03400000
		! worddata
	.word 0x80000000
	.long type_118285
		! -------- label,sizes,reg
	.long code_126500
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126501
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126502
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
		! -------- label,sizes,reg
	.long code_126503
	.word 0x00200007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00400000
	.text
	.align 8
	.global POSIX_IO_Str_vars_eq_0_code_123251
 ! arguments : [$123253,$8] [$123254,$9] [$121735,$10] [$121736,$11] 
 ! results    : [$125664,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
POSIX_IO_Str_vars_eq_0_code_123251:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126519
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126519:
	st	%r15, [%sp+92]
code_126505:
funtop_125641:
sumarm_125648:
	cmp	%r10, 0
	bne	sumarm_125649
	nop
sumarm_125656:
	cmp	%r11, 0
	bne	sumarm_125657
	nop
code_126507:
	ba	after_sum_125653
	or	%r0, 1, %r8
sumarm_125657:
nomatch_sum_125654:
	or	%r0, 0, %r8
after_sum_125653:
	ba	after_sum_125645 ! delay slot empty
	nop
sumarm_125649:
	cmp	%r10, 1
	bne	sumarm_125665
	nop
sumarm_125672:
	cmp	%r11, 1
	bne	sumarm_125673
	nop
code_126511:
	ba	after_sum_125669
	or	%r0, 1, %r8
sumarm_125673:
nomatch_sum_125670:
	or	%r0, 0, %r8
after_sum_125669:
	ba	after_sum_125645 ! delay slot empty
	nop
sumarm_125665:
sumarm_125687:
	cmp	%r11, 2
	bne	sumarm_125688
	nop
code_126514:
	ba	after_sum_125684
	or	%r0, 1, %r8
sumarm_125688:
nomatch_sum_125685:
	or	%r0, 0, %r8
after_sum_125684:
	ba	after_sum_125645 ! delay slot empty
	nop
sumarm_125680:
after_sum_125645:
code_126518:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_vars_eq_0_code_123251,(.-POSIX_IO_Str_vars_eq_0_code_123251)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_vars_eq_0_code_123256
 ! arguments : [$123258,$8] [$123259,$9] [$121765,$10] [$121766,$11] 
 ! results    : [$125640,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
POSIX_IO_Str_vars_eq_0_code_123256:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126527
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126527:
	st	%r15, [%sp+92]
code_126520:
funtop_125613:
sumarm_125620:
sumarm_125628:
	ld	[%r10], %r9
	ld	[%r11], %r8
	cmp	%r9, %r8
	or	%r0, 1, %r8
	be	cmpui_126521
	nop
code_126522:
	or	%r0, 0, %r8
cmpui_126521:
	ba	after_sum_125625 ! delay slot empty
	nop
sumarm_125629:
after_sum_125625:
	ba	after_sum_125617 ! delay slot empty
	nop
sumarm_125621:
after_sum_125617:
code_126526:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_vars_eq_0_code_123256,(.-POSIX_IO_Str_vars_eq_0_code_123256)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_wordTo_code_123261
 ! arguments : [$123263,$8] [$123264,$9] [$114790,$10] 
 ! results    : [$125610,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
POSIX_IO_Str_wordTo_code_123261:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126534
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126534:
	st	%r15, [%sp+92]
code_126528:
funtop_125604:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126529
	nop
code_126530:
	call	GCFromML ! delay slot empty
	nop
needgc_126529:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
code_126533:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_wordTo_code_123261,(.-POSIX_IO_Str_wordTo_code_123261)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_126529
	.word 0x00180007
	.word 0x00170000
	.word 0xbffc3800
	.word 0xbffc3800
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_toWord_code_123266
 ! arguments : [$123268,$8] [$123269,$9] [$114793,$10] 
 ! results    : [$125603,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
POSIX_IO_Str_toWord_code_123266:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126539
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126539:
	st	%r15, [%sp+92]
code_126535:
funtop_125590:
sumarm_125597:
	ba	after_sum_125594
	ld	[%r10], %r8
sumarm_125598:
after_sum_125594:
code_126538:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_toWord_code_123266,(.-POSIX_IO_Str_toWord_code_123266)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_anonfun_code_123271
 ! arguments : [$123273,$8] [$123274,$9] [$121774,$10] [$121775,$11] 
 ! results    : [$125589,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_anonfun_code_123271:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126548
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126548:
	st	%r15, [%sp+92]
code_126540:
funtop_125566:
sumarm_125573:
	ld	[%r10], %r10
	! making closure call
	sethi	%hi(orb_118392), %r8
	or	%r8, %lo(orb_118392), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 96, %sp
code_126543:
	! done making tail call
	ba	after_sum_125570 ! delay slot empty
	nop
sumarm_125574:
after_sum_125570:
code_126546:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_anonfun_code_123271,(.-POSIX_IO_Str_anonfun_code_123271)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_flags_code_123276
 ! arguments : [$123278,$8] [$123279,$9] [$114798,$10] 
 ! results    : [$125563,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_flags_code_123276:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126572
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126572:
	st	%r15, [%sp+92]
	st	%r10, [%sp+96]
code_126549:
funtop_125516:
	sethi	%hi(anonfun_114806), %r8
	or	%r8, %lo(anonfun_114806), %r10
	! making closure call
	sethi	%hi(_118913), %r8
	or	%r8, %lo(_118913), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126571:
	mov	%r8, %r9
code_126553:
	! done making normal call
	or	%r0, 0, %r10
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126568:
	mov	%r8, %r12
code_126554:
	! done making normal call
	sethi	%hi(type_121841), %r8
	or	%r8, %lo(type_121841), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r18
	sethi	%hi(Word32_STR_c_INT), %r8
	or	%r8, %lo(Word32_STR_c_INT), %r9
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
code_126569:
	mov	%r8, %r9
code_126561:
	! done making normal call
	! making closure call
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+96], %r10
code_126570:
	mov	%r8, %r9
code_126562:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126563
	nop
code_126564:
	call	GCFromML ! delay slot empty
	nop
needgc_126563:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
code_126567:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_flags_code_123276,(.-POSIX_IO_Str_flags_code_123276)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126568
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000000
	.long reify_122532
		! -------- label,sizes,reg
	.long code_126569
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000000
	.long reify_122532
		! -------- label,sizes,reg
	.long code_126570
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_126563
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126571
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000000
	.long reify_122532
	.text
	.align 8
	.global POSIX_IO_Str_anySet_code_123281
 ! arguments : [$123283,$8] [$123284,$9] [$121846,$10] [$121847,$11] 
 ! results    : [$125515,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_anySet_code_123281:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126588
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126588:
	st	%r15, [%sp+92]
code_126573:
funtop_125461:
sumarm_125468:
	ld	[%r10], %r10
sumarm_125478:
	ld	[%r11], %r11
	! making closure call
	sethi	%hi(andb_118393), %r8
	or	%r8, %lo(andb_118393), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126587:
	mov	%r8, %r10
code_126576:
	! done making normal call
	or	%r0, 0, %r11
	! making closure call
	sethi	%hi(PLUSEword_118956), %r8
	or	%r8, %lo(PLUSEword_118956), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126586:
code_126579:
	! done making normal call
	cmp	%r8, 0
	bne,pn	%icc,one_case_125507
	nop
zero_case_125506:
	ba	after_zeroone_125508
	or	%r0, 1, %r8
one_case_125507:
	or	%r0, 0, %r8
after_zeroone_125508:
	ba	after_sum_125475 ! delay slot empty
	nop
sumarm_125479:
after_sum_125475:
	ba	after_sum_125465 ! delay slot empty
	nop
sumarm_125469:
after_sum_125465:
code_126585:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_anySet_code_123281,(.-POSIX_IO_Str_anySet_code_123281)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126586
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126587
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_allSet_code_123286
 ! arguments : [$123288,$8] [$123289,$9] [$121861,$10] [$121862,$11] 
 ! results    : [$125460,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_allSet_code_123286:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126602
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126602:
	st	%r15, [%sp+92]
code_126589:
funtop_125416:
sumarm_125423:
	ld	[%r10], %r16
	st	%r16, [%sp+96]
sumarm_125433:
	ld	[%r11], %r11
	! making closure call
	sethi	%hi(andb_118393), %r8
	or	%r8, %lo(andb_118393), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r10
code_126601:
	mov	%r8, %r10
code_126592:
	! done making normal call
	! making closure call
	sethi	%hi(PLUSEword_118956), %r8
	or	%r8, %lo(PLUSEword_118956), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+96], %r11
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_126595:
	! done making tail call
	ba	after_sum_125430 ! delay slot empty
	nop
sumarm_125434:
after_sum_125430:
	ba	after_sum_125420 ! delay slot empty
	nop
sumarm_125424:
after_sum_125420:
code_126599:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_allSet_code_123286,(.-POSIX_IO_Str_allSet_code_123286)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126601
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_dupfd_code_123291
 ! arguments : [$123293,$8] [$123294,$9] [$121898,$10] [$121899,$11] 
 ! results    : [$125415,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_dupfd_code_123291:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126618
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126618:
	st	%r15, [%sp+92]
	st	%r11, [%sp+100]
code_126603:
funtop_125386:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126617:
	st	%r8, [%sp+96]
code_126605:
	! done making normal call
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126614:
	mov	%r8, %r9
code_126607:
	! done making normal call
	! making external call
	call	save_regs_MLtoC
	ld	[%sp+96], %r8
	call	posix_io_fcntl_d ! delay slot empty
	nop
code_126615:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r10
code_126608:
	! done making external call
	! making closure call
	sethi	%hi(strbindvar_r_wordToFD_118448), %r8
	or	%r8, %lo(strbindvar_r_wordToFD_118448), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r11, %r0
	add	%sp, 112, %sp
code_126611:
	! done making tail call
code_126613:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_dupfd_code_123291,(.-POSIX_IO_Str_dupfd_code_123291)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126614
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126615
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126617
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000000
	.long type_118285
	.text
	.align 8
	.global POSIX_IO_Str_getfd_code_123296
 ! arguments : [$123298,$8] [$123299,$9] [$114884,$10] 
 ! results    : [$125383,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_getfd_code_123296:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126630
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126630:
	st	%r15, [%sp+92]
code_126619:
funtop_125368:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126629:
code_126621:
	! done making normal call
	! making external call
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	posix_io_fcntl_gfd ! delay slot empty
	nop
code_126628:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r9
code_126622:
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126623
	nop
code_126624:
	call	GCFromML ! delay slot empty
	nop
needgc_126623:
	! done making external call
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
code_126627:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_getfd_code_123296,(.-POSIX_IO_Str_getfd_code_123296)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126628
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_126623
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126629
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_setfd_code_123301
 ! arguments : [$123303,$8] [$123304,$9] [$121927,$10] [$121928,$11] 
 ! results    : [$125367,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_setfd_code_123301:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126640
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126640:
	st	%r15, [%sp+92]
code_126631:
funtop_125344:
sumarm_125351:
	ld	[%r11], %r16
	st	%r16, [%sp+96]
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126639:
code_126633:
	! done making normal call
	! making external call
	call	save_regs_MLtoC
	ld	[%sp+96], %r9
	call	posix_io_fcntl_sfd ! delay slot empty
	nop
code_126638:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126634:
	! done making external call
	ba	after_sum_125348 ! delay slot empty
	nop
sumarm_125352:
after_sum_125348:
code_126637:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_setfd_code_123301,(.-POSIX_IO_Str_setfd_code_123301)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126638
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126639
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_getfl_code_123306
 ! arguments : [$123308,$8] [$123309,$9] [$114910,$10] 
 ! results    : [$125341,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_getfl_code_123306:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126661
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126661:
	st	%r15, [%sp+92]
code_126641:
funtop_125304:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126660:
code_126643:
	! done making normal call
	! making external call
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	posix_io_fcntl_gfl ! delay slot empty
	nop
code_126657:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126644:
	! done making external call
	ld	[%r8], %r10
	ld	[%r8+4], %r16
	st	%r16, [%sp+100]
	! making closure call
	sethi	%hi(strbindvar_r_wordTo_119096), %r8
	or	%r8, %lo(strbindvar_r_wordTo_119096), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126658:
	st	%r8, [%sp+96]
code_126647:
	! done making normal call
	! making closure call
	sethi	%hi(omodeFromWord_119098), %r8
	or	%r8, %lo(omodeFromWord_119098), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126659:
	mov	%r8, %r9
code_126650:
	! done making normal call
	add	%r4, 12, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126651
	nop
code_126652:
	call	GCFromML ! delay slot empty
	nop
needgc_126651:
	! allocating 2-record
	sethi	%hi(gctag_119104), %r8
	ld	[%r8+%lo(gctag_119104)], %r8
	st	%r8, [%r4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+4]
	st	%r9, [%r4+8]
	add	%r4, 4, %r8
	add	%r4, 12, %r4
	! done allocating 2 record
code_126656:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_getfl_code_123306,(.-POSIX_IO_Str_getfl_code_123306)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126657
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126658
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126659
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000000
	.long type_118303
		! -------- label,sizes,reg
	.long needgc_126651
	.word 0x001c000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000200
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x80000000
	.long type_118303
	.word 0x80000000
	.long type_118280
		! -------- label,sizes,reg
	.long code_126660
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_setfl_code_123311
 ! arguments : [$123313,$8] [$123314,$9] [$121960,$10] [$121961,$11] 
 ! results    : [$125303,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_setfl_code_123311:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126674
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126674:
	st	%r15, [%sp+92]
	st	%r11, [%sp+100]
code_126662:
funtop_125282:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126673:
	st	%r8, [%sp+96]
code_126664:
	! done making normal call
	! making closure call
	sethi	%hi(_121979), %r8
	or	%r8, %lo(_121979), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126671:
	mov	%r8, %r9
code_126667:
	! done making normal call
	! making external call
	call	save_regs_MLtoC
	ld	[%sp+96], %r8
	call	posix_io_fcntl_sfl ! delay slot empty
	nop
code_126672:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126668:
	! done making external call
code_126670:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_setfl_code_123311,(.-POSIX_IO_Str_setfl_code_123311)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126671
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126672
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126673
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x80000000
	.long type_118303
	.text
	.align 8
	.global POSIX_IO_Str_vars_eq_0_code_123316
 ! arguments : [$123318,$8] [$123319,$9] [$121987,$10] [$121988,$11] 
 ! results    : [$125251,$8] 
 ! destroys   :  $11 $10 $9 $8
 ! modifies   :  $11 $10 $9 $8
POSIX_IO_Str_vars_eq_0_code_123316:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126689
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126689:
	st	%r15, [%sp+92]
code_126675:
funtop_125228:
sumarm_125235:
	cmp	%r10, 0
	bne	sumarm_125236
	nop
sumarm_125243:
	cmp	%r11, 0
	bne	sumarm_125244
	nop
code_126677:
	ba	after_sum_125240
	or	%r0, 1, %r8
sumarm_125244:
nomatch_sum_125241:
	or	%r0, 0, %r8
after_sum_125240:
	ba	after_sum_125232 ! delay slot empty
	nop
sumarm_125236:
	cmp	%r10, 1
	bne	sumarm_125252
	nop
sumarm_125259:
	cmp	%r11, 1
	bne	sumarm_125260
	nop
code_126681:
	ba	after_sum_125256
	or	%r0, 1, %r8
sumarm_125260:
nomatch_sum_125257:
	or	%r0, 0, %r8
after_sum_125256:
	ba	after_sum_125232 ! delay slot empty
	nop
sumarm_125252:
sumarm_125274:
	cmp	%r11, 2
	bne	sumarm_125275
	nop
code_126684:
	ba	after_sum_125271
	or	%r0, 1, %r8
sumarm_125275:
nomatch_sum_125272:
	or	%r0, 0, %r8
after_sum_125271:
	ba	after_sum_125232 ! delay slot empty
	nop
sumarm_125267:
after_sum_125232:
code_126688:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_vars_eq_0_code_123316,(.-POSIX_IO_Str_vars_eq_0_code_123316)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_flock_code_123321
 ! arguments : [$123323,$8] [$123324,$9] [$121992,$10] [$121993,$11] [$121994,$12] [$121995,$13] [$121996,$18] 
 ! results    : [$125223,$8] 
 ! destroys   :  $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_flock_code_123321:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126697
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126697:
	st	%r15, [%sp+92]
code_126690:
funtop_125214:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126691
	nop
code_126692:
	call	GCFromML ! delay slot empty
	nop
needgc_126691:
	! allocating 5-record
	sethi	%hi(record_gctag_121998), %r8
	ld	[%r8+%lo(record_gctag_121998)], %r8
	st	%r8, [%r4]
	st	%r10, [%r4+4]
	st	%r11, [%r4+8]
	st	%r12, [%r4+12]
	st	%r13, [%r4+16]
	st	%r18, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
code_126696:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_flock_code_123321,(.-POSIX_IO_Str_flock_code_123321)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_126691
	.word 0x0018000d
	.word 0x00170000
	.word 0xbff80000
	.word 0xbffc1400
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118320
	.word 0x40000000
	.long record_123506
	.text
	.align 8
	.global POSIX_IO_Str_ltype_code_123326
 ! arguments : [$123328,$8] [$123329,$9] [$115028,$10] 
 ! results    : [$125213,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
POSIX_IO_Str_ltype_code_123326:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126702
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126702:
	st	%r15, [%sp+92]
code_126698:
funtop_125199:
sumarm_125206:
	ba	after_sum_125203
	ld	[%r10], %r8
sumarm_125207:
after_sum_125203:
code_126701:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_ltype_code_123326,(.-POSIX_IO_Str_ltype_code_123326)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_whence_code_123331
 ! arguments : [$123333,$8] [$123334,$9] [$115034,$10] 
 ! results    : [$125198,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
POSIX_IO_Str_whence_code_123331:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126707
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126707:
	st	%r15, [%sp+92]
code_126703:
funtop_125184:
sumarm_125191:
	ba	after_sum_125188
	ld	[%r10+16], %r8
sumarm_125192:
after_sum_125188:
code_126706:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_whence_code_123331,(.-POSIX_IO_Str_whence_code_123331)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_start_code_123336
 ! arguments : [$123338,$8] [$123339,$9] [$115040,$10] 
 ! results    : [$125183,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
POSIX_IO_Str_start_code_123336:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126712
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126712:
	st	%r15, [%sp+92]
code_126708:
funtop_125169:
sumarm_125176:
	ba	after_sum_125173
	ld	[%r10+12], %r8
sumarm_125177:
after_sum_125173:
code_126711:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_start_code_123336,(.-POSIX_IO_Str_start_code_123336)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_len_code_123341
 ! arguments : [$123343,$8] [$123344,$9] [$115046,$10] 
 ! results    : [$125168,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
POSIX_IO_Str_len_code_123341:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126717
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126717:
	st	%r15, [%sp+92]
code_126713:
funtop_125154:
sumarm_125161:
	ba	after_sum_125158
	ld	[%r10+4], %r8
sumarm_125162:
after_sum_125158:
code_126716:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_len_code_123341,(.-POSIX_IO_Str_len_code_123341)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_pid_code_123346
 ! arguments : [$123348,$8] [$123349,$9] [$115052,$10] 
 ! results    : [$125153,$8] 
 ! destroys   :  $10 $9 $8
 ! modifies   :  $10 $9 $8
POSIX_IO_Str_pid_code_123346:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126722
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126722:
	st	%r15, [%sp+92]
code_126718:
funtop_125139:
sumarm_125146:
	ba	after_sum_125143
	ld	[%r10+8], %r8
sumarm_125147:
after_sum_125143:
code_126721:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_pid_code_123346,(.-POSIX_IO_Str_pid_code_123346)

	.section	".rodata"
	.text
	.align 8
	.global POSIX_IO_Str_flockToRep_code_123351
 ! arguments : [$123353,$8] [$123354,$9] [$115099,$10] 
 ! results    : [$125138,$8] 
 ! destroys   :  $12 $11 $10 $9 $8
 ! modifies   :  $12 $11 $10 $9 $8
POSIX_IO_Str_flockToRep_code_123351:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126746
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126746:
	st	%r15, [%sp+92]
code_126723:
funtop_125072:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126724
	nop
code_126725:
	call	GCFromML ! delay slot empty
	nop
needgc_126724:
sumarm_125079:
	ld	[%r10+4], %r9
	ld	[%r10+12], %r8
	ld	[%r10+16], %r11
	ld	[%r10], %r10
sumarm_125096:
	cmp	%r10, 0
	bne	sumarm_125097
	nop
code_126727:
	sethi	%hi(f_rdlck_119481), %r10
	ld	[%r10+%lo(f_rdlck_119481)], %r10
	ba	after_sum_125093
	mov	%r10, %r12
sumarm_125097:
	cmp	%r10, 1
	bne	sumarm_125102
	nop
code_126730:
	sethi	%hi(f_unlck_119485), %r10
	ld	[%r10+%lo(f_unlck_119485)], %r10
	ba	after_sum_125093
	mov	%r10, %r12
sumarm_125102:
	sethi	%hi(f_wrlck_119483), %r10
	ld	[%r10+%lo(f_wrlck_119483)], %r10
	ba	after_sum_125093
	mov	%r10, %r12
sumarm_125106:
after_sum_125093:
sumarm_125114:
	cmp	%r11, 0
	bne	sumarm_125115
	nop
code_126735:
	sethi	%hi(seek_cur_118794), %r10
	ld	[%r10+%lo(seek_cur_118794)], %r10
	ba	after_sum_125111
	mov	%r10, %r11
sumarm_125115:
	cmp	%r11, 1
	bne	sumarm_125120
	nop
code_126738:
	sethi	%hi(seek_end_118796), %r10
	ld	[%r10+%lo(seek_end_118796)], %r10
	ba	after_sum_125111
	mov	%r10, %r11
sumarm_125120:
	sethi	%hi(seek_set_118792), %r10
	ld	[%r10+%lo(seek_set_118792)], %r10
	ba	after_sum_125111
	mov	%r10, %r11
sumarm_125124:
after_sum_125111:
	! allocating 5-record
	or	%r0, 41, %r10
	st	%r10, [%r4]
	st	%r12, [%r4+4]
	st	%r11, [%r4+8]
	st	%r8, [%r4+12]
	st	%r9, [%r4+16]
	or	%r0, 0, %r8
	st	%r8, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
	ba	after_sum_125076 ! delay slot empty
	nop
sumarm_125080:
after_sum_125076:
code_126745:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_flockToRep_code_123351,(.-POSIX_IO_Str_flockToRep_code_123351)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_126724
	.word 0x00180009
	.word 0x00170000
	.word 0xbffc2000
	.word 0xbffc2400
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! worddata
	.word 0x40000000
	.long record_123525
	.text
	.align 8
	.global POSIX_IO_Str_flockFromRep_code_123356
 ! arguments : [$123358,$8] [$123359,$9] [$122021,$10] [$122022,$11] 
 ! results    : [$125067,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_flockFromRep_code_123356:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 128, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126869
	mov	%sp, %fp
	add	%sp, 128, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 128, %sp
code_126869:
	st	%r15, [%sp+92]
	st	%r10, [%sp+124]
code_126747:
funtop_124813:
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126748
	nop
code_126749:
	call	GCFromML ! delay slot empty
	nop
needgc_126748:
	ld	[%r11], %r10
	ld	[%r11+4], %r16
	st	%r16, [%sp+120]
	ld	[%r11+8], %r16
	st	%r16, [%sp+116]
	ld	[%r11+12], %r16
	st	%r16, [%sp+112]
	ld	[%r11+16], %r16
	st	%r16, [%sp+108]
	sethi	%hi(f_rdlck_119481), %r8
	ld	[%r8+%lo(f_rdlck_119481)], %r8
	cmp	%r10, %r8
	or	%r0, 1, %r9
	be	cmpui_126752
	nop
code_126753:
	or	%r0, 0, %r9
cmpui_126752:
	sethi	%hi(seek_set_118792), %r8
	ld	[%r8+%lo(seek_set_118792)], %r8
	ld	[%sp+120], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r16
	st	%r16, [%sp+104]
	be	cmpui_126755
	nop
code_126756:
	or	%r0, 0, %r16
	st	%r16, [%sp+104]
cmpui_126755:
	cmp	%r9, 0
	bne,pn	%icc,one_case_124836
	nop
zero_case_124835:
	sethi	%hi(f_wrlck_119483), %r8
	ld	[%r8+%lo(f_wrlck_119483)], %r8
	cmp	%r10, %r8
	or	%r0, 1, %r8
	be	cmpui_126759
	nop
code_126760:
	or	%r0, 0, %r8
cmpui_126759:
	cmp	%r8, 0
	bne,pn	%icc,one_case_124845
	nop
zero_case_124844:
	sethi	%hi(f_unlck_119485), %r8
	ld	[%r8+%lo(f_unlck_119485)], %r8
	cmp	%r10, %r8
	or	%r0, 1, %r8
	be	cmpui_126763
	nop
code_126764:
	or	%r0, 0, %r8
cmpui_126763:
	cmp	%r8, 0
	bne,pn	%icc,one_case_124854
	nop
zero_case_124853:
	! making closure call
	sethi	%hi(toString_119545), %r8
	or	%r8, %lo(toString_119545), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126867:
	mov	%r8, %r11
code_126768:
	! done making normal call
	sethi	%hi(string_124410), %r8
	or	%r8, %lo(string_124410), %r10
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126856:
	st	%r8, [%sp+96]
code_126772:
	! done making normal call
	sethi	%hi(string_123691), %r8
	or	%r8, %lo(string_123691), %r10
	sethi	%hi(string_124391), %r8
	or	%r8, %lo(string_124391), %r11
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126857:
	mov	%r8, %r10
code_126777:
	! done making normal call
	sethi	%hi(string_123694), %r8
	or	%r8, %lo(string_123694), %r11
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126858:
	mov	%r8, %r10
code_126781:
	! done making normal call
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r11
code_126859:
	mov	%r8, %r10
code_126784:
	! done making normal call
	! making closure call
	sethi	%hi(mk_118413), %r8
	or	%r8, %lo(mk_118413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126860:
code_126787:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126788
	nop
code_126789:
	call	GCFromML ! delay slot empty
	nop
needgc_126788:
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_zeroone_124855
	or	%r0, 0, %r8
one_case_124854:
	or	%r0, 1, %r8
after_zeroone_124855:
	ba	after_zeroone_124846 ! delay slot empty
	nop
one_case_124845:
	or	%r0, 2, %r8
after_zeroone_124846:
	ba	after_zeroone_124837
	st	%r8, [%sp+100]
one_case_124836:
	or	%r0, 0, %r8
	st	%r8, [%sp+100]
after_zeroone_124837:
	ld	[%sp+104], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_124931
	nop
zero_case_124930:
	sethi	%hi(seek_cur_118794), %r8
	ld	[%r8+%lo(seek_cur_118794)], %r8
	ld	[%sp+120], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	be	cmpui_126797
	nop
code_126798:
	or	%r0, 0, %r8
cmpui_126797:
	cmp	%r8, 0
	bne,pn	%icc,one_case_124940
	nop
zero_case_124939:
	sethi	%hi(seek_end_118796), %r8
	ld	[%r8+%lo(seek_end_118796)], %r8
	ld	[%sp+120], %r17
	cmp	%r17, %r8
	or	%r0, 1, %r8
	be	cmpui_126801
	nop
code_126802:
	or	%r0, 0, %r8
cmpui_126801:
	cmp	%r8, 0
	bne,pn	%icc,one_case_124949
	nop
zero_case_124948:
	! making closure call
	sethi	%hi(toString_119545), %r8
	or	%r8, %lo(toString_119545), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+120], %r10
code_126868:
	mov	%r8, %r11
code_126806:
	! done making normal call
	sethi	%hi(string_124446), %r8
	or	%r8, %lo(string_124446), %r10
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126861:
	st	%r8, [%sp+96]
code_126810:
	! done making normal call
	sethi	%hi(string_123691), %r8
	or	%r8, %lo(string_123691), %r10
	sethi	%hi(string_124430), %r8
	or	%r8, %lo(string_124430), %r11
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126862:
	mov	%r8, %r10
code_126815:
	! done making normal call
	sethi	%hi(string_123694), %r8
	or	%r8, %lo(string_123694), %r11
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	jmpl	%r12, %r15
	ld	[%r9+8], %r9
code_126863:
	mov	%r8, %r10
code_126819:
	! done making normal call
	! making closure call
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r12, %r15
	ld	[%sp+96], %r11
code_126864:
	mov	%r8, %r10
code_126822:
	! done making normal call
	! making closure call
	sethi	%hi(mk_118413), %r8
	or	%r8, %lo(mk_118413), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126865:
code_126825:
	! done making normal call
	add	%r4, 24, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126826
	nop
code_126827:
	call	GCFromML ! delay slot empty
	nop
needgc_126826:
	mov	%r8, %r15
	ld	[%r1], %r17
	ld	[%r1+4], %sp
	ld	[%r2+808], %r8
	add	%sp, %r8, %sp
	mov	%r17, %r16
	jmpl	%r17, %r0 ! delay slot empty
	nop
	ba	after_zeroone_124950
	or	%r0, 0, %r8
one_case_124949:
	or	%r0, 1, %r8
after_zeroone_124950:
	ba	after_zeroone_124941 ! delay slot empty
	nop
one_case_124940:
	or	%r0, 0, %r8
after_zeroone_124941:
	ba	after_zeroone_124932
	st	%r8, [%sp+96]
one_case_124931:
	or	%r0, 2, %r8
	st	%r8, [%sp+96]
after_zeroone_124932:
	ld	[%sp+124], %r17
	cmp	%r17, 0
	bne,pn	%icc,one_case_125026
	nop
zero_case_125025:
	or	%r0, 0, %r8
	ba	after_zeroone_125027
	mov	%r8, %r9
one_case_125026:
	! making closure call
	sethi	%hi(wordToPid_119588), %r8
	or	%r8, %lo(wordToPid_119588), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+108], %r10
code_126866:
code_126837:
	! done making normal call
	add	%r4, 32, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_126838
	nop
code_126839:
	call	GCFromML ! delay slot empty
	nop
needgc_126838:
	sethi	%hi(type_119585), %r9
	or	%r9, %lo(type_119585), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	ld	[%r9+16], %r9
	cmp	%r9, 4
	ble,pn	%icc,dynamic_box_125051
	nop
code_126843:
	cmp	%r9, 255
	ble,pn	%icc,dynamic_nobox_125052
	nop
code_126844:
	ld	[%r9], %r9
	cmp	%r9, 12
	be,pn	%icc,dynamic_box_125051
	nop
code_126845:
	cmp	%r9, 4
	be,pn	%icc,dynamic_box_125051
	nop
code_126846:
	cmp	%r9, 8
	be,pn	%icc,dynamic_box_125051
	nop
dynamic_nobox_125052:
	ba	xinject_sum_dyn_after_125045 ! delay slot empty
	nop
dynamic_box_125051:
	or	%r0, 9, %r11
	sethi	%hi(type_118286), %r9
	or	%r9, %lo(type_118286), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_126851
	nop
code_126852:
	or	%r0, 0, %r9
cmpui_126851:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r11, %r11
	! allocating 1-record
	st	%r11, [%r4]
	st	%r8, [%r4+4]
	add	%r4, 4, %r8
	add	%r4, 8, %r4
	! done allocating 1 record
xinject_sum_dyn_after_125045:
	mov	%r8, %r9
after_zeroone_125027:
	! allocating 5-record
	sethi	%hi(gctag_119607), %r8
	ld	[%r8+%lo(gctag_119607)], %r8
	st	%r8, [%r4]
	ld	[%sp+100], %r17
	st	%r17, [%r4+4]
	ld	[%sp+112], %r17
	st	%r17, [%r4+8]
	st	%r9, [%r4+12]
	ld	[%sp+116], %r17
	st	%r17, [%r4+16]
	ld	[%sp+96], %r17
	st	%r17, [%r4+20]
	add	%r4, 4, %r8
	add	%r4, 24, %r4
	! done allocating 5 record
code_126855:
	ld	[%sp+92], %r15
	retl
	add	%sp, 128, %sp
	.size POSIX_IO_Str_flockFromRep_code_123356,(.-POSIX_IO_Str_flockFromRep_code_123356)

	.section	".rodata"
		! -------- label,sizes,reg
	.long needgc_126748
	.word 0x00200009
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0000000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126856
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0000000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126857
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0010000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126858
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0010000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126859
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0000000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126860
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0000000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long needgc_126788
	.word 0x00200009
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0000000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126861
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc00c0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126862
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc00d0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126863
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc00d0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126864
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc00c0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126865
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc00c0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long needgc_126826
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc00c0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long needgc_126838
	.word 0x0020000d
	.word 0x00170000
	.word 0x00000000
	.word 0x00000100
		! stacktrace
	.word 0x00000000
	.word 0x000f0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118286
		! -------- label,sizes,reg
	.long code_126866
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000f0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x40000000
	.long record_123506
		! -------- label,sizes,reg
	.long code_126867
	.word 0x00200009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc0000000
		! worddata
	.word 0x80000000
	.long type_118503
		! -------- label,sizes,reg
	.long code_126868
	.word 0x0020000b
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0xc00c0000
		! worddata
	.word 0x40000000
	.long record_123506
	.word 0x80000000
	.long type_118503
	.text
	.align 8
	.global POSIX_IO_Str_getlk_code_123361
 ! arguments : [$123363,$8] [$123364,$9] [$122053,$10] [$122054,$11] 
 ! results    : [$124812,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_getlk_code_123361:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126885
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126885:
	st	%r15, [%sp+92]
	st	%r11, [%sp+100]
code_126870:
funtop_124782:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126884:
	st	%r8, [%sp+96]
code_126872:
	! done making normal call
	! making closure call
	sethi	%hi(flockToRep_115098), %r8
	or	%r8, %lo(flockToRep_115098), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126881:
	mov	%r8, %r10
code_126874:
	! done making normal call
	! making external call
	sethi	%hi(f_getlk_119475), %r8
	ld	[%r8+%lo(f_getlk_119475)], %r9
	call	save_regs_MLtoC
	ld	[%sp+96], %r8
	call	posix_io_fcntl_l ! delay slot empty
	nop
code_126882:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_126876:
	! done making external call
	or	%r0, 1, %r10
	! making closure call
	sethi	%hi(flockFromRep_115128), %r8
	or	%r8, %lo(flockFromRep_115128), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_126878:
	! done making tail call
code_126880:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_getlk_code_123361,(.-POSIX_IO_Str_getlk_code_123361)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126881
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126882
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126884
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x40000000
	.long record_123525
	.text
	.align 8
	.global POSIX_IO_Str_setlk_code_123366
 ! arguments : [$123368,$8] [$123369,$9] [$122075,$10] [$122076,$11] 
 ! results    : [$124781,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_setlk_code_123366:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126901
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126901:
	st	%r15, [%sp+92]
	st	%r11, [%sp+100]
code_126886:
funtop_124751:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126900:
	st	%r8, [%sp+96]
code_126888:
	! done making normal call
	! making closure call
	sethi	%hi(flockToRep_115098), %r8
	or	%r8, %lo(flockToRep_115098), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126897:
	mov	%r8, %r10
code_126890:
	! done making normal call
	! making external call
	sethi	%hi(f_setlk_119477), %r8
	ld	[%r8+%lo(f_setlk_119477)], %r9
	call	save_regs_MLtoC
	ld	[%sp+96], %r8
	call	posix_io_fcntl_l ! delay slot empty
	nop
code_126898:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_126892:
	! done making external call
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(flockFromRep_115128), %r8
	or	%r8, %lo(flockFromRep_115128), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_126894:
	! done making tail call
code_126896:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_setlk_code_123366,(.-POSIX_IO_Str_setlk_code_123366)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126897
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126898
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126900
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x40000000
	.long record_123525
	.text
	.align 8
	.global POSIX_IO_Str_setlkw_code_123371
 ! arguments : [$123373,$8] [$123374,$9] [$122097,$10] [$122098,$11] 
 ! results    : [$124750,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_setlkw_code_123371:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126917
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126917:
	st	%r15, [%sp+92]
	st	%r11, [%sp+100]
code_126902:
funtop_124720:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126916:
	st	%r8, [%sp+96]
code_126904:
	! done making normal call
	! making closure call
	sethi	%hi(flockToRep_115098), %r8
	or	%r8, %lo(flockToRep_115098), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	jmpl	%r11, %r15
	ld	[%sp+100], %r10
code_126913:
	mov	%r8, %r10
code_126906:
	! done making normal call
	! making external call
	sethi	%hi(f_setlkw_119479), %r8
	ld	[%r8+%lo(f_setlkw_119479)], %r9
	call	save_regs_MLtoC
	ld	[%sp+96], %r8
	call	posix_io_fcntl_l ! delay slot empty
	nop
code_126914:
	call	load_regs_MLtoC ! delay slot empty
	nop
	mov	%r8, %r11
code_126908:
	! done making external call
	or	%r0, 0, %r10
	! making closure call
	sethi	%hi(flockFromRep_115128), %r8
	or	%r8, %lo(flockFromRep_115128), %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r9
	ld	[%sp+92], %r15
	jmpl	%r12, %r0
	add	%sp, 112, %sp
code_126910:
	! done making tail call
code_126912:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_setlkw_code_123371,(.-POSIX_IO_Str_setlkw_code_123371)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126913
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126914
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126916
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x000c0000
		! worddata
	.word 0x40000000
	.long record_123525
	.text
	.align 8
	.global POSIX_IO_Str_lseek_code_123376
 ! arguments : [$123378,$8] [$123379,$9] [$122119,$10] [$122120,$11] [$122121,$12] 
 ! results    : [$124719,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_lseek_code_123376:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 112, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126934
	mov	%sp, %fp
	add	%sp, 112, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 112, %sp
code_126934:
	st	%r15, [%sp+92]
	st	%r11, [%sp+100]
	st	%r12, [%sp+96]
code_126918:
funtop_124688:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126933:
	mov	%r8, %r9
code_126920:
	! done making normal call
sumarm_124702:
	ld	[%sp+96], %r17
	cmp	%r17, 0
	bne	sumarm_124703
	nop
code_126921:
	sethi	%hi(seek_cur_118794), %r8
	ld	[%r8+%lo(seek_cur_118794)], %r8
	ba	after_sum_124699
	mov	%r8, %r10
sumarm_124703:
	ld	[%sp+96], %r17
	cmp	%r17, 1
	bne	sumarm_124708
	nop
code_126924:
	sethi	%hi(seek_end_118796), %r8
	ld	[%r8+%lo(seek_end_118796)], %r8
	ba	after_sum_124699
	mov	%r8, %r10
sumarm_124708:
	sethi	%hi(seek_set_118792), %r8
	ld	[%r8+%lo(seek_set_118792)], %r8
	ba	after_sum_124699
	mov	%r8, %r10
sumarm_124712:
after_sum_124699:
	! making external call
	mov	%r9, %r8
	call	save_regs_MLtoC
	ld	[%sp+100], %r9
	call	posix_io_lseek ! delay slot empty
	nop
code_126932:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126929:
	! done making external call
code_126931:
	ld	[%sp+92], %r15
	retl
	add	%sp, 112, %sp
	.size POSIX_IO_Str_lseek_code_123376,(.-POSIX_IO_Str_lseek_code_123376)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126932
	.word 0x001c0007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126933
	.word 0x001c0009
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00030000
		! worddata
	.word 0x40000000
	.long record_123506
	.text
	.align 8
	.global POSIX_IO_Str_fsync_code_123381
 ! arguments : [$123383,$8] [$123384,$9] [$115224,$10] 
 ! results    : [$124687,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_fsync_code_123381:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 96, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_126943
	mov	%sp, %fp
	add	%sp, 96, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 96, %sp
code_126943:
	st	%r15, [%sp+92]
code_126935:
funtop_124676:
	! making closure call
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_126942:
code_126937:
	! done making normal call
	! making external call
	call	save_regs_MLtoC ! delay slot empty
	nop
	call	posix_io_fsync ! delay slot empty
	nop
code_126941:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126938:
	! done making external call
code_126940:
	ld	[%sp+92], %r15
	retl
	add	%sp, 96, %sp
	.size POSIX_IO_Str_fsync_code_123381,(.-POSIX_IO_Str_fsync_code_123381)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_126941
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_126942
	.word 0x00180007
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.text
	.align 8
	.global POSIX_IO_Str_main
 ! arguments : 
 ! results    : [$124675,$8] 
 ! destroys   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
 ! modifies   :  $f58 $f56 $f54 $f52 $f50 $f48 $f46 $f44 $f42 $f40 $f38 $f36 $f34 $f32 $f30 $f28 $f26 $f24 $f22 $f20 $f18 $f16 $f14 $f12 $f10 $f8 $f6 $f4 $f2 $f0 $31 $29 $28 $27 $26 $25 $24 $23 $22 $21 $20 $19 $18 $13 $12 $11 $10 $9 $8
POSIX_IO_Str_main:
	.proc   07
	mov	%sp, %fp
	sub	%sp, 160, %sp
	ld	[%r2+800], %r16
	cmp	%sp, %r16
	bg	code_127690
	mov	%sp, %fp
	add	%sp, 160, %sp
	or	%r0, 0, %r16
	mov	%r15, %r17
	call	NewStackletFromML ! delay slot empty
	nop
	sub	%sp, 160, %sp
code_127690:
	st	%r15, [%sp+92]
code_126944:
funtop_123459:
	call	save_regs_MLtoC
	or	%r0, 0, %r8
	call	AssertMirrorPtrArray ! delay slot empty
	nop
code_127686:
	call	load_regs_MLtoC ! delay slot empty
	nop
code_126945:
	sethi	%hi(PrePosix_STR_c_INT), %r8
	or	%r8, %lo(PrePosix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label open_mode_TYC
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_118280+-4), %r8
	st	%r9, [%r8+%lo(type_118280+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 48, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_126952
	nop
code_126953:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_126952:
	sethi	%hi(type_118280), %r8
	or	%r8, %lo(type_118280), %r12
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
	sethi	%hi(PrePosix_STR_c_INT), %r8
	or	%r8, %lo(PrePosix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label open_mode_sumarg_INT
	ld	[%r8+12], %r16
	st	%r16, [%sp+104]
	sethi	%hi(PrePosix_STR_c_INT), %r8
	or	%r8, %lo(PrePosix_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label open_mode_sum_INT
	ld	[%r8+16], %r16
	st	%r16, [%sp+100]
	sethi	%hi(POSIX_FileSys_STR_c_INT), %r8
	or	%r8, %lo(POSIX_FileSys_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label file_desc_TYC
	ld	[%r8+8], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_118285+-4), %r8
	st	%r9, [%r8+%lo(type_118285+-4)]
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r12
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
	sethi	%hi(POSIX_Process_STR_c_INT), %r8
	or	%r8, %lo(POSIX_Process_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label pid_TYC
	ld	[%r8+4], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_118286+-4), %r8
	st	%r9, [%r8+%lo(type_118286+-4)]
	sethi	%hi(type_118286), %r8
	or	%r8, %lo(type_118286), %r12
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
	! allocating 5-record
	! done allocating 5 record
	! allocating 1-record
	! done allocating 1 record
	sethi	%hi(POSIX_FileSys_STR_c_INT), %r8
	or	%r8, %lo(POSIX_FileSys_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label O_STR
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(type_118303+-4), %r8
	st	%r9, [%r8+%lo(type_118303+-4)]
	sethi	%hi(type_118303), %r8
	or	%r8, %lo(type_118303), %r12
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
	sethi	%hi(PLUSO_option_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label option_TYC
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! start making constructor call
	sethi	%hi(type_118286), %r8
	or	%r8, %lo(type_118286), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_127662:
code_127008:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_118320+-4), %r9
	st	%r10, [%r9+%lo(type_118320+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127013
	nop
code_127014:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127013:
	sethi	%hi(type_118320), %r9
	or	%r9, %lo(type_118320), %r13
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
	! allocating 14-record
	add	%r4, 60, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_127025
	nop
code_127026:
	call	GCFromML ! delay slot empty
	nop
needgc_127025:
	sethi	%hi(4194161), %r8
	or	%r8, %lo(4194161), %r8
	st	%r8, [%r4]
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+4]
	sethi	%hi(type_118286), %r8
	or	%r8, %lo(type_118286), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(record_123506), %r8
	or	%r8, %lo(record_123506), %r8
	st	%r8, [%r4+12]
	or	%r0, 256, %r8
	st	%r8, [%r4+16]
	sethi	%hi(record_123511), %r8
	or	%r8, %lo(record_123511), %r8
	st	%r8, [%r4+20]
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r8
	st	%r8, [%r4+24]
	sethi	%hi(type_118303), %r8
	or	%r8, %lo(type_118303), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+28]
	sethi	%hi(type_118280), %r8
	or	%r8, %lo(type_118280), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+32]
	ld	[%sp+104], %r17
	st	%r17, [%r4+36]
	ld	[%sp+100], %r17
	st	%r17, [%r4+40]
	sethi	%hi(record_123506), %r8
	or	%r8, %lo(record_123506), %r8
	st	%r8, [%r4+44]
	or	%r0, 256, %r8
	st	%r8, [%r4+48]
	sethi	%hi(record_123511), %r8
	or	%r8, %lo(record_123511), %r8
	st	%r8, [%r4+52]
	sethi	%hi(record_123525), %r8
	or	%r8, %lo(record_123525), %r8
	st	%r8, [%r4+56]
	add	%r4, 4, %r8
	add	%r4, 60, %r4
	! done allocating 14 record
	or	%r0, 111, %r10
	sethi	%hi(POSIX_IO_STR_c_INT+-4), %r9
	st	%r10, [%r9+%lo(POSIX_IO_STR_c_INT+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 48, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127046
	nop
code_127047:
	sub	%r4, 48, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127046:
	sethi	%hi(POSIX_IO_STR_c_INT), %r9
	or	%r9, %lo(POSIX_IO_STR_c_INT), %r13
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
	sethi	%hi(String_STR_r_INT), %r8
	or	%r8, %lo(String_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r13
	or	%r0, 111, %r9
	sethi	%hi(HAT_118386+-4), %r8
	st	%r9, [%r8+%lo(HAT_118386+-4)]
	sethi	%hi(HAT_118386), %r8
	or	%r8, %lo(HAT_118386), %r12
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
	sethi	%hi(PrePosix_STR_r_INT), %r8
	or	%r8, %lo(PrePosix_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r16
	st	%r16, [%sp+148]
	sethi	%hi(PrePosix_STR_r_INT), %r8
	or	%r8, %lo(PrePosix_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+28], %r16
	st	%r16, [%sp+144]
	sethi	%hi(PrePosix_STR_r_INT), %r8
	or	%r8, %lo(PrePosix_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+32], %r16
	st	%r16, [%sp+140]
	sethi	%hi(SysWord_STR_r_INT), %r8
	or	%r8, %lo(SysWord_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+44], %r13
	or	%r0, 111, %r9
	sethi	%hi(orb_118392+-4), %r8
	st	%r9, [%r8+%lo(orb_118392+-4)]
	sethi	%hi(orb_118392), %r8
	or	%r8, %lo(orb_118392), %r12
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
	sethi	%hi(SysWord_STR_r_INT), %r8
	or	%r8, %lo(SysWord_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+52], %r13
	or	%r0, 111, %r9
	sethi	%hi(andb_118393+-4), %r8
	st	%r9, [%r8+%lo(andb_118393+-4)]
	sethi	%hi(andb_118393), %r8
	or	%r8, %lo(andb_118393), %r12
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
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r8
	! done allocating 1 closures
	! allocating 3-record
	add	%r4, 16, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_127101
	nop
code_127102:
	call	GCFromML ! delay slot empty
	nop
needgc_127101:
	or	%r0, 1817, %r8
	st	%r8, [%r4]
	or	%r0, 2, %r8
	st	%r8, [%r4+4]
	sethi	%hi(Word32_STR_c_INT), %r8
	or	%r8, %lo(Word32_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	sethi	%hi(string_TYC), %r8
	or	%r8, %lo(string_TYC), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	add	%r4, 4, %r13
	add	%r4, 16, %r4
	! done allocating 3 record
	or	%r0, 256, %r11
	! making closure call
	sethi	%hi(o_r_INT), %r8
	or	%r8, %lo(o_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%r9], %r12
	ld	[%r9+4], %r8
	ld	[%r9+8], %r10
	jmpl	%r12, %r15
	mov	%r13, %r9
code_127687:
	mov	%r8, %r13
code_127110:
	! done making normal call
	sethi	%hi(SysWord_STR_r_INT), %r8
	or	%r8, %lo(SysWord_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+40], %r10
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r11
	! making closure call
	ld	[%r13], %r12
	ld	[%r13+4], %r8
	jmpl	%r12, %r15
	ld	[%r13+8], %r9
code_127673:
	st	%r8, [%sp+100]
code_127114:
	! done making normal call
	sethi	%hi(TiltExn_STR_r_INT), %r8
	or	%r8, %lo(TiltExn_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r8
	ld	[%r8+4], %r8
	or	%r0, 111, %r10
	sethi	%hi(mk_118413+-4), %r9
	st	%r10, [%r9+%lo(mk_118413+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127121
	nop
code_127122:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127121:
	sethi	%hi(mk_118413), %r9
	or	%r9, %lo(mk_118413), %r13
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
	sethi	%hi(POSIX_FileSys_STR_r_INT), %r8
	or	%r8, %lo(POSIX_FileSys_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r16
	st	%r16, [%sp+136]
	sethi	%hi(POSIX_Process_STR_r_INT), %r8
	or	%r8, %lo(POSIX_Process_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r16
	st	%r16, [%sp+132]
	sethi	%hi(POSIX_FileSys_STR_r_INT), %r8
	or	%r8, %lo(POSIX_FileSys_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r12
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(SysWord_STR_c_INT), %r8
	or	%r8, %lo(SysWord_STR_c_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
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
code_127663:
code_127145:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_121536+-4), %r9
	st	%r10, [%r9+%lo(_121536+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 24, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127150
	nop
code_127151:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127150:
	sethi	%hi(_121536), %r9
	or	%r9, %lo(_121536), %r13
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
	sethi	%hi(fs_intof_121528), %r8
	or	%r8, %lo(fs_intof_121528), %r8
	! done allocating 1 closures
	sethi	%hi(POSIX_FileSys_STR_r_INT), %r8
	or	%r8, %lo(POSIX_FileSys_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+16], %r8
	or	%r0, 111, %r10
	sethi	%hi(strbindvar_r_wordToFD_118448+-4), %r9
	st	%r10, [%r9+%lo(strbindvar_r_wordToFD_118448+-4)]
	sethi	%hi(strbindvar_r_wordToFD_118448), %r9
	or	%r9, %lo(strbindvar_r_wordToFD_118448), %r13
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
	or	%r0, 17, %r8
	sethi	%hi(type_118285), %r9
	or	%r9, %lo(type_118285), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127177
	nop
code_127178:
	or	%r0, 0, %r9
cmpui_127177:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_118285), %r9
	or	%r9, %lo(type_118285), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127181
	nop
code_127182:
	or	%r0, 0, %r9
cmpui_127181:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_118464), %r9
	st	%r8, [%r9+%lo(gctag_118464)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(pipe_114504), %r8
	or	%r8, %lo(pipe_114504), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(dup_121545), %r8
	or	%r8, %lo(dup_121545), %r8
	! done allocating 1 closures
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	sethi	%hi(dup_121545), %r8
	or	%r8, %lo(dup_121545), %r12
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
code_127669:
	st	%r8, [%sp+128]
code_127193:
	! done making normal call
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(dup2_114516), %r8
	or	%r8, %lo(dup2_114516), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(close_121570), %r8
	or	%r8, %lo(close_121570), %r8
	! done allocating 1 closures
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_123802), %r8
	or	%r8, %lo(record_123802), %r10
	sethi	%hi(close_121570), %r8
	or	%r8, %lo(close_121570), %r12
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
code_127674:
	st	%r8, [%sp+124]
code_127202:
	! done making normal call
	! start making constructor call
	or	%r0, 2, %r9
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_127675:
code_127203:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(reify_122481+-4), %r9
	st	%r10, [%r9+%lo(reify_122481+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 60, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127208
	nop
code_127209:
	sub	%r4, 60, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127208:
	sethi	%hi(reify_122481), %r9
	or	%r9, %lo(reify_122481), %r13
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
	sethi	%hi(PLUSNoption_out_118496), %r8
	st	%r9, [%r8+%lo(PLUSNoption_out_118496)]
	sethi	%hi(Word8Array_STR_r_INT), %r8
	or	%r8, %lo(Word8Array_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+24], %r8
	or	%r0, 111, %r10
	sethi	%hi(length_118500+-4), %r9
	st	%r10, [%r9+%lo(length_118500+-4)]
	sethi	%hi(length_118500), %r9
	or	%r9, %lo(length_118500), %r13
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
	sethi	%hi(PLUSO_bool_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label bool_TYC
	ld	[%r8], %r8
	or	%r0, 111, %r10
	sethi	%hi(type_118503+-4), %r9
	st	%r10, [%r9+%lo(type_118503+-4)]
	sethi	%hi(type_118503), %r9
	or	%r9, %lo(type_118503), %r13
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
	sethi	%hi(PLUSNbool_out_118504), %r8
	st	%r9, [%r8+%lo(PLUSNbool_out_118504)]
	sethi	%hi(PLUSO_bool_INT_r_INT), %r8
	or	%r8, %lo(PLUSO_bool_INT_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	sethi	%hi(Subscript_r_INT), %r8
	or	%r8, %lo(Subscript_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+4], %r8
	or	%r0, 111, %r10
	sethi	%hi(mk_118514+-4), %r9
	st	%r10, [%r9+%lo(mk_118514+-4)]
	sethi	%hi(mk_118514), %r9
	or	%r9, %lo(mk_118514), %r13
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
	sethi	%hi(PLUSO_option_INT_c_INT), %r8
	or	%r8, %lo(PLUSO_option_INT_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label option_sum_INT
	ld	[%r8+8], %r16
	st	%r16, [%sp+120]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(readArr_114536), %r8
	or	%r8, %lo(readArr_114536), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(readVec_114578), %r8
	or	%r8, %lo(readVec_114578), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(writeArr_114613), %r8
	or	%r8, %lo(writeArr_114613), %r8
	! done allocating 1 closures
	sethi	%hi(Word8Vector_STR_r_INT), %r8
	or	%r8, %lo(Word8Vector_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+12], %r8
	or	%r0, 111, %r10
	sethi	%hi(length_118679+-4), %r9
	st	%r10, [%r9+%lo(length_118679+-4)]
	sethi	%hi(length_118679), %r9
	or	%r9, %lo(length_118679), %r13
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
	sethi	%hi(writeVec_114655), %r8
	or	%r8, %lo(writeVec_114655), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(vars_eq_0_114706), %r8
	or	%r8, %lo(vars_eq_0_114706), %r8
	! done allocating 1 closures
	sethi	%hi(string_123910), %r8
	or	%r8, %lo(string_123910), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127664:
	mov	%r8, %r9
code_127285:
	! done making normal call
	sethi	%hi(seek_set_118792), %r8
	st	%r9, [%r8+%lo(seek_set_118792)]
	sethi	%hi(string_123928), %r8
	or	%r8, %lo(string_123928), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127676:
	mov	%r8, %r9
code_127289:
	! done making normal call
	sethi	%hi(seek_cur_118794), %r8
	st	%r9, [%r8+%lo(seek_cur_118794)]
	sethi	%hi(string_123946), %r8
	or	%r8, %lo(string_123946), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127677:
	mov	%r8, %r9
code_127293:
	! done making normal call
	sethi	%hi(seek_end_118796), %r8
	st	%r9, [%r8+%lo(seek_end_118796)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(vars_eq_0_114768), %r8
	or	%r8, %lo(vars_eq_0_114768), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(wordTo_114789), %r8
	or	%r8, %lo(wordTo_114789), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(toWord_114792), %r8
	or	%r8, %lo(toWord_114792), %r8
	! done allocating 1 closures
	! start making constructor call
	sethi	%hi(List_STR_c_INT), %r8
	or	%r8, %lo(List_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label list_TYC
	ld	[%r8], %r11
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r9
	ld	[%r11], %r10
	jmpl	%r10, %r15
	ld	[%r11+4], %r8
code_127678:
code_127301:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(reify_122532+-4), %r9
	st	%r10, [%r9+%lo(reify_122532+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127306
	nop
code_127307:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127306:
	sethi	%hi(reify_122532), %r9
	or	%r9, %lo(reify_122532), %r13
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
	ble	needgc_127318
	nop
code_127319:
	call	GCFromML ! delay slot empty
	nop
needgc_127318:
	or	%r0, 785, %r8
	st	%r8, [%r4]
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r8
	st	%r8, [%r4+4]
	sethi	%hi(Word32_STR_c_INT), %r8
	or	%r8, %lo(Word32_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+8]
	add	%r4, 4, %r9
	add	%r4, 12, %r4
	! done allocating 2 record
	sethi	%hi(List_STR_r_INT), %r8
	or	%r8, %lo(List_STR_r_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+4], %r10
	or	%r0, 256, %r11
	! making closure call
	ld	[%r10], %r12
	ld	[%r10+4], %r8
	jmpl	%r12, %r15
	ld	[%r10+8], %r10
code_127688:
code_127326:
	! done making normal call
	or	%r0, 111, %r10
	sethi	%hi(_118913+-4), %r9
	st	%r10, [%r9+%lo(_118913+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127331
	nop
code_127332:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127331:
	sethi	%hi(_118913), %r9
	or	%r9, %lo(_118913), %r13
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
	sethi	%hi(anonfun_114806), %r8
	or	%r8, %lo(anonfun_114806), %r8
	! done allocating 1 closures
	sethi	%hi(List_STR_c_INT), %r8
	or	%r8, %lo(List_STR_c_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	! Proj_c at label list_TYC
	ld	[%r8], %r16
	st	%r16, [%sp+96]
	! start making constructor call
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r9
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_127665:
code_127347:
	! done making constructor call
	or	%r0, 111, %r10
	sethi	%hi(type_121841+-4), %r9
	st	%r10, [%r9+%lo(type_121841+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127352
	nop
code_127353:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127352:
	sethi	%hi(type_121841), %r9
	or	%r9, %lo(type_121841), %r13
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
	sethi	%hi(flags_121771), %r8
	or	%r8, %lo(flags_121771), %r8
	! done allocating 1 closures
	! start making constructor call
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r9
	ld	[%sp+96], %r17
	ld	[%r17], %r10
	ld	[%sp+96], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_127666:
	mov	%r8, %r9
code_127366:
	! done making constructor call
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r10
	sethi	%hi(flags_121771), %r8
	or	%r8, %lo(flags_121771), %r12
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
code_127679:
	st	%r8, [%sp+116]
code_127371:
	! done making normal call
	sethi	%hi(Word32_STR_r_INT), %r8
	or	%r8, %lo(Word32_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8], %r8
	or	%r0, 111, %r10
	sethi	%hi(PLUSEword_118956+-4), %r9
	st	%r10, [%r9+%lo(PLUSEword_118956+-4)]
	ld	[%r2+792], %r9
	ld	[%r2+796], %r10
	add	%r9, 12, %r9
	cmp	%r9, %r10
	ble	afterMutateCheck_127378
	nop
code_127379:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127378:
	sethi	%hi(PLUSEword_118956), %r9
	or	%r9, %lo(PLUSEword_118956), %r13
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
	sethi	%hi(anySet_114817), %r8
	or	%r8, %lo(anySet_114817), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(allSet_114834), %r8
	or	%r8, %lo(allSet_114834), %r8
	! done allocating 1 closures
	sethi	%hi(string_124085), %r8
	or	%r8, %lo(string_124085), %r10
	! making closure call
	ld	[%sp+100], %r17
	ld	[%r17], %r11
	ld	[%sp+100], %r17
	ld	[%r17+4], %r8
	ld	[%sp+100], %r17
	jmpl	%r11, %r15
	ld	[%r17+8], %r9
code_127667:
	mov	%r8, %r9
code_127393:
	! done making normal call
	add	%r4, 8, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_127394
	nop
code_127395:
	call	GCFromML ! delay slot empty
	nop
needgc_127394:
	! allocating 1-record
	or	%r0, 9, %r8
	st	%r8, [%r4]
	st	%r9, [%r4+4]
	add	%r4, 4, %r16
	st	%r16, [%sp+112]
	add	%r4, 8, %r4
	! done allocating 1 record
	sethi	%hi(POSIX_FileSys_STR_r_INT), %r8
	or	%r8, %lo(POSIX_FileSys_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+68], %r16
	st	%r16, [%sp+108]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(dupfd_114875), %r8
	or	%r8, %lo(dupfd_114875), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(getfd_121918), %r8
	or	%r8, %lo(getfd_121918), %r8
	! done allocating 1 closures
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r10
	sethi	%hi(getfd_121918), %r8
	or	%r8, %lo(getfd_121918), %r12
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
code_127689:
	st	%r8, [%sp+104]
code_127407:
	! done making normal call
	add	%r4, 20, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_127408
	nop
code_127409:
	call	GCFromML ! delay slot empty
	nop
needgc_127408:
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(setfd_114896), %r8
	or	%r8, %lo(setfd_114896), %r8
	! done allocating 1 closures
	! allocating 4-record
	or	%r0, 3105, %r8
	st	%r8, [%r4]
	or	%r0, 5, %r8
	st	%r8, [%r4+4]
	or	%r0, 2, %r8
	st	%r8, [%r4+8]
	sethi	%hi(type_118303), %r8
	or	%r8, %lo(type_118303), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	sethi	%hi(type_118280), %r8
	or	%r8, %lo(type_118280), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+16]
	add	%r4, 4, %r18
	add	%r4, 20, %r4
	! done allocating 4 record
	ld	[%sp+108], %r17
	ld	[%r17+8], %r11
	or	%r0, 111, %r9
	sethi	%hi(strbindvar_r_wordTo_119096+-4), %r8
	st	%r9, [%r8+%lo(strbindvar_r_wordTo_119096+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 24, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_127420
	nop
code_127421:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127420:
	sethi	%hi(strbindvar_r_wordTo_119096), %r8
	or	%r8, %lo(strbindvar_r_wordTo_119096), %r10
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
	sethi	%hi(PrePosix_STR_r_INT), %r8
	or	%r8, %lo(PrePosix_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+36], %r11
	or	%r0, 111, %r9
	sethi	%hi(omodeFromWord_119098+-4), %r8
	st	%r9, [%r8+%lo(omodeFromWord_119098+-4)]
	sethi	%hi(omodeFromWord_119098), %r8
	or	%r8, %lo(omodeFromWord_119098), %r10
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
	sethi	%hi(type_118303), %r9
	or	%r9, %lo(type_118303), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127446
	nop
code_127447:
	or	%r0, 0, %r9
cmpui_127446:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_118280), %r9
	or	%r9, %lo(type_118280), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127450
	nop
code_127451:
	or	%r0, 0, %r9
cmpui_127450:
	sll	%r9, 9, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_119104), %r9
	st	%r8, [%r9+%lo(gctag_119104)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(getfl_121941), %r8
	or	%r8, %lo(getfl_121941), %r8
	! done allocating 1 closures
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(getfl_121941), %r8
	or	%r8, %lo(getfl_121941), %r12
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
code_127670:
	st	%r8, [%sp+100]
code_127459:
	! done making normal call
	ld	[%sp+108], %r17
	ld	[%r17+4], %r16
	st	%r16, [%sp+96]
	sethi	%hi(type_118303), %r8
	or	%r8, %lo(type_118303), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	sethi	%hi(SysWord_STR_c_INT), %r8
	or	%r8, %lo(SysWord_STR_c_INT), %r10
	ld	[%r2+804], %r8
	add	%r10, %r8, %r8
	ld	[%r8], %r10
	! making closure call
	sethi	%hi(onearg_INT), %r8
	or	%r8, %lo(onearg_INT), %r11
	ld	[%r2+804], %r8
	add	%r11, %r8, %r8
	ld	[%r8], %r11
	ld	[%r11], %r13
	ld	[%r11+4], %r8
	ld	[%r11+8], %r11
	jmpl	%r13, %r15
	ld	[%sp+96], %r12
code_127680:
	mov	%r8, %r13
code_127466:
	! done making normal call
	or	%r0, 111, %r9
	sethi	%hi(_121979+-4), %r8
	st	%r9, [%r8+%lo(_121979+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_127471
	nop
code_127472:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127471:
	sethi	%hi(_121979), %r8
	or	%r8, %lo(_121979), %r12
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
	sethi	%hi(setfl_114921), %r8
	or	%r8, %lo(setfl_114921), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(vars_eq_0_114938), %r8
	or	%r8, %lo(vars_eq_0_114938), %r8
	! done allocating 1 closures
	or	%r0, 41, %r8
	sethi	%hi(record_123506), %r9
	or	%r9, %lo(record_123506), %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127486
	nop
code_127487:
	or	%r0, 0, %r9
cmpui_127486:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_118320), %r9
	or	%r9, %lo(type_118320), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127490
	nop
code_127491:
	or	%r0, 0, %r9
cmpui_127490:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(record_123506), %r9
	or	%r9, %lo(record_123506), %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127493
	nop
code_127494:
	or	%r0, 0, %r9
cmpui_127493:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(record_gctag_121998), %r9
	st	%r8, [%r9+%lo(record_gctag_121998)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(flock_115024), %r8
	or	%r8, %lo(flock_115024), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(ltype_115027), %r8
	or	%r8, %lo(ltype_115027), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(whence_115033), %r8
	or	%r8, %lo(whence_115033), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(start_115039), %r8
	or	%r8, %lo(start_115039), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(len_115045), %r8
	or	%r8, %lo(len_115045), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(pid_115051), %r8
	or	%r8, %lo(pid_115051), %r8
	! done allocating 1 closures
	sethi	%hi(string_124278), %r8
	or	%r8, %lo(string_124278), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127671:
	mov	%r8, %r9
code_127504:
	! done making normal call
	sethi	%hi(f_getlk_119475), %r8
	st	%r9, [%r8+%lo(f_getlk_119475)]
	sethi	%hi(string_124295), %r8
	or	%r8, %lo(string_124295), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127681:
	mov	%r8, %r9
code_127508:
	! done making normal call
	sethi	%hi(f_setlk_119477), %r8
	st	%r9, [%r8+%lo(f_setlk_119477)]
	sethi	%hi(string_124313), %r8
	or	%r8, %lo(string_124313), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127682:
	mov	%r8, %r9
code_127512:
	! done making normal call
	sethi	%hi(f_setlkw_119479), %r8
	st	%r9, [%r8+%lo(f_setlkw_119479)]
	sethi	%hi(string_124330), %r8
	or	%r8, %lo(string_124330), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127683:
	mov	%r8, %r9
code_127516:
	! done making normal call
	sethi	%hi(f_rdlck_119481), %r8
	st	%r9, [%r8+%lo(f_rdlck_119481)]
	sethi	%hi(string_124347), %r8
	or	%r8, %lo(string_124347), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127684:
	mov	%r8, %r9
code_127520:
	! done making normal call
	sethi	%hi(f_wrlck_119483), %r8
	st	%r9, [%r8+%lo(f_wrlck_119483)]
	sethi	%hi(string_124364), %r8
	or	%r8, %lo(string_124364), %r10
	! making closure call
	sethi	%hi(osval_114457), %r8
	or	%r8, %lo(osval_114457), %r9
	ld	[%r9], %r11
	ld	[%r9+4], %r8
	jmpl	%r11, %r15
	ld	[%r9+8], %r9
code_127685:
	mov	%r8, %r9
code_127524:
	! done making normal call
	sethi	%hi(f_unlck_119485), %r8
	st	%r9, [%r8+%lo(f_unlck_119485)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(flockToRep_115098), %r8
	or	%r8, %lo(flockToRep_115098), %r8
	! done allocating 1 closures
	sethi	%hi(Int_STR_r_INT), %r8
	or	%r8, %lo(Int_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+88], %r8
	ld	[%r8+20], %r13
	or	%r0, 111, %r9
	sethi	%hi(toString_119545+-4), %r8
	st	%r9, [%r8+%lo(toString_119545+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_127533
	nop
code_127534:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127533:
	sethi	%hi(toString_119545), %r8
	or	%r8, %lo(toString_119545), %r12
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
	sethi	%hi(PLUSNoption_in_119581), %r8
	st	%r9, [%r8+%lo(PLUSNoption_in_119581)]
	! start making constructor call
	sethi	%hi(type_118286), %r8
	or	%r8, %lo(type_118286), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	ld	[%sp+120], %r17
	ld	[%r17], %r10
	ld	[%sp+120], %r17
	jmpl	%r10, %r15
	ld	[%r17+4], %r8
code_127668:
	mov	%r8, %r11
code_127550:
	! done making constructor call
	or	%r0, 111, %r9
	sethi	%hi(type_119585+-4), %r8
	st	%r9, [%r8+%lo(type_119585+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 24, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_127555
	nop
code_127556:
	sub	%r4, 24, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127555:
	sethi	%hi(type_119585), %r8
	or	%r8, %lo(type_119585), %r10
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
	sethi	%hi(POSIX_Process_STR_r_INT), %r8
	or	%r8, %lo(POSIX_Process_STR_r_INT), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r8
	ld	[%r8+8], %r11
	or	%r0, 111, %r9
	sethi	%hi(wordToPid_119588+-4), %r8
	st	%r9, [%r8+%lo(wordToPid_119588+-4)]
	sethi	%hi(wordToPid_119588), %r8
	or	%r8, %lo(wordToPid_119588), %r10
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
	or	%r0, 41, %r8
	sethi	%hi(record_123506), %r9
	or	%r9, %lo(record_123506), %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127580
	nop
code_127581:
	or	%r0, 0, %r9
cmpui_127580:
	sll	%r9, 8, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(type_118320), %r9
	or	%r9, %lo(type_118320), %r10
	ld	[%r2+804], %r9
	add	%r10, %r9, %r9
	ld	[%r9], %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127584
	nop
code_127585:
	or	%r0, 0, %r9
cmpui_127584:
	sll	%r9, 10, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(record_123506), %r9
	or	%r9, %lo(record_123506), %r9
	cmp	%r9, 3
	or	%r0, 1, %r9
	bgu	cmpui_127587
	nop
code_127588:
	or	%r0, 0, %r9
cmpui_127587:
	sll	%r9, 12, %r9
	add	%r9, %r0, %r9
	or	%r9, %r8, %r8
	sethi	%hi(gctag_119607), %r9
	st	%r8, [%r9+%lo(gctag_119607)]
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(flockFromRep_115128), %r8
	or	%r8, %lo(flockFromRep_115128), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(getlk_115181), %r8
	or	%r8, %lo(getlk_115181), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(setlk_115193), %r8
	or	%r8, %lo(setlk_115193), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(setlkw_115205), %r8
	or	%r8, %lo(setlkw_115205), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(lseek_115217), %r8
	or	%r8, %lo(lseek_115217), %r8
	! done allocating 1 closures
	! allocating 1 closures
	! allocating 3-record
	! done allocating 3 record
	sethi	%hi(fsync_122130), %r8
	or	%r8, %lo(fsync_122130), %r8
	! done allocating 1 closures
	sethi	%hi(type_118285), %r8
	or	%r8, %lo(type_118285), %r9
	ld	[%r2+804], %r8
	add	%r9, %r8, %r8
	ld	[%r8], %r9
	! allocating 2-record
	! done allocating 2 record
	sethi	%hi(record_124527), %r8
	or	%r8, %lo(record_124527), %r10
	sethi	%hi(fsync_122130), %r8
	or	%r8, %lo(fsync_122130), %r12
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
code_127672:
	mov	%r8, %r20
code_127602:
	! done making normal call
	add	%r4, 212, %r16
	ld	[%r2+20], %r5
	cmp	%r16, %r5
	ble	needgc_127603
	nop
code_127604:
	call	GCFromML ! delay slot empty
	nop
needgc_127603:
	sethi	%hi(16185), %r9
	or	%r9, %lo(16185), %r9
	sethi	%hi(record_123513), %r8
	or	%r8, %lo(record_123513), %r8
	cmp	%r8, 3
	or	%r0, 1, %r8
	bgu	cmpui_127607
	nop
code_127608:
	or	%r0, 0, %r8
cmpui_127607:
	sll	%r8, 14, %r8
	add	%r8, %r0, %r8
	or	%r8, %r9, %r9
	! allocating 7-record
	st	%r9, [%r4]
	sethi	%hi(vars_eq_0_114768), %r8
	or	%r8, %lo(vars_eq_0_114768), %r8
	st	%r8, [%r4+4]
	sethi	%hi(toWord_114792), %r8
	or	%r8, %lo(toWord_114792), %r8
	st	%r8, [%r4+8]
	sethi	%hi(wordTo_114789), %r8
	or	%r8, %lo(wordTo_114789), %r8
	st	%r8, [%r4+12]
	ld	[%sp+116], %r17
	st	%r17, [%r4+16]
	sethi	%hi(allSet_114834), %r8
	or	%r8, %lo(allSet_114834), %r8
	st	%r8, [%r4+20]
	sethi	%hi(anySet_114817), %r8
	or	%r8, %lo(anySet_114817), %r8
	st	%r8, [%r4+24]
	ld	[%sp+112], %r17
	st	%r17, [%r4+28]
	add	%r4, 4, %r23
	add	%r4, 32, %r4
	! done allocating 7 record
	ld	[%sp+108], %r17
	ld	[%r17], %r19
	ld	[%sp+108], %r17
	ld	[%r17+12], %r18
	ld	[%sp+108], %r17
	ld	[%r17+16], %r13
	ld	[%sp+108], %r17
	ld	[%r17+20], %r12
	ld	[%sp+108], %r17
	ld	[%r17+24], %r11
	ld	[%sp+108], %r17
	ld	[%r17+40], %r10
	ld	[%sp+108], %r17
	ld	[%r17+48], %r9
	sethi	%hi(16201), %r8
	or	%r8, %lo(16201), %r8
	sethi	%hi(type_118303), %r21
	or	%r21, %lo(type_118303), %r22
	ld	[%r2+804], %r21
	add	%r22, %r21, %r21
	ld	[%r21], %r21
	cmp	%r21, 3
	or	%r0, 1, %r21
	bgu	cmpui_127616
	nop
code_127617:
	or	%r0, 0, %r21
cmpui_127616:
	sll	%r21, 14, %r21
	add	%r21, %r0, %r21
	or	%r21, %r8, %r8
	sethi	%hi(type_118303), %r21
	or	%r21, %lo(type_118303), %r22
	ld	[%r2+804], %r21
	add	%r22, %r21, %r21
	ld	[%r21], %r21
	cmp	%r21, 3
	or	%r0, 1, %r21
	bgu	cmpui_127620
	nop
code_127621:
	or	%r0, 0, %r21
cmpui_127620:
	sll	%r21, 15, %r21
	add	%r21, %r0, %r21
	or	%r21, %r8, %r8
	sethi	%hi(type_118303), %r21
	or	%r21, %lo(type_118303), %r22
	ld	[%r2+804], %r21
	add	%r22, %r21, %r21
	ld	[%r21], %r21
	cmp	%r21, 3
	or	%r0, 1, %r21
	bgu	cmpui_127624
	nop
code_127625:
	or	%r0, 0, %r21
cmpui_127624:
	sll	%r21, 16, %r21
	add	%r21, %r0, %r21
	or	%r21, %r8, %r8
	! allocating 9-record
	st	%r8, [%r4]
	st	%r19, [%r4+4]
	ld	[%sp+96], %r17
	st	%r17, [%r4+8]
	sethi	%hi(strbindvar_r_wordTo_119096), %r8
	or	%r8, %lo(strbindvar_r_wordTo_119096), %r19
	ld	[%r2+804], %r8
	add	%r19, %r8, %r8
	ld	[%r8], %r8
	st	%r8, [%r4+12]
	st	%r18, [%r4+16]
	st	%r13, [%r4+20]
	st	%r12, [%r4+24]
	st	%r11, [%r4+28]
	st	%r10, [%r4+32]
	st	%r9, [%r4+36]
	add	%r4, 4, %r9
	add	%r4, 40, %r4
	! done allocating 9 record
	! allocating 6-record
	! done allocating 6 record
	! allocating 10-record
	sethi	%hi(261969), %r8
	or	%r8, %lo(261969), %r8
	st	%r8, [%r4]
	sethi	%hi(setfl_114921), %r8
	or	%r8, %lo(setfl_114921), %r8
	st	%r8, [%r4+4]
	sethi	%hi(lseek_115217), %r8
	or	%r8, %lo(lseek_115217), %r8
	st	%r8, [%r4+8]
	st	%r20, [%r4+12]
	or	%r0, 257, %r8
	st	%r8, [%r4+16]
	or	%r0, 257, %r8
	st	%r8, [%r4+20]
	sethi	%hi(vars_eq_0_114938), %r8
	or	%r8, %lo(vars_eq_0_114938), %r8
	st	%r8, [%r4+24]
	sethi	%hi(record_124611), %r8
	or	%r8, %lo(record_124611), %r8
	st	%r8, [%r4+28]
	sethi	%hi(getlk_115181), %r8
	or	%r8, %lo(getlk_115181), %r8
	st	%r8, [%r4+32]
	sethi	%hi(setlk_115193), %r8
	or	%r8, %lo(setlk_115193), %r8
	st	%r8, [%r4+36]
	sethi	%hi(setlkw_115205), %r8
	or	%r8, %lo(setlkw_115205), %r8
	st	%r8, [%r4+40]
	add	%r4, 4, %r8
	add	%r4, 44, %r4
	! done allocating 10 record
	! allocating 23-record
	sethi	%hi(2122317753), %r10
	or	%r10, %lo(2122317753), %r10
	st	%r10, [%r4]
	ld	[%sp+136], %r17
	st	%r17, [%r4+4]
	ld	[%sp+132], %r17
	st	%r17, [%r4+8]
	sethi	%hi(pipe_114504), %r10
	or	%r10, %lo(pipe_114504), %r10
	st	%r10, [%r4+12]
	ld	[%sp+128], %r17
	st	%r17, [%r4+16]
	sethi	%hi(dup2_114516), %r10
	or	%r10, %lo(dup2_114516), %r10
	st	%r10, [%r4+20]
	ld	[%sp+124], %r17
	st	%r17, [%r4+24]
	sethi	%hi(readVec_114578), %r10
	or	%r10, %lo(readVec_114578), %r10
	st	%r10, [%r4+28]
	sethi	%hi(readArr_114536), %r10
	or	%r10, %lo(readArr_114536), %r10
	st	%r10, [%r4+32]
	sethi	%hi(writeVec_114655), %r10
	or	%r10, %lo(writeVec_114655), %r10
	st	%r10, [%r4+36]
	sethi	%hi(writeArr_114613), %r10
	or	%r10, %lo(writeArr_114613), %r10
	st	%r10, [%r4+40]
	or	%r0, 257, %r10
	st	%r10, [%r4+44]
	or	%r0, 257, %r10
	st	%r10, [%r4+48]
	sethi	%hi(vars_eq_0_114706), %r10
	or	%r10, %lo(vars_eq_0_114706), %r10
	st	%r10, [%r4+52]
	st	%r23, [%r4+56]
	st	%r9, [%r4+60]
	ld	[%sp+148], %r17
	st	%r17, [%r4+64]
	ld	[%sp+144], %r17
	st	%r17, [%r4+68]
	ld	[%sp+140], %r17
	st	%r17, [%r4+72]
	sethi	%hi(dupfd_114875), %r9
	or	%r9, %lo(dupfd_114875), %r9
	st	%r9, [%r4+76]
	ld	[%sp+104], %r17
	st	%r17, [%r4+80]
	sethi	%hi(setfd_114896), %r9
	or	%r9, %lo(setfd_114896), %r9
	st	%r9, [%r4+84]
	ld	[%sp+100], %r17
	st	%r17, [%r4+88]
	st	%r8, [%r4+92]
	add	%r4, 4, %r13
	add	%r4, 96, %r4
	! done allocating 23 record
	or	%r0, 111, %r9
	sethi	%hi(POSIX_IO_STR_r_INT+-4), %r8
	st	%r9, [%r8+%lo(POSIX_IO_STR_r_INT+-4)]
	ld	[%r2+792], %r8
	ld	[%r2+796], %r9
	add	%r8, 12, %r8
	cmp	%r8, %r9
	ble	afterMutateCheck_127648
	nop
code_127649:
	sub	%r4, 12, %r16
	call	GCFromML ! delay slot empty
	nop
afterMutateCheck_127648:
	sethi	%hi(POSIX_IO_STR_r_INT), %r8
	or	%r8, %lo(POSIX_IO_STR_r_INT), %r12
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
code_127661:
	ld	[%sp+92], %r15
	retl
	add	%sp, 160, %sp
	.size POSIX_IO_Str_main,(.-POSIX_IO_Str_main)

	.section	".rodata"
		! -------- label,sizes,reg
	.long code_127662
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_127663
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_127664
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127665
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50050000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127666
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127667
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127668
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127669
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_127670
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127671
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127672
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_126952
	.word 0x00280008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_127013
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_127025
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00150000
	.word 0x00000000
		! -------- label,sizes,reg
	.long afterMutateCheck_127046
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000000
		! -------- label,sizes,reg
	.long needgc_127101
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_127673
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_127121
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.word 0x00000040
		! -------- label,sizes,reg
	.long afterMutateCheck_127150
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.word 0x00000054
		! -------- label,sizes,reg
	.long code_127674
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00050000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127675
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127208
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x40040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127676
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127677
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127678
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127306
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long needgc_127318
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127331
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127352
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50050000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127679
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127378
	.word 0x00280008
	.word 0x00170000
	.word 0x00000100
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long needgc_127394
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x54000000
	.word 0x00000055
		! -------- label,sizes,reg
	.long needgc_127408
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127420
	.word 0x00280008
	.word 0x00170000
	.word 0x00040800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55500000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127680
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127471
	.word 0x00280008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127681
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127682
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127683
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127684
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127685
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127533
	.word 0x00280008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127555
	.word 0x00280008
	.word 0x00170000
	.word 0x00000800
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long needgc_127603
	.word 0x00280008
	.word 0x00170000
	.word 0x00100000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x45550000
	.word 0x00000055
		! -------- label,sizes,reg
	.long afterMutateCheck_127648
	.word 0x00280008
	.word 0x00170000
	.word 0x00002000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_127686
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00000000
	.word 0x00000000
		! -------- label,sizes,reg
	.long code_127687
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x00010000
	.word 0x00000040
		! -------- label,sizes,reg
	.long code_127688
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x50040000
	.word 0x00000055
		! -------- label,sizes,reg
	.long code_127689
	.word 0x00280008
	.word 0x00170000
	.word 0x00000000
	.word 0x00000000
		! stacktrace
	.word 0x00000000
	.word 0x55400000
	.word 0x00000055
	.text
POSIX_IO_Str_unit_CODE_END_VAL:
	.section	".rodata"
		! endgcinfo with filler for alignment
	.globl POSIX_IO_Str_unit_GCTABLE_END_VAL
POSIX_IO_Str_unit_GCTABLE_END_VAL:
	.word 0x00000000
	.data
	.align 8
	.data
	.align 8
	.globl POSIX_IO_Str_unit_GLOBALS_BEGIN_VAL
POSIX_IO_Str_unit_GLOBALS_BEGIN_VAL:
		! Global
	.word 0x00000037
type_118280:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_118285:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_118286:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_123506:
	.word 0x00000008
		! static record tag
	.word 0x00001029
record_123511:
	.word 0x00000004
	.word 0xffffffff
	.word 0x00000003
	.word 0x00000003
	.word 0x00000100
		! static record tag
	.word 0x00000009
record_123513:
	.word 0x00000008
		! Global
	.word 0x00000037
type_118303:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000009
record_123525:
	.word 0x00000008
		! Global
	.word 0x00000037
type_118320:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
	.globl POSIX_IO_STR_c_INT
POSIX_IO_STR_c_INT:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
HAT_118386:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
orb_118392:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
andb_118393:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
osval_114457:
	.long POSIX_IO_Str_osval_code_123201
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
mk_118413:
	.word 0x00000102
	.word 0x00000102
	.word 0x0000004a
string_123691:
		! string size = 9
	.ascii "POSIX_IO."
.align 4
	.word 0x00000012
string_123694:
		! string size = 2
	.ascii ": "
.align 4
		! Global
	.word 0x00000037
_121536:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
fs_intof_121528:
	.long POSIX_IO_Str_fs_intof_code_123206
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
strbindvar_r_wordToFD_118448:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_118464:
	.word 0x00000102
		! static record tag
	.word 0x00000619
pipe_114504:
	.long POSIX_IO_Str_pipe_code_123211
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
dup_121545:
	.long POSIX_IO_Str_dup_code_123216
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
dup2_114516:
	.long POSIX_IO_Str_dup2_code_123221
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
close_121570:
	.long POSIX_IO_Str_close_code_123226
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000011
record_123802:
	.word 0x00000005
	.word 0x00000000
		! Global
	.word 0x00000037
reify_122481:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNoption_out_118496:
	.word 0x00000102
		! Global
	.word 0x00000037
length_118500:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
type_118503:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
PLUSNbool_out_118504:
	.word 0x00000102
		! Global
	.word 0x00000037
mk_118514:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
readArr_114536:
	.long POSIX_IO_Str_readArr_code_123231
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
readVec_114578:
	.long POSIX_IO_Str_readVec_code_123236
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
writeArr_114613:
	.long POSIX_IO_Str_writeArr_code_123241
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
length_118679:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
writeVec_114655:
	.long POSIX_IO_Str_writeVec_code_123246
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
vars_eq_0_114706:
	.long POSIX_IO_Str_vars_eq_0_code_123251
	.word 0x00000100
	.word 0x00000100
	.word 0x00000042
string_123910:
		! string size = 8
	.ascii "SEEK_SET"
.align 4
		! Global
	.word 0x00000027
seek_set_118792:
	.word 0x00000102
	.word 0x00000042
string_123928:
		! string size = 8
	.ascii "SEEK_CUR"
.align 4
		! Global
	.word 0x00000027
seek_cur_118794:
	.word 0x00000102
	.word 0x00000042
string_123946:
		! string size = 8
	.ascii "SEEK_END"
.align 4
		! Global
	.word 0x00000027
seek_end_118796:
	.word 0x00000102
		! static record tag
	.word 0x00000619
vars_eq_0_114768:
	.long POSIX_IO_Str_vars_eq_0_code_123256
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
wordTo_114789:
	.long POSIX_IO_Str_wordTo_code_123261
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
toWord_114792:
	.long POSIX_IO_Str_toWord_code_123266
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
reify_122532:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
_118913:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anonfun_114806:
	.long POSIX_IO_Str_anonfun_code_123271
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
type_121841:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
flags_121771:
	.long POSIX_IO_Str_flags_code_123276
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
PLUSEword_118956:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
anySet_114817:
	.long POSIX_IO_Str_anySet_code_123281
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
allSet_114834:
	.long POSIX_IO_Str_allSet_code_123286
	.word 0x00000100
	.word 0x00000100
	.word 0x0000003a
string_124085:
		! string size = 7
	.ascii "cloexec"
.align 4
		! static record tag
	.word 0x00000619
dupfd_114875:
	.long POSIX_IO_Str_dupfd_code_123291
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
getfd_121918:
	.long POSIX_IO_Str_getfd_code_123296
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
setfd_114896:
	.long POSIX_IO_Str_setfd_code_123301
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
strbindvar_r_wordTo_119096:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
omodeFromWord_119098:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_119104:
	.word 0x00000102
		! static record tag
	.word 0x00000619
getfl_121941:
	.long POSIX_IO_Str_getfl_code_123306
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000037
_121979:
	.word 0x00000102
	.word 0x00000102
		! static record tag
	.word 0x00000619
setfl_114921:
	.long POSIX_IO_Str_setfl_code_123311
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
vars_eq_0_114938:
	.long POSIX_IO_Str_vars_eq_0_code_123316
	.word 0x00000100
	.word 0x00000100
		! Global
	.word 0x00000027
record_gctag_121998:
	.word 0x00000102
		! static record tag
	.word 0x00000619
flock_115024:
	.long POSIX_IO_Str_flock_code_123321
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
ltype_115027:
	.long POSIX_IO_Str_ltype_code_123326
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
whence_115033:
	.long POSIX_IO_Str_whence_code_123331
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
start_115039:
	.long POSIX_IO_Str_start_code_123336
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
len_115045:
	.long POSIX_IO_Str_len_code_123341
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
pid_115051:
	.long POSIX_IO_Str_pid_code_123346
	.word 0x00000100
	.word 0x00000100
	.word 0x0000003a
string_124278:
		! string size = 7
	.ascii "F_GETLK"
.align 4
		! Global
	.word 0x00000027
f_getlk_119475:
	.word 0x00000102
	.word 0x0000003a
string_124295:
		! string size = 7
	.ascii "F_SETLK"
.align 4
		! Global
	.word 0x00000027
f_setlk_119477:
	.word 0x00000102
	.word 0x00000042
string_124313:
		! string size = 8
	.ascii "F_SETLKW"
.align 4
		! Global
	.word 0x00000027
f_setlkw_119479:
	.word 0x00000102
	.word 0x0000003a
string_124330:
		! string size = 7
	.ascii "F_RDLCK"
.align 4
		! Global
	.word 0x00000027
f_rdlck_119481:
	.word 0x00000102
	.word 0x0000003a
string_124347:
		! string size = 7
	.ascii "F_WRLCK"
.align 4
		! Global
	.word 0x00000027
f_wrlck_119483:
	.word 0x00000102
	.word 0x0000003a
string_124364:
		! string size = 7
	.ascii "F_UNLCK"
.align 4
		! Global
	.word 0x00000027
f_unlck_119485:
	.word 0x00000102
		! static record tag
	.word 0x00000619
flockToRep_115098:
	.long POSIX_IO_Str_flockToRep_code_123351
	.word 0x00000100
	.word 0x00000100
	.word 0x00000062
string_124391:
		! string size = 12
	.ascii "flockFromRep"
.align 4
	.word 0x00000092
string_124410:
		! string size = 18
	.ascii "unknown lock type "
.align 4
		! Global
	.word 0x00000037
toString_119545:
	.word 0x00000102
	.word 0x00000102
	.word 0x00000052
string_124430:
		! string size = 10
	.ascii "whFromWord"
.align 4
	.word 0x0000007a
string_124446:
		! string size = 15
	.ascii "unknown whence "
.align 4
		! Global
	.word 0x00000027
PLUSNoption_in_119581:
	.word 0x00000102
		! Global
	.word 0x00000037
type_119585:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000037
wordToPid_119588:
	.word 0x00000102
	.word 0x00000102
		! Global
	.word 0x00000027
gctag_119607:
	.word 0x00000102
		! static record tag
	.word 0x00000619
flockFromRep_115128:
	.long POSIX_IO_Str_flockFromRep_code_123356
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
getlk_115181:
	.long POSIX_IO_Str_getlk_code_123361
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
setlk_115193:
	.long POSIX_IO_Str_setlk_code_123366
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
setlkw_115205:
	.long POSIX_IO_Str_setlkw_code_123371
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
lseek_115217:
	.long POSIX_IO_Str_lseek_code_123376
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000619
fsync_122130:
	.long POSIX_IO_Str_fsync_code_123381
	.word 0x00000100
	.word 0x00000100
		! static record tag
	.word 0x00000011
record_124527:
	.word 0x00000005
	.word 0x00000000
		! static record tag
	.word 0x00003f31
record_124611:
	.long flock_115024
	.long ltype_115027
	.long whence_115033
	.long start_115039
	.long len_115045
	.long pid_115051
		! Global
	.word 0x00000037
	.globl POSIX_IO_STR_r_INT
POSIX_IO_STR_r_INT:
	.word 0x00000102
	.word 0x00000102
		! Module closure
	.word 0x00000619
	.globl POSIX_IO_Str_unit_closure
POSIX_IO_Str_unit_closure:
	.long POSIX_IO_Str_main
	.word 0x00000000
	.word 0x00000000
	.word 0x00000311
	.globl POSIX_IO_Str_unit
POSIX_IO_Str_unit:
	.long POSIX_IO_Str_unit_closure
	.long POSIX_IO_Str_unit_closure
	.globl POSIX_IO_Str_unit_GLOBALS_END_VAL
POSIX_IO_Str_unit_GLOBALS_END_VAL:
	.word 0x00000000
	.long 0 !filler

	.data
	.align 8
	.globl POSIX_IO_Str_unit_TRACE_GLOBALS_BEGIN_VAL
POSIX_IO_Str_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long POSIX_IO_STR_r_INT
	.long wordToPid_119588
	.long type_119585
	.long toString_119545
	.long _121979
	.long omodeFromWord_119098
	.long strbindvar_r_wordTo_119096
	.long PLUSEword_118956
	.long type_121841
	.long _118913
	.long reify_122532
	.long length_118679
	.long mk_118514
	.long type_118503
	.long length_118500
	.long reify_122481
	.long strbindvar_r_wordToFD_118448
	.long _121536
	.long mk_118413
	.long andb_118393
	.long orb_118392
	.long HAT_118386
	.long POSIX_IO_STR_c_INT
	.long type_118320
	.long type_118303
	.long type_118286
	.long type_118285
	.long type_118280
	.globl POSIX_IO_Str_unit_TRACE_GLOBALS_END_VAL
POSIX_IO_Str_unit_TRACE_GLOBALS_END_VAL:
		! filler so label is defined
	.word 0x00000000
