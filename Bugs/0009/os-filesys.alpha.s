	.set noat
	.rdata
		# gcinfo
	.globl OS_FileSys_unit_GCTABLE_BEGIN_VAL
OS_FileSys_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl OS_FileSys_unit_CODE_END_VAL
	.globl OS_FileSys_unit_CODE_BEGIN_VAL
OS_FileSys_unit_CODE_BEGIN_VAL:
	.text
 	.align 3
	.ent OS_FileSys_mkDir_code_89404
 # arguments : [$89406,$0] [$89407,$1] [$85081,$2] 
 # results    : [$91377,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_mkDir_code_89404:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_91383:
funtop_91356:
	lda	$1, reify_89355
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	beq	$0, else_case_91368
code_91385:
	lda	$1, mode777_87237
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$3, ($0)
	br	$31, after_ite_91369
else_case_91368:
	lda	$0, mode777_87237
	ldl	$3, ($0)
after_ite_91369:
	# making closure call 
	lda	$1, strbindvar_r_mkdir_87238
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_91390:
	# done making tail call
code_91392:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_mkDir_code_89404

	.rdata
	.text
 	.align 3
	.ent OS_FileSys_mkPath_code_89409
 # arguments : [$89411,$0] [$89412,$1] [$85122,$2] 
 # results    : [$91355,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_mkPath_code_89409:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_91394:
funtop_91331:
	# making closure call 
	lda	$1, _87281
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91402:
	ldgp	$gp, ($26)
	mov	$0, $4
code_91396:
	# done making normal call
	lda	$2, string_89828
	lda	$3, 1($31)
	# making closure call 
	lda	$1, strbindvar_r_toString_87273
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$5, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($5), 1
code_91398:
	# done making tail call
code_91400:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_mkPath_code_89409

	.rdata
		# -------- label,sizes,reg
	.long code_91402
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent OS_FileSys_walkPath_code_89414
 # arguments : [$89416,$0] [$89417,$1] [$87981,$2] [$87982,$3] [$87983,$4] 
 # results    : [$91325,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_walkPath_code_89414:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 24($sp)
	stl	$2, 32($sp)
	stl	$3, 28($sp)
code_91403:
funtop_91023:
	ldl	$25, 24($sp)
	ldl	$25, ($25)
	stl	$25, 12($sp)
	ldl	$25, 24($sp)
	ldl	$25, 4($25)
	stl	$25, 8($sp)
	ldl	$25, 32($sp)
	cmpeq	$25, 0, $0
	bne	$0, one_case_91036
zero_case_91035:
sumarm_91048:
	bne	$4, sumarm_91049
code_91405:
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$3, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	ldl	$2, 28($sp)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	jsr	$31, ($3), 1
code_91406:
	# done making tail call
	br	$31, after_sum_91045
sumarm_91049:
	ldl	$25, ($4)
	stl	$25, 20($sp)
	ldl	$25, 4($4)
	stl	$25, 16($sp)
	lda	$3, string_89828
	# making closure call 
	lda	$1, PLUSEstring_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91462:
	ldgp	$gp, ($26)
code_91409:
	# done making normal call
	bne	$0, one_case_91097
zero_case_91096:
	lda	$3, string_89847
	# making closure call 
	lda	$1, PLUSEstring_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91464:
	ldgp	$gp, ($26)
code_91412:
	# done making normal call
	bne	$0, one_case_91114
zero_case_91113:
	lda	$3, string_89850
	# making closure call 
	lda	$1, PLUSEstring_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91465:
	ldgp	$gp, ($26)
code_91415:
	# done making normal call
	bne	$0, one_case_91131
zero_case_91130:
sumarm_91143:
	ldl	$25, 16($sp)
	bne	$25, sumarm_91144
code_91417:
	# making closure call 
	lda	$1, isLink_87957
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91458:
	ldgp	$gp, ($26)
code_91419:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91420
code_91421:
	jsr	$26, GCFromML
gc_check_91420:
	bne	$0, one_case_91159
zero_case_91158:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 28($sp)
	stl	$25, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$3, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	jsr	$31, ($3), 1
code_91424:
	# done making tail call
	br	$31, after_zeroone_91160
one_case_91159:
	lda	$5, ($31)
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$6, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	ldl	$2, 32($sp)
	ldl	$3, 28($sp)
	ldl	$4, 20($sp)
	mov	$6, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	jsr	$31, ($6), 1
code_91426:
	# done making tail call
after_zeroone_91160:
	br	$31, after_sum_91140
sumarm_91144:
nomatch_sum_91141:
	# making closure call 
	lda	$1, isLink_87957
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91459:
	ldgp	$gp, ($26)
code_91429:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91430
code_91431:
	jsr	$26, GCFromML
gc_check_91430:
	bne	$0, one_case_91206
zero_case_91205:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 28($sp)
	stl	$25, 8($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	addl	$13, 12, $13
	# done allocating 2 record
	# making closure call 
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91467:
	ldgp	$gp, ($26)
code_91435:
	# done making normal call
	# making direct call 
	ldl	$at, 8($sp)
	stl	$at, 28($sp)
	ldl	$4, 16($sp)
	br	$31, funtop_91023
code_91436:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_91207
one_case_91206:
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$6, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	ldl	$2, 32($sp)
	ldl	$3, 28($sp)
	ldl	$4, 20($sp)
	ldl	$5, 16($sp)
	mov	$6, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	jsr	$31, ($6), 1
code_91438:
	# done making tail call
after_zeroone_91207:
after_sum_91140:
	br	$31, after_zeroone_91132
one_case_91131:
sumarm_91259:
	ldl	$25, 28($sp)
	bne	$25, sumarm_91260
code_91440:
	lda	$0, ($31)
	# making direct call 
	stl	$0, 28($sp)
	ldl	$4, 16($sp)
	br	$31, funtop_91023
code_91441:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_91256
sumarm_91260:
	ldl	$25, 28($sp)
	ldl	$25, 4($25)
	stl	$25, 8($sp)
	lda	$2, string_89850
	# making closure call 
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91463:
	ldgp	$gp, ($26)
code_91444:
	# done making normal call
	# making direct call 
	ldl	$at, 8($sp)
	stl	$at, 28($sp)
	ldl	$4, 16($sp)
	br	$31, funtop_91023
code_91445:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_91256
sumarm_91272:
after_sum_91256:
after_zeroone_91132:
	br	$31, after_zeroone_91115
one_case_91114:
	# making direct call 
	ldl	$4, 16($sp)
	br	$31, funtop_91023
code_91448:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_91115:
	br	$31, after_zeroone_91098
one_case_91097:
	# making direct call 
	ldl	$4, 16($sp)
	br	$31, funtop_91023
code_91450:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_91098:
	br	$31, after_sum_91045
sumarm_91059:
after_sum_91045:
	br	$31, after_zeroone_91037
one_case_91036:
	lda	$1, _87402
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	mov	$0, $26
	ldl	$27, ($15)
	ldl	$sp, 4($15)
	ldl	$0, 1096($12)
	addl	$sp, $0, $sp
	mov	$27, $27
	jsr	$31, ($27), 1
	lda	$0, ($31)
after_zeroone_91037:
code_91456:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_walkPath_code_89414

	.rdata
		# -------- label,sizes,reg
	.long code_91458
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00004450
		# -------- label,sizes,reg
	.long gc_check_91420
	.long 0x00001805
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00004450
		# -------- label,sizes,reg
	.long gc_check_91430
	.long 0x00001805
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00005510
		# -------- label,sizes,reg
	.long code_91459
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005510
		# -------- label,sizes,reg
	.long code_91462
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005550
		# -------- label,sizes,reg
	.long code_91463
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001110
		# -------- label,sizes,reg
	.long code_91464
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005550
		# -------- label,sizes,reg
	.long code_91465
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005550
		# -------- label,sizes,reg
	.long code_91467
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001110
	.text
 	.align 3
	.ent OS_FileSys_expandLink_code_89419
 # arguments : [$89421,$0] [$89422,$1] [$88067,$2] [$88068,$3] [$88069,$4] [$88070,$5] 
 # results    : [$91001,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_expandLink_code_89419:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 8($sp)
	stl	$3, 24($sp)
	mov	$4, $2
	stl	$5, 20($sp)
code_91468:
funtop_90944:
	ldl	$25, ($1)
	stl	$25, 16($sp)
	ldl	$25, 4($1)
	stl	$25, 12($sp)
	# making closure call 
	lda	$1, strbindvar_r_readlink_87268
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91486:
	ldgp	$gp, ($26)
	mov	$0, $2
code_91470:
	# done making normal call
	# making closure call 
	lda	$1, strbindvar_r_fromString_87411
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91483:
	ldgp	$gp, ($26)
code_91472:
	# done making normal call
	ldl	$2, 8($0)
	ldl	$0, 4($0)
	bne	$0, one_case_90977
zero_case_90976:
	ldl	$25, 8($sp)
	sublv	$25, 1, $25
	stl	$25, 8($sp)
	trapb
	# making closure call 
	lda	$1, _87422
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91488:
	ldgp	$gp, ($26)
	mov	$0, $4
code_91475:
	# done making normal call
	# making closure call 
	ldl	$25, 16($sp)
	ldl	$5, ($25)
	ldl	$25, 16($sp)
	ldl	$0, 4($25)
	ldl	$25, 16($sp)
	ldl	$1, 8($25)
	ldl	$2, 8($sp)
	ldl	$3, 24($sp)
	mov	$5, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($5), 1
code_91476:
	# done making tail call
	br	$31, after_zeroone_90978
one_case_90977:
	ldl	$25, 8($sp)
	sublv	$25, 1, $25
	stl	$25, 8($sp)
	trapb
	# making closure call 
	lda	$1, _87422
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91487:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91479:
	# done making normal call
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$4, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	ldl	$2, 8($sp)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($4), 1
code_91480:
	# done making tail call
after_zeroone_90978:
code_91482:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_expandLink_code_89419

	.rdata
		# -------- label,sizes,reg
	.long code_91483
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001540
		# -------- label,sizes,reg
	.long code_91486
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001540
		# -------- label,sizes,reg
	.long code_91487
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_91488
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001100
	.text
 	.align 3
	.ent OS_FileSys_gotoRoot_code_89424
 # arguments : [$89426,$0] [$89427,$1] [$88123,$2] [$88124,$3] 
 # results    : [$90943,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_gotoRoot_code_89424:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
	stl	$2, 16($sp)
	stl	$3, 12($sp)
code_91489:
funtop_90922:
	lda	$2, string_89914
	# making closure call 
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91496:
	ldgp	$gp, ($26)
code_91491:
	# done making normal call
	lda	$3, ($31)
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$5, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	ldl	$2, 16($sp)
	ldl	$4, 12($sp)
	mov	$5, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($5), 1
code_91492:
	# done making tail call
code_91494:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_gotoRoot_code_89424

	.rdata
		# -------- label,sizes,reg
	.long code_91496
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent OS_FileSys_fullPath_code_89439
 # arguments : [$89441,$0] [$89442,$1] [$85115,$2] 
 # results    : [$90870,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_fullPath_code_89439:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 8($sp)
code_91497:
funtop_90767:
	# making closure polycall
	lda	$1, strbindvar_r_getcwd_87174
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$2, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$2, $27
	jsr	$26, ($2), 1
code_91548:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_91499:
	# done making normal call
	# making closure call 
	lda	$1, strbindvar_r_fromString_87411
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91540:
	ldgp	$gp, ($26)
code_91501:
	# done making normal call
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91502
code_91503:
	jsr	$26, GCFromML
gc_check_91502:
	ldl	$25, 8($0)
	stl	$25, 8($sp)
	ldl	$0, 4($0)
	bne	$0, one_case_90795
zero_case_90794:
	# making closure call 
	lda	$1, strbindvar_r_fromString_87411
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91551:
	ldgp	$gp, ($26)
code_91507:
	# done making normal call
	ldl	$2, 8($0)
	# making closure call 
	lda	$1, _87422
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91541:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91509:
	# done making normal call
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91510
code_91511:
	jsr	$26, GCFromML
gc_check_91510:
	# allocating 1-record
	lda	$0, 265($31)
	stl	$0, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	addl	$13, 4, $4
	addl	$13, 8, $13
	# done allocating 1 record
	lda	$2, exn_handler_90820
	ldl	$0, 1096($12)
	subl	$sp, $0, $1
	# allocating 4-record
	lda	$0, 3105($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	stl	$4, 12($13)
	stl	$15, 16($13)
	addl	$13, 4, $0
	addl	$13, 20, $13
	# done allocating 4 record
	mov	$0, $15
	lda	$2, 64($31)
	# making closure call 
	lda	$1, gotoRoot_85121
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91549:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_91514:
	# done making normal call
	# making closure call 
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91542:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91516:
	# done making normal call
	# making closure call 
	lda	$1, _87461
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91543:
	ldgp	$gp, ($26)
code_91518:
	# done making normal call
	ldl	$15, 12($15)
	br	$31, exn_handler_after_90821
exn_handler_90820:
	ldgp	$gp, ($27)
	ldl	$0, 8($15)
	ldl	$15, 12($15)
	ldl	$25, ($0)
	stl	$25, 12($sp)
	stl	$26, 8($sp)
	# making closure call 
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91546:
	ldgp	$gp, ($26)
code_91523:
	# done making normal call
	ldl	$26, 8($sp)
	ldl	$27, ($15)
	ldl	$sp, 4($15)
	ldl	$0, 1096($12)
	addl	$sp, $0, $sp
	mov	$27, $27
	jsr	$31, ($27), 1
	lda	$0, ($31)
exn_handler_after_90821:
	br	$31, after_zeroone_90796
one_case_90795:
	# allocating 1-record
	lda	$0, 265($31)
	stl	$0, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	addl	$13, 4, $3
	addl	$13, 8, $13
	# done allocating 1 record
	lda	$2, exn_handler_90872
	ldl	$0, 1096($12)
	subl	$sp, $0, $1
	# allocating 4-record
	lda	$0, 3105($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	stl	$3, 12($13)
	stl	$15, 16($13)
	addl	$13, 4, $0
	addl	$13, 20, $13
	# done allocating 4 record
	mov	$0, $15
	lda	$2, 64($31)
	# making closure call 
	lda	$1, gotoRoot_85121
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91550:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_91527:
	# done making normal call
	# making closure call 
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91544:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91529:
	# done making normal call
	# making closure call 
	lda	$1, _87461
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91545:
	ldgp	$gp, ($26)
code_91531:
	# done making normal call
	ldl	$15, 12($15)
	br	$31, exn_handler_after_90873
exn_handler_90872:		; ILABEL hl
	ldgp	$gp, ($27)	; CATCH_EXN
	ldl	$0, 8($15)	; record_project(exnptr,2,freeRec)	Rtl/tortl.sml,573
	ldl	$15, 12($15)	; record_project(exnptr,3,exnptr)	Rtl/tortl.sml,574
	
				; Restore the int registers Rtl/tortl.sml,577-585
	ldl	$25, ($0)	; record_project(freeRec,0,??) ...
	stl	$25, 12($sp)	;	...		;;; SEGV
				; confusing that "resotring a register" involves saving to the stack
				; I could simply be confused.
	;; 
	stl	$26, 8($sp)	; MV(SREGI EXNARG, xr)
	# making closure call 
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91547:
	ldgp	$gp, ($26)
code_91536:
	# done making normal call
	ldl	$26, 8($sp)
	ldl	$27, ($15)
	ldl	$sp, 4($15)
	ldl	$0, 1096($12)
	addl	$sp, $0, $sp
	mov	$27, $27
	jsr	$31, ($27), 1
	lda	$0, ($31)
exn_handler_after_90873:
after_zeroone_90796:
code_91539:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_fullPath_code_89439

	.rdata
		# -------- label,sizes,reg
	.long code_91540
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_91502
	.long 0x00000805
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_91541
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_91510
	.long 0x00000805
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_91542
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91543
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_91544
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91545
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_91546
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91547
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91548
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91549
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_91550
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_91551
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent OS_FileSys_realPath_code_89444
 # arguments : [$89446,$0] [$89447,$1] [$85233,$2] 
 # results    : [$90758,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_realPath_code_89444:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 8($sp)
code_91552:
funtop_90708:
	# making closure call 
	lda	$1, strbindvar_r_isAbsolute_87495
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91569:
	ldgp	$gp, ($26)
code_91554:
	# done making normal call
	bne	$0, one_case_90723
zero_case_90722:
	# making closure call 
	lda	$1, fullPath_85114
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91571:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_91556:
	# done making normal call
	# making closure polycall
	lda	$1, strbindvar_r_getcwd_87174
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$2, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$2, $27
	jsr	$26, ($2), 1
code_91566:
	ldgp	$gp, ($26)
	mov	$0, $2
code_91558:
	# done making normal call
	# making closure call 
	lda	$1, fullPath_85114
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91567:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91559:
	# done making normal call
	# making closure call 
	lda	$1, strbindvar_r_mkRelative_87498
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_91561:
	# done making tail call
	br	$31, after_zeroone_90724
one_case_90723:
	# making closure call 
	lda	$1, fullPath_85114
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_91563:
	# done making tail call
after_zeroone_90724:
code_91565:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_realPath_code_89444

	.rdata
		# -------- label,sizes,reg
	.long code_91566
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91567
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91569
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_91571
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent OS_FileSys_setTime_code_89449
 # arguments : [$89451,$0] [$89452,$1] [$88250,$2] [$88251,$3] 
 # results    : [$90672,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_setTime_code_89449:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_91572:
funtop_90645:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91573
code_91574:
	jsr	$26, GCFromML
gc_check_91573:
sumarm_90657:
	bne	$3, sumarm_90658
code_91576:
	lda	$3, ($31)
	# making closure call 
	lda	$1, strbindvar_r_utime_87544
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_91578:
	# done making tail call
	br	$31, after_sum_90654
sumarm_90658:
	ldl	$1, ($3)
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $3
	addl	$13, 12, $13
	# done allocating 2 record
	# making closure call 
	lda	$1, strbindvar_r_utime_87544
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_91581:
	# done making tail call
	br	$31, after_sum_90654
sumarm_90673:
after_sum_90654:
code_91584:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_setTime_code_89449

	.rdata
		# -------- label,sizes,reg
	.long gc_check_91573
	.long 0x00000805
	.long 0x0000000c
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent OS_FileSys_cvt_code_89454
 # arguments : [$89456,$0] [$89457,$1] [$85294,$2] 
 # results    : [$90636,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
OS_FileSys_cvt_code_89454:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_91587:
funtop_90623:
sumarm_90631:
	bne	$2, sumarm_90632
code_91588:
	lda	$0, ($31)
	br	$31, after_sum_90628
sumarm_90632:
	cmpeq	$2, 1, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_90637
code_91591:
	lda	$0, 1($31)
	br	$31, after_sum_90628
sumarm_90637:
	lda	$0, 2($31)
	br	$31, after_sum_90628
sumarm_90641:
after_sum_90628:
code_91595:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_cvt_code_89454

	.rdata
	.text
 	.align 3
	.ent OS_FileSys_access_code_89459
 # arguments : [$89461,$0] [$89462,$1] [$88264,$2] [$88265,$3] 
 # results    : [$90622,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_access_code_89459:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
	stl	$3, 8($sp)
code_91596:
funtop_90594:
	lda	$2, cvt_85293
	# making closure call 
	lda	$1, _87616
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91606:
	ldgp	$gp, ($26)
	mov	$0, $1
code_91598:
	# done making normal call
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91604:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91599:
	# done making normal call
	# making closure call 
	lda	$1, strbindvar_r_access_87608
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_91601:
	# done making tail call
code_91603:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_access_code_89459

	.rdata
		# -------- label,sizes,reg
	.long code_91604
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_91606
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent OS_FileSys_tmpName_code_89464
 # arguments : [$89466,$0] [$89467,$1] 
 # results    : [$90593,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_tmpName_code_89464:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_91607:
funtop_90588:
	# making external call
	lda	$16, 256($31)
	lda	$27, posix_os_tmpname
	jsr	$26, save_regs_MLtoC
	jsr	$26, posix_os_tmpname
code_91611:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_91608:
	# done making external call
code_91610:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_tmpName_code_89464

	.rdata
		# -------- label,sizes,reg
	.long code_91611
	.long 0x00000805
	.long 0x00000e00
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent OS_FileSys_vars_eq_0_code_89469
 # arguments : [$89471,$0] [$89472,$1] [$88303,$2] [$88304,$3] 
 # results    : [$90587,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_vars_eq_0_code_89469:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
	stl	$3, 8($sp)
code_91612:
funtop_90526:
sumarm_90535:
sumarm_90543:
	ldl	$25, 12($sp)
	ldl	$2, ($25)
	ldl	$25, 8($sp)
	ldl	$3, ($25)
	# making closure call 
	lda	$1, PLUSEword_87663
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91624:
	ldgp	$gp, ($26)
code_91614:
	# done making normal call
	bne	$0, one_case_90566
zero_case_90565:
	lda	$0, ($31)
	br	$31, after_zeroone_90567
one_case_90566:
	ldl	$25, 12($sp)
	ldl	$2, 4($25)
	ldl	$25, 8($sp)
	ldl	$3, 4($25)
	# making closure call 
	lda	$1, PLUSEword_87663
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_91618:
	# done making tail call
after_zeroone_90567:
	br	$31, after_sum_90540
sumarm_90544:
after_sum_90540:
	br	$31, after_sum_90532
sumarm_90536:
after_sum_90532:
code_91622:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_vars_eq_0_code_89469

	.rdata
		# -------- label,sizes,reg
	.long code_91624
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent OS_FileSys_fileId_code_89474
 # arguments : [$89476,$0] [$89477,$1] [$85336,$2] 
 # results    : [$90520,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_fileId_code_89474:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
code_91625:
funtop_90461:
	lda	$1, Posix_STR_c_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	# Proj_c at label FileSys_STR
	ldl	$0, 16($0)
	# Proj_c at label ST_STR
	ldl	$25, 44($0)
	stl	$25, 8($sp)
	# making closure call 
	lda	$1, strbindvar_r_stat_87251
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91646:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_91628:
	# done making normal call
	# making closure call 
	lda	$1, _88332
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91642:
	ldgp	$gp, ($26)
	mov	$0, $2
code_91630:
	# done making normal call
	# making closure call 
	lda	$1, _88342
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91643:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_91632:
	# done making normal call
	# making closure call 
	lda	$1, _88356
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91644:
	ldgp	$gp, ($26)
	mov	$0, $2
code_91634:
	# done making normal call
	# making closure call 
	lda	$1, _88366
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_91645:
	ldgp	$gp, ($26)
	mov	$0, $1
code_91636:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91637
code_91638:
	jsr	$26, GCFromML
gc_check_91637:
	# allocating 2-record
	lda	$0, 17($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
code_91641:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_fileId_code_89474

	.rdata
		# -------- label,sizes,reg
	.long code_91642
	.long 0x00001007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000310
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_91643
	.long 0x00001007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000310
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_91644
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_91645
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_91637
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_91646
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent OS_FileSys_hash_code_89479
 # arguments : [$89481,$0] [$89482,$1] [$85349,$2] 
 # results    : [$90460,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_hash_code_89479:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_91647:
funtop_90422:
sumarm_90430:
	ldl	$2, 4($0)
	ldl	$25, ($0)
	stl	$25, 8($sp)
	lda	$3, 16($31)
	# making closure call 
	lda	$1, LTLT_87717
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91656:
	ldgp	$gp, ($26)
	mov	$0, $2
code_91649:
	# done making normal call
	# making closure call 
	lda	$1, PLUS_87716
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 8($sp)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_91651:
	# done making tail call
	br	$31, after_sum_90427
sumarm_90431:
after_sum_90427:
code_91654:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_hash_code_89479

	.rdata
		# -------- label,sizes,reg
	.long code_91656
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent OS_FileSys_compare_code_89484
 # arguments : [$89486,$0] [$89487,$1] [$88382,$2] [$88383,$3] 
 # results    : [$90421,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_compare_code_89484:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
code_91657:
funtop_90312:
sumarm_90320:
	ldl	$25, 4($2)
	stl	$25, 20($sp)
	ldl	$25, ($2)
	stl	$25, 16($sp)
sumarm_90334:
	ldl	$25, 4($3)
	stl	$25, 12($sp)
	ldl	$25, ($3)
	stl	$25, 8($sp)
	# making closure call 
	lda	$1, LT_87736
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	ldl	$3, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91678:
	ldgp	$gp, ($26)
code_91659:
	# done making normal call
	bne	$0, one_case_90356
zero_case_90355:
	# making closure call 
	lda	$1, GT_87741
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	ldl	$3, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91679:
	ldgp	$gp, ($26)
code_91662:
	# done making normal call
	bne	$0, one_case_90372
zero_case_90371:
	# making closure call 
	lda	$1, LT_87736
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	ldl	$3, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91680:
	ldgp	$gp, ($26)
code_91665:
	# done making normal call
	bne	$0, one_case_90388
zero_case_90387:
	# making closure call 
	lda	$1, GT_87741
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	ldl	$3, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_91681:
	ldgp	$gp, ($26)
code_91668:
	# done making normal call
	bne	$0, one_case_90404
zero_case_90403:
	lda	$0, ($31)
	br	$31, after_zeroone_90405
one_case_90404:
	lda	$0, 1($31)
after_zeroone_90405:
	br	$31, after_zeroone_90389
one_case_90388:
	lda	$0, 2($31)
after_zeroone_90389:
	br	$31, after_zeroone_90373
one_case_90372:
	lda	$0, 1($31)
after_zeroone_90373:
	br	$31, after_zeroone_90357
one_case_90356:
	lda	$0, 2($31)
after_zeroone_90357:
	br	$31, after_sum_90331
sumarm_90335:
after_sum_90331:
	br	$31, after_sum_90317
sumarm_90321:
after_sum_90317:
code_91677:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_compare_code_89484

	.rdata
		# -------- label,sizes,reg
	.long code_91678
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_91679
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_91680
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_91681
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent OS_FileSys_main
 # arguments : 
 # results    : [$90311,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
OS_FileSys_main:
	.mask (1 << 26), -96
	.frame $sp, 96
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -96($sp)
	stq	$26, 0($sp)	# push_ret
code_91682:
funtop_89492:
	lda	$16, ($31)
	lda	$27, AssertMirrorPtrArray
	jsr	$26, save_regs_MLtoC
	jsr	$26, AssertMirrorPtrArray
code_92141:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_91683:
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91684
code_91685:
	jsr	$26, GCFromML
gc_check_91684:
	lda	$1, Posix_STR_c_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	# Proj_c at label FileSys_STR
	ldl	$1, 16($0)
	# Proj_c at label dirstream_TYC
	ldl	$5, 12($1)
	# Proj_c at label access_mode_TYC
	ldl	$0, 48($1)
	# Proj_c at label access_mode_sumarg_INT
	ldl	$4, 52($1)
	# Proj_c at label access_mode_sum_INT
	ldl	$3, 56($1)
	# allocating 1-record
	# done allocating 1 record
	# allocating 5-record
	lda	$2, 7977($31)
	stl	$2, ($13)
	stl	$5, 4($13)
	stl	$0, 8($13)
	stl	$4, 12($13)
	stl	$3, 16($13)
	lda	$2, record_89525
	stl	$2, 20($13)
	addl	$13, 4, $7
	addl	$13, 24, $13
	# done allocating 5 record
	lda	$3, 111($31)
	lda	$2, OS_FileSys_STR_c_INT
	stl	$3, -4($2)
	ldl	$2, 1080($12)
	ldl	$3, 1084($12)
	addl	$2, 12, $2
	cmple	$2, $3, $2
	bne	$2, afterMutateCheck_91692
code_91694:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_91692:
	lda	$6, OS_FileSys_STR_c_INT
	ldl	$5, 1092($12)
	ldl	$4, 1080($12)
	mov	$6, $3
	mov	$5, $2
	stl	$3, ($4)
	stl	$2, 4($4)
	addl	$6, $5, $2
	ldl	$2, ($2)
	stl	$2, 8($4)
	addl	$4, 12, $2
	stl	$2, 1080($12)
	addl	$6, $5, $2
	stl	$7, ($2)
	# Proj_c at label ST_STR
	ldl	$25, 44($1)
	stl	$25, 88($sp)
	# allocating 3-record
	addl	$13, 72, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91703
code_91704:
	jsr	$26, GCFromML
gc_check_91703:
	lda	$2, 1817($31)
	stl	$2, ($13)
	ldl	$25, 88($sp)
	stl	$25, 4($13)
	lda	$3, bool_TYC
	ldl	$2, 1092($12)
	addl	$3, $2, $2
	ldl	$2, ($2)
	stl	$2, 8($13)
	lda	$3, string_TYC
	ldl	$2, 1092($12)
	addl	$3, $2, $2
	ldl	$2, ($2)
	stl	$2, 12($13)
	addl	$13, 4, $25
	stl	$25, 20($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	lda	$2, 785($31)
	stl	$2, ($13)
	lda	$3, String_STR_c_INT
	ldl	$2, 1092($12)
	addl	$3, $2, $2
	ldl	$2, ($2)
	stl	$2, 4($13)
	lda	$2, record_89563
	stl	$2, 8($13)
	addl	$13, 4, $25
	stl	$25, 32($sp)
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$2, 1817($31)
	stl	$2, ($13)
	ldl	$25, 88($sp)
	stl	$25, 4($13)
	lda	$3, Position_STR_c_INT
	ldl	$2, 1092($12)
	addl	$3, $2, $2
	ldl	$2, ($2)
	stl	$2, 8($13)
	lda	$3, string_TYC
	ldl	$2, 1092($12)
	addl	$3, $2, $2
	ldl	$2, ($2)
	stl	$2, 12($13)
	addl	$13, 4, $25
	stl	$25, 28($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$3, Time_STR_c_INT
	ldl	$2, 1092($12)
	addl	$3, $2, $2
	ldl	$2, ($2)
	# Proj_c at label time_TYC
	ldl	$3, 4($2)
	# allocating 3-record
	lda	$2, 1817($31)
	stl	$2, ($13)
	ldl	$25, 88($sp)
	stl	$25, 4($13)
	stl	$3, 8($13)
	lda	$3, string_TYC
	ldl	$2, 1092($12)
	addl	$3, $2, $2
	ldl	$2, ($2)
	stl	$2, 12($13)
	addl	$13, 4, $25
	stl	$25, 24($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$2, 785($31)
	stl	$2, ($13)
	stl	$0, 4($13)
	stl	$0, 8($13)
	addl	$13, 4, $25
	stl	$25, 84($sp)
	addl	$13, 12, $13
	# done allocating 2 record
	# Proj_c at label dev_TYC
	ldl	$25, 36($1)
	stl	$25, 80($sp)
	# Proj_c at label ino_TYC
	ldl	$25, 40($1)
	stl	$25, 76($sp)
	lda	$1, Posix_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$25, 16($0)
	stl	$25, 72($sp)
	ldl	$25, 72($sp)
	ldl	$25, 44($25)
	stl	$25, 68($sp)
	ldl	$25, 72($sp)
	ldl	$25, 48($25)
	stl	$25, 64($sp)
	ldl	$25, 72($sp)
	ldl	$25, 52($25)
	stl	$25, 60($sp)
	ldl	$25, 72($sp)
	ldl	$25, 56($25)
	stl	$25, 56($sp)
	ldl	$25, 72($sp)
	ldl	$2, 60($25)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_chdir_87173
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 24, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91718
code_91720:
	subl	$13, 24, $at
	jsr	$26, GCFromML
afterMutateCheck_91718:
	lda	$1, strbindvar_r_chdir_87173
	ldl	$0, 1092($12)
	ldl	$5, 1080($12)
	mov	$1, $4
	mov	$0, $3
	stl	$4, ($5)
	stl	$3, 4($5)
	addl	$1, $0, $3
	ldl	$3, ($3)
	stl	$3, 8($5)
	addl	$5, 12, $3
	stl	$3, 1080($12)
	addl	$1, $0, $0
	stl	$2, ($0)
	ldl	$25, 72($sp)
	ldl	$2, 64($25)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_getcwd_87174
	stl	$1, -4($0)
	lda	$1, strbindvar_r_getcwd_87174
	ldl	$0, 1092($12)
	ldl	$5, 1080($12)
	mov	$1, $4
	mov	$0, $3
	stl	$4, ($5)
	stl	$3, 4($5)
	addl	$1, $0, $3
	ldl	$3, ($3)
	stl	$3, 8($5)
	addl	$5, 12, $3
	stl	$3, 1080($12)
	addl	$1, $0, $0
	stl	$2, ($0)
	ldl	$25, 72($sp)
	ldl	$0, 80($25)
	ldl	$5, 12($0)
	lda	$2, Posix_STR_c_INT
	ldl	$1, 1092($12)
	addl	$2, $1, $1
	ldl	$1, ($1)
	# Proj_c at label FileSys_STR
	ldl	$1, 16($1)
	# Proj_c at label S_STR
	ldl	$1, 16($1)
	# Proj_c at label mode_TYC
	ldl	$25, 4($1)
	stl	$25, 8($sp)
	ldl	$2, 24($0)
	lda	$3, Posix_STR_c_INT
	ldl	$1, 1092($12)
	addl	$3, $1, $1
	ldl	$1, ($1)
	# Proj_c at label FileSys_STR
	ldl	$1, 16($1)
	# Proj_c at label S_STR
	ldl	$1, 16($1)
	# Proj_c at label mode_TYC
	ldl	$25, 4($1)
	stl	$25, 12($sp)
	ldl	$1, 40($0)
	lda	$4, Posix_STR_c_INT
	ldl	$3, 1092($12)
	addl	$4, $3, $3
	ldl	$3, ($3)
	# Proj_c at label FileSys_STR
	ldl	$3, 16($3)
	# Proj_c at label S_STR
	ldl	$3, 16($3)
	# Proj_c at label mode_TYC
	ldl	$25, 4($3)
	stl	$25, 16($sp)
	ldl	$0, 56($0)
	# allocating 2-record
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_91740
code_91741:
	jsr	$26, GCFromML
gc_check_91740:
	lda	$4, 529($31)
	or	$31, 3, $at
	ldl	$25, 16($sp)
	cmpult	$at, $25, $3
	sll	$3, 8, $3
	addl	$3, $31, $3
	or	$3, $4, $4
	stl	$4, ($13)
	or	$31, 3, $at
	ldl	$25, 16($sp)
	cmpult	$at, $25, $3
	stl	$0, 4($13)
	lda	$0, ($31)
	stl	$0, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$4, 529($31)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $3
	sll	$3, 8, $3
	addl	$3, $31, $3
	or	$3, $4, $4
	stl	$4, ($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $3
	stl	$1, 4($13)
	stl	$0, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$3, 529($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $1
	sll	$1, 8, $1
	addl	$1, $31, $1
	or	$1, $3, $3
	stl	$3, ($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $1
	stl	$2, 4($13)
	stl	$0, 8($13)
	addl	$13, 4, $4
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, Posix_STR_c_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	# Proj_c at label FileSys_STR
	ldl	$0, 16($0)
	# Proj_c at label S_STR
	ldl	$0, 16($0)
	# Proj_c at label flags_TYC
	ldl	$3, ($0)
	lda	$1, 111($31)
	lda	$0, reify_89355
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91748
code_91750:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_91748:
	lda	$2, reify_89355
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $7
	mov	$1, $6
	stl	$7, ($0)
	stl	$6, 4($0)
	addl	$2, $1, $6
	ldl	$6, ($6)
	stl	$6, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	# making closure call 
	ldl	$3, ($5)
	ldl	$0, 4($5)
	ldl	$1, 8($5)
	mov	$4, $2
	mov	$3, $27
	jsr	$26, ($3), 1
code_92124:
	ldgp	$gp, ($26)
	mov	$0, $2
code_91759:
	# done making normal call
	lda	$1, reify_89355
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	or	$31, 3, $at
	cmpult	$at, $0, $3
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 24, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91764
code_91766:
	subl	$13, 24, $at
	jsr	$26, GCFromML
afterMutateCheck_91764:
	beq	$3, else_case_89747
code_91768:
	lda	$1, 111($31)
	lda	$0, mode777_87237
	stl	$1, -4($0)
	lda	$1, mode777_87237
	ldl	$0, 1092($12)
	ldl	$5, 1080($12)
	mov	$1, $4
	mov	$0, $3
	stl	$4, ($5)
	stl	$3, 4($5)
	addl	$1, $0, $3
	ldl	$3, ($3)
	stl	$3, 8($5)
	addl	$5, 12, $3
	stl	$3, 1080($12)
	addl	$1, $0, $0
	stl	$2, ($0)
	br	$31, after_ite_89748
else_case_89747:
	lda	$1, 9($31)
	lda	$0, mode777_87237
	stl	$1, -4($0)
	lda	$1, 23($31)
	lda	$0, mode777_87237
	stl	$1, 4($0)
	lda	$0, mode777_87237
	stl	$2, ($0)
after_ite_89748:
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$2, 32($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_mkdir_87238
	stl	$1, -4($0)
	lda	$1, strbindvar_r_mkdir_87238
	ldl	$0, 1092($12)
	ldl	$5, 1080($12)
	mov	$1, $4
	mov	$0, $3
	stl	$4, ($5)
	stl	$3, 4($5)
	addl	$1, $0, $3
	ldl	$3, ($3)
	stl	$3, 8($5)
	addl	$5, 12, $3
	stl	$3, 1080($12)
	addl	$1, $0, $0
	stl	$2, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, mkDir_85080
	# done allocating 1 closures
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$25, 44($0)
	stl	$25, 52($sp)
	lda	$3, 256($31)
	# making closure call 
	lda	$1, o_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92136:
	ldgp	$gp, ($26)
	stl	$0, 20($sp)
code_91790:
	# done making normal call
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$0, 88($0)
	ldl	$25, ($0)
	stl	$25, 48($sp)
	ldl	$25, 48($sp)
	ldl	$6, ($25)
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$0, 88($0)
	ldl	$5, 4($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_stat_87251
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91795
code_91797:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_91795:
	lda	$4, strbindvar_r_stat_87251
	ldl	$3, 1092($12)
	ldl	$2, 1080($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1080($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, strbindvar_r_stat_87251
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$3, ($0)
	# making closure call 
	ldl	$25, 20($sp)
	ldl	$4, ($25)
	ldl	$25, 20($sp)
	ldl	$0, 4($25)
	ldl	$25, 20($sp)
	ldl	$1, 8($25)
	mov	$6, $2
	mov	$4, $27
	jsr	$26, ($4), 1
code_92125:
	ldgp	$gp, ($26)
	stl	$0, 44($sp)
code_91807:
	# done making normal call
	ldl	$25, 48($sp)
	ldl	$2, 20($25)
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$0, 88($0)
	ldl	$3, 8($0)
	# making closure call 
	ldl	$25, 20($sp)
	ldl	$4, ($25)
	ldl	$25, 20($sp)
	ldl	$0, 4($25)
	ldl	$25, 20($sp)
	ldl	$1, 8($25)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92137:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91808:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, isLink_87957
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 36, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91813
code_91815:
	subl	$13, 36, $at
	jsr	$26, GCFromML
afterMutateCheck_91813:
	lda	$2, isLink_87957
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$3, 56($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_readlink_87268
	stl	$1, -4($0)
	lda	$2, strbindvar_r_readlink_87268
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	lda	$1, OS_Path_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, 24($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_toString_87273
	stl	$1, -4($0)
	lda	$2, strbindvar_r_toString_87273
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	lda	$1, List_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, 48($0)
	lda	$1, record_89558
	lda	$3, 256($31)
	# making closure call 
	ldl	$4, ($2)
	ldl	$0, 4($2)
	ldl	$2, 8($2)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92126:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91842:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _87281
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91847
code_91849:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_91847:
	lda	$2, _87281
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	lda	$1, LibFail_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$1, 4($0)
	lda	$2, string_89875
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_92127:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91859:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _87402
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 24, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91864
code_91866:
	subl	$13, 24, $at
	jsr	$26, GCFromML
afterMutateCheck_91864:
	lda	$2, _87402
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	lda	$1, OS_Path_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, 20($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_fromString_87411
	stl	$1, -4($0)
	lda	$2, strbindvar_r_fromString_87411
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	lda	$1, List_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$0, 52($0)
	lda	$2, string_TYC
	ldl	$1, 1092($12)
	addl	$2, $1, $1
	ldl	$1, ($1)
	lda	$3, 256($31)
	# making closure call 
	ldl	$4, ($0)
	ldl	$5, 4($0)
	ldl	$2, 8($0)
	mov	$5, $0
	mov	$4, $27
	jsr	$26, ($4), 1
code_92128:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91886:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _87422
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91891
code_91893:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_91891:
	lda	$2, _87422
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, mkPath_85118
	# done allocating 1 closures
	# allocating 3 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$3, gotoRoot_85121
	# allocating 2-record
	# done allocating 2 record
	# allocating 3-record
	# done allocating 3 record
	lda	$2, expandLink_85120
	# allocating 2-record
	# done allocating 2 record
	# allocating 3-record
	# done allocating 3 record
	lda	$1, walkPath_85119
	lda	$0, walkPath_85119
	stl	$0, 8($3)
	# allocating 2-record
	# done allocating 2 record
	lda	$0, record_89949
	stl	$0, 8($2)
	# allocating 2-record
	# done allocating 2 record
	lda	$0, record_89955
	stl	$0, 8($1)
	# done allocating 3 closures
	lda	$3, 256($31)
	# making closure call 
	lda	$1, before_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 32($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92129:
	ldgp	$gp, ($26)
	mov	$0, $3
code_91903:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _87461
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 36, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91908
code_91910:
	subl	$13, 36, $at
	jsr	$26, GCFromML
afterMutateCheck_91908:
	lda	$2, _87461
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, fullPath_85114
	# done allocating 1 closures
	lda	$1, OS_Path_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, 84($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_isAbsolute_87495
	stl	$1, -4($0)
	lda	$2, strbindvar_r_isAbsolute_87495
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	lda	$1, OS_Path_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, 80($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_mkRelative_87498
	stl	$1, -4($0)
	lda	$2, strbindvar_r_mkRelative_87498
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, realPath_85232
	# done allocating 1 closures
	lda	$3, 256($31)
	# making closure call 
	lda	$1, o_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 28($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92130:
	ldgp	$gp, ($26)
	mov	$0, $5
code_91938:
	# done making normal call
	ldl	$25, 48($sp)
	ldl	$2, 52($25)
	lda	$1, strbindvar_r_stat_87251
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$3, ($0)
	# making closure call 
	ldl	$4, ($5)
	ldl	$0, 4($5)
	ldl	$1, 8($5)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92138:
	ldgp	$gp, ($26)
	stl	$0, 40($sp)
code_91940:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, o_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 24($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92139:
	ldgp	$gp, ($26)
	mov	$0, $5
code_91942:
	# done making normal call
	ldl	$25, 48($sp)
	ldl	$2, 60($25)
	lda	$1, strbindvar_r_stat_87251
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$3, ($0)
	# making closure call 
	ldl	$4, ($5)
	ldl	$0, 4($5)
	ldl	$1, 8($5)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92140:
	ldgp	$gp, ($26)
	stl	$0, 36($sp)
code_91944:
	# done making normal call
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$0, 88($0)
	ldl	$3, 40($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_utime_87544
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 24, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91949
code_91951:
	subl	$13, 24, $at
	jsr	$26, GCFromML
afterMutateCheck_91949:
	lda	$2, strbindvar_r_utime_87544
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, setTime_85258
	# done allocating 1 closures
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$25, 40($0)
	stl	$25, 32($sp)
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$25, 48($0)
	stl	$25, 28($sp)
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$0, 88($0)
	ldl	$25, 16($0)
	stl	$25, 24($sp)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, cvt_85293
	# done allocating 1 closures
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$0, 88($0)
	ldl	$5, 20($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_access_87608
	stl	$1, -4($0)
	lda	$4, strbindvar_r_access_87608
	ldl	$3, 1092($12)
	ldl	$2, 1080($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1080($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, List_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$1, 68($0)
	lda	$3, 256($31)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 84($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_92131:
	ldgp	$gp, ($26)
	mov	$0, $5
code_91969:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _87616
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 24, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_91974
code_91976:
	subl	$13, 24, $at
	jsr	$26, GCFromML
afterMutateCheck_91974:
	lda	$4, _87616
	ldl	$3, 1092($12)
	ldl	$2, 1080($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1080($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, access_85288
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, tmpName_85304
	# done allocating 1 closures
	lda	$1, SysWord_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, ($0)
	lda	$1, 111($31)
	lda	$0, PLUSEword_87663
	stl	$1, -4($0)
	lda	$4, PLUSEword_87663
	ldl	$3, 1092($12)
	ldl	$2, 1080($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1080($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, vars_eq_0_85311
	# done allocating 1 closures
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$25, 72($0)
	stl	$25, 20($sp)
	ldl	$25, 48($sp)
	ldl	$4, 36($25)
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 88($sp)
	ldl	$2, 80($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_92132:
	ldgp	$gp, ($26)
	mov	$0, $5
code_91995:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _88332
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_92000
code_92002:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_92000:
	lda	$4, _88332
	ldl	$3, 1092($12)
	ldl	$2, 1080($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1080($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, SysWord_STR_c_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$2, ($0)
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 80($sp)
	ldl	$4, 20($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_92133:
	ldgp	$gp, ($26)
	mov	$0, $3
code_92013:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _88342
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_92018
code_92020:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_92018:
	lda	$2, _88342
	ldl	$1, 1092($12)
	ldl	$0, 1080($12)
	mov	$2, $5
	mov	$1, $4
	stl	$5, ($0)
	stl	$4, 4($0)
	addl	$2, $1, $4
	ldl	$4, ($4)
	stl	$4, 8($0)
	addl	$0, 12, $0
	stl	$0, 1080($12)
	addl	$2, $1, $0
	stl	$3, ($0)
	ldl	$25, 72($sp)
	ldl	$0, 88($25)
	ldl	$25, 84($0)
	stl	$25, 20($sp)
	ldl	$25, 48($sp)
	ldl	$4, 32($25)
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 88($sp)
	ldl	$2, 76($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_92134:
	ldgp	$gp, ($26)
	mov	$0, $2
code_92030:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _88356
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_92035
code_92037:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_92035:
	lda	$1, _88356
	ldl	$0, 1092($12)
	ldl	$5, 1080($12)
	mov	$1, $4
	mov	$0, $3
	stl	$4, ($5)
	stl	$3, 4($5)
	addl	$1, $0, $3
	ldl	$3, ($3)
	stl	$3, 8($5)
	addl	$5, 12, $3
	stl	$3, 1080($12)
	addl	$1, $0, $0
	stl	$2, ($0)
	lda	$1, SysWord_STR_c_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$2, ($0)
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 76($sp)
	ldl	$4, 20($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_92135:
	ldgp	$gp, ($26)
code_92048:
	# done making normal call
	lda	$2, 111($31)
	lda	$1, _88366
	stl	$2, -4($1)
	ldl	$1, 1080($12)
	ldl	$2, 1084($12)
	addl	$1, 60, $1
	cmple	$1, $2, $1
	bne	$1, afterMutateCheck_92053
code_92055:
	subl	$13, 60, $at
	jsr	$26, GCFromML
afterMutateCheck_92053:
	lda	$5, _88366
	ldl	$4, 1092($12)
	ldl	$3, 1080($12)
	mov	$5, $2
	mov	$4, $1
	stl	$2, ($3)
	stl	$1, 4($3)
	addl	$5, $4, $1
	ldl	$1, ($1)
	stl	$1, 8($3)
	addl	$3, 12, $1
	stl	$1, 1080($12)
	addl	$5, $4, $1
	stl	$0, ($1)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, fileId_85335
	# done allocating 1 closures
	lda	$1, SysWord_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$0, 72($0)
	lda	$2, 111($31)
	lda	$1, PLUS_87716
	stl	$2, -4($1)
	lda	$5, PLUS_87716
	ldl	$4, 1092($12)
	ldl	$3, 1080($12)
	mov	$5, $2
	mov	$4, $1
	stl	$2, ($3)
	stl	$1, 4($3)
	addl	$5, $4, $1
	ldl	$1, ($1)
	stl	$1, 8($3)
	addl	$3, 12, $1
	stl	$1, 1080($12)
	addl	$5, $4, $1
	stl	$0, ($1)
	lda	$1, SysWord_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$0, 60($0)
	lda	$2, 111($31)
	lda	$1, LTLT_87717
	stl	$2, -4($1)
	lda	$5, LTLT_87717
	ldl	$4, 1092($12)
	ldl	$3, 1080($12)
	mov	$5, $2
	mov	$4, $1
	stl	$2, ($3)
	stl	$1, 4($3)
	addl	$5, $4, $1
	ldl	$1, ($1)
	stl	$1, 8($3)
	addl	$3, 12, $1
	stl	$1, 1080($12)
	addl	$5, $4, $1
	stl	$0, ($1)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, hash_85348
	# done allocating 1 closures
	lda	$1, SysWord_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$0, 88($0)
	ldl	$0, 16($0)
	lda	$2, 111($31)
	lda	$1, LT_87736
	stl	$2, -4($1)
	lda	$5, LT_87736
	ldl	$4, 1092($12)
	ldl	$3, 1080($12)
	mov	$5, $2
	mov	$4, $1
	stl	$2, ($3)
	stl	$1, 4($3)
	addl	$5, $4, $1
	ldl	$1, ($1)
	stl	$1, 8($3)
	addl	$3, 12, $1
	stl	$1, 1080($12)
	addl	$5, $4, $1
	stl	$0, ($1)
	lda	$1, SysWord_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$0, 88($0)
	ldl	$0, 8($0)
	lda	$2, 111($31)
	lda	$1, GT_87741
	stl	$2, -4($1)
	lda	$5, GT_87741
	ldl	$4, 1092($12)
	ldl	$3, 1080($12)
	mov	$5, $2
	mov	$4, $1
	stl	$2, ($3)
	stl	$1, 4($3)
	addl	$5, $4, $1
	ldl	$1, ($1)
	stl	$1, 8($3)
	addl	$3, 12, $1
	stl	$1, 1080($12)
	addl	$5, $4, $1
	stl	$0, ($1)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, compare_85355
	# done allocating 1 closures
	# allocating 3-record
	addl	$13, 112, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_92100
code_92101:
	jsr	$26, GCFromML
gc_check_92100:
	lda	$0, 1817($31)
	stl	$0, ($13)
	lda	$0, fileId_85335
	stl	$0, 4($13)
	lda	$0, hash_85348
	stl	$0, 8($13)
	lda	$0, compare_85355
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 23-record
	lda	$1, -71($31)
	ldah	$1, -32768($1)
	stl	$1, ($13)
	ldl	$25, 68($sp)
	stl	$25, 4($13)
	ldl	$25, 64($sp)
	stl	$25, 8($13)
	ldl	$25, 60($sp)
	stl	$25, 12($13)
	ldl	$25, 56($sp)
	stl	$25, 16($13)
	lda	$2, strbindvar_r_chdir_87173
	ldl	$1, 1092($12)
	addl	$2, $1, $1
	ldl	$1, ($1)
	stl	$1, 20($13)
	lda	$2, strbindvar_r_getcwd_87174
	ldl	$1, 1092($12)
	addl	$2, $1, $1
	ldl	$1, ($1)
	stl	$1, 24($13)
	lda	$1, mkDir_85080
	stl	$1, 28($13)
	ldl	$25, 52($sp)
	stl	$25, 32($13)
	ldl	$25, 44($sp)
	stl	$25, 36($13)
	lda	$2, isLink_87957
	ldl	$1, 1092($12)
	addl	$2, $1, $1
	ldl	$1, ($1)
	stl	$1, 40($13)
	lda	$2, strbindvar_r_readlink_87268
	ldl	$1, 1092($12)
	addl	$2, $1, $1
	ldl	$1, ($1)
	stl	$1, 44($13)
	lda	$1, fullPath_85114
	stl	$1, 48($13)
	lda	$1, realPath_85232
	stl	$1, 52($13)
	ldl	$25, 40($sp)
	stl	$25, 56($13)
	ldl	$25, 36($sp)
	stl	$25, 60($13)
	lda	$1, setTime_85258
	stl	$1, 64($13)
	ldl	$25, 32($sp)
	stl	$25, 68($13)
	ldl	$25, 28($sp)
	stl	$25, 72($13)
	ldl	$25, 24($sp)
	stl	$25, 76($13)
	lda	$1, access_85288
	stl	$1, 80($13)
	lda	$1, tmpName_85304
	stl	$1, 84($13)
	lda	$1, vars_eq_0_85311
	stl	$1, 88($13)
	stl	$0, 92($13)
	addl	$13, 4, $5
	addl	$13, 96, $13
	# done allocating 23 record
	lda	$1, 111($31)
	lda	$0, OS_FileSys_STR_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_92111
code_92113:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_92111:
	lda	$4, OS_FileSys_STR_r_INT
	ldl	$3, 1092($12)
	ldl	$2, 1080($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1080($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$0, 256($31)
code_92123:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 96($sp)
	ret	$31, ($26), 1
	.end OS_FileSys_main

	.rdata
		# -------- label,sizes,reg
	.long code_92124
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x50015400
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92125
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55015400
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92126
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92127
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92128
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92129
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55405000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92130
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55401000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92131
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55555000
	.long 0x00001155
		# -------- label,sizes,reg
	.long code_92132
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55555400
	.long 0x00001155
		# -------- label,sizes,reg
	.long code_92133
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55555000
	.long 0x00001055
		# -------- label,sizes,reg
	.long code_92134
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x54555400
	.long 0x00000045
		# -------- label,sizes,reg
	.long code_92135
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x54555000
	.long 0x00000005
		# -------- label,sizes,reg
	.long code_92136
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x54015000
	.long 0x00001555
		# -------- label,sizes,reg
	.long gc_check_91684
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_91692
	.long 0x00003006
	.long 0x00000083
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_91703
	.long 0x00003006
	.long 0x00000003
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.long 0x00001000
		# -------- label,sizes,reg
	.long afterMutateCheck_91718
	.long 0x00003006
	.long 0x00000004
	.long 0x00000000
		# stacktrace
	.long 0x50015400
	.long 0x00001555
		# -------- label,sizes,reg
	.long gc_check_91740
	.long 0x0000300c
	.long 0x00000020
	.long 0x00000007
		# stacktrace
	.long 0x50015550
	.long 0x00001555
		# worddata
	.long 0x00000000
	.long 0x00000010
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long afterMutateCheck_91748
	.long 0x00003006
	.long 0x00000038
	.long 0x00000000
		# stacktrace
	.long 0x50015400
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91764
	.long 0x00003008
	.long 0x00000000
	.long 0x00000004
		# stacktrace
	.long 0x50015400
	.long 0x00001555
		# worddata
	.long 0x00000002
	.long reify_89355
		# -------- label,sizes,reg
	.long afterMutateCheck_91795
	.long 0x00003006
	.long 0x00000060
	.long 0x00000000
		# stacktrace
	.long 0x55015400
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92137
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91813
	.long 0x00003006
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91847
	.long 0x00003006
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91864
	.long 0x00003006
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91891
	.long 0x00003006
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x55415000
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91908
	.long 0x00003006
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x55405000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92138
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55401000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92139
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55500000
	.long 0x00001555
		# -------- label,sizes,reg
	.long code_92140
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x55500000
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91949
	.long 0x00003006
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x55540000
	.long 0x00001555
		# -------- label,sizes,reg
	.long afterMutateCheck_91974
	.long 0x00003006
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x55555000
	.long 0x00001155
		# -------- label,sizes,reg
	.long afterMutateCheck_92000
	.long 0x00003006
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x55555400
	.long 0x00001155
		# -------- label,sizes,reg
	.long afterMutateCheck_92018
	.long 0x00003006
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x55555000
	.long 0x00001055
		# -------- label,sizes,reg
	.long afterMutateCheck_92035
	.long 0x00003006
	.long 0x00000004
	.long 0x00000000
		# stacktrace
	.long 0x54555400
	.long 0x00000045
		# -------- label,sizes,reg
	.long afterMutateCheck_92053
	.long 0x00003006
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x54555000
	.long 0x00000005
		# -------- label,sizes,reg
	.long gc_check_92100
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x54555000
	.long 0x00000005
		# -------- label,sizes,reg
	.long afterMutateCheck_92111
	.long 0x00003006
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_92141
	.long 0x00003006
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.long 0x00000000
	.text
OS_FileSys_unit_CODE_END_VAL:
	.rdata
		# endgcinfo with filler for alignment
	.globl OS_FileSys_unit_GCTABLE_END_VAL
OS_FileSys_unit_GCTABLE_END_VAL:
	.long 0x00000000
	.sdata
	.align 3
	.sdata
	.align 3
	.globl OS_FileSys_unit_GLOBALS_BEGIN_VAL
OS_FileSys_unit_GLOBALS_BEGIN_VAL:
		# static record tag
	.long 0x00000009
record_89525:
	.long 0x00000008
		# Global
	.long 0x00000037
	.globl OS_FileSys_STR_c_INT
OS_FileSys_STR_c_INT:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000211
record_89558:
	.long 0x00000001
	.long 0x00000000
		# static record tag
	.long 0x00000011
record_89563:
	.long 0x00000005
	.long 0x00000000
		# Global
	.long 0x00000037
strbindvar_r_chdir_87173:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_getcwd_87174:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
reify_89355:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
mode777_87237:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_mkdir_87238:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
mkDir_85080:
	.long OS_FileSys_mkDir_code_89404
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
strbindvar_r_stat_87251:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
isLink_87957:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_readlink_87268:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_toString_87273:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000002
string_89828:
		# string size = 0
	# .ascii "" (zero length string)
		# Global
	.long 0x00000037
_87281:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000000a
string_89847:
		# string size = 1
	.ascii "."
	.align 2
	.long 0x00000012
string_89850:
		# string size = 2
	.ascii ".."
	.align 2
	.long 0x00000072
string_89875:
		# string size = 14
	.ascii "too many links"
	.align 2
		# Global
	.long 0x00000037
_87402:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_fromString_87411:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
_87422:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000000a
string_89914:
		# string size = 1
	.ascii "/"
	.align 2
		# static record tag
	.long 0x00000619
mkPath_85118:
	.long OS_FileSys_mkPath_code_89409
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
gotoRoot_85121:
	.long OS_FileSys_gotoRoot_code_89424
	.long 0x00000100
	.long walkPath_85119
		# static record tag
	.long 0x00000311
record_89931:
	.long walkPath_85119
	.long gotoRoot_85121
		# static record tag
	.long 0x00000619
expandLink_85120:
	.long OS_FileSys_expandLink_code_89419
	.long 0x00000100
	.long record_89931
		# static record tag
	.long 0x00000311
record_89940:
	.long mkPath_85118
	.long expandLink_85120
		# static record tag
	.long 0x00000619
walkPath_85119:
	.long OS_FileSys_walkPath_code_89414
	.long 0x00000100
	.long record_89940
		# static record tag
	.long 0x00000311
record_89949:
	.long walkPath_85119
	.long gotoRoot_85121
		# static record tag
	.long 0x00000311
record_89955:
	.long mkPath_85118
	.long expandLink_85120
		# Global
	.long 0x00000037
_87461:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
fullPath_85114:
	.long OS_FileSys_fullPath_code_89439
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
strbindvar_r_isAbsolute_87495:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_mkRelative_87498:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
realPath_85232:
	.long OS_FileSys_realPath_code_89444
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
strbindvar_r_utime_87544:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
setTime_85258:
	.long OS_FileSys_setTime_code_89449
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
cvt_85293:
	.long OS_FileSys_cvt_code_89454
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
strbindvar_r_access_87608:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
_87616:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
access_85288:
	.long OS_FileSys_access_code_89459
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
tmpName_85304:
	.long OS_FileSys_tmpName_code_89464
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
PLUSEword_87663:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
vars_eq_0_85311:
	.long OS_FileSys_vars_eq_0_code_89469
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
_88332:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
_88342:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
_88356:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
_88366:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
fileId_85335:
	.long OS_FileSys_fileId_code_89474
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
PLUS_87716:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
LTLT_87717:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
hash_85348:
	.long OS_FileSys_hash_code_89479
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
LT_87736:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
GT_87741:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
compare_85355:
	.long OS_FileSys_compare_code_89484
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl OS_FileSys_STR_r_INT
OS_FileSys_STR_r_INT:
	.long 0x00000102
	.long 0x00000102
		# Module closure
	.long 0x00000619
	.globl OS_FileSys_unit_closure
OS_FileSys_unit_closure:
	.long OS_FileSys_main
	.long 0x00000000
	.long 0x00000000
	.long 0x00000311
	.globl OS_FileSys_unit
OS_FileSys_unit:
	.long OS_FileSys_unit_closure
	.long OS_FileSys_unit_closure
	.globl OS_FileSys_unit_GLOBALS_END_VAL
OS_FileSys_unit_GLOBALS_END_VAL:
	.long 0x00000000
	.long 0 #filler

	.sdata
	.align 3
	.globl OS_FileSys_unit_TRACE_GLOBALS_BEGIN_VAL
OS_FileSys_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long OS_FileSys_STR_r_INT
	.long GT_87741
	.long LT_87736
	.long LTLT_87717
	.long PLUS_87716
	.long _88366
	.long _88356
	.long _88342
	.long _88332
	.long PLUSEword_87663
	.long _87616
	.long strbindvar_r_access_87608
	.long strbindvar_r_utime_87544
	.long strbindvar_r_mkRelative_87498
	.long strbindvar_r_isAbsolute_87495
	.long _87461
	.long _87422
	.long strbindvar_r_fromString_87411
	.long _87402
	.long _87281
	.long strbindvar_r_toString_87273
	.long strbindvar_r_readlink_87268
	.long isLink_87957
	.long strbindvar_r_stat_87251
	.long strbindvar_r_mkdir_87238
	.long mode777_87237
	.long reify_89355
	.long strbindvar_r_getcwd_87174
	.long strbindvar_r_chdir_87173
	.long OS_FileSys_STR_c_INT
	.globl OS_FileSys_unit_TRACE_GLOBALS_END_VAL
OS_FileSys_unit_TRACE_GLOBALS_END_VAL:
		# filler so label is defined
	.long 0x00000000
