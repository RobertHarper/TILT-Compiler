	.set noat
	.rdata
		# gcinfo
	.globl HashTableFn_unit_GCTABLE_BEGIN_VAL
HashTableFn_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl HashTableFn_unit_CODE_END_VAL
	.globl HashTableFn_unit_CODE_BEGIN_VAL
HashTableFn_unit_CODE_BEGIN_VAL:
	.text
 	.align 3
	.ent HashTableFn__code_10910
 # arguments : [$10912,$0] [$4472,$1] 
 # results    : [$15389,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
HashTableFn__code_10910:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15395:
funtop_15380:
	# allocating 1-record
	# done allocating 1 record
	lda	$0, record_15387
code_15397:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn__code_10910

	.rdata
	.text
 	.align 3
	.ent HashTableFn_functor_var_c_code_10905
 # arguments : [$10907,$0] [$4314,$1] 
 # results    : [$15377,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
HashTableFn_functor_var_c_code_10905:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $3
code_15398:
funtop_15351:
	addl	$13, 44, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15399
code_15400:
	jsr	$26, GCFromML
gc_check_15399:
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, HashTableFn__code_10910
	stl	$0, 4($13)
	stl	$3, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$3, 4($13)
	lda	$1, _5247
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$3, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_15404:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_functor_var_c_code_10905

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15399
	.long 0x00000805
	.long 0x01ff0ff8
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn__code_10921
 # arguments : [$10923,$0] [$5816,$1] 
 # results    : [$15350,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
HashTableFn__code_10921:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15405:
funtop_15341:
	# allocating 1-record
	# done allocating 1 record
	lda	$0, record_15348
code_15407:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn__code_10921

	.rdata
	.text
 	.align 3
	.ent HashTableFn_mkTable_inner_code_10947
 # arguments : [$10949,$0] [$10950,$1] [$7866,$2] [$7867,$3] 
 # results    : [$15335,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_mkTable_inner_code_10947:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$3, 8($sp)
code_15408:
funtop_15289:
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15437:
	ldgp	$gp, ($26)
	mov	$0, $17
code_15409:
	# done making normal call
	lda	$16, 1($31)
	lda	$0, 510($31)
	cmple	$16, $0, $0
	bne	$0, array_ptr_alloc_15307
code_15411:
	lda	$27, alloc_bigptrarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigptrarray
code_15435:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $9
code_15412:
	br	$31, array_ptr_aftert_15306
array_ptr_alloc_15307:
	sll	$16, 2, $0
	addl	$0, $31, $0
	sll	$0, 3, $1
	addl	$1, $31, $1
	or	$1, 5, $1
	lda	$0, 1($31)
	addl	$0, $16, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15414
code_15415:
	jsr	$26, GCFromML
gc_check_15414:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $9
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$16, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_15313
array_init_loopto_15314:
	addl	$9, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_15313:
	bge	$1, array_init_loopto_15314
array_ptr_aftert_15306:
	lda	$17, ($31)
	lda	$2, 1($31)
	# initializing int/ptr array start
	sll	$2, 2, $16
	addl	$16, $31, $16
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_15323
code_15421:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_15436:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_15422:
	br	$31, array_int_after_15322
array_int_small_15323:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15424
code_15425:
	jsr	$26, GCFromML
gc_check_15424:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_15330
array_init_loopto_15331:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_15330:
	bge	$1, array_init_loopto_15331
array_int_after_15322:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15430
code_15431:
	jsr	$26, GCFromML
gc_check_15430:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$9, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_15434:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_mkTable_inner_code_10947

	.rdata
		# -------- label,sizes,reg
	.long code_15435
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_15414
	.long 0x00000805
	.long 0x00020000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_15436
	.long 0x00000805
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_15424
	.long 0x00000805
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_15430
	.long 0x00000805
	.long 0x00000208
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_15437
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent HashTableFn_mkTable_r_code_10939
 # arguments : [$10941,$0] [$4487,$1] [$10942,$2] [$4488,$3] 
 # results    : [$15283,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_mkTable_r_code_10939:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_15438:
funtop_15258:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15439
code_15440:
	jsr	$26, GCFromML
gc_check_15439:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_alloc_5875
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_15449:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15443:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15444
code_15445:
	jsr	$26, GCFromML
gc_check_15444:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_mkTable_inner_code_10947
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15448:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_mkTable_r_code_10939

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15439
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_15444
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15449
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_look_code_10979
 # arguments : [$10981,$0] [$10982,$1] [$4536,$2] 
 # results    : [$15111,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_look_code_10979:
	.mask (1 << 26), -80
	.frame $sp, 80
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -80($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 60($sp)
	stl	$1, 56($sp)
code_15450:
funtop_15006:
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 60($sp)
	ldl	$25, ($25)
	stl	$25, 8($sp)
	# Proj_c at label var_poly_c_4509_INT
	ldl	$25, 60($sp)
	ldl	$25, 4($25)
	stl	$25, 12($sp)
	ldl	$25, 56($sp)
	ldl	$25, ($25)
	stl	$25, 52($sp)
	ldl	$25, 56($sp)
	ldl	$25, 4($25)
	stl	$25, 48($sp)
	ldl	$25, 56($sp)
	ldl	$25, 8($25)
	stl	$25, 28($sp)
	ldl	$25, 56($sp)
	ldl	$25, 12($25)
	stl	$25, 24($sp)
	ldl	$25, 56($sp)
	ldl	$1, 16($25)
	ldl	$25, 56($sp)
	ldl	$25, 20($25)
	stl	$25, 20($sp)
	ldl	$25, 56($sp)
	ldl	$25, 24($25)
	stl	$25, 64($sp)
	ldl	$25, 56($sp)
	ldl	$25, 28($25)
	stl	$25, 44($sp)
	ldl	$25, 56($sp)
	ldl	$25, 32($25)
	stl	$25, 40($sp)
	ldl	$25, 56($sp)
	ldl	$25, 36($25)
	stl	$25, 36($sp)
	ldl	$25, 56($sp)
	ldl	$25, 40($25)
	stl	$25, 32($sp)
	ldl	$25, 56($sp)
	ldl	$25, 44($25)
	stl	$25, 16($sp)
sumarm_15050:
	bne	$2, sumarm_15051
code_15451:
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 28($sp)
	ldl	$3, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15493:
	ldgp	$gp, ($26)
code_15452:
	# done making normal call
	addl	$13, 20, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15453
code_15454:
	jsr	$26, GCFromML
gc_check_15453:
	# allocating 4-record
	lda	$2, 2081($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $1
	sll	$1, 9, $1
	addl	$1, $31, $1
	or	$1, $2, $2
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $1
	sll	$1, 10, $1
	addl	$1, $31, $1
	or	$1, $2, $2
	stl	$2, ($13)
	ldl	$25, 32($sp)
	stl	$25, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $1
	ldl	$25, 40($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $1
	ldl	$25, 36($sp)
	stl	$25, 12($13)
	stl	$0, 16($13)
	addl	$13, 4, $4
	addl	$13, 20, $13
	# done allocating 4 record
	# making closure call 
	ldl	$25, 24($sp)
	ldl	$5, ($25)
	ldl	$25, 24($sp)
	ldl	$0, 4($25)
	ldl	$25, 24($sp)
	ldl	$1, 8($25)
	ldl	$2, 28($sp)
	ldl	$3, 16($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_15494:
	ldgp	$gp, ($26)
code_15456:
	# done making normal call
	# int sub start
	ldl	$25, 52($sp)
	ldl	$0, ($25)
	# int sub end
	addlv	$0, 1, $3
	trapb
	ldl	$0, 1076($12)
	ldl	$1, 1080($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_15460
code_15462:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_15460:
	ldl	$2, 1076($12)
	ldl	$1, 52($sp)
	lda	$0, ($31)
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	ldl	$25, 52($sp)
	stl	$3, ($25)
	# int sub start
	ldl	$25, 52($sp)
	ldl	$3, ($25)
	# int sub end
	# making closure call 
	ldl	$25, 20($sp)
	ldl	$4, ($25)
	ldl	$25, 20($sp)
	ldl	$0, 4($25)
	ldl	$25, 20($sp)
	ldl	$1, 8($25)
	ldl	$2, 48($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15492:
	ldgp	$gp, ($26)
code_15468:
	# done making normal call
	ldl	$0, 64($sp)
	br	$31, after_sum_15047
sumarm_15051:
	ldl	$25, ($2)
	stl	$25, 28($sp)
	ldl	$25, 4($2)
	stl	$25, 24($sp)
	ldl	$25, 8($2)
	stl	$25, 20($sp)
	ldl	$25, 12($2)
	stl	$25, 16($sp)
	# making closure call 
	lda	$1, PLUSEword_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 32($sp)
	ldl	$3, 28($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15496:
	ldgp	$gp, ($26)
code_15471:
	# done making normal call
	addl	$13, 20, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15472
code_15473:
	jsr	$26, GCFromML
gc_check_15472:
	bne	$0, one_case_15172
zero_case_15171:
	lda	$0, ($31)
	br	$31, after_zeroone_15173
one_case_15172:
	# making closure call 
	ldl	$25, 44($sp)
	ldl	$4, ($25)
	ldl	$25, 44($sp)
	ldl	$0, 4($25)
	ldl	$25, 44($sp)
	ldl	$1, 8($25)
	ldl	$2, 40($sp)
	ldl	$3, 24($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15495:
	ldgp	$gp, ($26)
code_15477:
	# done making normal call
	addl	$13, 20, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15478
code_15479:
	jsr	$26, GCFromML
gc_check_15478:
after_zeroone_15173:
	bne	$0, one_case_15188
zero_case_15187:
	# making direct call 
	ldl	$0, 60($sp)
	ldl	$1, 56($sp)
	ldl	$2, 16($sp)
	lda	$27, HashTableFn_look_code_10979
	jsr	$26, HashTableFn_look_code_10979
code_15497:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15482:
	# done making normal call
	addl	$13, 20, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15483
code_15484:
	jsr	$26, GCFromML
gc_check_15483:
sumarm_15206:
	bne	$2, sumarm_15207
code_15486:
	ldl	$0, 64($sp)
	br	$31, after_sum_15203
sumarm_15207:
nomatch_sum_15204:
	# allocating 4-record
	lda	$1, 2081($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 28($sp)
	stl	$25, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	ldl	$25, 20($sp)
	stl	$25, 12($13)
	stl	$2, 16($13)
	addl	$13, 4, $0
	addl	$13, 20, $13
	# done allocating 4 record
after_sum_15203:
	br	$31, after_zeroone_15189
one_case_15188:
	# allocating 4-record
	lda	$1, 2081($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 32($sp)
	stl	$25, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 40($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	ldl	$25, 36($sp)
	stl	$25, 12($13)
	ldl	$25, 16($sp)
	stl	$25, 16($13)
	addl	$13, 4, $0
	addl	$13, 20, $13
	# done allocating 4 record
after_zeroone_15189:
	br	$31, after_sum_15047
sumarm_15112:
after_sum_15047:
code_15491:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 80($sp)
	ret	$31, ($26), 1
	.end HashTableFn_look_code_10979

	.rdata
		# -------- label,sizes,reg
	.long code_15492
	.long 0x00002806
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.long 0x00000001
		# -------- label,sizes,reg
	.long code_15493
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x053c5450
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15453
	.long 0x0000280a
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x053c5450
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long afterMutateCheck_15460
	.long 0x00002806
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x05000400
	.long 0x00000001
		# -------- label,sizes,reg
	.long gc_check_15472
	.long 0x0000280e
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x507c3d50
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15478
	.long 0x0000280e
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x503c3d50
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15483
	.long 0x0000280a
	.long 0x00000004
	.long 0x00000000
		# stacktrace
	.long 0x00003c50
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15494
	.long 0x00002806
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x05000400
	.long 0x00000001
		# -------- label,sizes,reg
	.long code_15495
	.long 0x0000280e
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x503c3d50
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15496
	.long 0x0000280e
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x507c3d50
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15497
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00003c50
	.long 0x00000001
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_10969
 # arguments : [$10971,$0] [$10972,$1] [$7884,$2] [$7885,$3] 
 # results    : [$15005,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_10969:
	.mask (1 << 26), -80
	.frame $sp, 80
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -80($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$1, $4
	stl	$2, 64($sp)
	stl	$3, 60($sp)
code_15498:
funtop_14844:
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 8($sp)
	ldl	$25, ($25)
	stl	$25, 44($sp)
	# Proj_c at label var_poly_c_4509_INT
	ldl	$25, 8($sp)
	ldl	$25, 4($25)
	stl	$25, 40($sp)
	ldl	$0, ($4)
	ldl	$1, 4($4)
	ldl	$25, 8($4)
	stl	$25, 56($sp)
	ldl	$25, 12($4)
	stl	$25, 52($sp)
	ldl	$25, 16($4)
	stl	$25, 36($sp)
	ldl	$25, 20($4)
	stl	$25, 32($sp)
	ldl	$25, 24($4)
	stl	$25, 28($sp)
	ldl	$25, 28($4)
	stl	$25, 12($sp)
sumarm_14873:
	ldl	$25, 8($0)
	stl	$25, 24($sp)
	ldl	$25, ($0)
	stl	$25, 20($sp)
	# ptr sub start
	ldl	$25, 20($sp)
	ldl	$25, ($25)
	stl	$25, 48($sp)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 48($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15525:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15499:
	# done making normal call
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$3, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	ldl	$2, 64($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15518:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_15500:
	# done making normal call
	# making closure call 
	lda	$1, fromInt_5861
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15519:
	ldgp	$gp, ($26)
code_15502:
	# done making normal call
	subl	$0, 1, $3
	# making closure call 
	lda	$1, andb_5860
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15520:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15504:
	# done making normal call
	# making closure call 
	lda	$1, toIntX_5859
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15521:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15506:
	# done making normal call
	addl	$13, 80, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15507
code_15508:
	jsr	$26, GCFromML
gc_check_15507:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 44($sp)
	stl	$25, 4($13)
	ldl	$25, 40($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 12-record
	lda	$2, -159($31)
	ldah	$2, 1($2)
	ldl	$25, 8($sp)
	ldl	$1, ($25)
	or	$31, 3, $at
	cmpult	$at, $1, $1
	sll	$1, 16, $1
	addl	$1, $31, $1
	or	$1, $2, $2
	ldl	$25, 8($sp)
	ldl	$1, 4($25)
	or	$31, 3, $at
	cmpult	$at, $1, $1
	sll	$1, 17, $1
	addl	$1, $31, $1
	or	$1, $2, $2
	stl	$2, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 20($sp)
	stl	$25, 8($13)
	ldl	$25, 48($sp)
	stl	$25, 12($13)
	ldl	$25, 56($sp)
	stl	$25, 16($13)
	ldl	$25, 52($sp)
	stl	$25, 20($13)
	ldl	$25, 36($sp)
	stl	$25, 24($13)
	ldl	$25, 32($sp)
	stl	$25, 28($13)
	ldl	$25, 28($sp)
	stl	$25, 32($13)
	ldl	$25, 8($sp)
	ldl	$1, ($25)
	or	$31, 3, $at
	cmpult	$at, $1, $1
	ldl	$25, 64($sp)
	stl	$25, 36($13)
	ldl	$25, 8($sp)
	ldl	$1, 4($25)
	or	$31, 3, $at
	cmpult	$at, $1, $1
	ldl	$25, 60($sp)
	stl	$25, 40($13)
	ldl	$25, 12($sp)
	stl	$25, 44($13)
	ldl	$25, 16($sp)
	stl	$25, 48($13)
	addl	$13, 4, $2
	addl	$13, 52, $13
	# done allocating 12 record
	# allocating 3-record
	lda	$1, 1561($31)
	stl	$1, ($13)
	lda	$1, HashTableFn_look_code_10979
	stl	$1, 4($13)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $25
	stl	$25, 12($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	ldl	$25, 52($sp)
	ldl	$4, ($25)
	ldl	$25, 52($sp)
	ldl	$0, 4($25)
	ldl	$25, 52($sp)
	ldl	$1, 8($25)
	ldl	$2, 48($sp)
	ldl	$3, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15523:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15510:
	# done making normal call
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$3, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15522:
	ldgp	$gp, ($26)
	mov	$0, $4
code_15511:
	# done making normal call
sumarm_14990:
	bne	$4, sumarm_14991
code_15512:
	lda	$0, 256($31)
	br	$31, after_sum_14987
sumarm_14991:
nomatch_sum_14988:
	# making closure call 
	ldl	$25, 56($sp)
	ldl	$5, ($25)
	ldl	$25, 56($sp)
	ldl	$0, 4($25)
	ldl	$25, 56($sp)
	ldl	$1, 8($25)
	ldl	$2, 48($sp)
	ldl	$3, 16($sp)
	mov	$5, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 80($sp)
	jsr	$31, ($5), 1
code_15514:
	# done making tail call
after_sum_14987:
	br	$31, after_sum_14870
sumarm_14874:
after_sum_14870:
code_15517:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 80($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_10969

	.rdata
		# -------- label,sizes,reg
	.long code_15518
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0xd5555410
	.long 0x00000003
		# worddata
	.long 0x00000008
	.long 0x00000008
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15519
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0xd5555410
	.long 0x00000003
		# worddata
	.long 0x00000008
	.long 0x00000008
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15520
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0xd5555410
	.long 0x00000003
		# worddata
	.long 0x00000008
	.long 0x00000008
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15521
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0xd5555410
	.long 0x00000003
		# worddata
	.long 0x00000008
	.long 0x00000008
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15507
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0xd5555410
	.long 0x00000003
		# worddata
	.long 0x00000008
	.long 0x00000008
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15522
	.long 0x00002806
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x11000000
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15523
	.long 0x00002806
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x11000040
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15525
	.long 0x0000280a
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0xd5555450
	.long 0x00000003
		# worddata
	.long 0x00000008
	.long 0x00000008
	.long 0x00000004
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_insert_inner_code_10964
 # arguments : [$10966,$0] [$10967,$1] [$4513,$2] 
 # results    : [$14839,$0] 
 # destroys   :  $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_insert_inner_code_10964:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $10
code_15526:
funtop_14796:
	addl	$13, 64, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15527
code_15528:
	jsr	$26, GCFromML
gc_check_15527:
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$9, ($0)
	# Proj_c at label var_poly_c_4509_INT
	ldl	$2, 4($0)
	ldl	$8, ($1)
	ldl	$7, 4($1)
	ldl	$6, 8($1)
	ldl	$5, 12($1)
	ldl	$4, 16($1)
	ldl	$3, 20($1)
	ldl	$1, 24($1)
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$9, 4($13)
	stl	$2, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 8-record
	lda	$0, -191($31)
	ldah	$0, 1($0)
	stl	$0, ($13)
	stl	$10, 4($13)
	stl	$8, 8($13)
	stl	$7, 12($13)
	stl	$6, 16($13)
	stl	$5, 20($13)
	stl	$4, 24($13)
	stl	$3, 28($13)
	stl	$1, 32($13)
	addl	$13, 4, $1
	addl	$13, 36, $13
	# done allocating 8 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_10969
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15531:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_insert_inner_code_10964

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15527
	.long 0x00000805
	.long 0x01ff0c03
	.long 0x01ff0800
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_insert_r_code_10955
 # arguments : [$10957,$0] [$4509,$1] [$10958,$2] [$4510,$3] 
 # results    : [$14791,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_insert_r_code_10955:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 36($sp)
code_15532:
funtop_14693:
	# Proj_c at label type_5871_INT
	ldl	$25, ($0)
	stl	$25, 12($sp)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 4($0)
	stl	$25, 32($sp)
	ldl	$25, ($2)
	stl	$25, 28($sp)
	ldl	$25, 4($2)
	stl	$25, 24($sp)
	# start making constructor call
	lda	$1, type_5879
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, ($0)
	ldl	$0, 4($0)
	ldl	$1, 12($sp)
	ldl	$2, 36($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15554:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_15534:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15535
code_15536:
	jsr	$26, GCFromML
gc_check_15535:
	# done making constructor call
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	addl	$13, 4, $25
	stl	$25, 20($sp)
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, length_5923
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15555:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15539:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, update_5954
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15551:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_15541:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, sub_5963
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15552:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_15543:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_growTableIfNeeded_5992
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15553:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15545:
	# done making normal call
	addl	$13, 60, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15546
code_15547:
	jsr	$26, GCFromML
gc_check_15546:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 32($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 7-record
	lda	$0, 32569($31)
	stl	$0, ($13)
	ldl	$25, 16($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	ldl	$25, 8($sp)
	stl	$25, 12($13)
	stl	$1, 16($13)
	lda	$0, ($31)
	stl	$0, 20($13)
	ldl	$25, 28($sp)
	stl	$25, 24($13)
	ldl	$25, 24($sp)
	stl	$25, 28($13)
	addl	$13, 4, $1
	addl	$13, 32, $13
	# done allocating 7 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_insert_inner_code_10964
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15550:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_insert_r_code_10955

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15535
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055050
		# -------- label,sizes,reg
	.long code_15551
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055510
		# -------- label,sizes,reg
	.long code_15552
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055540
		# -------- label,sizes,reg
	.long code_15553
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055150
		# -------- label,sizes,reg
	.long gc_check_15546
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00055150
		# -------- label,sizes,reg
	.long code_15554
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055040
		# -------- label,sizes,reg
	.long code_15555
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055410
	.text
 	.align 3
	.ent HashTableFn_look_inner_code_11059
 # arguments : [$11061,$0] [$11062,$1] [$4649,$2] 
 # results    : [$14601,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_look_inner_code_11059:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 40($sp)
	stl	$1, 36($sp)
	mov	$2, $1
code_15556:
funtop_14568:
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 40($sp)
	ldl	$25, ($25)
	stl	$25, 8($sp)
	# Proj_c at label var_poly_c_4622_INT
	ldl	$25, 40($sp)
	ldl	$25, 4($25)
	stl	$25, 12($sp)
	ldl	$25, 36($sp)
	ldl	$25, ($25)
	stl	$25, 32($sp)
	ldl	$25, 36($sp)
	ldl	$25, 4($25)
	stl	$25, 28($sp)
	ldl	$25, 36($sp)
	ldl	$0, 8($25)
	ldl	$25, 36($sp)
	ldl	$2, 12($25)
sumarm_14596:
	bne	$1, sumarm_14597
code_15557:
	mov	$0, $26
	ldl	$27, ($15)
	ldl	$sp, 4($15)
	ldl	$0, 1092($12)
	addl	$sp, $0, $sp
	mov	$27, $27
	jsr	$31, ($27), 1
	lda	$0, ($31)
	br	$31, after_sum_14593
sumarm_14597:
	ldl	$3, ($1)
	ldl	$25, 4($1)
	stl	$25, 24($sp)
	ldl	$25, 8($1)
	stl	$25, 20($sp)
	ldl	$25, 12($1)
	stl	$25, 16($sp)
	# making closure call 
	lda	$1, PLUSEword_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15572:
	ldgp	$gp, ($26)
code_15561:
	# done making normal call
	bne	$0, one_case_14667
zero_case_14666:
	lda	$0, ($31)
	br	$31, after_zeroone_14668
one_case_14667:
	# making closure call 
	ldl	$25, 28($sp)
	ldl	$4, ($25)
	ldl	$25, 28($sp)
	ldl	$0, 4($25)
	ldl	$25, 28($sp)
	ldl	$1, 8($25)
	ldl	$2, 32($sp)
	ldl	$3, 24($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15571:
	ldgp	$gp, ($26)
code_15564:
	# done making normal call
after_zeroone_14668:
	bne	$0, one_case_14683
zero_case_14682:
	# making direct call 
	ldl	$1, 16($sp)
	br	$31, funtop_14568
code_15566:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_14684
one_case_14683:
	ldl	$0, 20($sp)
after_zeroone_14684:
	br	$31, after_sum_14593
sumarm_14602:
after_sum_14593:
code_15570:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_look_inner_code_11059

	.rdata
		# -------- label,sizes,reg
	.long code_15571
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00140d40
		# worddata
	.long 0x00000000
	.long 0x0000000c
		# -------- label,sizes,reg
	.long code_15572
	.long 0x0000180b
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00177d50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11049
 # arguments : [$11051,$0] [$11052,$1] [$4628,$2] 
 # results    : [$14567,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11049:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	stl	$2, 44($sp)
code_15573:
funtop_14451:
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 8($sp)
	ldl	$25, ($25)
	stl	$25, 40($sp)
	# Proj_c at label var_poly_c_4622_INT
	ldl	$25, 8($sp)
	ldl	$25, 4($25)
	stl	$25, 12($sp)
	ldl	$25, ($1)
	stl	$25, 36($sp)
	ldl	$0, 4($1)
	ldl	$25, 8($1)
	stl	$25, 32($sp)
	ldl	$25, 12($1)
	stl	$25, 28($sp)
	ldl	$1, 16($1)
sumarm_14474:
	ldl	$25, 4($0)
	stl	$25, 24($sp)
	ldl	$0, ($0)
	# ptr sub start
	ldl	$25, ($0)
	stl	$25, 20($sp)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 44($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15596:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15574:
	# done making normal call
	# making closure call 
	ldl	$25, 32($sp)
	ldl	$3, ($25)
	ldl	$25, 32($sp)
	ldl	$0, 4($25)
	ldl	$25, 32($sp)
	ldl	$1, 8($25)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15590:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15575:
	# done making normal call
	# making closure call 
	lda	$1, fromInt_5861
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15591:
	ldgp	$gp, ($26)
code_15577:
	# done making normal call
	subl	$0, 1, $3
	# making closure call 
	lda	$1, andb_5860
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15592:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15579:
	# done making normal call
	# making closure call 
	lda	$1, toIntX_5859
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15593:
	ldgp	$gp, ($26)
	mov	$0, $3
code_15581:
	# done making normal call
	addl	$13, 48, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15582
code_15583:
	jsr	$26, GCFromML
gc_check_15582:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 40($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 4-record
	lda	$1, 1569($31)
	ldl	$25, 8($sp)
	ldl	$0, ($25)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	sll	$0, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 8($sp)
	ldl	$0, ($25)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	ldl	$25, 44($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	ldl	$25, 24($sp)
	stl	$25, 12($13)
	ldl	$25, 16($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_look_inner_code_11059
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $25
	stl	$25, 16($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	ldl	$25, 28($sp)
	ldl	$4, ($25)
	ldl	$25, 28($sp)
	ldl	$0, 4($25)
	ldl	$25, 28($sp)
	ldl	$1, 8($25)
	ldl	$2, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15595:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15585:
	# done making normal call
	# making closure call 
	ldl	$25, 16($sp)
	ldl	$3, ($25)
	ldl	$25, 16($sp)
	ldl	$0, 4($25)
	ldl	$25, 16($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	jsr	$31, ($3), 1
code_15586:
	# done making tail call
	br	$31, after_sum_14471
sumarm_14475:
after_sum_14471:
code_15589:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11049

	.rdata
		# -------- label,sizes,reg
	.long code_15590
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d45450
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15591
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d45450
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15592
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d45450
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15593
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d45450
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15582
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d45450
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15595
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_15596
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d55450
		# worddata
	.long 0x00000004
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_lookup_inner_code_11044
 # arguments : [$11046,$0] [$11047,$1] [$4626,$2] 
 # results    : [$14450,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_lookup_inner_code_11044:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $3
	mov	$1, $0
	mov	$2, $9
code_15597:
funtop_14395:
	addl	$13, 52, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15598
code_15599:
	jsr	$26, GCFromML
gc_check_15598:
	# Proj_c at label type_8029_INT
	ldl	$2, ($3)
	# Proj_c at label type_5871_INT
	ldl	$1, 4($3)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$8, 8($3)
	# Proj_c at label var_poly_c_4622_INT
	ldl	$4, 12($3)
	ldl	$7, ($0)
	ldl	$6, 4($0)
	ldl	$5, 8($0)
	ldl	$3, 12($0)
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$8, 4($13)
	stl	$4, 8($13)
	addl	$13, 4, $4
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 5-record
	lda	$0, 7977($31)
	stl	$0, ($13)
	stl	$7, 4($13)
	stl	$9, 8($13)
	stl	$6, 12($13)
	stl	$5, 16($13)
	stl	$3, 20($13)
	addl	$13, 4, $3
	addl	$13, 24, $13
	# done allocating 5 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11049
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$3, vararg_INT
	ldl	$0, 1088($12)
	addl	$3, $0, $0
	ldl	$3, ($0)
	ldl	$5, ($3)
	ldl	$0, 4($3)
	ldl	$3, 8($3)
	mov	$5, $27
	jsr	$26, ($5), 1
code_15605:
	ldgp	$gp, ($26)
code_15602:
	# done making normal call
code_15604:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_lookup_inner_code_11044

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15598
	.long 0x00000805
	.long 0x00000209
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15605
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_lookup_r_code_11035
 # arguments : [$11037,$0] [$4622,$1] [$11038,$2] [$4623,$3] 
 # results    : [$14390,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_lookup_r_code_11035:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 32($sp)
code_15606:
funtop_14327:
	# Proj_c at label type_5871_INT
	ldl	$25, ($0)
	stl	$25, 28($sp)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 4($0)
	stl	$25, 24($sp)
	ldl	$25, ($2)
	stl	$25, 20($sp)
	ldl	$25, 4($2)
	stl	$25, 16($sp)
	# start making constructor call
	lda	$1, type_5879
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, ($0)
	ldl	$0, 4($0)
	ldl	$1, 28($sp)
	ldl	$2, 32($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15620:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_15608:
	# done making constructor call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, length_5923
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15618:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_15610:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, sub_5963
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15619:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15612:
	# done making normal call
	addl	$13, 56, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15613
code_15614:
	jsr	$26, GCFromML
gc_check_15613:
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 3873($31)
	stl	$0, ($13)
	ldl	$25, 32($sp)
	stl	$25, 4($13)
	ldl	$25, 28($sp)
	stl	$25, 8($13)
	ldl	$25, 24($sp)
	stl	$25, 12($13)
	ldl	$25, 32($sp)
	stl	$25, 16($13)
	addl	$13, 4, $2
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 4-record
	lda	$0, 3873($31)
	stl	$0, ($13)
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	ldl	$25, 16($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_lookup_inner_code_11044
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15617:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_lookup_r_code_11035

	.rdata
		# -------- label,sizes,reg
	.long code_15618
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00015540
		# -------- label,sizes,reg
	.long code_15619
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00015510
		# -------- label,sizes,reg
	.long gc_check_15613
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00015510
		# -------- label,sizes,reg
	.long code_15620
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00015500
	.text
 	.align 3
	.ent HashTableFn_look_inner_code_11121
 # arguments : [$11123,$0] [$11124,$1] [$4714,$2] 
 # results    : [$14216,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_look_inner_code_11121:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 44($sp)
	stl	$1, 40($sp)
	mov	$2, $1
code_15621:
funtop_14181:
	# Proj_c at label type_6297_INT
	ldl	$25, 44($sp)
	ldl	$25, ($25)
	stl	$25, 36($sp)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 44($sp)
	ldl	$25, 4($25)
	stl	$25, 8($sp)
	# Proj_c at label var_poly_c_4687_INT
	ldl	$25, 44($sp)
	ldl	$25, 8($25)
	stl	$25, 12($sp)
	ldl	$25, 40($sp)
	ldl	$25, ($25)
	stl	$25, 32($sp)
	ldl	$25, 40($sp)
	ldl	$25, 4($25)
	stl	$25, 28($sp)
	ldl	$25, 40($sp)
	ldl	$0, 8($25)
	ldl	$25, 40($sp)
	ldl	$2, 12($25)
sumarm_14212:
	bne	$1, sumarm_14213
code_15622:
	br	$31, after_sum_14209
sumarm_14213:
	ldl	$3, ($1)
	ldl	$25, 4($1)
	stl	$25, 24($sp)
	ldl	$25, 8($1)
	stl	$25, 20($sp)
	ldl	$25, 12($1)
	stl	$25, 16($sp)
	# making closure call 
	lda	$1, PLUSEword_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15653:
	ldgp	$gp, ($26)
code_15625:
	# done making normal call
	addl	$13, 8, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15626
code_15627:
	jsr	$26, GCFromML
gc_check_15626:
	bne	$0, one_case_14282
zero_case_14281:
	lda	$0, ($31)
	br	$31, after_zeroone_14283
one_case_14282:
	# making closure call 
	ldl	$25, 28($sp)
	ldl	$4, ($25)
	ldl	$25, 28($sp)
	ldl	$0, 4($25)
	ldl	$25, 28($sp)
	ldl	$1, 8($25)
	ldl	$2, 32($sp)
	ldl	$3, 24($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15652:
	ldgp	$gp, ($26)
code_15631:
	# done making normal call
	addl	$13, 8, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15632
code_15633:
	jsr	$26, GCFromML
gc_check_15632:
after_zeroone_14283:
	bne	$0, one_case_14298
zero_case_14297:
	# making direct call 
	ldl	$1, 16($sp)
	br	$31, funtop_14181
code_15636:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_14299
one_case_14298:
	ldl	$25, 36($sp)
	ldl	$1, 16($25)
	cmple	$1, 4, $0
	bne	$0, dynamic_box_14318
code_15639:
	cmple	$1, 255, $0
	bne	$0, dynamic_nobox_14319
code_15641:
	ldl	$1, ($1)
	cmpeq	$1, 12, $0
	bne	$0, dynamic_box_14318
code_15643:
	cmpeq	$1, 4, $0
	bne	$0, dynamic_box_14318
code_15645:
	cmpeq	$1, 8, $0
	bne	$0, dynamic_box_14318
dynamic_nobox_14319:
	ldl	$0, 20($sp)
	br	$31, xinject_sum_dyn_after_14315
dynamic_box_14318:
	# allocating 1-record
	lda	$1, 9($31)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	sll	$0, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	addl	$13, 4, $0
	addl	$13, 8, $13
	# done allocating 1 record
xinject_sum_dyn_after_14315:
after_zeroone_14299:
	br	$31, after_sum_14209
sumarm_14217:
after_sum_14209:
code_15651:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_look_inner_code_11121

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15626
	.long 0x0000180b
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00577d50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15632
	.long 0x00001807
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00540d40
		# worddata
	.long 0x00000000
	.long 0x0000000c
		# -------- label,sizes,reg
	.long code_15652
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00540d40
		# worddata
	.long 0x00000000
	.long 0x0000000c
		# -------- label,sizes,reg
	.long code_15653
	.long 0x0000180b
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00577d50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11112
 # arguments : [$11114,$0] [$11115,$1] [$4693,$2] 
 # results    : [$14180,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11112:
	.mask (1 << 26), -64
	.frame $sp, 64
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -64($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$1, $3
	stl	$2, 44($sp)
code_15654:
funtop_14060:
	# Proj_c at label type_6297_INT
	ldl	$25, 8($sp)
	ldl	$25, ($25)
	stl	$25, 40($sp)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 8($sp)
	ldl	$25, 4($25)
	stl	$25, 36($sp)
	# Proj_c at label var_poly_c_4687_INT
	ldl	$25, 8($sp)
	ldl	$25, 8($25)
	stl	$25, 32($sp)
	ldl	$25, ($3)
	stl	$25, 28($sp)
	ldl	$0, 4($3)
	ldl	$1, 8($3)
	ldl	$25, 12($3)
	stl	$25, 24($sp)
	ldl	$25, 16($3)
	stl	$25, 48($sp)
	ldl	$25, 20($3)
	stl	$25, 12($sp)
sumarm_14088:
	ldl	$0, ($0)
	# ptr sub start
	ldl	$25, ($0)
	stl	$25, 20($sp)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15677:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15655:
	# done making normal call
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$3, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	ldl	$2, 44($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15671:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_15656:
	# done making normal call
	# making closure call 
	lda	$1, fromInt_5861
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15672:
	ldgp	$gp, ($26)
code_15658:
	# done making normal call
	subl	$0, 1, $3
	# making closure call 
	lda	$1, andb_5860
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15673:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15660:
	# done making normal call
	# making closure call 
	lda	$1, toIntX_5859
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15674:
	ldgp	$gp, ($26)
	mov	$0, $3
code_15662:
	# done making normal call
	addl	$13, 52, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15663
code_15664:
	jsr	$26, GCFromML
gc_check_15663:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	ldl	$25, 40($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	ldl	$25, 32($sp)
	stl	$25, 12($13)
	addl	$13, 4, $2
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 4-record
	lda	$1, 1569($31)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	sll	$0, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	ldl	$25, 44($sp)
	stl	$25, 4($13)
	ldl	$25, 28($sp)
	stl	$25, 8($13)
	ldl	$25, 24($sp)
	stl	$25, 12($13)
	ldl	$25, 12($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_look_inner_code_11121
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $25
	stl	$25, 12($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	ldl	$25, 48($sp)
	ldl	$4, ($25)
	ldl	$25, 48($sp)
	ldl	$0, 4($25)
	ldl	$25, 48($sp)
	ldl	$1, 8($25)
	ldl	$2, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15676:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15666:
	# done making normal call
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$3, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 64($sp)
	jsr	$31, ($3), 1
code_15667:
	# done making tail call
	br	$31, after_sum_14085
sumarm_14089:
after_sum_14085:
code_15670:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 64($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11112

	.rdata
		# -------- label,sizes,reg
	.long code_15671
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01d55410
		# worddata
	.long 0x00000008
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15672
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01d55410
		# worddata
	.long 0x00000008
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15673
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01d55410
		# worddata
	.long 0x00000008
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15674
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01d55410
		# worddata
	.long 0x00000008
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15663
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01d55410
		# worddata
	.long 0x00000008
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15676
	.long 0x00002005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_15677
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01d55450
		# worddata
	.long 0x00000008
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_find_inner_code_11107
 # arguments : [$11109,$0] [$11110,$1] [$4691,$2] 
 # results    : [$14059,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_find_inner_code_11107:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $3
	mov	$1, $0
	mov	$2, $11
code_15678:
funtop_13986:
	addl	$13, 60, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15679
code_15680:
	jsr	$26, GCFromML
gc_check_15679:
	# Proj_c at label type_8110_INT
	ldl	$2, ($3)
	# Proj_c at label type_6297_INT
	ldl	$10, 4($3)
	# Proj_c at label type_5871_INT
	ldl	$1, 8($3)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$9, 12($3)
	# Proj_c at label var_poly_c_4687_INT
	ldl	$4, 16($3)
	ldl	$8, ($0)
	ldl	$7, 4($0)
	ldl	$6, 8($0)
	ldl	$5, 12($0)
	ldl	$3, 16($0)
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$10, 4($13)
	stl	$9, 8($13)
	stl	$4, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 6-record
	lda	$0, 16177($31)
	stl	$0, ($13)
	stl	$8, 4($13)
	stl	$11, 8($13)
	stl	$7, 12($13)
	stl	$6, 16($13)
	stl	$5, 20($13)
	stl	$3, 24($13)
	addl	$13, 4, $3
	addl	$13, 28, $13
	# done allocating 6 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11112
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$3, vararg_INT
	ldl	$0, 1088($12)
	addl	$3, $0, $0
	ldl	$3, ($0)
	ldl	$5, ($3)
	ldl	$0, 4($3)
	ldl	$3, 8($3)
	mov	$5, $27
	jsr	$26, ($5), 1
code_15686:
	ldgp	$gp, ($26)
code_15683:
	# done making normal call
code_15685:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_find_inner_code_11107

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15679
	.long 0x00000805
	.long 0x00000809
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15686
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_find_r_code_11098
 # arguments : [$11100,$0] [$4687,$1] [$11101,$2] [$4688,$3] 
 # results    : [$13981,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_find_r_code_11098:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 40($sp)
code_15687:
funtop_13894:
	# Proj_c at label type_5871_INT
	ldl	$25, ($0)
	stl	$25, 36($sp)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 4($0)
	stl	$25, 32($sp)
	ldl	$25, ($2)
	stl	$25, 28($sp)
	ldl	$25, 4($2)
	stl	$25, 24($sp)
	# start making constructor call
	lda	$1, option_TYC
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 40($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_15707:
	ldgp	$gp, ($26)
	stl	$0, 20($sp)
code_15689:
	# done making constructor call
	# start making constructor call
	lda	$1, type_5879
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, ($0)
	ldl	$0, 4($0)
	ldl	$1, 36($sp)
	ldl	$2, 40($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15703:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15691:
	# done making constructor call
	# start making constructor call
	lda	$1, option_sum_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 40($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_15704:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_15693:
	# done making constructor call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, length_5923
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15705:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_15695:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, sub_5963
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15706:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15697:
	# done making normal call
	addl	$13, 64, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15698
code_15699:
	jsr	$26, GCFromML
gc_check_15698:
	# allocating 1 closures
	# allocating 5-record
	lda	$0, 7977($31)
	stl	$0, ($13)
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	ldl	$25, 36($sp)
	stl	$25, 12($13)
	ldl	$25, 32($sp)
	stl	$25, 16($13)
	ldl	$25, 40($sp)
	stl	$25, 20($13)
	addl	$13, 4, $2
	addl	$13, 24, $13
	# done allocating 5 record
	# allocating 5-record
	lda	$0, 7977($31)
	stl	$0, ($13)
	ldl	$25, 28($sp)
	stl	$25, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	lda	$0, ($31)
	stl	$0, 12($13)
	stl	$1, 16($13)
	ldl	$25, 24($sp)
	stl	$25, 20($13)
	addl	$13, 4, $1
	addl	$13, 24, $13
	# done allocating 5 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_find_inner_code_11107
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15702:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_find_r_code_11098

	.rdata
		# -------- label,sizes,reg
	.long code_15703
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00155400
		# -------- label,sizes,reg
	.long code_15704
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00155500
		# -------- label,sizes,reg
	.long code_15705
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00155540
		# -------- label,sizes,reg
	.long code_15706
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00155450
		# -------- label,sizes,reg
	.long gc_check_15698
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00155450
		# -------- label,sizes,reg
	.long code_15707
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00155000
	.text
 	.align 3
	.ent HashTableFn_look_inner_code_11190
 # arguments : [$11192,$0] [$11193,$1] [$4781,$2] 
 # results    : [$13758,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_look_inner_code_11190:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 44($sp)
	stl	$1, 40($sp)
	mov	$2, $1
code_15708:
funtop_13725:
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 44($sp)
	ldl	$25, ($25)
	stl	$25, 8($sp)
	# Proj_c at label var_poly_c_4752_INT
	ldl	$25, 44($sp)
	ldl	$25, 4($25)
	stl	$25, 12($sp)
	ldl	$25, 40($sp)
	ldl	$25, ($25)
	stl	$25, 36($sp)
	ldl	$25, 40($sp)
	ldl	$25, 4($25)
	stl	$25, 32($sp)
	ldl	$25, 40($sp)
	ldl	$0, 8($25)
	ldl	$25, 40($sp)
	ldl	$2, 12($25)
sumarm_13753:
	bne	$1, sumarm_13754
code_15709:
	mov	$0, $26
	ldl	$27, ($15)
	ldl	$sp, 4($15)
	ldl	$0, 1092($12)
	addl	$sp, $0, $sp
	mov	$27, $27
	jsr	$31, ($27), 1
	lda	$0, ($31)
	br	$31, after_sum_13750
sumarm_13754:
	ldl	$25, ($1)
	stl	$25, 28($sp)
	ldl	$25, 4($1)
	stl	$25, 24($sp)
	ldl	$25, 8($1)
	stl	$25, 20($sp)
	ldl	$25, 12($1)
	stl	$25, 16($sp)
	# making closure call 
	lda	$1, PLUSEword_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 28($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15733:
	ldgp	$gp, ($26)
code_15713:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15714
code_15715:
	jsr	$26, GCFromML
gc_check_15714:
	bne	$0, one_case_13825
zero_case_13824:
	lda	$0, ($31)
	br	$31, after_zeroone_13826
one_case_13825:
	# making closure call 
	ldl	$25, 32($sp)
	ldl	$4, ($25)
	ldl	$25, 32($sp)
	ldl	$0, 4($25)
	ldl	$25, 32($sp)
	ldl	$1, 8($25)
	ldl	$2, 36($sp)
	ldl	$3, 24($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15732:
	ldgp	$gp, ($26)
code_15719:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15720
code_15721:
	jsr	$26, GCFromML
gc_check_15720:
after_zeroone_13826:
	bne	$0, one_case_13841
zero_case_13840:
	# making direct call 
	ldl	$0, 44($sp)
	ldl	$1, 40($sp)
	ldl	$2, 16($sp)
	lda	$27, HashTableFn_look_inner_code_11190
	jsr	$26, HashTableFn_look_inner_code_11190
code_15734:
	ldgp	$gp, ($26)
code_15724:
	# done making normal call
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15725
code_15726:
	jsr	$26, GCFromML
gc_check_15725:
	ldl	$3, ($0)
	ldl	$2, 4($0)
	# allocating 4-record
	lda	$1, 2081($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 28($sp)
	stl	$25, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	ldl	$25, 20($sp)
	stl	$25, 12($13)
	stl	$2, 16($13)
	addl	$13, 4, $2
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 2-record
	lda	$1, 529($31)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	sll	$0, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	stl	$3, 4($13)
	stl	$2, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	br	$31, after_zeroone_13842
one_case_13841:
	# allocating 2-record
	lda	$1, 529($31)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	sll	$0, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	or	$31, 3, $at
	ldl	$25, 12($sp)
	cmpult	$at, $25, $0
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
after_zeroone_13842:
	br	$31, after_sum_13750
sumarm_13759:
after_sum_13750:
code_15731:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_look_inner_code_11190

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15714
	.long 0x0000180b
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x005d3d50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15720
	.long 0x00001809
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00503d50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15725
	.long 0x00001809
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00003c50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15732
	.long 0x00001809
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00503d50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15733
	.long 0x0000180b
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x005d3d50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15734
	.long 0x00001809
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00003c50
		# worddata
	.long 0x00000000
	.long 0x0000000c
	.long 0x00000000
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11180
 # arguments : [$11182,$0] [$11183,$1] [$4758,$2] 
 # results    : [$13724,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11180:
	.mask (1 << 26), -64
	.frame $sp, 64
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -64($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$1, $0
	stl	$2, 36($sp)
code_15735:
funtop_13581:
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 8($sp)
	ldl	$25, ($25)
	stl	$25, 32($sp)
	# Proj_c at label var_poly_c_4752_INT
	ldl	$25, 8($sp)
	ldl	$25, 4($25)
	stl	$25, 12($sp)
	ldl	$25, ($0)
	stl	$25, 28($sp)
	ldl	$2, 4($0)
	ldl	$1, 8($0)
	ldl	$25, 12($0)
	stl	$25, 52($sp)
	ldl	$25, 16($0)
	stl	$25, 48($sp)
	ldl	$25, 20($0)
	stl	$25, 16($sp)
sumarm_13606:
	ldl	$25, 4($2)
	stl	$25, 24($sp)
	ldl	$0, ($2)
	ldl	$25, 8($2)
	stl	$25, 44($sp)
	# ptr sub start
	ldl	$25, ($0)
	stl	$25, 40($sp)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 40($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15771:
	ldgp	$gp, ($26)
	stl	$0, 20($sp)
code_15736:
	# done making normal call
	# making closure call 
	ldl	$25, 16($sp)
	ldl	$3, ($25)
	ldl	$25, 16($sp)
	ldl	$0, 4($25)
	ldl	$25, 16($sp)
	ldl	$1, 8($25)
	ldl	$2, 36($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15764:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15737:
	# done making normal call
	# making closure call 
	lda	$1, fromInt_5861
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15765:
	ldgp	$gp, ($26)
code_15739:
	# done making normal call
	subl	$0, 1, $3
	# making closure call 
	lda	$1, andb_5860
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15766:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15741:
	# done making normal call
	# making closure call 
	lda	$1, toIntX_5859
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15767:
	ldgp	$gp, ($26)
	stl	$0, 20($sp)
code_15743:
	# done making normal call
	addl	$13, 48, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15744
code_15745:
	jsr	$26, GCFromML
gc_check_15744:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 32($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 4-record
	lda	$1, 1569($31)
	ldl	$25, 8($sp)
	ldl	$0, ($25)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	sll	$0, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 8($sp)
	ldl	$0, ($25)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	ldl	$25, 36($sp)
	stl	$25, 4($13)
	ldl	$25, 28($sp)
	stl	$25, 8($13)
	ldl	$25, 24($sp)
	stl	$25, 12($13)
	ldl	$25, 16($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_look_inner_code_11190
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $25
	stl	$25, 16($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	ldl	$25, 52($sp)
	ldl	$4, ($25)
	ldl	$25, 52($sp)
	ldl	$0, 4($25)
	ldl	$25, 52($sp)
	ldl	$1, 8($25)
	ldl	$2, 40($sp)
	ldl	$3, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15770:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15747:
	# done making normal call
	# making closure call 
	ldl	$25, 16($sp)
	ldl	$3, ($25)
	ldl	$25, 16($sp)
	ldl	$0, 4($25)
	ldl	$25, 16($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15768:
	ldgp	$gp, ($26)
code_15748:
	# done making normal call
	ldl	$25, ($0)
	stl	$25, 16($sp)
	ldl	$4, 4($0)
	# making closure call 
	ldl	$25, 48($sp)
	ldl	$5, ($25)
	ldl	$25, 48($sp)
	ldl	$0, 4($25)
	ldl	$25, 48($sp)
	ldl	$1, 8($25)
	ldl	$2, 40($sp)
	ldl	$3, 20($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_15769:
	ldgp	$gp, ($26)
code_15749:
	# done making normal call
	# int sub start
	ldl	$25, 44($sp)
	ldl	$0, ($25)
	# int sub end
	sublv	$0, 1, $3
	trapb
	ldl	$0, 1076($12)
	ldl	$1, 1080($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_15753
code_15755:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_15753:
	ldl	$2, 1076($12)
	ldl	$1, 44($sp)
	lda	$0, ($31)
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	ldl	$25, 44($sp)
	stl	$3, ($25)
	ldl	$0, 16($sp)
	br	$31, after_sum_13603
sumarm_13607:
after_sum_13603:
code_15763:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 64($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11180

	.rdata
		# -------- label,sizes,reg
	.long code_15764
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x055d5050
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15765
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x055d5050
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15766
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x055d5050
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15767
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x055d5050
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_15744
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x055d5050
		# worddata
	.long 0x00000004
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15768
	.long 0x00002005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01500040
		# -------- label,sizes,reg
	.long code_15769
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00400340
		# worddata
	.long 0x00000000
	.long 0x0000000c
		# -------- label,sizes,reg
	.long afterMutateCheck_15753
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00400340
		# worddata
	.long 0x00000000
	.long 0x0000000c
		# -------- label,sizes,reg
	.long code_15770
	.long 0x00002005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x01500140
		# -------- label,sizes,reg
	.long code_15771
	.long 0x00002007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x055d5150
		# worddata
	.long 0x00000004
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_remove_inner_code_11175
 # arguments : [$11177,$0] [$11178,$1] [$4756,$2] 
 # results    : [$13580,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_remove_inner_code_11175:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $3
	mov	$1, $0
	mov	$2, $10
code_15772:
funtop_13522:
	addl	$13, 56, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15773
code_15774:
	jsr	$26, GCFromML
gc_check_15773:
	# Proj_c at label type_8193_INT
	ldl	$2, ($3)
	# Proj_c at label type_5871_INT
	ldl	$1, 4($3)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$9, 8($3)
	# Proj_c at label var_poly_c_4752_INT
	ldl	$4, 12($3)
	ldl	$8, ($0)
	ldl	$7, 4($0)
	ldl	$6, 8($0)
	ldl	$5, 12($0)
	ldl	$3, 16($0)
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$9, 4($13)
	stl	$4, 8($13)
	addl	$13, 4, $4
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 6-record
	lda	$0, 16177($31)
	stl	$0, ($13)
	stl	$8, 4($13)
	stl	$10, 8($13)
	stl	$7, 12($13)
	stl	$6, 16($13)
	stl	$5, 20($13)
	stl	$3, 24($13)
	addl	$13, 4, $3
	addl	$13, 28, $13
	# done allocating 6 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11180
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$3, vararg_INT
	ldl	$0, 1088($12)
	addl	$3, $0, $0
	ldl	$3, ($0)
	ldl	$5, ($3)
	ldl	$0, 4($3)
	ldl	$3, 8($3)
	mov	$5, $27
	jsr	$26, ($5), 1
code_15780:
	ldgp	$gp, ($26)
code_15777:
	# done making normal call
code_15779:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_remove_inner_code_11175

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15773
	.long 0x00000805
	.long 0x00000409
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15780
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_remove_r_code_11166
 # arguments : [$11168,$0] [$4752,$1] [$11169,$2] [$4753,$3] 
 # results    : [$13517,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_remove_r_code_11166:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 36($sp)
code_15781:
funtop_13442:
	# Proj_c at label type_5871_INT
	ldl	$25, ($0)
	stl	$25, 32($sp)
	# Proj_c at label funct_arg_c_5808_INT
	ldl	$25, 4($0)
	stl	$25, 28($sp)
	ldl	$25, ($2)
	stl	$25, 24($sp)
	ldl	$25, 4($2)
	stl	$25, 20($sp)
	# start making constructor call
	lda	$1, type_5879
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, ($0)
	ldl	$0, 4($0)
	ldl	$1, 32($sp)
	ldl	$2, 36($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15798:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_15783:
	# done making constructor call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, length_5923
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15795:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_15785:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, sub_5963
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15796:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_15787:
	# done making normal call
	lda	$3, 256($31)
	# making closure call 
	lda	$1, update_5954
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15797:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15789:
	# done making normal call
	addl	$13, 60, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15790
code_15791:
	jsr	$26, GCFromML
gc_check_15790:
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 3873($31)
	stl	$0, ($13)
	ldl	$25, 36($sp)
	stl	$25, 4($13)
	ldl	$25, 32($sp)
	stl	$25, 8($13)
	ldl	$25, 28($sp)
	stl	$25, 12($13)
	ldl	$25, 36($sp)
	stl	$25, 16($13)
	addl	$13, 4, $2
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 5-record
	lda	$0, 7977($31)
	stl	$0, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	ldl	$25, 8($sp)
	stl	$25, 12($13)
	stl	$1, 16($13)
	ldl	$25, 20($sp)
	stl	$25, 20($13)
	addl	$13, 4, $1
	addl	$13, 24, $13
	# done allocating 5 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_remove_inner_code_11175
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15794:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end HashTableFn_remove_r_code_11166

	.rdata
		# -------- label,sizes,reg
	.long code_15795
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055500
		# -------- label,sizes,reg
	.long code_15796
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055540
		# -------- label,sizes,reg
	.long code_15797
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055450
		# -------- label,sizes,reg
	.long gc_check_15790
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00055450
		# -------- label,sizes,reg
	.long code_15798
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00055400
	.text
 	.align 3
	.ent HashTableFn_numItems_inner_code_11239
 # arguments : [$11241,$0] [$11242,$1] [$4837,$2] 
 # results    : [$13441,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
HashTableFn_numItems_inner_code_11239:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15799:
funtop_13423:
sumarm_13431:
	ldl	$0, 8($2)
	# int sub start
	ldl	$0, ($0)
	# int sub end
	br	$31, after_sum_13428
sumarm_13432:
after_sum_13428:
code_15802:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_numItems_inner_code_11239

	.rdata
	.text
 	.align 3
	.ent HashTableFn_numItems_r_code_11232
 # arguments : [$11234,$0] [$4833,$1] [$11235,$2] [$4834,$3] 
 # results    : [$13421,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
HashTableFn_numItems_r_code_11232:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15803:
funtop_13413:
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, record_13420
	# done allocating 1 closures
code_15805:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_numItems_r_code_11232

	.rdata
	.text
 	.align 3
	.ent HashTableFn_listItems_inner_code_11253
 # arguments : [$11255,$0] [$11256,$1] [$4853,$2] 
 # results    : [$13412,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_listItems_inner_code_11253:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15806:
funtop_13384:
sumarm_13392:
	ldl	$3, 8($2)
	ldl	$0, ($2)
	# ptr sub start
	ldl	$2, ($0)
	# ptr sub end
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_15807:
	# done making tail call
	br	$31, after_sum_13389
sumarm_13393:
after_sum_13389:
code_15810:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_listItems_inner_code_11253

	.rdata
	.text
 	.align 3
	.ent HashTableFn_listItems_r_code_11244
 # arguments : [$11246,$0] [$4849,$1] [$11247,$2] [$4850,$3] 
 # results    : [$13378,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_listItems_r_code_11244:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_15812:
funtop_13353:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15813
code_15814:
	jsr	$26, GCFromML
gc_check_15813:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_listItems_6570
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_15823:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15817:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15818
code_15819:
	jsr	$26, GCFromML
gc_check_15818:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_listItems_inner_code_11253
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15822:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_listItems_r_code_11244

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15813
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_15818
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15823
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_listItemsi_inner_code_11271
 # arguments : [$11273,$0] [$11274,$1] [$4876,$2] 
 # results    : [$13352,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_listItemsi_inner_code_11271:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15824:
funtop_13324:
sumarm_13332:
	ldl	$3, 8($2)
	ldl	$0, ($2)
	# ptr sub start
	ldl	$2, ($0)
	# ptr sub end
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_15825:
	# done making tail call
	br	$31, after_sum_13329
sumarm_13333:
after_sum_13329:
code_15828:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_listItemsi_inner_code_11271

	.rdata
	.text
 	.align 3
	.ent HashTableFn_listItemsi_r_code_11261
 # arguments : [$11263,$0] [$4872,$1] [$11264,$2] [$4873,$3] 
 # results    : [$13318,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_listItemsi_r_code_11261:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_15830:
funtop_13293:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15831
code_15832:
	jsr	$26, GCFromML
gc_check_15831:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_listItemsi_6605
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_15841:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15835:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15836
code_15837:
	jsr	$26, GCFromML
gc_check_15836:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_listItemsi_inner_code_11271
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15840:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_listItemsi_r_code_11261

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15831
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_15836
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15841
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11294
 # arguments : [$11296,$0] [$11297,$1] [$4952,$2] 
 # results    : [$13292,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11294:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_15842:
funtop_13208:
	ldl	$2, ($1)
	ldl	$1, 4($1)
sumarm_13220:
	ldl	$25, ($0)
	stl	$25, 16($sp)
	ldl	$25, 8($0)
	stl	$25, 12($sp)
	ldl	$25, 4($0)
	stl	$25, 8($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15874:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15843:
	# done making normal call
	# ptr sub start
	ldl	$25, 16($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15871:
	ldgp	$gp, ($26)
	mov	$0, $17
code_15844:
	# done making normal call
	lda	$16, 1($31)
	lda	$0, 510($31)
	cmple	$16, $0, $0
	bne	$0, array_ptr_alloc_13256
code_15846:
	lda	$27, alloc_bigptrarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigptrarray
code_15872:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $9
code_15847:
	br	$31, array_ptr_aftert_13255
array_ptr_alloc_13256:
	sll	$16, 2, $0
	addl	$0, $31, $0
	sll	$0, 3, $1
	addl	$1, $31, $1
	or	$1, 5, $1
	lda	$0, 1($31)
	addl	$0, $16, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15849
code_15850:
	jsr	$26, GCFromML
gc_check_15849:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $9
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$16, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_13262
array_init_loopto_13263:
	addl	$9, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_13262:
	bge	$1, array_init_loopto_13263
array_ptr_aftert_13255:
	# int sub start
	ldl	$25, 12($sp)
	ldl	$17, ($25)
	# int sub end
	lda	$2, 1($31)
	# initializing int/ptr array start
	sll	$2, 2, $16
	addl	$16, $31, $16
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_13274
code_15856:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_15873:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_15857:
	br	$31, array_int_after_13273
array_int_small_13274:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15859
code_15860:
	jsr	$26, GCFromML
gc_check_15859:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_13281
array_init_loopto_13282:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_13281:
	bge	$1, array_init_loopto_13282
array_int_after_13273:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15865
code_15866:
	jsr	$26, GCFromML
gc_check_15865:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$9, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_13217
sumarm_13221:
after_sum_13217:
code_15870:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11294

	.rdata
		# -------- label,sizes,reg
	.long code_15871
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_15872
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long gc_check_15849
	.long 0x00001005
	.long 0x00020000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_15873
	.long 0x00001005
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_15859
	.long 0x00001005
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_15865
	.long 0x00001005
	.long 0x00000208
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_15874
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000150
	.text
 	.align 3
	.ent HashTableFn_mapi_inner_code_11289
 # arguments : [$11291,$0] [$11292,$1] [$4950,$2] 
 # results    : [$13202,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
HashTableFn_mapi_inner_code_11289:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15875:
funtop_13191:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15876
code_15877:
	jsr	$26, GCFromML
gc_check_15876:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11294
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15880:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_mapi_inner_code_11289

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15876
	.long 0x00000805
	.long 0x01ff0ffe
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_mapi_r_code_11279
 # arguments : [$11281,$0] [$4945,$1] [$11282,$2] [$4946,$3] 
 # results    : [$13185,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_mapi_r_code_11279:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_15881:
funtop_13156:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15882
code_15883:
	jsr	$26, GCFromML
gc_check_15882:
	# Proj_c at label 'b_TYV
	ldl	$2, 4($1)
	# Proj_c at label 'a_TYV
	ldl	$1, ($1)
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $5
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_mapi_6719
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_15892:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15886:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15887
code_15888:
	jsr	$26, GCFromML
gc_check_15887:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_mapi_inner_code_11289
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15891:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_mapi_r_code_11279

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15882
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_15887
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15892
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11320
 # arguments : [$11322,$0] [$11323,$1] [$4985,$2] 
 # results    : [$13155,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11320:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_15893:
funtop_13071:
	ldl	$2, ($1)
	ldl	$1, 4($1)
sumarm_13083:
	ldl	$25, ($0)
	stl	$25, 16($sp)
	ldl	$25, 8($0)
	stl	$25, 12($sp)
	ldl	$25, 4($0)
	stl	$25, 8($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15925:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15894:
	# done making normal call
	# ptr sub start
	ldl	$25, 16($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15922:
	ldgp	$gp, ($26)
	mov	$0, $17
code_15895:
	# done making normal call
	lda	$16, 1($31)
	lda	$0, 510($31)
	cmple	$16, $0, $0
	bne	$0, array_ptr_alloc_13119
code_15897:
	lda	$27, alloc_bigptrarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigptrarray
code_15923:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $9
code_15898:
	br	$31, array_ptr_aftert_13118
array_ptr_alloc_13119:
	sll	$16, 2, $0
	addl	$0, $31, $0
	sll	$0, 3, $1
	addl	$1, $31, $1
	or	$1, 5, $1
	lda	$0, 1($31)
	addl	$0, $16, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15900
code_15901:
	jsr	$26, GCFromML
gc_check_15900:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $9
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$16, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_13125
array_init_loopto_13126:
	addl	$9, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_13125:
	bge	$1, array_init_loopto_13126
array_ptr_aftert_13118:
	# int sub start
	ldl	$25, 12($sp)
	ldl	$17, ($25)
	# int sub end
	lda	$2, 1($31)
	# initializing int/ptr array start
	sll	$2, 2, $16
	addl	$16, $31, $16
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_13137
code_15907:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_15924:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_15908:
	br	$31, array_int_after_13136
array_int_small_13137:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15910
code_15911:
	jsr	$26, GCFromML
gc_check_15910:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_13144
array_init_loopto_13145:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_13144:
	bge	$1, array_init_loopto_13145
array_int_after_13136:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15916
code_15917:
	jsr	$26, GCFromML
gc_check_15916:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$9, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_13080
sumarm_13084:
after_sum_13080:
code_15921:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11320

	.rdata
		# -------- label,sizes,reg
	.long code_15922
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_15923
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long gc_check_15900
	.long 0x00001005
	.long 0x00020000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_15924
	.long 0x00001005
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_15910
	.long 0x00001005
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_15916
	.long 0x00001005
	.long 0x00000208
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_15925
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000150
	.text
 	.align 3
	.ent HashTableFn_map_inner_code_11315
 # arguments : [$11317,$0] [$11318,$1] [$4983,$2] 
 # results    : [$13065,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
HashTableFn_map_inner_code_11315:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_15926:
funtop_13054:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15927
code_15928:
	jsr	$26, GCFromML
gc_check_15927:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11320
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15931:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_map_inner_code_11315

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15927
	.long 0x00000805
	.long 0x01ff0ffe
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_map_r_code_11305
 # arguments : [$11307,$0] [$4978,$1] [$11308,$2] [$4979,$3] 
 # results    : [$13048,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_map_r_code_11305:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_15932:
funtop_13019:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15933
code_15934:
	jsr	$26, GCFromML
gc_check_15933:
	# Proj_c at label 'a_TYV
	ldl	$2, ($1)
	# Proj_c at label 'b_TYV
	ldl	$1, 4($1)
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	stl	$4, 12($13)
	addl	$13, 4, $5
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_map_6783
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_15943:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15937:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15938
code_15939:
	jsr	$26, GCFromML
gc_check_15938:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_map_inner_code_11315
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15942:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_map_r_code_11305

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15933
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_15938
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15943
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11350
 # arguments : [$11352,$0] [$11353,$1] [$5020,$2] 
 # results    : [$13018,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11350:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $3
code_15944:
funtop_12954:
	# Proj_c at label type_8705_INT
	ldl	$25, ($0)
	stl	$25, 24($sp)
	# Proj_c at label type_6829_INT
	ldl	$25, 4($0)
	stl	$25, 20($sp)
	# Proj_c at label var_poly_c_5011_INT
	ldl	$25, 8($0)
	stl	$25, 8($sp)
	ldl	$2, ($1)
	ldl	$25, 4($1)
	stl	$25, 16($sp)
	ldl	$1, 8($1)
sumarm_12977:
	ldl	$25, ($3)
	stl	$25, 12($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15956:
	ldgp	$gp, ($26)
	mov	$0, $4
code_15945:
	# done making normal call
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 20($sp)
	ldl	$2, 24($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_15953:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15947:
	# done making normal call
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15954:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15948:
	# done making normal call
	# ptr sub start
	ldl	$25, 12($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_15949:
	# done making tail call
	br	$31, after_sum_12974
sumarm_12978:
after_sum_12974:
code_15952:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11350

	.rdata
		# -------- label,sizes,reg
	.long code_15953
	.long 0x00001007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000350
		# worddata
	.long 0x00000008
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_15954
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_15956
	.long 0x00001007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001750
		# worddata
	.long 0x00000008
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11344
 # arguments : [$11346,$0] [$11347,$1] [$5018,$2] 
 # results    : [$12949,$0] 
 # destroys   :  $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11344:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$1, $0
code_15957:
funtop_12911:
	addl	$13, 48, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15958
code_15959:
	jsr	$26, GCFromML
gc_check_15958:
	# Proj_c at label type_8705_INT
	ldl	$25, 8($sp)
	ldl	$6, ($25)
	# Proj_c at label type_6829_INT
	ldl	$25, 8($sp)
	ldl	$3, 4($25)
	# Proj_c at label var_poly_c_5011_INT
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	ldl	$5, ($0)
	ldl	$4, 4($0)
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$6, 4($13)
	stl	$3, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $3
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$1, 1305($31)
	ldl	$25, 8($sp)
	ldl	$0, 8($25)
	ldl	$0, 4($0)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$5, 4($13)
	ldl	$25, 8($sp)
	ldl	$0, 8($25)
	ldl	$0, 4($0)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	stl	$2, 8($13)
	stl	$4, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11350
	stl	$0, 4($13)
	stl	$3, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15962:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11344

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15958
	.long 0x00000807
	.long 0x01ff0f81
	.long 0x01ff0f84
		# stacktrace
	.long 0x00000010
		# worddata
	.long 0x0000020c
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_foldi_inner_code_11339
 # arguments : [$11341,$0] [$11342,$1] [$5016,$2] 
 # results    : [$12910,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_foldi_inner_code_11339:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $5
	mov	$2, $6
code_15963:
funtop_12866:
	addl	$13, 44, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15964
code_15965:
	jsr	$26, GCFromML
gc_check_15964:
	# Proj_c at label type_8705_INT
	ldl	$4, ($0)
	# Proj_c at label type_8627_INT
	ldl	$2, 4($0)
	# Proj_c at label type_6829_INT
	ldl	$1, 8($0)
	# Proj_c at label var_poly_c_5011_INT
	ldl	$3, 12($0)
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$6, 4($13)
	stl	$5, 8($13)
	addl	$13, 4, $3
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11344
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$3, vararg_INT
	ldl	$0, 1088($12)
	addl	$3, $0, $0
	ldl	$3, ($0)
	ldl	$5, ($3)
	ldl	$0, 4($3)
	ldl	$3, 8($3)
	mov	$5, $27
	jsr	$26, ($5), 1
code_15971:
	ldgp	$gp, ($26)
code_15968:
	# done making normal call
code_15970:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_foldi_inner_code_11339

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15964
	.long 0x00000805
	.long 0x00000061
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_15971
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_foldi_r_code_11331
 # arguments : [$11333,$0] [$5011,$1] [$11334,$2] [$5012,$3] 
 # results    : [$12861,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_foldi_r_code_11331:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 24($sp)
code_15972:
funtop_12796:
	# Proj_c at label type_5871_INT
	ldl	$25, ($0)
	stl	$25, 20($sp)
	# Proj_c at label _5819_INT
	ldl	$0, 4($0)
	# Proj_c at label 'b_TYV
	ldl	$25, 24($sp)
	ldl	$25, 4($25)
	stl	$25, 16($sp)
	# Proj_c at label 'a_TYV
	ldl	$25, 24($sp)
	ldl	$25, ($25)
	stl	$25, 12($sp)
	# start making constructor call
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 12($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_15989:
	ldgp	$gp, ($26)
code_15973:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15974
code_15975:
	jsr	$26, GCFromML
gc_check_15974:
	# done making constructor call
	# allocating 1-record
	# done allocating 1 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# start making constructor call
	lda	$1, type_5879
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, ($0)
	ldl	$0, 4($0)
	ldl	$1, 20($sp)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_15990:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15978:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15979
code_15980:
	jsr	$26, GCFromML
gc_check_15979:
	# done making constructor call
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, ($31)
	stl	$0, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 1-record
	# done allocating 1 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_foldi_6845
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_15991:
	ldgp	$gp, ($26)
	mov	$0, $2
code_15983:
	# done making normal call
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_15984
code_15985:
	jsr	$26, GCFromML
gc_check_15984:
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 3873($31)
	stl	$0, ($13)
	lda	$0, record_12837
	stl	$0, 4($13)
	lda	$0, record_12815
	stl	$0, 8($13)
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	ldl	$25, 24($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_foldi_inner_code_11339
	stl	$0, 4($13)
	stl	$1, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_15988:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end HashTableFn_foldi_r_code_11331

	.rdata
		# -------- label,sizes,reg
	.long gc_check_15974
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001540
		# -------- label,sizes,reg
	.long gc_check_15979
	.long 0x00001005
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00001110
		# -------- label,sizes,reg
	.long gc_check_15984
	.long 0x00001005
	.long 0x00000004
	.long 0x00000000
		# stacktrace
	.long 0x00001100
		# -------- label,sizes,reg
	.long code_15989
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001540
		# -------- label,sizes,reg
	.long code_15990
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001110
		# -------- label,sizes,reg
	.long code_15991
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001100
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11399
 # arguments : [$11401,$0] [$11402,$1] [$5047,$2] 
 # results    : [$12795,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11399:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $3
code_15992:
funtop_12731:
	# Proj_c at label type_8813_INT
	ldl	$25, ($0)
	stl	$25, 24($sp)
	# Proj_c at label type_6883_INT
	ldl	$25, 4($0)
	stl	$25, 20($sp)
	# Proj_c at label var_poly_c_5038_INT
	ldl	$25, 8($0)
	stl	$25, 8($sp)
	ldl	$2, ($1)
	ldl	$25, 4($1)
	stl	$25, 16($sp)
	ldl	$1, 8($1)
sumarm_12754:
	ldl	$25, ($3)
	stl	$25, 12($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16004:
	ldgp	$gp, ($26)
	mov	$0, $4
code_15993:
	# done making normal call
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 20($sp)
	ldl	$2, 24($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_16001:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15995:
	# done making normal call
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16002:
	ldgp	$gp, ($26)
	mov	$0, $1
code_15996:
	# done making normal call
	# ptr sub start
	ldl	$25, 12($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_15997:
	# done making tail call
	br	$31, after_sum_12751
sumarm_12755:
after_sum_12751:
code_16000:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11399

	.rdata
		# -------- label,sizes,reg
	.long code_16001
	.long 0x00001007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000350
		# worddata
	.long 0x00000008
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_16002
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_16004
	.long 0x00001007
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001750
		# worddata
	.long 0x00000008
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11393
 # arguments : [$11395,$0] [$11396,$1] [$5045,$2] 
 # results    : [$12726,$0] 
 # destroys   :  $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11393:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$1, $0
code_16005:
funtop_12688:
	addl	$13, 48, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16006
code_16007:
	jsr	$26, GCFromML
gc_check_16006:
	# Proj_c at label type_8813_INT
	ldl	$25, 8($sp)
	ldl	$6, ($25)
	# Proj_c at label type_6883_INT
	ldl	$25, 8($sp)
	ldl	$3, 4($25)
	# Proj_c at label var_poly_c_5038_INT
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	ldl	$5, ($0)
	ldl	$4, 4($0)
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$6, 4($13)
	stl	$3, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $3
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$1, 1305($31)
	ldl	$25, 8($sp)
	ldl	$0, 8($25)
	ldl	$0, 4($0)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$5, 4($13)
	ldl	$25, 8($sp)
	ldl	$0, 8($25)
	ldl	$0, 4($0)
	or	$31, 3, $at
	cmpult	$at, $0, $0
	stl	$2, 8($13)
	stl	$4, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11399
	stl	$0, 4($13)
	stl	$3, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16010:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11393

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16006
	.long 0x00000807
	.long 0x01ff0f81
	.long 0x01ff0f84
		# stacktrace
	.long 0x00000010
		# worddata
	.long 0x0000020c
	.long 0x00000008
	.text
 	.align 3
	.ent HashTableFn_fold_inner_code_11388
 # arguments : [$11390,$0] [$11391,$1] [$5043,$2] 
 # results    : [$12687,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_fold_inner_code_11388:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $5
	mov	$2, $6
code_16011:
funtop_12643:
	addl	$13, 44, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16012
code_16013:
	jsr	$26, GCFromML
gc_check_16012:
	# Proj_c at label type_8813_INT
	ldl	$4, ($0)
	# Proj_c at label type_8738_INT
	ldl	$2, 4($0)
	# Proj_c at label type_6883_INT
	ldl	$1, 8($0)
	# Proj_c at label var_poly_c_5038_INT
	ldl	$3, 12($0)
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$6, 4($13)
	stl	$5, 8($13)
	addl	$13, 4, $3
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11393
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$3, vararg_INT
	ldl	$0, 1088($12)
	addl	$3, $0, $0
	ldl	$3, ($0)
	ldl	$5, ($3)
	ldl	$0, 4($3)
	ldl	$3, 8($3)
	mov	$5, $27
	jsr	$26, ($5), 1
code_16019:
	ldgp	$gp, ($26)
code_16016:
	# done making normal call
code_16018:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_fold_inner_code_11388

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16012
	.long 0x00000805
	.long 0x00000061
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16019
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_fold_r_code_11380
 # arguments : [$11382,$0] [$5038,$1] [$11383,$2] [$5039,$3] 
 # results    : [$12638,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_fold_r_code_11380:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 24($sp)
code_16020:
funtop_12572:
	# Proj_c at label type_5871_INT
	ldl	$25, ($0)
	stl	$25, 20($sp)
	# Proj_c at label _5819_INT
	ldl	$0, 4($0)
	# Proj_c at label 'b_TYV
	ldl	$25, 24($sp)
	ldl	$25, 4($25)
	stl	$25, 16($sp)
	# Proj_c at label 'a_TYV
	ldl	$25, 24($sp)
	ldl	$25, ($25)
	stl	$25, 12($sp)
	# start making constructor call
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 12($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_16037:
	ldgp	$gp, ($26)
code_16021:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16022
code_16023:
	jsr	$26, GCFromML
gc_check_16022:
	# done making constructor call
	# allocating 1-record
	# done allocating 1 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	ldl	$25, 20($sp)
	stl	$25, 12($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# start making constructor call
	lda	$1, type_5879
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$3, ($0)
	ldl	$0, 4($0)
	ldl	$1, 20($sp)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16038:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16026:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16027
code_16028:
	jsr	$26, GCFromML
gc_check_16027:
	# done making constructor call
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, ($31)
	stl	$0, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 1-record
	# done allocating 1 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_fold_6899
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$1, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_16039:
	ldgp	$gp, ($26)
	mov	$0, $2
code_16031:
	# done making normal call
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16032
code_16033:
	jsr	$26, GCFromML
gc_check_16032:
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 3873($31)
	stl	$0, ($13)
	lda	$0, record_12614
	stl	$0, 4($13)
	lda	$0, record_12592
	stl	$0, 8($13)
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	ldl	$25, 24($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_fold_inner_code_11388
	stl	$0, 4($13)
	stl	$1, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16036:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end HashTableFn_fold_r_code_11380

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16022
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001540
		# -------- label,sizes,reg
	.long gc_check_16027
	.long 0x00001005
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00001110
		# -------- label,sizes,reg
	.long gc_check_16032
	.long 0x00001005
	.long 0x00000004
	.long 0x00000000
		# stacktrace
	.long 0x00001100
		# -------- label,sizes,reg
	.long code_16037
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001540
		# -------- label,sizes,reg
	.long code_16038
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001110
		# -------- label,sizes,reg
	.long code_16039
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001100
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11442
 # arguments : [$11444,$0] [$11445,$1] [$5071,$2] 
 # results    : [$12571,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11442:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_16040:
funtop_12536:
	ldl	$2, ($1)
	ldl	$1, 4($1)
sumarm_12548:
	ldl	$25, ($0)
	stl	$25, 8($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16047:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16041:
	# done making normal call
	# ptr sub start
	ldl	$25, 8($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_16042:
	# done making tail call
	br	$31, after_sum_12545
sumarm_12549:
after_sum_12545:
code_16045:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11442

	.rdata
		# -------- label,sizes,reg
	.long code_16047
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent HashTableFn_filteri_inner_code_11437
 # arguments : [$11439,$0] [$11440,$1] [$5069,$2] 
 # results    : [$12530,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
HashTableFn_filteri_inner_code_11437:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_16048:
funtop_12519:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16049
code_16050:
	jsr	$26, GCFromML
gc_check_16049:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11442
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16053:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_filteri_inner_code_11437

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16049
	.long 0x00000805
	.long 0x01ff0ffe
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_filteri_r_code_11429
 # arguments : [$11431,$0] [$5065,$1] [$11432,$2] [$5066,$3] 
 # results    : [$12513,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_filteri_r_code_11429:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_16054:
funtop_12488:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16055
code_16056:
	jsr	$26, GCFromML
gc_check_16055:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_filteri_6949
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_16065:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16059:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16060
code_16061:
	jsr	$26, GCFromML
gc_check_16060:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_filteri_inner_code_11437
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16064:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_filteri_r_code_11429

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16055
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_16060
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16065
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11466
 # arguments : [$11468,$0] [$11469,$1] [$5094,$2] 
 # results    : [$12487,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11466:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_16066:
funtop_12452:
	ldl	$2, ($1)
	ldl	$1, 4($1)
sumarm_12464:
	ldl	$25, ($0)
	stl	$25, 8($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16073:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16067:
	# done making normal call
	# ptr sub start
	ldl	$25, 8($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_16068:
	# done making tail call
	br	$31, after_sum_12461
sumarm_12465:
after_sum_12461:
code_16071:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11466

	.rdata
		# -------- label,sizes,reg
	.long code_16073
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent HashTableFn_filter_inner_code_11461
 # arguments : [$11463,$0] [$11464,$1] [$5092,$2] 
 # results    : [$12446,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
HashTableFn_filter_inner_code_11461:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_16074:
funtop_12435:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16075
code_16076:
	jsr	$26, GCFromML
gc_check_16075:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11466
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16079:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_filter_inner_code_11461

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16075
	.long 0x00000805
	.long 0x01ff0ffe
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_filter_r_code_11453
 # arguments : [$11455,$0] [$5088,$1] [$11456,$2] [$5089,$3] 
 # results    : [$12429,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_filter_r_code_11453:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_16080:
funtop_12404:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16081
code_16082:
	jsr	$26, GCFromML
gc_check_16081:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$4, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_filter_6984
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_16091:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16085:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16086
code_16087:
	jsr	$26, GCFromML
gc_check_16086:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_filter_inner_code_11461
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16090:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_filter_r_code_11453

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16081
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_16086
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16091
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_copy_inner_code_11485
 # arguments : [$11487,$0] [$11488,$1] [$5115,$2] 
 # results    : [$12403,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_copy_inner_code_11485:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_16092:
funtop_12329:
sumarm_12337:
	ldl	$0, ($2)
	ldl	$25, 8($2)
	stl	$25, 12($sp)
	ldl	$25, 4($2)
	stl	$25, 8($sp)
	# ptr sub start
	ldl	$2, ($0)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16122:
	ldgp	$gp, ($26)
	mov	$0, $17
code_16093:
	# done making normal call
	lda	$16, 1($31)
	lda	$0, 510($31)
	cmple	$16, $0, $0
	bne	$0, array_ptr_alloc_12367
code_16095:
	lda	$27, alloc_bigptrarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigptrarray
code_16120:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $9
code_16096:
	br	$31, array_ptr_aftert_12366
array_ptr_alloc_12367:
	sll	$16, 2, $0
	addl	$0, $31, $0
	sll	$0, 3, $1
	addl	$1, $31, $1
	or	$1, 5, $1
	lda	$0, 1($31)
	addl	$0, $16, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16098
code_16099:
	jsr	$26, GCFromML
gc_check_16098:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $9
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$16, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_12373
array_init_loopto_12374:
	addl	$9, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_12373:
	bge	$1, array_init_loopto_12374
array_ptr_aftert_12366:
	# int sub start
	ldl	$25, 12($sp)
	ldl	$17, ($25)
	# int sub end
	lda	$2, 1($31)
	# initializing int/ptr array start
	sll	$2, 2, $16
	addl	$16, $31, $16
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_12385
code_16105:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_16121:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_16106:
	br	$31, array_int_after_12384
array_int_small_12385:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16108
code_16109:
	jsr	$26, GCFromML
gc_check_16108:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_12392
array_init_loopto_12393:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_12392:
	bge	$1, array_init_loopto_12393
array_int_after_12384:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16114
code_16115:
	jsr	$26, GCFromML
gc_check_16114:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$9, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_12334
sumarm_12338:
after_sum_12334:
code_16119:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_copy_inner_code_11485

	.rdata
		# -------- label,sizes,reg
	.long code_16120
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long gc_check_16098
	.long 0x00000805
	.long 0x00020000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_16121
	.long 0x00000805
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_16108
	.long 0x00000805
	.long 0x00000200
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_16114
	.long 0x00000805
	.long 0x00000208
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_16122
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent HashTableFn_copy_r_code_11477
 # arguments : [$11479,$0] [$5111,$1] [$11480,$2] [$5112,$3] 
 # results    : [$12323,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_copy_r_code_11477:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_16123:
funtop_12298:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16124
code_16125:
	jsr	$26, GCFromML
gc_check_16124:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_copy_7020
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_16134:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16128:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16129
code_16130:
	jsr	$26, GCFromML
gc_check_16129:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_copy_inner_code_11485
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16133:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_copy_r_code_11477

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16124
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_16129
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16134
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_bucketSizes_inner_code_11501
 # arguments : [$11503,$0] [$11504,$1] [$5144,$2] 
 # results    : [$12297,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_bucketSizes_inner_code_11501:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_16135:
funtop_12272:
sumarm_12280:
	ldl	$0, ($2)
	# ptr sub start
	ldl	$2, ($0)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_16136:
	# done making tail call
	br	$31, after_sum_12277
sumarm_12281:
after_sum_12277:
code_16139:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_bucketSizes_inner_code_11501

	.rdata
	.text
 	.align 3
	.ent HashTableFn_bucketSizes_r_code_11493
 # arguments : [$11495,$0] [$5140,$1] [$11496,$2] [$5141,$3] 
 # results    : [$12266,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_bucketSizes_r_code_11493:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_16141:
funtop_12241:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16142
code_16143:
	jsr	$26, GCFromML
gc_check_16142:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_bucketSizes_7066
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_16152:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16146:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16147
code_16148:
	jsr	$26, GCFromML
gc_check_16147:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_bucketSizes_inner_code_11501
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16151:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_bucketSizes_r_code_11493

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16142
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_16147
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16152
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11522
 # arguments : [$11524,$0] [$11525,$1] [$4927,$2] 
 # results    : [$12240,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11522:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_16153:
funtop_12205:
	ldl	$2, ($1)
	ldl	$1, 4($1)
sumarm_12217:
	ldl	$25, ($0)
	stl	$25, 8($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16160:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16154:
	# done making normal call
	# ptr sub start
	ldl	$25, 8($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_16155:
	# done making tail call
	br	$31, after_sum_12214
sumarm_12218:
after_sum_12214:
code_16158:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11522

	.rdata
		# -------- label,sizes,reg
	.long code_16160
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent HashTableFn_app_inner_code_11517
 # arguments : [$11519,$0] [$11520,$1] [$4925,$2] 
 # results    : [$12199,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
HashTableFn_app_inner_code_11517:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_16161:
funtop_12188:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16162
code_16163:
	jsr	$26, GCFromML
gc_check_16162:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11522
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16166:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_app_inner_code_11517

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16162
	.long 0x00000805
	.long 0x01ff0ffe
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_copy_app_r_code_11509
 # arguments : [$11511,$0] [$5184,$1] [$11512,$2] [$5185,$3] 
 # results    : [$12182,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_copy_app_r_code_11509:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_16167:
funtop_12155:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16168
code_16169:
	jsr	$26, GCFromML
gc_check_16168:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	lda	$0, record_11638
	stl	$0, 8($13)
	stl	$4, 12($13)
	addl	$13, 4, $5
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_app_6677
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_16178:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16172:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16173
code_16174:
	jsr	$26, GCFromML
gc_check_16173:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_app_inner_code_11517
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16177:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_copy_app_r_code_11509

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16168
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_16173
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16178
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_anonfun_code_11546
 # arguments : [$11548,$0] [$11549,$1] [$4902,$2] 
 # results    : [$12154,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_anonfun_code_11546:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_16179:
funtop_12119:
	ldl	$2, ($1)
	ldl	$1, 4($1)
sumarm_12131:
	ldl	$25, ($0)
	stl	$25, 8($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_16186:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16180:
	# done making normal call
	# ptr sub start
	ldl	$25, 8($sp)
	ldl	$2, ($25)
	# ptr sub end
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_16181:
	# done making tail call
	br	$31, after_sum_12128
sumarm_12132:
after_sum_12128:
code_16184:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_anonfun_code_11546

	.rdata
		# -------- label,sizes,reg
	.long code_16186
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent HashTableFn_appi_inner_code_11541
 # arguments : [$11543,$0] [$11544,$1] [$4900,$2] 
 # results    : [$12113,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
HashTableFn_appi_inner_code_11541:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_16187:
funtop_12102:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16188
code_16189:
	jsr	$26, GCFromML
gc_check_16188:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_anonfun_code_11546
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16192:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_appi_inner_code_11541

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16188
	.long 0x00000805
	.long 0x01ff0ffe
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_copy_appi_r_code_11533
 # arguments : [$11535,$0] [$5201,$1] [$11536,$2] [$5202,$3] 
 # results    : [$12096,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_copy_appi_r_code_11533:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$0, $4
code_16193:
funtop_12069:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16194
code_16195:
	jsr	$26, GCFromML
gc_check_16194:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	lda	$0, record_11638
	stl	$0, 12($13)
	addl	$13, 4, $5
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$3, 256($31)
	# making closure call 
	lda	$1, strbindvar_r_appi_6639
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_16204:
	ldgp	$gp, ($26)
	mov	$0, $1
code_16198:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16199
code_16200:
	jsr	$26, GCFromML
gc_check_16199:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_appi_inner_code_11541
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_16203:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_copy_appi_r_code_11533

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16194
	.long 0x00000805
	.long 0x00000012
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_16199
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16204
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent HashTableFn_functor_var_r_code_10916
 # arguments : [$10918,$0] [$5808,$1] [$10919,$2] [$4315,$3] 
 # results    : [$12065,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_functor_var_r_code_10916:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 24($sp)
	stl	$3, 8($sp)
code_16205:
funtop_11811:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16206
code_16207:
	jsr	$26, GCFromML
gc_check_16206:
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, HashTableFn__code_10921
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	addl	$13, 4, $25
	stl	$25, 20($sp)
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_mkTable_r_code_10939
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $25
	stl	$25, 16($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	ldl	$25, 8($sp)
	ldl	$25, ($25)
	stl	$25, 12($sp)
	lda	$1, word_TYC
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$2, ($0)
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 24($sp)
	ldl	$4, 12($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_16217:
	ldgp	$gp, ($26)
code_16211:
	# done making normal call
	addl	$13, 472, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_16212
code_16213:
	jsr	$26, GCFromML
gc_check_16212:
	ldl	$25, 8($sp)
	ldl	$22, 4($25)
	# allocating 1 closures
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	addl	$13, 4, $3
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	stl	$22, 4($13)
	stl	$0, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$1, 1561($31)
	stl	$1, ($13)
	lda	$1, HashTableFn_insert_r_code_10955
	stl	$1, 4($13)
	stl	$3, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $21
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	addl	$13, 4, $3
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	stl	$22, 4($13)
	stl	$0, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$1, 1561($31)
	stl	$1, ($13)
	lda	$1, HashTableFn_lookup_r_code_11035
	stl	$1, 4($13)
	stl	$3, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $20
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	addl	$13, 4, $3
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	stl	$22, 4($13)
	stl	$0, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$1, 1561($31)
	stl	$1, ($13)
	lda	$1, HashTableFn_find_r_code_11098
	stl	$1, 4($13)
	stl	$3, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $19
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$1, 785($31)
	stl	$1, ($13)
	stl	$22, 4($13)
	stl	$0, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_remove_r_code_11166
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $18
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$17, record_11922
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_listItems_r_code_11244
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $16
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_listItemsi_r_code_11261
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $11
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_mapi_r_code_11279
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $10
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_map_r_code_11305
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $9
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 20($sp)
	stl	$25, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_foldi_r_code_11331
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $8
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 20($sp)
	stl	$25, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_fold_r_code_11380
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $7
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_filteri_r_code_11429
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $6
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_filter_r_code_11453
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $5
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_copy_r_code_11477
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_bucketSizes_r_code_11493
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $3
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_copy_app_r_code_11509
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $2
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, HashTableFn_copy_appi_r_code_11533
	stl	$0, 4($13)
	ldl	$25, 24($sp)
	stl	$25, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	stl	$22, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 20-record
	lda	$22, 32673($31)
	ldah	$22, 4095($22)
	stl	$22, ($13)
	lda	$22, record_11803
	stl	$22, 4($13)
	stl	$0, 8($13)
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	stl	$21, 16($13)
	stl	$20, 20($13)
	stl	$19, 24($13)
	stl	$18, 28($13)
	stl	$17, 32($13)
	stl	$16, 36($13)
	stl	$11, 40($13)
	stl	$2, 44($13)
	stl	$1, 48($13)
	stl	$9, 52($13)
	stl	$10, 56($13)
	stl	$7, 60($13)
	stl	$8, 64($13)
	stl	$5, 68($13)
	stl	$6, 72($13)
	stl	$4, 76($13)
	stl	$3, 80($13)
	addl	$13, 4, $0
	addl	$13, 84, $13
	# done allocating 20 record
code_16216:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end HashTableFn_functor_var_r_code_10916

	.rdata
		# -------- label,sizes,reg
	.long gc_check_16206
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001010
		# -------- label,sizes,reg
	.long gc_check_16212
	.long 0x00001005
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00001550
		# -------- label,sizes,reg
	.long code_16217
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001550
	.text
 	.align 3
	.ent HashTableFn_main
 # arguments : 
 # results    : [$11810,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
HashTableFn_main:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_16218:
funtop_11564:
	lda	$16, ($31)
	lda	$27, AssertMirrorPtrArray
	jsr	$26, save_regs_MLtoC
	jsr	$26, AssertMirrorPtrArray
code_16427:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_16219:
	lda	$1, HashTableRep_STR_c_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	# Proj_c at label table_TYC
	ldl	$5, 16($0)
	lda	$1, 111($31)
	lda	$0, _5247
	stl	$1, -4($0)
	ldl	$2, 1076($12)
	ldl	$1, 1080($12)
	lda	$0, 264($31)
	addl	$2, $0, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_16225
code_16227:
	lda	$at, 264($31)
	subl	$13, $at, $at
	jsr	$26, GCFromML
afterMutateCheck_16225:
	lda	$4, _5247
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	# allocating 2-record
	# done allocating 2 record
	lda	$1, HashTableRep_STR_c_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	# Proj_c at label hiddenThinModule_INT
	ldl	$0, ($0)
	# Proj_c at label bucket_TYC
	ldl	$5, ($0)
	lda	$1, 111($31)
	lda	$0, type_5879
	stl	$1, -4($0)
	lda	$4, type_5879
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	# allocating 2-record
	# done allocating 2 record
	lda	$1, Word_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 36($0)
	lda	$1, 111($31)
	lda	$0, toIntX_5859
	stl	$1, -4($0)
	lda	$4, toIntX_5859
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, Word_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 52($0)
	lda	$1, 111($31)
	lda	$0, andb_5860
	stl	$1, -4($0)
	lda	$4, andb_5860
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, Word_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 40($0)
	lda	$1, 111($31)
	lda	$0, fromInt_5861
	stl	$1, -4($0)
	lda	$4, fromInt_5861
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 8($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_alloc_5875
	stl	$1, -4($0)
	lda	$4, strbindvar_r_alloc_5875
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, Array_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 24($0)
	lda	$1, 111($31)
	lda	$0, length_5923
	stl	$1, -4($0)
	lda	$4, length_5923
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, Array_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 32($0)
	lda	$1, 111($31)
	lda	$0, update_5954
	stl	$1, -4($0)
	lda	$4, update_5954
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, Array_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 28($0)
	lda	$1, 111($31)
	lda	$0, sub_5963
	stl	$1, -4($0)
	lda	$4, sub_5963
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 16($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_growTableIfNeeded_5992
	stl	$1, -4($0)
	lda	$4, strbindvar_r_growTableIfNeeded_5992
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 20($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_listItems_6570
	stl	$1, -4($0)
	lda	$4, strbindvar_r_listItems_6570
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 24($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_listItemsi_6605
	stl	$1, -4($0)
	lda	$4, strbindvar_r_listItemsi_6605
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 36($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_mapi_6719
	stl	$1, -4($0)
	lda	$4, strbindvar_r_mapi_6719
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 40($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_map_6783
	stl	$1, -4($0)
	lda	$4, strbindvar_r_map_6783
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 44($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_foldi_6845
	stl	$1, -4($0)
	lda	$4, strbindvar_r_foldi_6845
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 48($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_fold_6899
	stl	$1, -4($0)
	lda	$4, strbindvar_r_fold_6899
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 52($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_filteri_6949
	stl	$1, -4($0)
	lda	$4, strbindvar_r_filteri_6949
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 56($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_filter_6984
	stl	$1, -4($0)
	lda	$4, strbindvar_r_filter_6984
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 60($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_copy_7020
	stl	$1, -4($0)
	lda	$4, strbindvar_r_copy_7020
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 64($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_bucketSizes_7066
	stl	$1, -4($0)
	lda	$4, strbindvar_r_bucketSizes_7066
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 32($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_app_6677
	stl	$1, -4($0)
	lda	$4, strbindvar_r_app_6677
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$1, HashTableRep_STR_r_INT
	ldl	$0, 1088($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 28($0)
	lda	$1, 111($31)
	lda	$0, strbindvar_r_appi_6639
	stl	$1, -4($0)
	lda	$4, strbindvar_r_appi_6639
	ldl	$3, 1088($12)
	ldl	$2, 1076($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1076($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	# allocating 2-record
	# done allocating 2 record
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, functor_var_r_4313
	# done allocating 1 closures
	lda	$0, 256($31)
code_16426:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end HashTableFn_main

	.rdata
		# -------- label,sizes,reg
	.long afterMutateCheck_16225
	.long 0x00000805
	.long 0x00000e20
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_16427
	.long 0x00000805
	.long 0x00000e00
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
	.text
HashTableFn_unit_CODE_END_VAL:
	.rdata
		# endgcinfo with filler for alignment
	.globl HashTableFn_unit_GCTABLE_END_VAL
HashTableFn_unit_GCTABLE_END_VAL:
	.long 0x00000000
	.sdata
	.align 3
	.sdata
	.align 3
	.globl HashTableFn_unit_GLOBALS_BEGIN_VAL
HashTableFn_unit_GLOBALS_BEGIN_VAL:
		# Global
	.long 0x00000037
_5247:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000211
record_11626:
	.long HashTableFn_functor_var_c_code_10905
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl HashTableFn_FCT_c_INT
HashTableFn_FCT_c_INT:
	.long record_11626
	.long record_11626
		# Global
	.long 0x00000037
type_5879:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000011
record_11638:
	.long 0x00000005
	.long 0x00000000
		# Global
	.long 0x00000037
toIntX_5859:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
andb_5860:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
fromInt_5861:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_alloc_5875:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
length_5923:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
update_5954:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
sub_5963:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_growTableIfNeeded_5992:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_listItems_6570:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_listItemsi_6605:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_mapi_6719:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_map_6783:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_foldi_6845:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_fold_6899:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_filteri_6949:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_filter_6984:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_copy_7020:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_bucketSizes_7066:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_app_6677:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
strbindvar_r_appi_6639:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000311
record_11803:
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl HashTableFn_FCT_r_INT
HashTableFn_FCT_r_INT:
	.long functor_var_r_4313
	.long functor_var_r_4313
		# static record tag
	.long 0x00000619
functor_var_r_4313:
	.long HashTableFn_functor_var_r_code_10916
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
record_11922:
	.long HashTableFn_numItems_r_code_11232
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000009
record_12592:
	.long 0x00000009
		# static record tag
	.long 0x00000009
record_12614:
	.long 0x00000009
		# static record tag
	.long 0x00000009
record_12815:
	.long 0x00000009
		# static record tag
	.long 0x00000009
record_12837:
	.long 0x00000009
		# static record tag
	.long 0x00000619
record_13420:
	.long HashTableFn_numItems_inner_code_11239
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000009
record_15348:
	.long 0x00000008
		# static record tag
	.long 0x00000009
record_15387:
	.long 0x00000008
		# Module closure
	.long 0x00000619
	.globl HashTableFn_unit_closure
HashTableFn_unit_closure:
	.long HashTableFn_main
	.long 0x00000000
	.long 0x00000000
	.long 0x00000311
	.globl HashTableFn_unit
HashTableFn_unit:
	.long HashTableFn_unit_closure
	.long HashTableFn_unit_closure
	.globl HashTableFn_unit_GLOBALS_END_VAL
HashTableFn_unit_GLOBALS_END_VAL:
	.long 0x00000000
	.long 0 #filler

	.sdata
	.align 3
	.globl HashTableFn_unit_TRACE_GLOBALS_BEGIN_VAL
HashTableFn_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long strbindvar_r_appi_6639
	.long strbindvar_r_app_6677
	.long strbindvar_r_bucketSizes_7066
	.long strbindvar_r_copy_7020
	.long strbindvar_r_filter_6984
	.long strbindvar_r_filteri_6949
	.long strbindvar_r_fold_6899
	.long strbindvar_r_foldi_6845
	.long strbindvar_r_map_6783
	.long strbindvar_r_mapi_6719
	.long strbindvar_r_listItemsi_6605
	.long strbindvar_r_listItems_6570
	.long strbindvar_r_growTableIfNeeded_5992
	.long sub_5963
	.long update_5954
	.long length_5923
	.long strbindvar_r_alloc_5875
	.long fromInt_5861
	.long andb_5860
	.long toIntX_5859
	.long type_5879
	.long _5247
	.globl HashTableFn_unit_TRACE_GLOBALS_END_VAL
HashTableFn_unit_TRACE_GLOBALS_END_VAL:
		# filler so label is defined
	.long 0x00000000
