	.set noat
	.rdata
		# gcinfo
	.globl SplayTree_unit_GCTABLE_BEGIN_VAL
SplayTree_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl SplayTree_unit_CODE_END_VAL
	.globl SplayTree_unit_CODE_BEGIN_VAL
SplayTree_unit_CODE_BEGIN_VAL:
	.text
 	.align 3
	.ent SplayTree__code_169371
 # arguments : [$169373,$0] [$167741,$1] 
 # results    : [$170665,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
SplayTree__code_169371:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_170671:
funtop_170657:
	# allocating 1-record
	# done allocating 1 record
	lda	$0, record_170663
code_170673:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree__code_169371

	.rdata
	.text
 	.align 3
	.ent SplayTree__code_169376
 # arguments : [$169378,$0] [$167745,$1] 
 # results    : [$170652,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree__code_169376:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 12($sp)
code_170674:
funtop_170633:
	# start making constructor call
	lda	$0, record_169483
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 12($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_170683:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_170675:
	# done making constructor call
	# start making constructor call
	lda	$0, record_169483
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 12($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_170682:
	ldgp	$gp, ($26)
	mov	$0, $1
code_170676:
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170677
code_170678:
	jsr	$26, GCFromML
gc_check_170677:
	# done making constructor call
	# allocating 5-record
	lda	$0, 7209($31)
	stl	$0, ($13)
	lda	$0, 5($31)
	stl	$0, 4($13)
	lda	$0, 3($31)
	stl	$0, 8($13)
	ldl	$25, 8($sp)
	stl	$25, 12($13)
	stl	$1, 16($13)
	ldl	$25, 12($sp)
	stl	$25, 20($13)
	addl	$13, 4, $0
	addl	$13, 24, $13
	# done allocating 5 record
code_170681:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree__code_169376

	.rdata
		# -------- label,sizes,reg
	.long code_170682
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long gc_check_170677
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_170683
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
	.text
 	.align 3
	.ent SplayTree__code_169381
 # arguments : [$169383,$0] [$167748,$1] 
 # results    : [$170626,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree__code_169381:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_170684:
funtop_170615:
	# start making constructor call
	lda	$0, record_169487
	ldl	$2, ($0)
	ldl	$0, 4($0)
	mov	$2, $27
	jsr	$26, ($2), 1
code_170691:
	ldgp	$gp, ($26)
	mov	$0, $1
code_170685:
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170686
code_170687:
	jsr	$26, GCFromML
gc_check_170686:
	# done making constructor call
	# allocating 5-record
	lda	$0, 4137($31)
	stl	$0, ($13)
	lda	$0, 4($31)
	stl	$0, 4($13)
	lda	$0, -1($31)
	stl	$0, 8($13)
	lda	$0, 1($31)
	stl	$0, 12($13)
	lda	$0, 2($31)
	stl	$0, 16($13)
	stl	$1, 20($13)
	addl	$13, 4, $0
	addl	$13, 24, $13
	# done allocating 5 record
code_170690:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree__code_169381

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170686
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_170691
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent SplayTree__code_169391
 # arguments : [$169393,$0] [$167812,$1] 
 # results    : [$170612,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
SplayTree__code_169391:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_170692:
funtop_170605:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170693
code_170694:
	jsr	$26, GCFromML
gc_check_170693:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$1, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_170697:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree__code_169391

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170693
	.long 0x00000805
	.long 0x01ff0ffe
	.long 0x01ff0ffc
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent SplayTree__code_169396
 # arguments : [$169398,$0] [$167818,$1] 
 # results    : [$170598,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree__code_169396:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_170698:
funtop_170587:
	# start making constructor call
	lda	$0, record_169495
	ldl	$2, ($0)
	ldl	$0, 4($0)
	mov	$2, $27
	jsr	$26, ($2), 1
code_170705:
	ldgp	$gp, ($26)
	mov	$0, $1
code_170699:
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170700
code_170701:
	jsr	$26, GCFromML
gc_check_170700:
	# done making constructor call
	# allocating 5-record
	lda	$0, 4137($31)
	stl	$0, ($13)
	lda	$0, 4($31)
	stl	$0, 4($13)
	lda	$0, -1($31)
	stl	$0, 8($13)
	lda	$0, 1($31)
	stl	$0, 12($13)
	lda	$0, 4($31)
	stl	$0, 16($13)
	stl	$1, 20($13)
	addl	$13, 4, $0
	addl	$13, 24, $13
	# done allocating 5 record
code_170704:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree__code_169396

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170700
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_170705
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent SplayTree_vars_eq_0_code_169406
 # arguments : [$169408,$0] [$169409,$1] [$169087,$2] [$169088,$3] 
 # results    : [$170522,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree_vars_eq_0_code_169406:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	stl	$1, 12($sp)
	stl	$2, 20($sp)
	stl	$3, 16($sp)
code_170706:
funtop_170495:
sumarm_170506:
	ldl	$25, 20($sp)
	bne	$25, sumarm_170507
sumarm_170514:
	ldl	$25, 16($sp)
	bne	$25, sumarm_170515
code_170708:
	lda	$0, 1($31)
	br	$31, after_sum_170511
sumarm_170515:
nomatch_sum_170512:
	lda	$0, ($31)
after_sum_170511:
	br	$31, after_sum_170503
sumarm_170507:
sumarm_170530:
	lda	$0, 255($31)
	ldl	$25, 16($sp)
	cmple	$25, $0, $0
	bne	$0, nomatch_sum_170528
code_170712:
	ldl	$25, 20($sp)
	ldl	$2, ($25)
	ldl	$25, 16($sp)
	ldl	$3, ($25)
	# making direct call 
	ldl	$0, 8($sp)
	ldl	$1, 12($sp)
	lda	$27, SplayTree_vars_eq_0_code_169406
	jsr	$26, SplayTree_vars_eq_0_code_169406
code_170724:
	ldgp	$gp, ($26)
code_170713:
	# done making normal call
	bne	$0, one_case_170550
zero_case_170549:
	lda	$0, ($31)
	br	$31, after_zeroone_170551
one_case_170550:
	ldl	$25, 20($sp)
	ldl	$2, 4($25)
	ldl	$25, 16($sp)
	ldl	$3, 4($25)
	# making direct call 
	ldl	$0, 8($sp)
	ldl	$1, 12($sp)
	lda	$27, SplayTree_vars_eq_0_code_169406
	jsr	$26, SplayTree_vars_eq_0_code_169406
code_170725:
	ldgp	$gp, ($26)
code_170716:
	# done making normal call
after_zeroone_170551:
	bne	$0, one_case_170567
zero_case_170566:
	lda	$0, ($31)
	br	$31, after_zeroone_170568
one_case_170567:
	ldl	$25, 20($sp)
	ldl	$2, 8($25)
	ldl	$25, 16($sp)
	ldl	$3, 8($25)
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$4, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($4), 1
code_170719:
	# done making tail call
after_zeroone_170568:
	br	$31, after_sum_170527
sumarm_170531:
nomatch_sum_170528:
	lda	$0, ($31)
after_sum_170527:
	br	$31, after_sum_170503
sumarm_170523:
after_sum_170503:
code_170723:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end SplayTree_vars_eq_0_code_169406

	.rdata
		# -------- label,sizes,reg
	.long code_170724
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000550
		# -------- label,sizes,reg
	.long code_170725
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000540
	.text
 	.align 3
	.ent SplayTree_polyPLUSEsplay_INT_r_code_169401
 # arguments : [$169403,$0] [$167753,$1] [$169404,$2] [$167754,$3] 
 # results    : [$170490,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
SplayTree_polyPLUSEsplay_INT_r_code_169401:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
code_170727:
funtop_170481:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170728
code_170729:
	jsr	$26, GCFromML
gc_check_170728:
	ldl	$1, ($3)
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, SplayTree_vars_eq_0_code_169406
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_170732:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree_polyPLUSEsplay_INT_r_code_169401

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170728
	.long 0x00000805
	.long 0x01ff0ff8
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent SplayTree_adj_code_169426
 # arguments : [$169428,$0] [$169429,$1] [$167939,$2] 
 # results    : [$169933,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree_adj_code_169426:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 28($sp)
	stl	$1, 24($sp)
code_170733:
funtop_169909:
	# Proj_c at label type_168512_INT
	ldl	$25, 28($sp)
	ldl	$0, ($25)
	# Proj_c at label var_poly_c_167930_INT
	ldl	$25, 28($sp)
	ldl	$25, 4($25)
	stl	$25, 8($sp)
	ldl	$25, 24($sp)
	ldl	$25, ($25)
	stl	$25, 12($sp)
	ldl	$25, 24($sp)
	ldl	$0, 4($25)
	ldl	$25, 24($sp)
	ldl	$25, 8($25)
	stl	$25, 20($sp)
sumarm_169929:
	bne	$2, sumarm_169930
code_170734:
	br	$31, after_sum_169926
sumarm_169930:
	ldl	$25, 8($2)
	stl	$25, 44($sp)
	ldl	$25, ($2)
	stl	$25, 40($sp)
	ldl	$25, 4($2)
	stl	$25, 36($sp)
	# making closure call 
	ldl	$25, 20($sp)
	ldl	$3, ($25)
	ldl	$25, 20($sp)
	ldl	$0, 4($25)
	ldl	$25, 20($sp)
	ldl	$1, 8($25)
	ldl	$2, 44($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_170800:
	ldgp	$gp, ($26)
code_170736:
	# done making normal call
	addl	$13, 84, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170737
code_170738:
	jsr	$26, GCFromML
gc_check_170737:
sumarm_169955:
	bne	$0, sumarm_169956
code_170740:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, ($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $1
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 40($sp)
	stl	$25, 8($13)
	ldl	$25, 36($sp)
	stl	$25, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_169952
sumarm_169956:
	cmpeq	$0, 1, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_169977
sumarm_169985:
	ldl	$25, 40($sp)
	bne	$25, sumarm_169986
code_170744:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, 1($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $1
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	ldl	$25, 36($sp)
	stl	$25, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_169982
sumarm_169986:
	ldl	$25, 40($sp)
	ldl	$25, 8($25)
	stl	$25, 16($sp)
	ldl	$25, 40($sp)
	ldl	$25, ($25)
	stl	$25, 32($sp)
	ldl	$25, 40($sp)
	ldl	$25, 4($25)
	stl	$25, 12($sp)
	# making closure call 
	ldl	$25, 20($sp)
	ldl	$3, ($25)
	ldl	$25, 20($sp)
	ldl	$0, 4($25)
	ldl	$25, 20($sp)
	ldl	$1, 8($25)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_170801:
	ldgp	$gp, ($26)
code_170746:
	# done making normal call
	addl	$13, 148, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170747
code_170748:
	jsr	$26, GCFromML
gc_check_170747:
sumarm_170028:
	bne	$0, sumarm_170029
code_170750:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, ($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $2
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	ldl	$25, 32($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_170025
sumarm_170029:
	cmpeq	$0, 1, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_170062
sumarm_170070:
	ldl	$25, 32($sp)
	bne	$25, sumarm_170071
code_170754:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, 1($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $2
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	ldl	$25, 32($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_170067
sumarm_170071:
nomatch_sum_170068:
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $25
	stl	$25, 12($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# making direct call 
	ldl	$0, 28($sp)
	ldl	$1, 24($sp)
	ldl	$2, 32($sp)
	lda	$27, SplayTree_adj_code_169426
	jsr	$26, SplayTree_adj_code_169426
code_170796:
	ldgp	$gp, ($26)
code_170756:
	# done making normal call
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170757
code_170758:
	jsr	$26, GCFromML
gc_check_170757:
	ldl	$4, ($0)
	ldl	$3, 4($0)
	ldl	$2, 8($0)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$2, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$3, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
after_sum_170067:
	br	$31, after_sum_170025
sumarm_170062:
nomatch_sum_170026:
sumarm_170153:
	ldl	$25, 12($sp)
	bne	$25, sumarm_170154
code_170761:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, 2($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $2
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	ldl	$25, 32($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_170150
sumarm_170154:
nomatch_sum_170151:
	# making direct call 
	ldl	$0, 28($sp)
	ldl	$1, 24($sp)
	ldl	$2, 12($sp)
	lda	$27, SplayTree_adj_code_169426
	jsr	$26, SplayTree_adj_code_169426
code_170797:
	ldgp	$gp, ($26)
code_170763:
	# done making normal call
	addl	$13, 48, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170764
code_170765:
	jsr	$26, GCFromML
gc_check_170764:
	ldl	$4, ($0)
	ldl	$3, 4($0)
	ldl	$2, 8($0)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$2, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $2
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 32($sp)
	stl	$25, 4($13)
	stl	$3, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
after_sum_170150:
after_sum_170025:
	br	$31, after_sum_169982
sumarm_170007:
after_sum_169982:
	br	$31, after_sum_169952
sumarm_169977:
nomatch_sum_169953:
sumarm_170236:
	ldl	$25, 36($sp)
	bne	$25, sumarm_170237
code_170769:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, 2($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $1
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 40($sp)
	stl	$25, 8($13)
	ldl	$25, 12($sp)
	stl	$25, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_170233
sumarm_170237:
	ldl	$25, 36($sp)
	ldl	$25, 8($25)
	stl	$25, 16($sp)
	ldl	$25, 36($sp)
	ldl	$25, ($25)
	stl	$25, 12($sp)
	ldl	$25, 36($sp)
	ldl	$25, 4($25)
	stl	$25, 32($sp)
	# making closure call 
	ldl	$25, 20($sp)
	ldl	$3, ($25)
	ldl	$25, 20($sp)
	ldl	$0, 4($25)
	ldl	$25, 20($sp)
	ldl	$1, 8($25)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_170802:
	ldgp	$gp, ($26)
code_170771:
	# done making normal call
	addl	$13, 148, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170772
code_170773:
	jsr	$26, GCFromML
gc_check_170772:
sumarm_170279:
	bne	$0, sumarm_170280
code_170775:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, ($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $2
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 40($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	ldl	$25, 32($sp)
	stl	$25, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_170276
sumarm_170280:
	cmpeq	$0, 2, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_170313
sumarm_170321:
	ldl	$25, 32($sp)
	bne	$25, sumarm_170322
code_170779:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, 2($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $2
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 40($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	ldl	$25, 32($sp)
	stl	$25, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_170318
sumarm_170322:
nomatch_sum_170319:
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 40($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $25
	stl	$25, 12($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# making direct call 
	ldl	$0, 28($sp)
	ldl	$1, 24($sp)
	ldl	$2, 32($sp)
	lda	$27, SplayTree_adj_code_169426
	jsr	$26, SplayTree_adj_code_169426
code_170798:
	ldgp	$gp, ($26)
code_170781:
	# done making normal call
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170782
code_170783:
	jsr	$26, GCFromML
gc_check_170782:
	ldl	$4, ($0)
	ldl	$3, 4($0)
	ldl	$2, 8($0)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	stl	$3, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
after_sum_170318:
	br	$31, after_sum_170276
sumarm_170313:
nomatch_sum_170277:
sumarm_170404:
	ldl	$25, 12($sp)
	bne	$25, sumarm_170405
code_170786:
	# allocating 2-record
	lda	$1, 17($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 9, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	lda	$0, 1($31)
	stl	$0, 4($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $2
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 40($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	ldl	$25, 32($sp)
	stl	$25, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	br	$31, after_sum_170401
sumarm_170405:
nomatch_sum_170402:
	# making direct call 
	ldl	$0, 28($sp)
	ldl	$1, 24($sp)
	ldl	$2, 12($sp)
	lda	$27, SplayTree_adj_code_169426
	jsr	$26, SplayTree_adj_code_169426
code_170799:
	ldgp	$gp, ($26)
code_170788:
	# done making normal call
	addl	$13, 48, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170789
code_170790:
	jsr	$26, GCFromML
gc_check_170789:
	ldl	$4, ($0)
	ldl	$3, 4($0)
	ldl	$2, 8($0)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$2, 4($13)
	ldl	$25, 32($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	addl	$13, 4, $2
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	ldl	$25, 40($sp)
	stl	$25, 4($13)
	stl	$3, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	ldl	$25, 44($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$4, 4($13)
	stl	$1, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
after_sum_170401:
after_sum_170276:
	br	$31, after_sum_170233
sumarm_170258:
after_sum_170233:
after_sum_169952:
	br	$31, after_sum_169926
sumarm_169934:
after_sum_169926:
code_170795:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end SplayTree_adj_code_169426

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170737
	.long 0x00001807
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00d45450
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_170747
	.long 0x00001809
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00c55350
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_170757
	.long 0x00001807
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00000350
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_170764
	.long 0x00001809
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00c50310
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_170772
	.long 0x00001809
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00d15350
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_170782
	.long 0x00001807
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00000350
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long gc_check_170789
	.long 0x00001809
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00d10310
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_170796
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000350
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_170797
	.long 0x00001809
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00c50310
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_170798
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000350
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_170799
	.long 0x00001809
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d10310
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_170800
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d45450
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_170801
	.long 0x00001809
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00c55350
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_170802
	.long 0x00001809
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00d15350
		# worddata
	.long 0x00000000
	.long 0x00000008
	.long 0x00000000
	.long 0x00000008
	.text
 	.align 3
	.ent SplayTree_splay_inner_code_169420
 # arguments : [$169422,$0] [$169423,$1] [$169111,$2] [$169112,$3] 
 # results    : [$169836,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree_splay_inner_code_169420:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $4
	stl	$3, 28($sp)
code_170803:
funtop_169763:
	# Proj_c at label type_168512_INT
	ldl	$25, ($0)
	stl	$25, 24($sp)
	# Proj_c at label type_168491_INT
	ldl	$6, 4($0)
	# Proj_c at label var_poly_c_167930_INT
	ldl	$25, 8($0)
	stl	$25, 8($sp)
	ldl	$25, ($1)
	stl	$25, 20($sp)
	ldl	$25, 4($1)
	stl	$25, 16($sp)
	ldl	$25, 8($1)
	stl	$25, 12($sp)
	lda	$1, order_TYC
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
	mov	$6, $1
	mov	$5, $27
	jsr	$26, ($5), 1
code_170824:
	ldgp	$gp, ($26)
	mov	$0, $1
code_170806:
	# done making normal call
	addl	$13, 44, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170807
code_170808:
	jsr	$26, GCFromML
gc_check_170807:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 24($sp)
	stl	$25, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, SplayTree_adj_code_169426
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 28($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_170825:
	ldgp	$gp, ($26)
code_170810:
	# done making normal call
	addl	$13, 84, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170811
code_170812:
	jsr	$26, GCFromML
gc_check_170811:
	ldl	$1, ($0)
	ldl	$4, 4($0)
	ldl	$3, 8($0)
sumarm_169832:
	bne	$1, sumarm_169833
code_170814:
	ldl	$0, 12($sp)
	br	$31, after_sum_169829
sumarm_169833:
	ldl	$0, ($1)
	bne	$0, sumarm_169837
code_170816:
	ldl	$2, 4($1)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$4, 4($13)
	stl	$3, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	stl	$2, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	lda	$0, ($31)
	stl	$0, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	br	$31, after_sum_169829
sumarm_169837:
	ldl	$0, ($1)
	cmpeq	$0, 1, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_169861
code_170819:
	ldl	$2, 4($1)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$4, 4($13)
	stl	$3, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	stl	$2, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	lda	$0, 1($31)
	stl	$0, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	br	$31, after_sum_169829
sumarm_169861:
	ldl	$2, 4($1)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$4, 4($13)
	stl	$3, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	stl	$2, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	lda	$0, 2($31)
	stl	$0, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	br	$31, after_sum_169829
sumarm_169885:
after_sum_169829:
code_170823:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end SplayTree_splay_inner_code_169420

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170807
	.long 0x00001005
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00005550
		# -------- label,sizes,reg
	.long gc_check_170811
	.long 0x00001005
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_170824
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005550
		# -------- label,sizes,reg
	.long code_170825
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent SplayTree_splay_r_code_169415
 # arguments : [$169417,$0] [$167930,$1] [$169418,$2] [$167931,$3] 
 # results    : [$169757,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree_splay_r_code_169415:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
code_170826:
funtop_169722:
	# start making constructor call
	lda	$0, record_169499
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 8($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_170833:
	ldgp	$gp, ($26)
	mov	$0, $1
code_170827:
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170828
code_170829:
	jsr	$26, GCFromML
gc_check_170828:
	# done making constructor call
	# allocating 3-record
	# done allocating 3 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	ldl	$25, 8($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, SplayTree_splay_inner_code_169420
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, record_169756
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_170832:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree_splay_r_code_169415

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170828
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_170833
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent SplayTree_lrotate_inner_code_169453
 # arguments : [$169455,$0] [$169456,$1] [$168115,$2] 
 # results    : [$169662,$0] 
 # destroys   :  $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $7 $6 $5 $4 $3 $2 $1 $0
SplayTree_lrotate_inner_code_169453:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
code_170834:
funtop_169648:
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170835
code_170836:
	jsr	$26, GCFromML
gc_check_170835:
sumarm_169658:
	bne	$2, sumarm_169659
code_170838:
	mov	$1, $0
	br	$31, after_sum_169655
sumarm_169659:
	ldl	$7, 8($2)
	ldl	$6, ($2)
	ldl	$0, 4($2)
sumarm_169678:
	bne	$0, sumarm_169679
code_170840:
	br	$31, after_sum_169675
sumarm_169679:
	ldl	$5, 8($0)
	ldl	$3, ($0)
	ldl	$4, 4($0)
	# allocating 3-record
	lda	$2, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $2, $2
	stl	$2, ($13)
	stl	$6, 4($13)
	stl	$3, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	stl	$7, 12($13)
	addl	$13, 4, $3
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$2, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $2, $2
	stl	$2, ($13)
	stl	$3, 4($13)
	stl	$4, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	stl	$5, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# making direct call 
	mov	$0, $2
	br	$31, funtop_169648
code_170842:
	# done making self tail call
	lda	$0, ($31)
	mov	$0, $2
	br	$31, after_sum_169675
sumarm_169683:
after_sum_169675:
	mov	$2, $0
	br	$31, after_sum_169655
sumarm_169663:
after_sum_169655:
code_170846:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree_lrotate_inner_code_169453

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170835
	.long 0x00000805
	.long 0x01ff0f06
	.long 0x01ff0f00
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent SplayTree_join_inner_code_169462
 # arguments : [$169464,$0] [$169465,$1] [$169155,$2] [$169156,$3] 
 # results    : [$169599,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree_join_inner_code_169462:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$1, $0
	stl	$3, 12($sp)
code_170847:
funtop_169570:
	ldl	$1, ($0)
	ldl	$0, 4($0)
sumarm_169584:
	bne	$2, sumarm_169585
sumarm_169593:
	ldl	$25, 12($sp)
	bne	$25, sumarm_169594
code_170849:
	mov	$0, $3
	br	$31, after_sum_169590
sumarm_169594:
nomatch_sum_169591:
	ldl	$3, 12($sp)
after_sum_169590:
	mov	$3, $0
	br	$31, after_sum_169581
sumarm_169585:
nomatch_sum_169582:
sumarm_169606:
	ldl	$25, 12($sp)
	bne	$25, sumarm_169607
code_170852:
	br	$31, after_sum_169603
sumarm_169607:
nomatch_sum_169604:
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_170863:
	ldgp	$gp, ($26)
code_170854:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170855
code_170856:
	jsr	$26, GCFromML
gc_check_170855:
sumarm_169623:
	bne	$0, sumarm_169624
code_170858:
	ldl	$3, 12($sp)
	br	$31, after_sum_169620
sumarm_169624:
	ldl	$3, 8($0)
	ldl	$2, ($0)
	# allocating 3-record
	lda	$1, 793($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 10, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	stl	$2, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	stl	$3, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	mov	$0, $3
	br	$31, after_sum_169620
sumarm_169628:
after_sum_169620:
	mov	$3, $2
after_sum_169603:
	mov	$2, $0
after_sum_169581:
code_170862:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree_join_inner_code_169462

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170855
	.long 0x00000805
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_170863
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent SplayTree_join_r_code_169448
 # arguments : [$169450,$0] [$168146,$1] [$169451,$2] [$168147,$3] 
 # results    : [$169565,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
SplayTree_join_r_code_169448:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
code_170864:
funtop_169542:
	addl	$13, 44, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_170865
code_170866:
	jsr	$26, GCFromML
gc_check_170865:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, SplayTree_lrotate_inner_code_169453
	stl	$0, 4($13)
	stl	$4, 8($13)
	lda	$0, ($31)
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	lda	$0, ($31)
	stl	$0, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, SplayTree_join_inner_code_169462
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_170869:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree_join_r_code_169448

	.rdata
		# -------- label,sizes,reg
	.long gc_check_170865
	.long 0x00000805
	.long 0x01ff0ff0
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent SplayTree_main
 # arguments : 
 # results    : [$169541,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
SplayTree_main:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_170870:
funtop_169476:
	lda	$16, ($31)
	lda	$27, AssertMirrorPtrArray
	jsr	$26, save_regs_MLtoC
	jsr	$26, AssertMirrorPtrArray
code_170874:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_170871:
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 3-record
	# done allocating 3 record
	# allocating 4-record
	# done allocating 4 record
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, polyPLUSEsplay_INT_r_167752
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, splay_r_168110
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, join_r_168190
	# done allocating 1 closures
	# allocating 4-record
	# done allocating 4 record
	lda	$0, 256($31)
code_170873:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end SplayTree_main

	.rdata
		# -------- label,sizes,reg
	.long code_170874
	.long 0x00000805
	.long 0x00000e00
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
	.text
SplayTree_unit_CODE_END_VAL:
	.rdata
		# endgcinfo with filler for alignment
	.globl SplayTree_unit_GCTABLE_END_VAL
SplayTree_unit_GCTABLE_END_VAL:
	.long 0x00000000
	.sdata
	.align 3
	.sdata
	.align 3
	.globl SplayTree_unit_GLOBALS_BEGIN_VAL
SplayTree_unit_GLOBALS_BEGIN_VAL:
		# static record tag
	.long 0x00000211
record_169483:
	.long SplayTree__code_169371
	.long 0x00000100
		# static record tag
	.long 0x00000211
record_169487:
	.long SplayTree__code_169376
	.long 0x00000100
		# static record tag
	.long 0x00000211
record_169491:
	.long SplayTree__code_169381
	.long 0x00000100
		# static record tag
	.long 0x00000211
record_169495:
	.long SplayTree__code_169391
	.long 0x00000100
		# static record tag
	.long 0x00000211
record_169499:
	.long SplayTree__code_169396
	.long 0x00000100
		# static record tag
	.long 0x00000719
record_169504:
	.long record_169483
	.long record_169487
	.long record_169491
		# static record tag
	.long 0x00000f21
record_169510:
	.long record_169504
	.long record_169483
	.long record_169487
	.long record_169491
		# Global
	.long 0x0000006f
	.globl SplayTree_STR_c_INT
SplayTree_STR_c_INT:
	.long record_169510
	.long record_169510
		# static record tag
	.long 0x00000619
polyPLUSEsplay_INT_r_167752:
	.long SplayTree_polyPLUSEsplay_INT_r_code_169401
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
splay_r_168110:
	.long SplayTree_splay_r_code_169415
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
join_r_168190:
	.long SplayTree_join_r_code_169448
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000f21
record_169539:
	.long 0x00000100
	.long polyPLUSEsplay_INT_r_167752
	.long splay_r_168110
	.long join_r_168190
		# Global
	.long 0x0000006f
	.globl SplayTree_STR_r_INT
SplayTree_STR_r_INT:
	.long record_169539
	.long record_169539
		# static record tag
	.long 0x00000719
record_169738:
	.long 0x00000000
	.long 0x00000000
	.long 0x00000000
		# static record tag
	.long 0x00000311
record_169742:
	.long 0x00000001
	.long 0x00000000
		# static record tag
	.long 0x00000719
record_169756:
	.long 0x00000000
	.long record_169738
	.long record_169742
		# static record tag
	.long 0x00000009
record_170663:
	.long 0x00000008
		# Module closure
	.long 0x00000619
	.globl SplayTree_unit_closure
SplayTree_unit_closure:
	.long SplayTree_main
	.long 0x00000000
	.long 0x00000000
	.long 0x00000311
	.globl SplayTree_unit
SplayTree_unit:
	.long SplayTree_unit_closure
	.long SplayTree_unit_closure
	.globl SplayTree_unit_GLOBALS_END_VAL
SplayTree_unit_GLOBALS_END_VAL:
	.long 0x00000000
	.long 0 #filler

	.sdata
	.align 3
	.globl SplayTree_unit_TRACE_GLOBALS_BEGIN_VAL
SplayTree_unit_TRACE_GLOBALS_BEGIN_VAL:
	.globl SplayTree_unit_TRACE_GLOBALS_END_VAL
SplayTree_unit_TRACE_GLOBALS_END_VAL:
		# filler so label is defined
	.long 0x00000000
