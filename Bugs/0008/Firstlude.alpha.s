	.set noat
	.rdata
		# gcinfo
	.globl Prelude_unit_GCTABLE_BEGIN_VAL
Prelude_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl Prelude_unit_CODE_END_VAL
	.globl Prelude_unit_CODE_BEGIN_VAL
Prelude_unit_CODE_BEGIN_VAL:
	.text
 	.align 3
	.ent Prelude__code_5488
 # arguments : [$5490,$0] [$2520,$1] 
 # results    : [$8910,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
Prelude__code_5488:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8916:
funtop_8902:
	# allocating 1-record
	# done allocating 1 record
	lda	$0, record_8908
code_8918:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5488

	.rdata
	.text
 	.align 3
	.ent Prelude__code_5493
 # arguments : [$5495,$0] [$2524,$1] 
 # results    : [$8897,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude__code_5493:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
code_8919:
funtop_8885:
	# start making constructor call
	lda	$0, record_5943
	ldl	$2, ($0)
	ldl	$0, 4($0)
	ldl	$1, 8($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_8926:
	ldgp	$gp, ($26)
	mov	$0, $1
code_8920:
	addl	$13, 20, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_8921
code_8922:
	jsr	$26, GCFromML
gc_check_8921:
	# done making constructor call
	# allocating 4-record
	lda	$0, 3105($31)
	stl	$0, ($13)
	lda	$0, 5($31)
	stl	$0, 4($13)
	lda	$0, 2($31)
	stl	$0, 8($13)
	ldl	$25, 8($sp)
	stl	$25, 12($13)
	stl	$1, 16($13)
	addl	$13, 4, $0
	addl	$13, 20, $13
	# done allocating 4 record
code_8925:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5493

	.rdata
		# -------- label,sizes,reg
	.long gc_check_8921
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_8926
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent Prelude__code_5498
 # arguments : [$5500,$0] [$2527,$1] 
 # results    : [$8878,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude__code_5498:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8927:
funtop_8867:
	# start making constructor call
	lda	$0, record_5947
	ldl	$2, ($0)
	ldl	$0, 4($0)
	mov	$2, $27
	jsr	$26, ($2), 1
code_8934:
	ldgp	$gp, ($26)
	mov	$0, $1
code_8928:
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_8929
code_8930:
	jsr	$26, GCFromML
gc_check_8929:
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
code_8933:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5498

	.rdata
		# -------- label,sizes,reg
	.long gc_check_8929
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_8934
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude__code_5503
 # arguments : [$5505,$0] [$2574,$1] 
 # results    : [$8866,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
Prelude__code_5503:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8935:
funtop_8858:
	# allocating 1-record
	# done allocating 1 record
	lda	$0, record_8864
code_8937:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5503

	.rdata
	.text
 	.align 3
	.ent Prelude__code_5508
 # arguments : [$5510,$0] [$2578,$1] 
 # results    : [$8857,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
Prelude__code_5508:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8938:
funtop_8851:
	# allocating 1-record
	# done allocating 1 record
	lda	$0, record_8856
code_8940:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5508

	.rdata
	.text
 	.align 3
	.ent Prelude__code_5513
 # arguments : [$5515,$0] [$2581,$1] 
 # results    : [$8844,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude__code_5513:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8941:
funtop_8833:
	# start making constructor call
	lda	$0, record_5959
	ldl	$2, ($0)
	ldl	$0, 4($0)
	mov	$2, $27
	jsr	$26, ($2), 1
code_8948:
	ldgp	$gp, ($26)
	mov	$0, $1
code_8942:
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_8943
code_8944:
	jsr	$26, GCFromML
gc_check_8943:
	# done making constructor call
	# allocating 5-record
	lda	$0, 4137($31)
	stl	$0, ($13)
	lda	$0, 4($31)
	stl	$0, 4($13)
	lda	$0, -1($31)
	stl	$0, 8($13)
	lda	$0, ($31)
	stl	$0, 12($13)
	lda	$0, 1($31)
	stl	$0, 16($13)
	stl	$1, 20($13)
	addl	$13, 4, $0
	addl	$13, 24, $13
	# done allocating 5 record
code_8947:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5513

	.rdata
		# -------- label,sizes,reg
	.long gc_check_8943
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_8948
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude__code_5518
 # arguments : [$5520,$0] [$2584,$1] 
 # results    : [$8832,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
Prelude__code_5518:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8949:
funtop_8824:
	# allocating 1-record
	# done allocating 1 record
	lda	$0, record_8830
code_8951:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5518

	.rdata
	.text
 	.align 3
	.ent Prelude__code_5523
 # arguments : [$5525,$0] [$2588,$1] 
 # results    : [$2588,$0] 
 # destroys   :  $0
 # modifies   :  $0
Prelude__code_5523:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $0
code_8952:
funtop_8820:
code_8954:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5523

	.rdata
	.text
 	.align 3
	.ent Prelude__code_5528
 # arguments : [$5530,$0] [$2591,$1] 
 # results    : [$8813,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude__code_5528:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8955:
funtop_8802:
	# start making constructor call
	lda	$0, record_5971
	ldl	$2, ($0)
	ldl	$0, 4($0)
	mov	$2, $27
	jsr	$26, ($2), 1
code_8962:
	ldgp	$gp, ($26)
	mov	$0, $1
code_8956:
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_8957
code_8958:
	jsr	$26, GCFromML
gc_check_8957:
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
code_8961:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude__code_5528

	.rdata
		# -------- label,sizes,reg
	.long gc_check_8957
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_8962
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_vars_eq_0_code_5533
 # arguments : [$5535,$0] [$5536,$1] [$4427,$2] [$4428,$3] 
 # results    : [$8788,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_vars_eq_0_code_5533:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_8963:
funtop_8766:
	bne	$2, one_case_8773
zero_case_8772:
sumarm_8780:
	bne	$3, sumarm_8781
code_8965:
	lda	$0, 1($31)
	br	$31, after_sum_8777
sumarm_8781:
nomatch_sum_8778:
	lda	$0, ($31)
after_sum_8777:
	br	$31, after_zeroone_8774
one_case_8773:
sumarm_8794:
	cmpeq	$3, 1, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_8795
code_8969:
	lda	$0, 1($31)
	br	$31, after_sum_8791
sumarm_8795:
nomatch_sum_8792:
	lda	$0, ($31)
after_sum_8791:
after_zeroone_8774:
code_8972:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_vars_eq_0_code_5533

	.rdata
	.text
 	.align 3
	.ent Prelude_vars_eq_0_code_5543
 # arguments : [$5545,$0] [$5546,$1] [$4433,$2] [$4434,$3] 
 # results    : [$8718,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_vars_eq_0_code_5543:
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
code_8973:
funtop_8691:
sumarm_8702:
	ldl	$25, 20($sp)
	bne	$25, sumarm_8703
sumarm_8710:
	ldl	$25, 16($sp)
	bne	$25, sumarm_8711
code_8975:
	lda	$0, 1($31)
	br	$31, after_sum_8707
sumarm_8711:
nomatch_sum_8708:
	lda	$0, ($31)
after_sum_8707:
	br	$31, after_sum_8699
sumarm_8703:
sumarm_8726:
	lda	$0, 255($31)
	ldl	$25, 16($sp)
	cmple	$25, $0, $0
	bne	$0, nomatch_sum_8724
code_8979:
	ldl	$25, 20($sp)
	ldl	$2, ($25)
	ldl	$25, 16($sp)
	ldl	$3, ($25)
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$4, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	mov	$4, $27
	jsr	$26, ($4), 1
code_8988:
	ldgp	$gp, ($26)
code_8980:
	# done making normal call
	bne	$0, one_case_8747
zero_case_8746:
	lda	$0, ($31)
	br	$31, after_zeroone_8748
one_case_8747:
	ldl	$25, 20($sp)
	ldl	$1, 4($25)
	ldl	$25, 16($sp)
	ldl	$0, 4($25)
	# making direct call 
	stl	$1, 20($sp)
	stl	$0, 16($sp)
	br	$31, funtop_8691
code_8983:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_8748:
	br	$31, after_sum_8723
sumarm_8727:
nomatch_sum_8724:
	lda	$0, ($31)
after_sum_8723:
	br	$31, after_sum_8699
sumarm_8719:
after_sum_8699:
code_8987:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Prelude_vars_eq_0_code_5543

	.rdata
		# -------- label,sizes,reg
	.long code_8988
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000540
	.text
 	.align 3
	.ent Prelude_polyPLUSElist_INT_r_code_5538
 # arguments : [$5540,$0] [$2532,$1] [$5541,$2] [$2533,$3] 
 # results    : [$8686,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
Prelude_polyPLUSElist_INT_r_code_5538:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
code_8989:
funtop_8677:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_8990
code_8991:
	jsr	$26, GCFromML
gc_check_8990:
	ldl	$1, ($3)
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_vars_eq_0_code_5543
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_8994:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_polyPLUSElist_INT_r_code_5538

	.rdata
		# -------- label,sizes,reg
	.long gc_check_8990
	.long 0x00000805
	.long 0x01ff0ff8
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_vars_eq_0_code_5557
 # arguments : [$5559,$0] [$5560,$1] [$4450,$2] [$4451,$3] 
 # results    : [$8638,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_vars_eq_0_code_5557:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $5
code_8995:
funtop_8607:
	# Proj_c at label type_3499_INT
	ldl	$4, ($0)
	# Proj_c at label mpoly_var_c_2596_INT
	ldl	$25, 4($0)
	stl	$25, 8($sp)
sumarm_8622:
	bne	$5, sumarm_8623
sumarm_8630:
	bne	$3, sumarm_8631
code_8997:
	lda	$0, 1($31)
	br	$31, after_sum_8627
sumarm_8631:
nomatch_sum_8628:
	lda	$0, ($31)
after_sum_8627:
	br	$31, after_sum_8619
sumarm_8623:
sumarm_8646:
	lda	$0, 255($31)
	cmple	$3, $0, $0
	bne	$0, nomatch_sum_8644
code_9001:
	ldl	$2, 16($4)
	cmple	$2, 4, $0
	bne	$0, dynamic_box_8657
code_9003:
	cmple	$2, 255, $0
	bne	$0, dynamic_nobox_8658
code_9005:
	ldl	$2, ($2)
	cmpeq	$2, 12, $0
	bne	$0, dynamic_box_8657
code_9007:
	cmpeq	$2, 4, $0
	bne	$0, dynamic_box_8657
code_9009:
	cmpeq	$2, 8, $0
	bne	$0, dynamic_box_8657
dynamic_nobox_8658:
	br	$31, projsum_single_after_8654
dynamic_box_8657:
	ldl	$5, ($5)
projsum_single_after_8654:
	ldl	$2, 16($4)
	cmple	$2, 4, $0
	bne	$0, dynamic_box_8665
code_9014:
	cmple	$2, 255, $0
	bne	$0, dynamic_nobox_8666
code_9016:
	ldl	$2, ($2)
	cmpeq	$2, 12, $0
	bne	$0, dynamic_box_8665
code_9018:
	cmpeq	$2, 4, $0
	bne	$0, dynamic_box_8665
code_9020:
	cmpeq	$2, 8, $0
	bne	$0, dynamic_box_8665
dynamic_nobox_8666:
	br	$31, projsum_single_after_8662
dynamic_box_8665:
	ldl	$3, ($3)
projsum_single_after_8662:
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$5, $2
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_9024:
	# done making tail call
	br	$31, after_sum_8643
sumarm_8647:
nomatch_sum_8644:
	lda	$0, ($31)
after_sum_8643:
	br	$31, after_sum_8619
sumarm_8639:
after_sum_8619:
code_9028:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_vars_eq_0_code_5557

	.rdata
	.text
 	.align 3
	.ent Prelude_polyPLUSEoption_INT_r_code_5552
 # arguments : [$5554,$0] [$2596,$1] [$5555,$2] [$2597,$3] 
 # results    : [$8602,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
Prelude_polyPLUSEoption_INT_r_code_5552:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
code_9030:
funtop_8580:
	addl	$13, 52, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9031
code_9032:
	jsr	$26, GCFromML
gc_check_9031:
	# allocating 5-record
	lda	$0, 4137($31)
	stl	$0, ($13)
	lda	$0, 4($31)
	stl	$0, 4($13)
	lda	$0, 1($31)
	stl	$0, 8($13)
	lda	$0, 1($31)
	stl	$0, 12($13)
	lda	$0, 2($31)
	stl	$0, 16($13)
	stl	$4, 20($13)
	addl	$13, 4, $1
	addl	$13, 24, $13
	# done allocating 5 record
	ldl	$2, ($3)
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$4, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_vars_eq_0_code_5557
	stl	$0, 4($13)
	stl	$1, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9035:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_polyPLUSEoption_INT_r_code_5552

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9031
	.long 0x00000805
	.long 0x01ff0ff8
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_vars_eq_0_code_5568
 # arguments : [$5570,$0] [$5571,$1] [$4460,$2] [$4461,$3] 
 # results    : [$8549,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_vars_eq_0_code_5568:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9036:
funtop_8524:
sumarm_8533:
	bne	$2, sumarm_8534
sumarm_8541:
	bne	$3, sumarm_8542
code_9038:
	lda	$0, 1($31)
	br	$31, after_sum_8538
sumarm_8542:
nomatch_sum_8539:
	lda	$0, ($31)
after_sum_8538:
	br	$31, after_sum_8530
sumarm_8534:
	cmpeq	$2, 1, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_8550
sumarm_8557:
	cmpeq	$3, 1, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_8558
code_9044:
	lda	$0, 1($31)
	br	$31, after_sum_8554
sumarm_8558:
nomatch_sum_8555:
	lda	$0, ($31)
after_sum_8554:
	br	$31, after_sum_8530
sumarm_8550:
sumarm_8572:
	cmpeq	$3, 2, $0
	cmpeq	$0, $31, $0
	bne	$0, sumarm_8573
code_9048:
	lda	$0, 1($31)
	br	$31, after_sum_8569
sumarm_8573:
nomatch_sum_8570:
	lda	$0, ($31)
after_sum_8569:
	br	$31, after_sum_8530
sumarm_8565:
after_sum_8530:
code_9052:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_vars_eq_0_code_5568

	.rdata
	.text
 	.align 3
	.ent Prelude_anonfun_code_5573
 # arguments : [$5575,$0] [$5576,$1] [$4465,$2] [$4466,$3] 
 # results    : [$8523,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_anonfun_code_5573:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9053:
funtop_8518:
	cmpeq	$2, $3, $0
code_9055:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5573

	.rdata
	.text
 	.align 3
	.ent Prelude_anonfun_code_5578
 # arguments : [$5580,$0] [$5581,$1] [$4468,$2] [$4469,$3] 
 # results    : [$8517,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_anonfun_code_5578:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9056:
funtop_8512:
	cmpeq	$2, $3, $0
code_9058:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5578

	.rdata
	.text
 	.align 3
	.ent Prelude_anonfun_code_5583
 # arguments : [$5585,$0] [$5586,$1] [$2760,$2] 
 # results    : [$8507,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Prelude_anonfun_code_5583:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9059:
funtop_8498:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9060
code_9061:
	jsr	$26, GCFromML
gc_check_9060:
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_3690
	ldl	$0, ($0)
	stl	$0, 4($13)
	stl	$2, 8($13)
	lda	$0, string_8506
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_9065:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5583

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9060
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_anonfun_code_5588
 # arguments : [$5590,$0] [$5591,$1] [$2774,$2] 
 # results    : [$8493,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Prelude_anonfun_code_5588:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9066:
funtop_8482:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9067
code_9068:
	jsr	$26, GCFromML
gc_check_9067:
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_3702
	ldl	$0, ($0)
	stl	$0, 4($13)
	stl	$2, 8($13)
	lda	$0, string_8492
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_9072:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5588

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9067
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_vector_eq_loop_code_5609
 # arguments : [$5611,$0] [$5612,$1] [$2789,$2] 
 # results    : [$8479,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_vector_eq_loop_code_5609:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 32($sp)
	stl	$1, 28($sp)
	stl	$2, 36($sp)
code_9073:
funtop_8417:
	# Proj_c at label type_3710_INT
	ldl	$25, 32($sp)
	ldl	$25, ($25)
	stl	$25, 24($sp)
	# Proj_c at label var_poly_c_2776_INT
	ldl	$25, 32($sp)
	ldl	$25, 4($25)
	stl	$25, 8($sp)
	ldl	$25, 28($sp)
	ldl	$25, ($25)
	stl	$25, 20($sp)
	ldl	$25, 28($sp)
	ldl	$0, 4($25)
	ldl	$25, 28($sp)
	ldl	$1, 8($25)
	ldl	$25, 28($sp)
	ldl	$25, 12($25)
	stl	$25, 16($sp)
	ldl	$25, 36($sp)
	cmpule	$0, $25, $0
	bne	$0, one_case_8440
zero_case_8439:
	# making direct call 
	lda	$0, polyVsub_INT
	ldl	$3, ($0)
	ldl	$0, 24($sp)
	ldl	$2, 36($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9088:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_9076:
	# done making normal call
	# making direct call 
	lda	$0, polyVsub_INT
	ldl	$3, ($0)
	ldl	$0, 24($sp)
	ldl	$1, 16($sp)
	ldl	$2, 36($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9086:
	ldgp	$gp, ($26)
	mov	$0, $3
code_9078:
	# done making normal call
	# making closure call 
	ldl	$25, 20($sp)
	ldl	$4, ($25)
	ldl	$25, 20($sp)
	ldl	$0, 4($25)
	ldl	$25, 20($sp)
	ldl	$1, 8($25)
	ldl	$2, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_9087:
	ldgp	$gp, ($26)
code_9079:
	# done making normal call
	bne	$0, one_case_8465
zero_case_8464:
	lda	$0, ($31)
	br	$31, after_zeroone_8466
one_case_8465:
	ldl	$25, 36($sp)
	addl	$25, 1, $0
	# making direct call 
	stl	$0, 36($sp)
	br	$31, funtop_8417
code_9082:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_8466:
	br	$31, after_zeroone_8441
one_case_8440:
	lda	$0, 1($31)
after_zeroone_8441:
code_9085:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end Prelude_vector_eq_loop_code_5609

	.rdata
		# -------- label,sizes,reg
	.long code_9086
	.long 0x00001807
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x000144d0
		# worddata
	.long 0x00000000
	.long 0x00000008
		# -------- label,sizes,reg
	.long code_9087
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00014000
		# -------- label,sizes,reg
	.long code_9088
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00015510
	.text
 	.align 3
	.ent Prelude_anonfun_code_5603
 # arguments : [$5605,$0] [$5606,$1] [$4478,$2] [$4479,$3] 
 # results    : [$8408,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_anonfun_code_5603:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 20($sp)
	stl	$2, 28($sp)
	stl	$3, 24($sp)
code_9089:
funtop_8360:
	# Proj_c at label type_3710_INT
	ldl	$25, ($0)
	stl	$25, 16($sp)
	# Proj_c at label var_poly_c_2776_INT
	ldl	$25, 4($0)
	stl	$25, 12($sp)
	# making direct call 
	lda	$0, polyLen_INT
	ldl	$2, ($0)
	ldl	$0, 16($sp)
	ldl	$1, 28($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_9103:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_9091:
	# done making normal call
	# making direct call 
	lda	$0, polyLen_INT
	ldl	$2, ($0)
	ldl	$0, 16($sp)
	ldl	$1, 24($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_9102:
	ldgp	$gp, ($26)
	mov	$0, $3
code_9093:
	# done making normal call
	addl	$13, 48, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9094
code_9095:
	jsr	$26, GCFromML
gc_check_9094:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 16($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	addl	$13, 4, $2
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 4-record
	lda	$0, 3361($31)
	stl	$0, ($13)
	ldl	$25, 20($sp)
	stl	$25, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	ldl	$25, 28($sp)
	stl	$25, 12($13)
	ldl	$25, 24($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_vector_eq_loop_code_5609
	stl	$0, 4($13)
	stl	$2, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	ldl	$25, 8($sp)
	cmpeq	$25, $3, $0
	bne	$0, one_case_8404
zero_case_8403:
	lda	$0, ($31)
	br	$31, after_zeroone_8405
one_case_8404:
	lda	$2, ($31)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_9099:
	# done making tail call
after_zeroone_8405:
code_9101:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5603

	.rdata
		# -------- label,sizes,reg
	.long code_9102
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005540
		# -------- label,sizes,reg
	.long gc_check_9094
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005540
		# -------- label,sizes,reg
	.long code_9103
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005540
	.text
 	.align 3
	.ent Prelude_vector_eq_inner_code_5598
 # arguments : [$5600,$0] [$5601,$1] [$2780,$2] 
 # results    : [$8355,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_vector_eq_inner_code_5598:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $3
code_9105:
funtop_8338:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9106
code_9107:
	jsr	$26, GCFromML
gc_check_9106:
	# Proj_c at label type_3710_INT
	ldl	$2, ($0)
	# Proj_c at label var_poly_c_2776_INT
	ldl	$1, 4($0)
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
	lda	$0, Prelude_anonfun_code_5603
	stl	$0, 4($13)
	stl	$1, 8($13)
	stl	$3, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9110:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_vector_eq_inner_code_5598

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9106
	.long 0x00000805
	.long 0x01ff0ff9
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_vector_eq_r_code_5593
 # arguments : [$5595,$0] [$2776,$1] [$5596,$2] [$2777,$3] 
 # results    : [$8332,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_vector_eq_r_code_5593:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9111:
funtop_8320:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9112
code_9113:
	jsr	$26, GCFromML
gc_check_9112:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_vector_eq_inner_code_5598
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9116:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_vector_eq_r_code_5593

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9112
	.long 0x00000805
	.long 0x01ff0ff2
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_anonfun_code_5631
 # arguments : [$5633,$0] [$5634,$1] [$4493,$2] [$4494,$3] 
 # results    : [$8319,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_anonfun_code_5631:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9117:
funtop_8314:
	cmpeq	$2, $3, $0
code_9119:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5631

	.rdata
	.text
 	.align 3
	.ent Prelude_vars_eq_0_code_5636
 # arguments : [$5638,$0] [$5639,$1] [$4520,$2] [$4521,$3] 
 # results    : [$8313,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_vars_eq_0_code_5636:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
	stl	$3, 8($sp)
code_9120:
funtop_8243:
sumarm_8252:
sumarm_8260:
	ldl	$25, 12($sp)
	ldl	$2, ($25)
	ldl	$25, 8($sp)
	ldl	$3, ($25)
	# making closure call 
	lda	$1, PLUSEstring_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_9131:
	ldgp	$gp, ($26)
code_9122:
	# done making normal call
	bne	$0, one_case_8284
zero_case_8283:
	lda	$0, ($31)
	br	$31, after_zeroone_8285
one_case_8284:
	ldl	$25, 12($sp)
	ldl	$1, 4($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	cmpeq	$1, $0, $0
after_zeroone_8285:
	bne	$0, one_case_8299
zero_case_8298:
	lda	$0, ($31)
	br	$31, after_zeroone_8300
one_case_8299:
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	ldl	$25, 8($sp)
	ldl	$0, 8($25)
	cmpeq	$1, $0, $0
after_zeroone_8300:
	br	$31, after_sum_8257
sumarm_8261:
after_sum_8257:
	br	$31, after_sum_8249
sumarm_8253:
after_sum_8249:
code_9130:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_vars_eq_0_code_5636

	.rdata
		# -------- label,sizes,reg
	.long code_9131
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent Prelude_anonfun_code_5654
 # arguments : [$5656,$0] [$5657,$1] [$2863,$2] 
 # results    : [$8242,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_anonfun_code_5654:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$1, $0
code_9132:
funtop_8222:
	ldl	$1, ($0)
	ldl	$25, 4($0)
	stl	$25, 12($sp)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9138:
	ldgp	$gp, ($26)
	mov	$0, $2
code_9133:
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
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_9134:
	# done making tail call
code_9136:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5654

	.rdata
		# -------- label,sizes,reg
	.long code_9138
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent Prelude_o_inner_code_5646
 # arguments : [$5648,$0] [$5649,$1] [$4547,$2] [$4548,$3] 
 # results    : [$8221,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_o_inner_code_5646:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 28($sp)
	mov	$3, $4
code_9139:
funtop_8158:
	# Proj_c at label type_3848_INT
	ldl	$25, ($0)
	stl	$25, 24($sp)
	# Proj_c at label type_3847_INT
	ldl	$25, 4($0)
	stl	$25, 20($sp)
	# Proj_c at label type_3846_INT
	ldl	$25, 8($0)
	stl	$25, 16($sp)
	# Proj_c at label var_poly_c_2852_INT
	ldl	$25, 12($0)
	stl	$25, 12($sp)
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 24($sp)
	ldl	$2, 16($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_9152:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_9141:
	# done making normal call
	# making closure call 
	lda	$1, onearg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 16($sp)
	ldl	$2, 20($sp)
	ldl	$4, 28($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_9151:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9143:
	# done making normal call
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9144
code_9145:
	jsr	$26, GCFromML
gc_check_9144:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	ldl	$25, 8($sp)
	stl	$25, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_anonfun_code_5654
	stl	$0, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $4
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$1, vararg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	ldl	$1, 24($sp)
	ldl	$2, 20($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_9153:
	ldgp	$gp, ($26)
code_9148:
	# done making normal call
code_9150:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Prelude_o_inner_code_5646

	.rdata
		# -------- label,sizes,reg
	.long code_9151
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001450
		# -------- label,sizes,reg
	.long gc_check_9144
	.long 0x00001005
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00001450
		# -------- label,sizes,reg
	.long code_9152
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00005540
		# -------- label,sizes,reg
	.long code_9153
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_o_r_code_5641
 # arguments : [$5643,$0] [$2852,$1] [$5644,$2] [$2853,$3] 
 # results    : [$8152,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
Prelude_o_r_code_5641:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
code_9154:
funtop_8133:
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9155
code_9156:
	jsr	$26, GCFromML
gc_check_9155:
	# Proj_c at label 'a_TYV
	ldl	$3, ($4)
	# Proj_c at label 'b_TYV
	ldl	$2, 4($4)
	# Proj_c at label 'c_TYV
	ldl	$1, 8($4)
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 3873($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$2, 8($13)
	stl	$3, 12($13)
	stl	$4, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_o_inner_code_5646
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9159:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_o_r_code_5641

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9155
	.long 0x00000805
	.long 0x01ff0ff0
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_before_inner_code_5674
 # arguments : [$5676,$0] [$5677,$1] [$4571,$2] [$4572,$3] 
 # results    : [$4571,$0] 
 # destroys   :  $3 $1 $0
 # modifies   :  $3 $1 $0
Prelude_before_inner_code_5674:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$2, $0
code_9160:
funtop_8127:
code_9162:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_before_inner_code_5674

	.rdata
	.text
 	.align 3
	.ent Prelude_before_r_code_5669
 # arguments : [$5671,$0] [$2868,$1] [$5672,$2] [$2869,$3] 
 # results    : [$8121,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_before_r_code_5669:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9163:
funtop_8114:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9164
code_9165:
	jsr	$26, GCFromML
gc_check_9164:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_before_inner_code_5674
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9168:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_before_r_code_5669

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9164
	.long 0x00000805
	.long 0x01ff0ff2
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_ignore_inner_code_5686
 # arguments : [$5688,$0] [$5689,$1] [$2884,$2] 
 # results    : [$8113,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Prelude_ignore_inner_code_5686:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
code_9169:
funtop_8107:
	lda	$0, 256($31)
code_9171:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_ignore_inner_code_5686

	.rdata
	.text
 	.align 3
	.ent Prelude_ignore_r_code_5681
 # arguments : [$5683,$0] [$2880,$1] [$5684,$2] [$2881,$3] 
 # results    : [$8106,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_ignore_r_code_5681:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9172:
funtop_8079:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9173
code_9174:
	jsr	$26, GCFromML
gc_check_9173:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_ignore_inner_code_5686
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $6
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# allocating 2-record
	# done allocating 2 record
	lda	$4, record_8096
	# making closure call 
	lda	$2, vararg_INT
	ldl	$0, 1092($12)
	addl	$2, $0, $0
	ldl	$2, ($0)
	ldl	$5, ($2)
	ldl	$0, 4($2)
	ldl	$3, 8($2)
	mov	$4, $2
	mov	$6, $4
	mov	$5, $27
	jsr	$26, ($5), 1
code_9180:
	ldgp	$gp, ($26)
code_9177:
	# done making normal call
code_9179:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_ignore_r_code_5681

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9173
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_9180
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_exnName_code_5693
 # arguments : [$5695,$0] [$5696,$1] [$2890,$2] 
 # results    : [$8078,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_exnName_code_5693:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9181:
funtop_8074:
	# making external call
	mov	$2, $16
	lda	$27, exnNameRuntime
	jsr	$26, save_regs_MLtoC
	jsr	$26, exnNameRuntime
code_9185:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_9182:
	# done making external call
code_9184:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_exnName_code_5693

	.rdata
		# -------- label,sizes,reg
	.long code_9185
	.long 0x00000805
	.long 0x00000e00
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_exnMessage_code_5698
 # arguments : [$5700,$0] [$5701,$1] [$2893,$2] 
 # results    : [$8073,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_exnMessage_code_5698:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9186:
funtop_8069:
	# making external call
	mov	$2, $16
	lda	$27, exnMessageRuntime
	jsr	$26, save_regs_MLtoC
	jsr	$26, exnMessageRuntime
code_9190:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_9187:
	# done making external call
code_9189:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_exnMessage_code_5698

	.rdata
		# -------- label,sizes,reg
	.long code_9190
	.long 0x00000805
	.long 0x00000e00
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_revappend_inner_code_5708
 # arguments : [$5710,$0] [$5711,$1] [$4582,$2] [$4583,$3] 
 # results    : [$8043,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
Prelude_revappend_inner_code_5708:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$3, $4
code_9191:
funtop_8029:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9192
code_9193:
	jsr	$26, GCFromML
gc_check_9192:
sumarm_8039:
	bne	$2, sumarm_8040
code_9195:
	mov	$4, $0
	br	$31, after_sum_8036
sumarm_8040:
	ldl	$3, ($2)
	ldl	$2, 4($2)
	# allocating 2-record
	lda	$1, 529($31)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	sll	$0, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $1
	stl	$1, ($13)
	or	$31, 3, $at
	ldl	$25, 8($sp)
	cmpult	$at, $25, $0
	stl	$3, 4($13)
	stl	$4, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# making direct call 
	mov	$0, $4
	br	$31, funtop_8029
code_9197:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_8036
sumarm_8044:
after_sum_8036:
code_9200:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_revappend_inner_code_5708

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9192
	.long 0x00000805
	.long 0x01ff0ff4
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent Prelude_rev_inner_code_5715
 # arguments : [$5717,$0] [$5718,$1] [$2899,$2] 
 # results    : [$8028,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_rev_inner_code_5715:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $0
code_9201:
funtop_8015:
	ldl	$1, ($0)
	ldl	$3, 4($0)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_9202:
	# done making tail call
code_9204:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_rev_inner_code_5715

	.rdata
	.text
 	.align 3
	.ent Prelude_rev_r_code_5703
 # arguments : [$5705,$0] [$2895,$1] [$5706,$2] [$2896,$3] 
 # results    : [$8009,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_rev_r_code_5703:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9206:
funtop_7987:
	addl	$13, 44, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9207
code_9208:
	jsr	$26, GCFromML
gc_check_9207:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_revappend_inner_code_5708
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
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
	lda	$0, Prelude_rev_inner_code_5715
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9211:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_rev_r_code_5703

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9207
	.long 0x00000805
	.long 0x01ff0ff2
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_lengthPRIME_inner_code_5729
 # arguments : [$5731,$0] [$5732,$1] [$4614,$2] [$4615,$3] 
 # results    : [$7971,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_lengthPRIME_inner_code_5729:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9212:
funtop_7959:
sumarm_7967:
	bne	$3, sumarm_7968
code_9213:
	mov	$2, $0
	br	$31, after_sum_7964
sumarm_7968:
	ldl	$1, 4($3)
	addlv	$2, 1, $0
	trapb
	# making direct call 
	mov	$0, $2
	mov	$1, $3
	br	$31, funtop_7959
code_9215:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_7964
sumarm_7972:
after_sum_7964:
code_9218:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_lengthPRIME_inner_code_5729

	.rdata
	.text
 	.align 3
	.ent Prelude_length_inner_code_5734
 # arguments : [$5736,$0] [$5737,$1] [$2932,$2] 
 # results    : [$7958,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_length_inner_code_5734:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $3
code_9219:
funtop_7948:
	lda	$2, ($31)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_9220:
	# done making tail call
code_9222:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_length_inner_code_5734

	.rdata
	.text
 	.align 3
	.ent Prelude_length_r_code_5724
 # arguments : [$5726,$0] [$2928,$1] [$5727,$2] [$2929,$3] 
 # results    : [$7942,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_length_r_code_5724:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9224:
funtop_7930:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9225
code_9226:
	jsr	$26, GCFromML
gc_check_9225:
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$1, record_7937
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 537($31)
	stl	$0, ($13)
	lda	$0, Prelude_length_inner_code_5734
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9229:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_length_r_code_5724

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9225
	.long 0x00000805
	.long 0x01ff0ff0
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_array_inner_code_5746
 # arguments : [$5748,$0] [$5749,$1] [$4636,$2] [$4637,$3] 
 # results    : [$7924,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_array_inner_code_5746:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	mov	$2, $4
	mov	$3, $2
code_9230:
funtop_7899:
	# Proj_c at label type_3980_INT
	ldl	$25, 8($sp)
	ldl	$1, ($25)
	# Proj_c at label var_poly_c_2960_INT
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	cmplt	$4, 0, $0
	bne	$0, one_case_7914
zero_case_7913:
	# making direct call 
	lda	$0, polyArray_INT
	ldl	$3, ($0)
	mov	$1, $0
	mov	$4, $1
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_9233:
	# done making tail call
	br	$31, after_zeroone_7915
one_case_7914:
	lda	$1, mk_3683
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
after_zeroone_7915:
code_9238:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_array_inner_code_5746

	.rdata
	.text
 	.align 3
	.ent Prelude_array_r_code_5741
 # arguments : [$5743,$0] [$2960,$1] [$5744,$2] [$2961,$3] 
 # results    : [$7893,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_array_r_code_5741:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9240:
funtop_7881:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9241
code_9242:
	jsr	$26, GCFromML
gc_check_9241:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_array_inner_code_5746
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9245:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_array_r_code_5741

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9241
	.long 0x00000805
	.long 0x01ff0ff2
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_vsub_inner_code_5760
 # arguments : [$5762,$0] [$5763,$1] [$4641,$2] [$4642,$3] 
 # results    : [$7875,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_vsub_inner_code_5760:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 20($sp)
	stl	$3, 16($sp)
code_9246:
funtop_7845:
	# Proj_c at label type_4003_INT
	ldl	$25, ($0)
	stl	$25, 12($sp)
	# Proj_c at label var_poly_c_2975_INT
	ldl	$25, 4($0)
	stl	$25, 8($sp)
	# making direct call 
	lda	$0, polyLen_INT
	ldl	$2, ($0)
	ldl	$0, 12($sp)
	ldl	$1, 20($sp)
	mov	$2, $27
	jsr	$26, ($2), 1
code_9257:
	ldgp	$gp, ($26)
code_9248:
	# done making normal call
	ldl	$25, 16($sp)
	cmpule	$0, $25, $0
	bne	$0, one_case_7866
zero_case_7865:
	# making direct call 
	lda	$0, polyVsub_INT
	ldl	$3, ($0)
	ldl	$0, 12($sp)
	ldl	$1, 20($sp)
	ldl	$2, 16($sp)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_9251:
	# done making tail call
	br	$31, after_zeroone_7867
one_case_7866:
	lda	$1, mk_3679
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
after_zeroone_7867:
code_9256:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Prelude_vsub_inner_code_5760

	.rdata
		# -------- label,sizes,reg
	.long code_9257
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000450
	.text
 	.align 3
	.ent Prelude_vsub_r_code_5755
 # arguments : [$5757,$0] [$2975,$1] [$5758,$2] [$2976,$3] 
 # results    : [$7839,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_vsub_r_code_5755:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9259:
funtop_7827:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9260
code_9261:
	jsr	$26, GCFromML
gc_check_9260:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_vsub_inner_code_5760
	stl	$0, 4($13)
	stl	$1, 8($13)
	lda	$0, 256($31)
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9264:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_vsub_r_code_5755

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9260
	.long 0x00000805
	.long 0x01ff0ff2
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_ord_code_5769
 # arguments : [$5771,$0] [$5772,$1] [$2992,$2] 
 # results    : [$2992,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
Prelude_ord_code_5769:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_9265:
funtop_7823:
code_9267:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_ord_code_5769

	.rdata
	.text
 	.align 3
	.ent Prelude_chr_code_5774
 # arguments : [$5776,$0] [$5777,$1] [$2995,$2] 
 # results    : [$2995,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
Prelude_chr_code_5774:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_9268:
funtop_7819:
code_9270:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_chr_code_5774

	.rdata
	.text
 	.align 3
	.ent Prelude_size_code_5779
 # arguments : [$5781,$0] [$5782,$1] [$3001,$2] 
 # results    : [$7817,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Prelude_size_code_5779:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9271:
funtop_7808:
load_nonstall_tag_7814:
	ldl	$1, -4($2)
	cmpeq	$1, 15, $0
	bne	$0, load_nonstall_tag_7814
load_true_tag_7815:
	and	$1, 3, $0
	bne	$0, loaded_tag_7816
code_9274:
	ldl	$1, -4($1)
	br	$31, load_true_tag_7815
loaded_tag_7816:
	zap	$1, 240, $0
	srl	$0, 3, $0
	addl	$0, $31, $0
code_9277:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_size_code_5779

	.rdata
	.text
 	.align 3
	.ent Prelude_explode_loop_code_5789
 # arguments : [$5791,$0] [$5792,$1] [$4649,$2] [$4650,$3] 
 # results    : [$7785,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_explode_loop_code_5789:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
	mov	$2, $5
	mov	$3, $2
code_9278:
funtop_7760:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9279
code_9280:
	jsr	$26, GCFromML
gc_check_9279:
	ldl	$1, ($4)
	ldl	$0, 4($4)
	cmpult	$5, $0, $0
	bne	$0, one_case_7773
zero_case_7772:
	# making closure call 
	lda	$1, _4039
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_9284:
	# done making tail call
	br	$31, after_zeroone_7774
one_case_7773:
	addl	$5, 1, $3
	# int sub start
	addl	$5, $1, $0
	lda	$1, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $1, $1
	# int sub end
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$2, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# making direct call 
	mov	$3, $5
	mov	$0, $2
	br	$31, funtop_7760
code_9286:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_7774:
code_9288:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_explode_loop_code_5789

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9279
	.long 0x00000805
	.long 0x00000014
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_explode_code_5784
 # arguments : [$5786,$0] [$5787,$1] [$3004,$2] 
 # results    : [$7759,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_explode_code_5784:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9290:
funtop_7728:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9291
code_9292:
	jsr	$26, GCFromML
gc_check_9291:
load_nonstall_tag_7734:
	ldl	$1, -4($2)
	cmpeq	$1, 15, $0
	bne	$0, load_nonstall_tag_7734
load_true_tag_7735:
	and	$1, 3, $0
	bne	$0, loaded_tag_7736
code_9296:
	ldl	$1, -4($1)
	br	$31, load_true_tag_7735
loaded_tag_7736:
	zap	$1, 240, $1
	srl	$1, 3, $1
	addl	$1, $31, $1
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 273($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_explode_loop_code_5789
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	lda	$2, ($31)
	lda	$3, ($31)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_9298:
	# done making tail call
code_9300:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_explode_code_5784

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9291
	.long 0x00000805
	.long 0x00000004
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_wordcopyLoop_code_5803
 # arguments : [$5805,$0] [$5806,$1] [$4679,$2] [$4680,$3] 
 # results    : [$7706,$0] 
 # destroys   :  $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_wordcopyLoop_code_5803:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $5
	mov	$3, $4
code_9302:
funtop_7687:
	ldl	$2, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	cmpult	$4, $2, $2
	bne	$2, one_case_7702
zero_case_7701:
	lda	$0, 256($31)
	br	$31, after_zeroone_7703
one_case_7702:
	addl	$5, 1, $8
	addl	$4, 1, $7
	# int sub start
	s4addl	$5, $3, $2
	ldl	$6, ($2)
	# int sub end
	ldl	$2, 1080($12)
	ldl	$3, 1084($12)
	addl	$2, 12, $2
	cmple	$2, $3, $2
	bne	$2, afterMutateCheck_9308
code_9310:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9308:
	sll	$4, 2, $5
	addl	$5, $31, $5
	ldl	$4, 1080($12)
	mov	$0, $3
	mov	$5, $2
	stl	$3, ($4)
	stl	$2, 4($4)
	addl	$4, 12, $2
	stl	$2, 1080($12)
	addl	$0, $5, $0
	stl	$6, ($0)
	# making direct call 
	mov	$8, $5
	mov	$7, $4
	br	$31, funtop_7687
code_9317:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_7703:
code_9319:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_wordcopyLoop_code_5803

	.rdata
		# -------- label,sizes,reg
	.long afterMutateCheck_9308
	.long 0x00000805
	.long 0x01ff0e03
	.long 0x01ff0e00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_bytecopyLoop_code_5814
 # arguments : [$5816,$0] [$5817,$1] [$5217,$2] [$5218,$3] 
 # results    : [$7666,$0] 
 # destroys   :  $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_bytecopyLoop_code_5814:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
	mov	$2, $5
	mov	$3, $8
code_9320:
funtop_7647:
	ldl	$0, ($4)
	ldl	$7, 4($4)
	ldl	$2, 8($4)
	cmpult	$8, $0, $0
	bne	$0, one_case_7662
zero_case_7661:
	lda	$0, 256($31)
	br	$31, after_zeroone_7663
one_case_7662:
	addl	$5, 1, $1
	addl	$8, 1, $0
	# int sub start
	addl	$5, $2, $2
	lda	$6, ($2)
	ldq_u	$at, ($2)
	extbl	$at, $6, $6
	# int sub end
	ldl	$2, 1080($12)
	ldl	$3, 1084($12)
	addl	$2, 12, $2
	cmple	$2, $3, $2
	bne	$2, afterMutateCheck_9326
code_9328:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9326:
	ldl	$5, 1080($12)
	mov	$7, $2
	mov	$8, $3
	stl	$2, ($5)
	stl	$3, 4($5)
	addl	$5, 12, $2
	stl	$2, 1080($12)
	addl	$7, $8, $5
	lda	$3, ($5)
	ldq_u	$2, ($5)
	mskbl	$2, $3, $2
	insbl	$6, $3, $3
	or	$3, $2, $3
	stq_u	$3, ($5)
	# making direct call 
	mov	$1, $5
	mov	$0, $8
	br	$31, funtop_7647
code_9337:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_7663:
code_9339:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_bytecopyLoop_code_5814

	.rdata
		# -------- label,sizes,reg
	.long afterMutateCheck_9326
	.long 0x00000805
	.long 0x01ff0e90
	.long 0x01ff0e00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_HAT_code_5798
 # arguments : [$5800,$0] [$5801,$1] [$4713,$2] [$4714,$3] 
 # results    : [$7574,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_HAT_code_5798:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 24($sp)
	stl	$3, 20($sp)
code_9340:
funtop_7544:
load_nonstall_tag_7550:
	ldl	$25, 24($sp)
	ldl	$1, -4($25)
	cmpeq	$1, 15, $0
	bne	$0, load_nonstall_tag_7550
load_true_tag_7551:
	and	$1, 3, $0
	bne	$0, loaded_tag_7552
code_9343:
	ldl	$1, -4($1)
	br	$31, load_true_tag_7551
loaded_tag_7552:
	zap	$1, 240, $25
	stl	$25, 16($sp)
	ldl	$25, 16($sp)
	srl	$25, 3, $25
	stl	$25, 16($sp)
	ldl	$25, 16($sp)
	addl	$25, $31, $25
	stl	$25, 16($sp)
load_nonstall_tag_7557:
	ldl	$25, 20($sp)
	ldl	$1, -4($25)
	cmpeq	$1, 15, $0
	bne	$0, load_nonstall_tag_7557
load_true_tag_7558:
	and	$1, 3, $0
	bne	$0, loaded_tag_7559
code_9347:
	ldl	$1, -4($1)
	br	$31, load_true_tag_7558
loaded_tag_7559:
	zap	$1, 240, $0
	srl	$0, 3, $0
	addl	$0, $31, $0
	ldl	$25, 16($sp)
	addl	$25, $0, $25
	stl	$25, 12($sp)
	lda	$2, ($31)
	# making closure call 
	lda	$1, chr_2994
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9373:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9349:
	# done making normal call
	# initializing int/ptr array start
	ldl	$25, 12($sp)
	addl	$25, 3, $0
	zap	$0, 240, $2
	srl	$2, 2, $2
	addl	$2, $31, $2
	sll	$1, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $0
	sll	$0, 16, $17
	addl	$17, $31, $17
	or	$17, $0, $17
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_7581
code_9351:
	ldl	$16, 12($sp)
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_9370:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_9352:
	br	$31, array_int_after_7580
array_int_small_7581:
	ldl	$25, 12($sp)
	sll	$25, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9354
code_9355:
	jsr	$26, GCFromML
gc_check_9354:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_7588
array_init_loopto_7589:
	ldl	$25, 8($sp)
	addl	$25, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_7588:
	bge	$1, array_init_loopto_7589
array_int_after_7580:
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9360
code_9361:
	jsr	$26, GCFromML
gc_check_9360:
	ldl	$25, 16($sp)
	addl	$25, 3, $0
	zap	$0, 240, $0
	srl	$0, 2, $0
	addl	$0, $31, $0
	addl	$0, 0, $1
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	ldl	$25, 24($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_wordcopyLoop_code_5803
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	lda	$2, ($31)
	lda	$3, ($31)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_9371:
	ldgp	$gp, ($26)
code_9363:
	# done making normal call
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9364
code_9365:
	jsr	$26, GCFromML
gc_check_9364:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	ldl	$25, 20($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_bytecopyLoop_code_5814
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	lda	$2, ($31)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 16($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_9372:
	ldgp	$gp, ($26)
code_9367:
	# done making normal call
	ldl	$at, 8($sp)
	stl	$at, 8($sp)
code_9369:
	ldl	$0, 8($sp)
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Prelude_HAT_code_5798

	.rdata
		# -------- label,sizes,reg
	.long code_9370
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001400
		# -------- label,sizes,reg
	.long gc_check_9354
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001400
		# -------- label,sizes,reg
	.long gc_check_9360
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001410
		# -------- label,sizes,reg
	.long gc_check_9364
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000410
		# -------- label,sizes,reg
	.long code_9371
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000410
		# -------- label,sizes,reg
	.long code_9372
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_9373
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001400
	.text
 	.align 3
	.ent Prelude_anonfun_code_5835
 # arguments : [$5837,$0] [$5838,$1] [$3104,$2] 
 # results    : [$7520,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_anonfun_code_5835:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $3
code_9374:
funtop_7499:
	ldl	$0, ($1)
	ldl	$7, 4($1)
	ldl	$6, 8($1)
	ldl	$2, 12($1)
sumarm_7515:
	bne	$3, sumarm_7516
code_9375:
	lda	$0, 256($31)
	br	$31, after_sum_7512
sumarm_7516:
	ldl	$5, ($3)
	ldl	$25, 4($3)
	stl	$25, 8($sp)
	ldl	$1, 1080($12)
	ldl	$3, 1084($12)
	addl	$1, 12, $1
	cmple	$1, $3, $1
	bne	$1, afterMutateCheck_9380
code_9382:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9380:
	ldl	$4, 1080($12)
	mov	$6, $3
	mov	$7, $1
	stl	$3, ($4)
	stl	$1, 4($4)
	addl	$4, 12, $1
	stl	$1, 1080($12)
	addl	$6, $7, $4
	lda	$3, ($4)
	ldq_u	$1, ($4)
	mskbl	$1, $3, $1
	insbl	$5, $3, $3
	or	$3, $1, $3
	stq_u	$3, ($4)
	# making closure call 
	ldl	$4, ($0)
	ldl	$3, 4($0)
	ldl	$1, 8($0)
	mov	$3, $0
	mov	$4, $27
	jsr	$26, ($4), 1
code_9396:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9391:
	# done making normal call
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_9392:
	# done making tail call
	br	$31, after_sum_7512
sumarm_7521:
after_sum_7512:
code_9395:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5835

	.rdata
		# -------- label,sizes,reg
	.long code_9396
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long afterMutateCheck_9380
	.long 0x00000805
	.long 0x00000041
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent Prelude_loop_code_5830
 # arguments : [$5832,$0] [$5833,$1] [$3102,$2] 
 # results    : [$7493,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
Prelude_loop_code_5830:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $4
code_9398:
funtop_7473:
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9399
code_9400:
	jsr	$26, GCFromML
gc_check_9399:
	ldl	$3, ($1)
	ldl	$2, 4($1)
	addl	$4, 1, $1
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 1313($31)
	stl	$0, ($13)
	stl	$3, 4($13)
	stl	$4, 8($13)
	stl	$2, 12($13)
	stl	$1, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_anonfun_code_5835
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9403:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_loop_code_5830

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9399
	.long 0x00000805
	.long 0x01ff0fe2
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_implode_code_5825
 # arguments : [$5827,$0] [$5828,$1] [$3091,$2] 
 # results    : [$7429,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_implode_code_5825:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
code_9404:
funtop_7406:
	# making closure call 
	lda	$1, _4142
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9428:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_9406:
	# done making normal call
	lda	$2, ($31)
	# making closure call 
	lda	$1, chr_2994
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9425:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9407:
	# done making normal call
	# initializing int/ptr array start
	ldl	$25, 8($sp)
	addl	$25, 3, $0
	zap	$0, 240, $2
	srl	$2, 2, $2
	addl	$2, $31, $2
	sll	$1, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $0
	sll	$0, 16, $17
	addl	$17, $31, $17
	or	$17, $0, $17
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_7436
code_9409:
	ldl	$16, 8($sp)
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_9426:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_9410:
	br	$31, array_int_after_7435
array_int_small_7436:
	ldl	$25, 8($sp)
	sll	$25, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9412
code_9413:
	jsr	$26, GCFromML
gc_check_9412:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_7443
array_init_loopto_7444:
	ldl	$25, 8($sp)
	addl	$25, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_7443:
	bge	$1, array_init_loopto_7444
array_int_after_7435:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9418
code_9419:
	jsr	$26, GCFromML
gc_check_9418:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_loop_code_5830
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, 258($31)
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	stl	$0, 8($1)
	# done allocating 1 closures
	lda	$2, ($31)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9429:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9421:
	# done making normal call
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9427:
	ldgp	$gp, ($26)
code_9422:
	# done making normal call
	ldl	$at, 8($sp)
	stl	$at, 8($sp)
code_9424:
	ldl	$0, 8($sp)
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_implode_code_5825

	.rdata
		# -------- label,sizes,reg
	.long code_9425
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_9426
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_9412
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_9418
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_9427
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_9428
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_9429
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent Prelude_anonfun_code_5860
 # arguments : [$5862,$0] [$5863,$1] [$3133,$2] 
 # results    : [$7382,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_anonfun_code_5860:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $3
code_9430:
funtop_7361:
	ldl	$0, ($1)
	ldl	$7, 4($1)
	ldl	$6, 8($1)
	ldl	$2, 12($1)
sumarm_7377:
	bne	$3, sumarm_7378
code_9431:
	lda	$0, 256($31)
	br	$31, after_sum_7374
sumarm_7378:
	ldl	$5, ($3)
	ldl	$25, 4($3)
	stl	$25, 8($sp)
	ldl	$1, 1080($12)
	ldl	$3, 1084($12)
	addl	$1, 12, $1
	cmple	$1, $3, $1
	bne	$1, afterMutateCheck_9436
code_9438:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9436:
	ldl	$4, 1080($12)
	mov	$6, $3
	mov	$7, $1
	stl	$3, ($4)
	stl	$1, 4($4)
	addl	$4, 12, $1
	stl	$1, 1080($12)
	addl	$6, $7, $4
	lda	$3, ($4)
	ldq_u	$1, ($4)
	mskbl	$1, $3, $1
	insbl	$5, $3, $3
	or	$3, $1, $3
	stq_u	$3, ($4)
	# making closure call 
	ldl	$4, ($0)
	ldl	$3, 4($0)
	ldl	$1, 8($0)
	mov	$3, $0
	mov	$4, $27
	jsr	$26, ($4), 1
code_9452:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9447:
	# done making normal call
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_9448:
	# done making tail call
	br	$31, after_sum_7374
sumarm_7383:
after_sum_7374:
code_9451:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5860

	.rdata
		# -------- label,sizes,reg
	.long code_9452
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long afterMutateCheck_9436
	.long 0x00000805
	.long 0x00000041
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent Prelude_loop_code_5855
 # arguments : [$5857,$0] [$5858,$1] [$3131,$2] 
 # results    : [$7355,$0] 
 # destroys   :  $4 $3 $2 $1 $0
 # modifies   :  $4 $3 $2 $1 $0
Prelude_loop_code_5855:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $4
code_9454:
funtop_7335:
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9455
code_9456:
	jsr	$26, GCFromML
gc_check_9455:
	ldl	$3, ($1)
	ldl	$2, 4($1)
	subl	$4, 1, $1
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 1313($31)
	stl	$0, ($13)
	stl	$3, 4($13)
	stl	$4, 8($13)
	stl	$2, 12($13)
	stl	$1, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_anonfun_code_5860
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_9459:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_loop_code_5855

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9455
	.long 0x00000805
	.long 0x01ff0fe2
	.long 0x01ff0fe0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_revImplode_code_5850
 # arguments : [$5852,$0] [$5853,$1] [$4752,$2] [$4753,$3] 
 # results    : [$7289,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_revImplode_code_5850:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 16($sp)
	stl	$3, 12($sp)
code_9460:
funtop_7275:
	lda	$2, ($31)
	# making closure call 
	lda	$1, chr_2994
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9481:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9461:
	# done making normal call
	# initializing int/ptr array start
	ldl	$25, 16($sp)
	addl	$25, 3, $0
	zap	$0, 240, $2
	srl	$2, 2, $2
	addl	$2, $31, $2
	sll	$1, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $0
	sll	$0, 16, $17
	addl	$17, $31, $17
	or	$17, $0, $17
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_7296
code_9463:
	ldl	$16, 16($sp)
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_9479:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_9464:
	br	$31, array_int_after_7295
array_int_small_7296:
	ldl	$25, 16($sp)
	sll	$25, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9466
code_9467:
	jsr	$26, GCFromML
gc_check_9466:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_7303
array_init_loopto_7304:
	ldl	$25, 8($sp)
	addl	$25, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_7303:
	bge	$1, array_init_loopto_7304
array_int_after_7295:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9472
code_9473:
	jsr	$26, GCFromML
gc_check_9472:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_loop_code_5855
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, 258($31)
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	stl	$0, 8($1)
	# done allocating 1 closures
	ldl	$25, 16($sp)
	subl	$25, 1, $2
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9482:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9475:
	# done making normal call
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9480:
	ldgp	$gp, ($26)
code_9476:
	# done making normal call
	ldl	$at, 8($sp)
	stl	$at, 8($sp)
code_9478:
	ldl	$0, 8($sp)
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Prelude_revImplode_code_5850

	.rdata
		# -------- label,sizes,reg
	.long code_9479
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_9466
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_9472
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_9480
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_9481
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_9482
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent Prelude_bytecopyLoop_code_5880
 # arguments : [$5882,$0] [$5883,$1] [$5238,$2] [$5239,$3] 
 # results    : [$7254,$0] 
 # destroys   :  $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_bytecopyLoop_code_5880:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
	mov	$2, $5
	mov	$3, $8
code_9483:
funtop_7235:
	ldl	$7, ($4)
	ldl	$2, 4($4)
	ldl	$0, 8($4)
	cmpult	$8, $0, $0
	bne	$0, one_case_7250
zero_case_7249:
	lda	$0, 256($31)
	br	$31, after_zeroone_7251
one_case_7250:
	addl	$5, 1, $1
	addl	$8, 1, $0
	# int sub start
	addl	$5, $2, $2
	lda	$6, ($2)
	ldq_u	$at, ($2)
	extbl	$at, $6, $6
	# int sub end
	ldl	$2, 1080($12)
	ldl	$3, 1084($12)
	addl	$2, 12, $2
	cmple	$2, $3, $2
	bne	$2, afterMutateCheck_9489
code_9491:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9489:
	ldl	$5, 1080($12)
	mov	$7, $2
	mov	$8, $3
	stl	$2, ($5)
	stl	$3, 4($5)
	addl	$5, 12, $2
	stl	$2, 1080($12)
	addl	$7, $8, $5
	lda	$3, ($5)
	ldq_u	$2, ($5)
	mskbl	$2, $3, $2
	insbl	$6, $3, $3
	or	$3, $2, $3
	stq_u	$3, ($5)
	# making direct call 
	mov	$1, $5
	mov	$0, $8
	br	$31, funtop_7235
code_9500:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_7251:
code_9502:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_bytecopyLoop_code_5880

	.rdata
		# -------- label,sizes,reg
	.long afterMutateCheck_9489
	.long 0x00000805
	.long 0x01ff0e90
	.long 0x01ff0e00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_substring_code_5875
 # arguments : [$5877,$0] [$5878,$1] [$4767,$2] [$4768,$3] [$4769,$4] 
 # results    : [$7178,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_substring_code_5875:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 28($sp)
	mov	$3, $5
code_9503:
funtop_7123:
	cmplt	$5, 0, $3
	cmplt	$4, 0, $2
load_nonstall_tag_7137:
	ldl	$25, 28($sp)
	ldl	$1, -4($25)
	cmpeq	$1, 15, $0
	bne	$0, load_nonstall_tag_7137
load_true_tag_7138:
	and	$1, 3, $0
	bne	$0, loaded_tag_7139
code_9506:
	ldl	$1, -4($1)
	br	$31, load_true_tag_7138
loaded_tag_7139:
	zap	$1, 240, $25
	stl	$25, 24($sp)
	ldl	$25, 24($sp)
	srl	$25, 3, $25
	stl	$25, 24($sp)
	ldl	$25, 24($sp)
	addl	$25, $31, $25
	stl	$25, 24($sp)
	bne	$3, one_case_7143
zero_case_7142:
	stl	$5, 20($sp)
	br	$31, after_zeroone_7144
one_case_7143:
	lda	$1, mk_3659
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
	stl	$0, 20($sp)
after_zeroone_7144:
	bne	$2, one_case_7155
zero_case_7154:
	stl	$4, 16($sp)
	br	$31, after_zeroone_7156
one_case_7155:
	lda	$1, mk_3659
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
	stl	$0, 16($sp)
after_zeroone_7156:
	ldl	$at, 20($sp)
	ldl	$25, 16($sp)
	addl	$25, $at, $25
	stl	$25, 12($sp)
	lda	$2, ($31)
	# making closure call 
	lda	$1, chr_2994
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_9537:
	ldgp	$gp, ($26)
	mov	$0, $1
code_9516:
	# done making normal call
	# initializing int/ptr array start
	ldl	$25, 16($sp)
	addl	$25, 3, $0
	zap	$0, 240, $2
	srl	$2, 2, $2
	addl	$2, $31, $2
	sll	$1, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $0
	sll	$0, 16, $17
	addl	$17, $31, $17
	or	$17, $0, $17
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_7185
code_9518:
	ldl	$16, 16($sp)
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_9538:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_9519:
	br	$31, array_int_after_7184
array_int_small_7185:
	ldl	$25, 16($sp)
	sll	$25, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9521
code_9522:
	jsr	$26, GCFromML
gc_check_9521:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_7192
array_init_loopto_7193:
	ldl	$25, 8($sp)
	addl	$25, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_7192:
	bge	$1, array_init_loopto_7193
array_int_after_7184:
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9527
code_9528:
	jsr	$26, GCFromML
gc_check_9527:
	ldl	$at, 12($sp)
	ldl	$25, 24($sp)
	cmpult	$25, $at, $0
	bne	$0, one_case_7200
zero_case_7199:
	ldl	$25, 16($sp)
	addl	$25, 0, $1
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 793($31)
	stl	$0, ($13)
	ldl	$25, 8($sp)
	stl	$25, 4($13)
	ldl	$25, 28($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, Prelude_bytecopyLoop_code_5880
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	lda	$3, ($31)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_9539:
	ldgp	$gp, ($26)
code_9531:
	# done making normal call
	br	$31, after_zeroone_7201
one_case_7200:
	lda	$1, mk_3659
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
after_zeroone_7201:
	ldl	$at, 8($sp)
	stl	$at, 8($sp)
code_9536:
	ldl	$0, 8($sp)
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Prelude_substring_code_5875

	.rdata
		# -------- label,sizes,reg
	.long code_9537
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00004000
		# -------- label,sizes,reg
	.long code_9538
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00004000
		# -------- label,sizes,reg
	.long gc_check_9521
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00004000
		# -------- label,sizes,reg
	.long gc_check_9527
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00004010
		# -------- label,sizes,reg
	.long code_9539
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent Prelude_char_eq_code_5891
 # arguments : [$5893,$0] [$5894,$1] [$4783,$2] [$4784,$3] 
 # results    : [$7122,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
Prelude_char_eq_code_5891:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9540:
funtop_7117:
	cmpeq	$2, $3, $0
code_9542:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_char_eq_code_5891

	.rdata
	.text
 	.align 3
	.ent Prelude_create_code_5896
 # arguments : [$5898,$0] [$5899,$1] [$3188,$2] 
 # results    : [$7095,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_create_code_5896:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $16
code_9543:
funtop_7079:
	cmplt	$31, $16, $0
	bne	$0, one_case_7088
zero_case_7087:
	lda	$1, mk_3683
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
	br	$31, after_zeroone_7089
one_case_7088:
	lda	$1, ($31)
	# initializing int/ptr array start
	addl	$16, 3, $0
	zap	$0, 240, $2
	srl	$2, 2, $2
	addl	$2, $31, $2
	sll	$1, 8, $0
	addl	$0, $31, $0
	or	$0, $1, $0
	sll	$0, 16, $17
	addl	$17, $31, $17
	or	$17, $0, $17
	lda	$0, 510($31)
	cmple	$2, $0, $0
	bne	$0, array_int_small_7108
code_9549:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_9560:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_9550:
	br	$31, array_int_after_7107
array_int_small_7108:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9552
code_9553:
	jsr	$26, GCFromML
gc_check_9552:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_7115
array_init_loopto_7116:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_7115:
	bge	$1, array_init_loopto_7116
array_int_after_7107:
	mov	$3, $0
after_zeroone_7089:
code_9559:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_create_code_5896

	.rdata
		# -------- label,sizes,reg
	.long code_9560
	.long 0x00000805
	.long 0x00000e00
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9552
	.long 0x00000805
	.long 0x00000e00
	.long 0x00000e00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_imod_code_5901
 # arguments : [$5903,$0] [$5904,$1] [$4813,$2] [$4814,$3] 
 # results    : [$7077,$0] 
 # destroys   :  $24 $23 $3 $2 $1 $0
 # modifies   :  $24 $23 $3 $2 $1 $0
Prelude_imod_code_5901:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9561:
funtop_7023:
	cmplt	$31, $3, $0
	mov	$2, $24
	mov	$3, $25
	lda	$27, __reml
	jsr	$23, __reml
code_9573:
	ldgp	$gp, ($23)
	mov	$27, $1
code_9562:
	trapb
	bne	$0, one_case_7035
zero_case_7034:
	lda	$0, ($31)
	br	$31, after_zeroone_7036
one_case_7035:
	cmple	$31, $1, $0
after_zeroone_7036:
	bne	$0, one_case_7047
zero_case_7046:
	cmplt	$3, 0, $0
	bne	$0, one_case_7056
zero_case_7055:
	lda	$0, ($31)
	br	$31, after_zeroone_7057
one_case_7056:
	cmple	$1, 0, $0
after_zeroone_7057:
	br	$31, after_zeroone_7048
one_case_7047:
	lda	$0, 1($31)
after_zeroone_7048:
	bne	$0, one_case_7071
zero_case_7070:
	addlv	$1, $3, $0
	trapb
	br	$31, after_zeroone_7072
one_case_7071:
	mov	$1, $0
after_zeroone_7072:
code_9572:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_imod_code_5901

	.rdata
		# -------- label,sizes,reg
	.long code_9573
	.long 0x00000805
	.long 0x007f0ff0
	.long 0x007f0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_idiv_code_5906
 # arguments : [$5908,$0] [$5909,$1] [$4820,$2] [$4821,$3] 
 # results    : [$7021,$0] 
 # destroys   :  $24 $23 $3 $2 $1 $0
 # modifies   :  $24 $23 $3 $2 $1 $0
Prelude_idiv_code_5906:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9574:
funtop_6953:
	cmple	$31, $2, $0
	mov	$2, $24
	mov	$3, $25
	lda	$27, __divl
	jsr	$23, __divl
code_9588:
	ldgp	$gp, ($23)
	mov	$27, $1
code_9575:
	trapb
	bne	$0, one_case_6965
zero_case_6964:
	lda	$0, ($31)
	br	$31, after_zeroone_6966
one_case_6965:
	cmplt	$31, $3, $0
after_zeroone_6966:
	bne	$0, one_case_6977
zero_case_6976:
	cmple	$2, 0, $0
	bne	$0, one_case_6986
zero_case_6985:
	lda	$0, ($31)
	br	$31, after_zeroone_6987
one_case_6986:
	cmplt	$3, 0, $0
after_zeroone_6987:
	br	$31, after_zeroone_6978
one_case_6977:
	lda	$0, 1($31)
after_zeroone_6978:
	bne	$0, one_case_7001
zero_case_7000:
	mullv	$3, $1, $0
	trapb
	cmpeq	$0, $2, $0
	bne	$0, one_case_7013
zero_case_7012:
	sublv	$1, 1, $0
	trapb
	br	$31, after_zeroone_7014
one_case_7013:
	mov	$1, $0
after_zeroone_7014:
	br	$31, after_zeroone_7002
one_case_7001:
	mov	$1, $0
after_zeroone_7002:
code_9587:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_idiv_code_5906

	.rdata
		# -------- label,sizes,reg
	.long code_9588
	.long 0x00000805
	.long 0x007f0ff0
	.long 0x007f0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_anonfun_code_5911
 # arguments : [$5913,$0] [$5914,$1] [$3237,$2] 
 # results    : [$6948,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Prelude_anonfun_code_5911:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9589:
funtop_6934:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9590
code_9591:
	jsr	$26, GCFromML
gc_check_9590:
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_4317
	ldl	$0, ($0)
	stl	$0, 4($13)
	stl	$2, 8($13)
	lda	$0, string_6947
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_9595:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5911

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9590
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_anonfun_code_5916
 # arguments : [$5918,$0] [$5919,$1] [$3243,$2] 
 # results    : [$6929,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Prelude_anonfun_code_5916:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9596:
funtop_6910:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9597
code_9598:
	jsr	$26, GCFromML
gc_check_9597:
	# allocating 3-record
	lda	$0, 1049($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_4320
	ldl	$0, ($0)
	stl	$0, 4($13)
	stl	$2, 8($13)
	lda	$0, string_6928
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_9602:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5916

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9597
	.long 0x00000805
	.long 0x01ff0ff8
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_anonfun_code_5921
 # arguments : [$5923,$0] [$5924,$1] [$3249,$2] 
 # results    : [$6905,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Prelude_anonfun_code_5921:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9603:
funtop_6885:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9604
code_9605:
	jsr	$26, GCFromML
gc_check_9604:
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_4323
	ldl	$0, ($0)
	stl	$0, 4($13)
	stl	$2, 8($13)
	lda	$0, string_6904
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
code_9609:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_anonfun_code_5921

	.rdata
		# -------- label,sizes,reg
	.long gc_check_9604
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent Prelude_main
 # arguments : 
 # results    : [$6884,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Prelude_main:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_9610:
funtop_5929:
	lda	$16, ($31)
	lda	$27, AssertMirrorPtrArray
	jsr	$26, save_regs_MLtoC
	jsr	$26, AssertMirrorPtrArray
code_10237:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_9611:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9612
code_9613:
	jsr	$26, GCFromML
gc_check_9612:
	# allocating 1-record
	# done allocating 1 record
	# allocating 5-record
	# done allocating 5 record
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
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 1-record
	# done allocating 1 record
	# allocating 5-record
	# done allocating 5 record
	# allocating 2-record
	# done allocating 2 record
	# allocating 1-record
	# done allocating 1 record
	# allocating 5-record
	# done allocating 5 record
	# allocating 5-record
	# done allocating 5 record
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, vars_eq_0_2498
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, polyPLUSElist_INT_r_2531
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, polyPLUSEoption_INT_r_2595
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, vars_eq_0_2632
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_2663
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_2672
	# done allocating 1 closures
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6057
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Match_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9619
code_9621:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9619:
	lda	$4, Match_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9630
code_9631:
	jsr	$26, GCFromML
gc_check_9630:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6081
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Bind_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9637
code_9639:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9637:
	lda	$4, Bind_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9648
code_9649:
	jsr	$26, GCFromML
gc_check_9648:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6109
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Overflow_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9655
code_9657:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9655:
	lda	$4, Overflow_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9666
code_9667:
	jsr	$26, GCFromML
gc_check_9666:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6132
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Div_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9673
code_9675:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9673:
	lda	$4, Div_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9684
code_9685:
	jsr	$26, GCFromML
gc_check_9684:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6155
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Mod_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9691
code_9693:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9691:
	lda	$4, Mod_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9702
code_9703:
	jsr	$26, GCFromML
gc_check_9702:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6179
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Quot_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9709
code_9711:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9709:
	lda	$4, Quot_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9720
code_9721:
	jsr	$26, GCFromML
gc_check_9720:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6204
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Floor_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9727
code_9729:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9727:
	lda	$4, Floor_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9738
code_9739:
	jsr	$26, GCFromML
gc_check_9738:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6228
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Sqrt_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9745
code_9747:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9745:
	lda	$4, Sqrt_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9756
code_9757:
	jsr	$26, GCFromML
gc_check_9756:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6251
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Exp_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9763
code_9765:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9763:
	lda	$4, Exp_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9774
code_9775:
	jsr	$26, GCFromML
gc_check_9774:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6273
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Ln_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9781
code_9783:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9781:
	lda	$4, Ln_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9792
code_9793:
	jsr	$26, GCFromML
gc_check_9792:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6296
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Ord_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9799
code_9801:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9799:
	lda	$4, Ord_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9810
code_9811:
	jsr	$26, GCFromML
gc_check_9810:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6319
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Chr_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9817
code_9819:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9817:
	lda	$4, Chr_r_INT
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
	lda	$2, exncounter
	ldl	$0, ($2)
	addl	$0, 1, $1
	stl	$1, ($2)
	# allocating 3-record
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9828
code_9829:
	jsr	$26, GCFromML
gc_check_9828:
	lda	$1, 1561($31)
	stl	$1, ($13)
	stl	$0, 4($13)
	lda	$1, 256($31)
	stl	$1, 8($13)
	lda	$1, string_6348
	stl	$1, 12($13)
	addl	$13, 4, $6
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$2, 111($31)
	lda	$1, mk_3659
	stl	$2, -4($1)
	ldl	$1, 1080($12)
	ldl	$2, 1084($12)
	addl	$1, 12, $1
	cmple	$1, $2, $1
	bne	$1, afterMutateCheck_9835
code_9837:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9835:
	lda	$5, mk_3659
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
	stl	$6, ($1)
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9846
code_9847:
	jsr	$26, GCFromML
gc_check_9846:
	lda	$1, 529($31)
	stl	$1, ($13)
	stl	$0, 4($13)
	lda	$1, mk_3659
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Substring_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9854
code_9856:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9854:
	lda	$4, Substring_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9865
code_9866:
	jsr	$26, GCFromML
gc_check_9865:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6376
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Hd_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9872
code_9874:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9872:
	lda	$4, Hd_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9883
code_9884:
	jsr	$26, GCFromML
gc_check_9883:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6398
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Tl_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9890
code_9892:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9890:
	lda	$4, Tl_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9901
code_9902:
	jsr	$26, GCFromML
gc_check_9901:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6425
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, NthTail_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9908
code_9910:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9908:
	lda	$4, NthTail_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9919
code_9920:
	jsr	$26, GCFromML
gc_check_9919:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6448
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Nth_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9926
code_9928:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9926:
	lda	$4, Nth_r_INT
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
	lda	$2, exncounter
	ldl	$0, ($2)
	addl	$0, 1, $1
	stl	$1, ($2)
	# allocating 3-record
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9937
code_9938:
	jsr	$26, GCFromML
gc_check_9937:
	lda	$1, 1561($31)
	stl	$1, ($13)
	stl	$0, 4($13)
	lda	$1, 256($31)
	stl	$1, 8($13)
	lda	$1, string_6477
	stl	$1, 12($13)
	addl	$13, 4, $6
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$2, 111($31)
	lda	$1, mk_3679
	stl	$2, -4($1)
	ldl	$1, 1080($12)
	ldl	$2, 1084($12)
	addl	$1, 12, $1
	cmple	$1, $2, $1
	bne	$1, afterMutateCheck_9944
code_9946:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9944:
	lda	$5, mk_3679
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
	stl	$6, ($1)
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9955
code_9956:
	jsr	$26, GCFromML
gc_check_9955:
	lda	$1, 529($31)
	stl	$1, ($13)
	stl	$0, 4($13)
	lda	$1, mk_3679
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Subscript_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_9963
code_9965:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9963:
	lda	$4, Subscript_r_INT
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
	lda	$2, exncounter
	ldl	$0, ($2)
	addl	$0, 1, $1
	stl	$1, ($2)
	# allocating 3-record
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9974
code_9975:
	jsr	$26, GCFromML
gc_check_9974:
	lda	$1, 1561($31)
	stl	$1, ($13)
	stl	$0, 4($13)
	lda	$1, 256($31)
	stl	$1, 8($13)
	lda	$1, string_6507
	stl	$1, 12($13)
	addl	$13, 4, $6
	addl	$13, 16, $13
	# done allocating 3 record
	lda	$2, 111($31)
	lda	$1, mk_3683
	stl	$2, -4($1)
	ldl	$1, 1080($12)
	ldl	$2, 1084($12)
	addl	$1, 12, $1
	cmple	$1, $2, $1
	bne	$1, afterMutateCheck_9981
code_9983:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_9981:
	lda	$5, mk_3683
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
	stl	$6, ($1)
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_9992
code_9993:
	jsr	$26, GCFromML
gc_check_9992:
	lda	$1, 529($31)
	stl	$1, ($13)
	stl	$0, 4($13)
	lda	$1, mk_3683
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Size_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10000
code_10002:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10000:
	lda	$4, Size_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10011
code_10012:
	jsr	$26, GCFromML
gc_check_10011:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6542
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Interrupt_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10018
code_10020:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10018:
	lda	$4, Interrupt_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	lda	$0, exn_stamp_3690
	stl	$2, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_2759
	# done allocating 1 closures
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10030
code_10031:
	jsr	$26, GCFromML
gc_check_10030:
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_3690
	ldl	$0, ($0)
	stl	$0, 4($13)
	lda	$0, anonfun_2759
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Io_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10038
code_10040:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10038:
	lda	$4, Io_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10049
code_10050:
	jsr	$26, GCFromML
gc_check_10049:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6587
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Domain_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10056
code_10058:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10056:
	lda	$4, Domain_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	# allocating 3-record
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10067
code_10068:
	jsr	$26, GCFromML
gc_check_10067:
	lda	$0, 1561($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, string_6611
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Span_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10074
code_10076:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10074:
	lda	$4, Span_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	lda	$0, exn_stamp_3702
	stl	$2, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_2773
	# done allocating 1 closures
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10086
code_10087:
	jsr	$26, GCFromML
gc_check_10086:
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_3702
	ldl	$0, ($0)
	stl	$0, 4($13)
	lda	$0, anonfun_2773
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, Fail_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10094
code_10096:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10094:
	lda	$4, Fail_r_INT
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
	lda	$0, vector_eq_r_2806
	# done allocating 1 closures
	lda	$5, ($31)
	lda	$3, 256($31)
	# making closure call 
	lda	$1, vector_eq_r_2806
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_10232:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_10105:
	# done making normal call
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_2816
	# done allocating 1 closures
	lda	$2, anonfun_2816
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$3, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	jsr	$26, ($3), 1
code_10236:
	ldgp	$gp, ($26)
	mov	$0, $5
code_10106:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, PLUSEstring_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10111
code_10113:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10111:
	lda	$4, PLUSEstring_INT
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
	lda	$0, vars_eq_0_2824
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, o_r_2867
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, before_r_2879
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, ignore_r_2888
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, exnName_2889
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, exnMessage_2892
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, rev_r_2927
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, length_r_2959
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, array_r_2974
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, vsub_r_2990
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, ord_2991
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, chr_2994
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, size_3000
	# done allocating 1 closures
	lda	$5, ($31)
	lda	$3, 256($31)
	# making closure call 
	lda	$1, rev_r_2927
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_10233:
	ldgp	$gp, ($26)
	mov	$0, $5
code_10122:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _4039
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10127
code_10129:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10127:
	lda	$4, _4039
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
	lda	$0, explode_3003
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, HAT_3071
	# done allocating 1 closures
	lda	$5, ($31)
	lda	$3, 256($31)
	# making closure call 
	lda	$1, length_r_2959
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_10234:
	ldgp	$gp, ($26)
	mov	$0, $5
code_10138:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _4142
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10143
code_10145:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10143:
	lda	$4, _4142
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
	lda	$0, implode_3090
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, revImplode_3121
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, substring_3150
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, char_eq_3175
	# done allocating 1 closures
	lda	$2, char_eq_3175
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$3, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	jsr	$26, ($3), 1
code_10235:
	ldgp	$gp, ($26)
	mov	$0, $5
code_10154:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, string_eq
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10159
code_10161:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10159:
	lda	$4, string_eq
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
	lda	$0, create_3187
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, imod_3194
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, idiv_3212
	# done allocating 1 closures
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	lda	$0, exn_stamp_4317
	stl	$2, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_3236
	# done allocating 1 closures
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10171
code_10172:
	jsr	$26, GCFromML
gc_check_10171:
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_4317
	ldl	$0, ($0)
	stl	$0, 4($13)
	lda	$0, anonfun_3236
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, LibFail_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10179
code_10181:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10179:
	lda	$4, LibFail_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	lda	$0, exn_stamp_4320
	stl	$2, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_3242
	# done allocating 1 closures
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10191
code_10192:
	jsr	$26, GCFromML
gc_check_10191:
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_4320
	ldl	$0, ($0)
	stl	$0, 4($13)
	lda	$0, anonfun_3242
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, RuntimeError_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10199
code_10201:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10199:
	lda	$4, RuntimeError_r_INT
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
	lda	$1, exncounter
	ldl	$2, ($1)
	addl	$2, 1, $0
	stl	$0, ($1)
	lda	$0, exn_stamp_4323
	stl	$2, ($0)
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_3248
	# done allocating 1 closures
	# allocating 2-record
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_10211
code_10212:
	jsr	$26, GCFromML
gc_check_10211:
	lda	$0, 529($31)
	stl	$0, ($13)
	lda	$0, exn_stamp_4323
	ldl	$0, ($0)
	stl	$0, 4($13)
	lda	$0, anonfun_3248
	stl	$0, 8($13)
	addl	$13, 4, $5
	addl	$13, 12, $13
	# done allocating 2 record
	lda	$1, 111($31)
	lda	$0, RuntimeErrorPRIME_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_10219
code_10221:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_10219:
	lda	$4, RuntimeErrorPRIME_r_INT
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
code_10231:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Prelude_main

	.rdata
		# -------- label,sizes,reg
	.long code_10232
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_10233
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_10234
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_10235
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10000
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10011
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10018
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10030
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10038
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10049
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10056
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10067
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10074
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10086
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10094
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_10236
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long afterMutateCheck_10111
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long afterMutateCheck_10127
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long afterMutateCheck_10143
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long afterMutateCheck_10159
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10171
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10179
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10191
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10199
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_10211
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_10219
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9612
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9619
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9630
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9637
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9648
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9655
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9666
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9673
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9684
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9691
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9702
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9709
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9720
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9727
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9738
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9745
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9756
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9763
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9774
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9781
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9792
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9799
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9810
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9817
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9828
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9835
	.long 0x00000805
	.long 0x00000040
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9846
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9854
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9865
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9872
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9883
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9890
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9901
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9908
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9919
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9926
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9937
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9944
	.long 0x00000805
	.long 0x00000040
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9955
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9963
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9974
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_9981
	.long 0x00000805
	.long 0x00000040
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long gc_check_9992
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_10237
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
Prelude_unit_CODE_END_VAL:
	.rdata
		# endgcinfo with filler for alignment
	.globl Prelude_unit_GCTABLE_END_VAL
Prelude_unit_GCTABLE_END_VAL:
	.long 0x00000000
	.sdata
	.align 3
	.sdata
	.align 3
	.globl Prelude_unit_GLOBALS_BEGIN_VAL
Prelude_unit_GLOBALS_BEGIN_VAL:
		# static record tag
	.long 0x00000009
record_5934:
	.long 0x00000008
		# Global
	.long 0x0000006f
	.globl bool_TYC
bool_TYC:
	.long record_5934
	.long record_5934
		# Global
	.long 0x0000006f
	.globl bool_sumarg_INT
bool_sumarg_INT:
	.globl order_sumarg_INT
order_sumarg_INT:
	.globl Match_c_INT
Match_c_INT:
	.globl Bind_c_INT
Bind_c_INT:
	.globl Overflow_c_INT
Overflow_c_INT:
	.globl Div_c_INT
Div_c_INT:
	.globl Mod_c_INT
Mod_c_INT:
	.globl Quot_c_INT
Quot_c_INT:
	.globl Floor_c_INT
Floor_c_INT:
	.globl Sqrt_c_INT
Sqrt_c_INT:
	.globl Exp_c_INT
Exp_c_INT:
	.globl Ln_c_INT
Ln_c_INT:
	.globl Ord_c_INT
Ord_c_INT:
	.globl Chr_c_INT
Chr_c_INT:
	.globl Substring_c_INT
Substring_c_INT:
	.globl Hd_c_INT
Hd_c_INT:
	.globl Tl_c_INT
Tl_c_INT:
	.globl NthTail_c_INT
NthTail_c_INT:
	.globl Nth_c_INT
Nth_c_INT:
	.globl Subscript_c_INT
Subscript_c_INT:
	.globl Size_c_INT
Size_c_INT:
	.globl Interrupt_c_INT
Interrupt_c_INT:
	.globl Io_c_INT
Io_c_INT:
	.globl Domain_c_INT
Domain_c_INT:
	.globl Span_c_INT
Span_c_INT:
	.globl Fail_c_INT
Fail_c_INT:
	.globl LibFail_c_INT
LibFail_c_INT:
	.globl RuntimeError_c_INT
RuntimeError_c_INT:
	.globl RuntimeErrorPRIME_c_INT
RuntimeErrorPRIME_c_INT:
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00001029
record_5939:
	.long 0x00000004
	.long 0xffffffff
	.long 0x00000002
	.long 0x00000002
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl bool_sum_INT
bool_sum_INT:
	.long record_5939
	.long record_5939
		# static record tag
	.long 0x00000211
record_5943:
	.long Prelude__code_5488
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl list_TYC
list_TYC:
	.long record_5943
	.long record_5943
		# static record tag
	.long 0x00000211
record_5947:
	.long Prelude__code_5493
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl list_sumarg_INT
list_sumarg_INT:
	.long record_5947
	.long record_5947
		# static record tag
	.long 0x00000211
record_5951:
	.long Prelude__code_5498
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl list_sum_INT
list_sum_INT:
	.long record_5951
	.long record_5951
		# static record tag
	.long 0x00000211
record_5955:
	.long Prelude__code_5503
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl susp_TYC
susp_TYC:
	.long record_5955
	.long record_5955
		# static record tag
	.long 0x00000211
record_5959:
	.long Prelude__code_5508
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl susp_sumarg_INT
susp_sumarg_INT:
	.long record_5959
	.long record_5959
		# static record tag
	.long 0x00000211
record_5963:
	.long Prelude__code_5513
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl susp_sum_INT
susp_sum_INT:
	.long record_5963
	.long record_5963
		# static record tag
	.long 0x00000211
record_5967:
	.long Prelude__code_5518
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl option_TYC
option_TYC:
	.long record_5967
	.long record_5967
		# static record tag
	.long 0x00000211
record_5971:
	.long Prelude__code_5523
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl option_sumarg_INT
option_sumarg_INT:
	.long record_5971
	.long record_5971
		# static record tag
	.long 0x00000211
record_5975:
	.long Prelude__code_5528
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl option_sum_INT
option_sum_INT:
	.long record_5975
	.long record_5975
		# static record tag
	.long 0x00000009
record_5977:
	.long 0x00000008
		# Global
	.long 0x0000006f
	.globl order_TYC
order_TYC:
	.long record_5977
	.long record_5977
		# static record tag
	.long 0x00001029
record_5982:
	.long 0x00000004
	.long 0xffffffff
	.long 0x00000003
	.long 0x00000003
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl order_sum_INT
order_sum_INT:
	.long record_5982
	.long record_5982
		# Global
	.long 0x0000006f
	.globl int32_TYC
int32_TYC:
	.globl word_TYC
word_TYC:
	.long 0x00000002
	.long 0x00000002
		# static record tag
	.long 0x00000211
record_5985:
	.long 0x00000001
	.long 0x00000000
		# Global
	.long 0x0000006f
	.globl string_TYC
string_TYC:
	.long record_5985
	.long record_5985
		# static record tag
	.long 0x00000009
record_5987:
	.long 0x00000008
		# Global
	.long 0x0000006f
	.globl substring_TYC
substring_TYC:
	.long record_5987
	.long record_5987
		# static record tag
	.long 0x00001c29
record_5994:
	.long 0x00000005
	.long 0x00000003
	.long record_5985
	.long 0x00000002
	.long 0x00000002
		# Global
	.long 0x0000006f
	.globl substring_sumarg_INT
substring_sumarg_INT:
	.long record_5994
	.long record_5994
		# static record tag
	.long 0x00001029
record_5997:
	.long 0x00000004
	.long 0xffffffff
	.long 0x00000000
	.long 0x00000001
	.long record_5994
		# Global
	.long 0x0000006f
	.globl substring_sum_INT
substring_sum_INT:
	.long record_5997
	.long record_5997
		# Global
	.long 0x0000006f
	.globl PLUSEbool_INT
PLUSEbool_INT:
	.long vars_eq_0_2498
	.long vars_eq_0_2498
		# static record tag
	.long 0x00000619
vars_eq_0_2498:
	.long Prelude_vars_eq_0_code_5533
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl PLUSElist_INT_r_INT
PLUSElist_INT_r_INT:
	.long polyPLUSElist_INT_r_2531
	.long polyPLUSElist_INT_r_2531
		# static record tag
	.long 0x00000619
polyPLUSElist_INT_r_2531:
	.long Prelude_polyPLUSElist_INT_r_code_5538
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl PLUSEoption_INT_r_INT
PLUSEoption_INT_r_INT:
	.long polyPLUSEoption_INT_r_2595
	.long polyPLUSEoption_INT_r_2595
		# static record tag
	.long 0x00000619
polyPLUSEoption_INT_r_2595:
	.long Prelude_polyPLUSEoption_INT_r_code_5552
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl PLUSEorder_INT
PLUSEorder_INT:
	.long vars_eq_0_2632
	.long vars_eq_0_2632
		# static record tag
	.long 0x00000619
vars_eq_0_2632:
	.long Prelude_vars_eq_0_code_5568
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl PLUSEint32_INT
PLUSEint32_INT:
	.long anonfun_2663
	.long anonfun_2663
		# static record tag
	.long 0x00000619
anonfun_2663:
	.long Prelude_anonfun_code_5573
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl PLUSEword_INT
PLUSEword_INT:
	.long anonfun_2672
	.long anonfun_2672
		# static record tag
	.long 0x00000619
anonfun_2672:
	.long Prelude_anonfun_code_5578
	.long 0x00000100
	.long 0x00000100
	.long 0x0000002a
string_6057:
		# string size = 5
	.ascii "Match"
	.align 2
		# Global
	.long 0x00000037
	.globl Match_r_INT
Match_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000022
string_6081:
		# string size = 4
	.ascii "Bind"
	.align 2
		# Global
	.long 0x00000037
	.globl Bind_r_INT
Bind_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000042
string_6109:
		# string size = 8
	.ascii "Overflow"
	.align 2
		# Global
	.long 0x00000037
	.globl Overflow_r_INT
Overflow_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000001a
string_6132:
		# string size = 3
	.ascii "Div"
	.align 2
		# Global
	.long 0x00000037
	.globl Div_r_INT
Div_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000001a
string_6155:
		# string size = 3
	.ascii "Mod"
	.align 2
		# Global
	.long 0x00000037
	.globl Mod_r_INT
Mod_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000022
string_6179:
		# string size = 4
	.ascii "Quot"
	.align 2
		# Global
	.long 0x00000037
	.globl Quot_r_INT
Quot_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000002a
string_6204:
		# string size = 5
	.ascii "Floor"
	.align 2
		# Global
	.long 0x00000037
	.globl Floor_r_INT
Floor_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000022
string_6228:
		# string size = 4
	.ascii "Sqrt"
	.align 2
		# Global
	.long 0x00000037
	.globl Sqrt_r_INT
Sqrt_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000001a
string_6251:
		# string size = 3
	.ascii "Exp"
	.align 2
		# Global
	.long 0x00000037
	.globl Exp_r_INT
Exp_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000012
string_6273:
		# string size = 2
	.ascii "Ln"
	.align 2
		# Global
	.long 0x00000037
	.globl Ln_r_INT
Ln_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000001a
string_6296:
		# string size = 3
	.ascii "Ord"
	.align 2
		# Global
	.long 0x00000037
	.globl Ord_r_INT
Ord_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000001a
string_6319:
		# string size = 3
	.ascii "Chr"
	.align 2
		# Global
	.long 0x00000037
	.globl Chr_r_INT
Chr_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000004a
string_6348:
		# string size = 9
	.ascii "Substring"
	.align 2
		# Global
	.long 0x00000037
mk_3659:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
	.globl Substring_r_INT
Substring_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000012
string_6376:
		# string size = 2
	.ascii "Hd"
	.align 2
		# Global
	.long 0x00000037
	.globl Hd_r_INT
Hd_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000012
string_6398:
		# string size = 2
	.ascii "Tl"
	.align 2
		# Global
	.long 0x00000037
	.globl Tl_r_INT
Tl_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000003a
string_6425:
		# string size = 7
	.ascii "NthTail"
	.align 2
		# Global
	.long 0x00000037
	.globl NthTail_r_INT
NthTail_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000001a
string_6448:
		# string size = 3
	.ascii "Nth"
	.align 2
		# Global
	.long 0x00000037
	.globl Nth_r_INT
Nth_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000004a
string_6477:
		# string size = 9
	.ascii "Subscript"
	.align 2
		# Global
	.long 0x00000037
mk_3679:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
	.globl Subscript_r_INT
Subscript_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000022
string_6507:
		# string size = 4
	.ascii "Size"
	.align 2
		# Global
	.long 0x00000037
mk_3683:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
	.globl Size_r_INT
Size_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000004a
string_6542:
		# string size = 9
	.ascii "Interrupt"
	.align 2
		# Global
	.long 0x00000037
	.globl Interrupt_r_INT
Interrupt_r_INT:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000027
exn_stamp_3690:
	.long 0x00000102
		# static record tag
	.long 0x00000619
anonfun_2759:
	.long Prelude_anonfun_code_5583
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl Io_r_INT
Io_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000032
string_6587:
		# string size = 6
	.ascii "Domain"
	.align 2
		# Global
	.long 0x00000037
	.globl Domain_r_INT
Domain_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x00000022
string_6611:
		# string size = 4
	.ascii "Span"
	.align 2
		# Global
	.long 0x00000037
	.globl Span_r_INT
Span_r_INT:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000027
exn_stamp_3702:
	.long 0x00000102
		# static record tag
	.long 0x00000619
anonfun_2773:
	.long Prelude_anonfun_code_5588
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl Fail_r_INT
Fail_r_INT:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x0000006f
	.globl vector_eq_r_INT
vector_eq_r_INT:
	.long vector_eq_r_2806
	.long vector_eq_r_2806
		# static record tag
	.long 0x00000619
vector_eq_r_2806:
	.long Prelude_vector_eq_r_code_5593
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
anonfun_2816:
	.long Prelude_anonfun_code_5631
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl PLUSEstring_INT
PLUSEstring_INT:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x0000006f
	.globl PLUSEsubstring_INT
PLUSEsubstring_INT:
	.long vars_eq_0_2824
	.long vars_eq_0_2824
		# static record tag
	.long 0x00000619
vars_eq_0_2824:
	.long Prelude_vars_eq_0_code_5636
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl o_r_INT
o_r_INT:
	.long o_r_2867
	.long o_r_2867
		# static record tag
	.long 0x00000619
o_r_2867:
	.long Prelude_o_r_code_5641
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl before_r_INT
before_r_INT:
	.long before_r_2879
	.long before_r_2879
		# static record tag
	.long 0x00000619
before_r_2879:
	.long Prelude_before_r_code_5669
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl ignore_r_INT
ignore_r_INT:
	.long ignore_r_2888
	.long ignore_r_2888
		# static record tag
	.long 0x00000619
ignore_r_2888:
	.long Prelude_ignore_r_code_5681
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl exnName
exnName:
	.long exnName_2889
	.long exnName_2889
		# static record tag
	.long 0x00000619
exnName_2889:
	.long Prelude_exnName_code_5693
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl exnMessage
exnMessage:
	.long exnMessage_2892
	.long exnMessage_2892
		# static record tag
	.long 0x00000619
exnMessage_2892:
	.long Prelude_exnMessage_code_5698
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl rev_r_INT
rev_r_INT:
	.long rev_r_2927
	.long rev_r_2927
		# static record tag
	.long 0x00000619
rev_r_2927:
	.long Prelude_rev_r_code_5703
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl length_r_INT
length_r_INT:
	.long length_r_2959
	.long length_r_2959
		# static record tag
	.long 0x00000619
length_r_2959:
	.long Prelude_length_r_code_5724
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl array_r_INT
array_r_INT:
	.long array_r_2974
	.long array_r_2974
		# static record tag
	.long 0x00000619
array_r_2974:
	.long Prelude_array_r_code_5741
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl vsub_r_INT
vsub_r_INT:
	.long vsub_r_2990
	.long vsub_r_2990
		# static record tag
	.long 0x00000619
vsub_r_2990:
	.long Prelude_vsub_r_code_5755
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl ord
ord:
	.long ord_2991
	.long ord_2991
		# static record tag
	.long 0x00000619
ord_2991:
	.long Prelude_ord_code_5769
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl chr
chr:
	.long chr_2994
	.long chr_2994
		# static record tag
	.long 0x00000619
chr_2994:
	.long Prelude_chr_code_5774
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000009
	.globl stringmaxsize
stringmaxsize:
	.globl vectormaxlength
vectormaxlength:
	.globl arraymaxlength
arraymaxlength:
	.long 0x00100000
		# Global
	.long 0x0000006f
	.globl size
size:
	.long size_3000
	.long size_3000
		# static record tag
	.long 0x00000619
size_3000:
	.long Prelude_size_code_5779
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
_4039:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x0000006f
	.globl explode
explode:
	.long explode_3003
	.long explode_3003
		# static record tag
	.long 0x00000619
explode_3003:
	.long Prelude_explode_code_5784
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl HAT
HAT:
	.long HAT_3071
	.long HAT_3071
		# static record tag
	.long 0x00000619
HAT_3071:
	.long Prelude_HAT_code_5798
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
_4142:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x0000006f
	.globl implode
implode:
	.long implode_3090
	.long implode_3090
		# static record tag
	.long 0x00000619
implode_3090:
	.long Prelude_implode_code_5825
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl revImplode
revImplode:
	.long revImplode_3121
	.long revImplode_3121
		# static record tag
	.long 0x00000619
revImplode_3121:
	.long Prelude_revImplode_code_5850
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl substring
substring:
	.long substring_3150
	.long substring_3150
		# static record tag
	.long 0x00000619
substring_3150:
	.long Prelude_substring_code_5875
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl char_eq
char_eq:
	.long char_eq_3175
	.long char_eq_3175
		# static record tag
	.long 0x00000619
char_eq_3175:
	.long Prelude_char_eq_code_5891
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl string_eq
string_eq:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x0000006f
	.globl create
create:
	.long create_3187
	.long create_3187
		# static record tag
	.long 0x00000619
create_3187:
	.long Prelude_create_code_5896
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl imod
imod:
	.long imod_3194
	.long imod_3194
		# static record tag
	.long 0x00000619
imod_3194:
	.long Prelude_imod_code_5901
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl idiv
idiv:
	.long idiv_3212
	.long idiv_3212
		# static record tag
	.long 0x00000619
idiv_3212:
	.long Prelude_idiv_code_5906
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000027
exn_stamp_4317:
	.long 0x00000102
		# static record tag
	.long 0x00000619
anonfun_3236:
	.long Prelude_anonfun_code_5911
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl LibFail_r_INT
LibFail_r_INT:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000027
exn_stamp_4320:
	.long 0x00000102
		# static record tag
	.long 0x00000619
anonfun_3242:
	.long Prelude_anonfun_code_5916
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl RuntimeError_r_INT
RuntimeError_r_INT:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000027
exn_stamp_4323:
	.long 0x00000102
		# static record tag
	.long 0x00000619
anonfun_3248:
	.long Prelude_anonfun_code_5921
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl RuntimeErrorPRIME_r_INT
RuntimeErrorPRIME_r_INT:
	.long 0x00000102
	.long 0x00000102
	.long 0x0000006a
string_6904:
		# string size = 13
	.ascii "RuntimeError'"
	.align 2
	.long 0x00000062
string_6928:
		# string size = 12
	.ascii "RuntimeError"
	.align 2
	.long 0x0000003a
string_6947:
		# string size = 7
	.ascii "LibFail"
	.align 2
		# static record tag
	.long 0x00000619
record_7937:
	.long Prelude_lengthPRIME_inner_code_5729
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000011
record_8096:
	.long 0x00000005
	.long 0x00000000
	.long 0x00000022
string_8492:
		# string size = 4
	.ascii "Fail"
	.align 2
	.long 0x00000012
string_8506:
		# string size = 2
	.ascii "Io"
	.align 2
		# static record tag
	.long 0x00000009
record_8830:
	.long 0x00000008
		# static record tag
	.long 0x00000009
record_8856:
	.long 0x00000009
		# static record tag
	.long 0x00000009
record_8864:
	.long 0x00000008
		# static record tag
	.long 0x00000009
record_8908:
	.long 0x00000008
		# Module closure
	.long 0x00000619
	.globl Prelude_unit_closure
Prelude_unit_closure:
	.long Prelude_main
	.long 0x00000000
	.long 0x00000000
	.long 0x00000311
	.globl Prelude_unit
Prelude_unit:
	.long Prelude_unit_closure
	.long Prelude_unit_closure
	.globl Prelude_unit_GLOBALS_END_VAL
Prelude_unit_GLOBALS_END_VAL:
	.long 0x00000000
	.long 0 #filler

	.sdata
	.align 3
	.globl Prelude_unit_TRACE_GLOBALS_BEGIN_VAL
Prelude_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long RuntimeErrorPRIME_r_INT
	.long RuntimeError_r_INT
	.long LibFail_r_INT
	.long string_eq
	.long _4142
	.long _4039
	.long PLUSEstring_INT
	.long Fail_r_INT
	.long Span_r_INT
	.long Domain_r_INT
	.long Io_r_INT
	.long Interrupt_r_INT
	.long Size_r_INT
	.long mk_3683
	.long Subscript_r_INT
	.long mk_3679
	.long Nth_r_INT
	.long NthTail_r_INT
	.long Tl_r_INT
	.long Hd_r_INT
	.long Substring_r_INT
	.long mk_3659
	.long Chr_r_INT
	.long Ord_r_INT
	.long Ln_r_INT
	.long Exp_r_INT
	.long Sqrt_r_INT
	.long Floor_r_INT
	.long Quot_r_INT
	.long Mod_r_INT
	.long Div_r_INT
	.long Overflow_r_INT
	.long Bind_r_INT
	.long Match_r_INT
	.globl Prelude_unit_TRACE_GLOBALS_END_VAL
Prelude_unit_TRACE_GLOBALS_END_VAL:
		# filler so label is defined
	.long 0x00000000
