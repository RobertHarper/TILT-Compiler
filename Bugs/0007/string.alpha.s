	.set noat
	.rdata
		# gcinfo
	.globl String_unit_GCTABLE_BEGIN_VAL
String_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl String_unit_CODE_END_VAL
	.globl String_unit_CODE_BEGIN_VAL
String_unit_CODE_BEGIN_VAL:
	.text
 	.align 3
	.ent String_anonfun_code_137716
 # arguments : [$137718,$0] [$137719,$1] [$134202,$2] 
 # results    : [$134202,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
String_anonfun_code_137716:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $0
code_140913:
funtop_140904:
code_140915:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137716

	.rdata
	.text
 	.align 3
	.ent String_size_code_137721
 # arguments : [$137723,$0] [$137724,$1] [$134211,$2] 
 # results    : [$140902,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_size_code_137721:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_140916:
funtop_140893:
load_nonstall_tag_140899:
	ldl	$1, -4($2)
	cmpeq	$1, 15, $0
	bne	$0, load_nonstall_tag_140899
load_true_tag_140900:
	and	$1, 3, $0
	bne	$0, loaded_tag_140901
code_140919:
	ldl	$1, -4($1)
	br	$31, load_true_tag_140900
loaded_tag_140901:
	zap	$1, 240, $0
	srl	$0, 3, $0
	addl	$0, $31, $0
code_140922:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_size_code_137721

	.rdata
	.text
 	.align 3
	.ent String_str_code_137726
 # arguments : [$137728,$0] [$137729,$1] [$134221,$2] 
 # results    : [$140887,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_str_code_137726:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_140923:
funtop_140881:
	# ptr sub start
	lda	$1, chars_135000
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	s4addl	$2, $0, $0
	ldl	$0, ($0)
	# ptr sub end
code_140926:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_str_code_137726

	.rdata
	.text
 	.align 3
	.ent String_sub_code_137731
 # arguments : [$137733,$0] [$137734,$1] [$135952,$2] [$135953,$3] 
 # results    : [$140875,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
String_sub_code_137731:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_140927:
funtop_140851:
load_nonstall_tag_140858:
	ldl	$1, -4($2)
	cmpeq	$1, 15, $0
	bne	$0, load_nonstall_tag_140858
load_true_tag_140859:
	and	$1, 3, $0
	bne	$0, loaded_tag_140860
code_140930:
	ldl	$1, -4($1)
	br	$31, load_true_tag_140859
loaded_tag_140860:
	zap	$1, 240, $0
	srl	$0, 3, $0
	addl	$0, $31, $0
	cmpule	$0, $3, $0
	bne	$0, one_case_140868
zero_case_140867:
	# int sub start
	addl	$3, $2, $0
	lda	$1, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $1, $1
	# int sub end
	mov	$1, $0
	br	$31, after_zeroone_140869
one_case_140868:
	lda	$1, mk_135018
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
after_zeroone_140869:
code_140937:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_sub_code_137731

	.rdata
	.text
 	.align 3
	.ent String_substring_code_137736
 # arguments : [$137738,$0] [$137739,$1] [$135956,$2] [$135957,$3] [$135958,$4] 
 # results    : [$140845,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_substring_code_137736:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 16($sp)
	stl	$3, 8($sp)
	stl	$4, 12($sp)
code_140938:
funtop_140774:
	ldl	$25, 8($sp)
	cmplt	$25, 0, $0
	bne	$0, one_case_140783
zero_case_140782:
	ldl	$25, 12($sp)
	cmplt	$25, 0, $0
	br	$31, after_zeroone_140784
one_case_140783:
	lda	$0, 1($31)
after_zeroone_140784:
	bne	$0, one_case_140795
zero_case_140794:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_140956:
	ldgp	$gp, ($26)
	mov	$0, $1
code_140942:
	# done making normal call
	ldl	$at, 12($sp)
	ldl	$25, 8($sp)
	addlv	$25, $at, $0
	trapb
	cmplt	$1, $0, $0
	br	$31, after_zeroone_140796
one_case_140795:
	lda	$0, 1($31)
after_zeroone_140796:
	bne	$0, one_case_140817
zero_case_140816:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_140957:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_140945:
	# done making normal call
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_140954:
	ldgp	$gp, ($26)
	mov	$0, $4
code_140946:
	# done making normal call
	# making closure call 
	lda	$1, unsafeSubstring_135046
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	ldl	$3, 8($sp)
	mov	$5, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($5), 1
code_140948:
	# done making tail call
	br	$31, after_zeroone_140818
one_case_140817:
	lda	$1, mk_135053
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
after_zeroone_140818:
code_140953:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_substring_code_137736

	.rdata
		# -------- label,sizes,reg
	.long code_140954
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_140956
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_140957
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
	.text
 	.align 3
	.ent String_fill_code_137751
 # arguments : [$137753,$0] [$137754,$1] [$134268,$2] 
 # results    : [$140754,$0] 
 # destroys   :  $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $7 $6 $5 $4 $3 $2 $1 $0
String_fill_code_137751:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $7
code_140958:
funtop_140733:
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$6, 12($1)
	cmpult	$7, $0, $0
	bne	$0, one_case_140750
zero_case_140749:
	lda	$0, 256($31)
	br	$31, after_zeroone_140751
one_case_140750:
	addl	$3, $7, $0
	addl	$7, 1, $5
	# int sub start
	addl	$0, $2, $0
	lda	$4, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $4, $4
	# int sub end
	ldl	$0, 1080($12)
	ldl	$2, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $2, $0
	bne	$0, afterMutateCheck_140964
code_140966:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_140964:
	ldl	$3, 1080($12)
	mov	$6, $0
	mov	$7, $2
	stl	$0, ($3)
	stl	$2, 4($3)
	addl	$3, 12, $0
	stl	$0, 1080($12)
	addl	$6, $7, $3
	lda	$2, ($3)
	ldq_u	$0, ($3)
	mskbl	$0, $2, $0
	insbl	$4, $2, $2
	or	$2, $0, $2
	stq_u	$2, ($3)
	# making direct call 
	mov	$5, $7
	br	$31, funtop_140733
code_140975:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_140751:
code_140977:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_fill_code_137751

	.rdata
		# -------- label,sizes,reg
	.long afterMutateCheck_140964
	.long 0x00000805
	.long 0x01ff0f42
	.long 0x01ff0f00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_newVec_code_137746
 # arguments : [$137748,$0] [$137749,$1] [$134263,$2] 
 # results    : [$140675,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_newVec_code_137746:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 20($sp)
code_140978:
funtop_140655:
	ldl	$25, ($1)
	stl	$25, 16($sp)
	ldl	$25, 4($1)
	stl	$25, 12($sp)
	ldl	$25, 20($sp)
	cmplt	$31, $25, $0
	bne	$0, one_case_140668
zero_case_140667:
	lda	$1, mk_134990
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
	stl	$0, 8($sp)
	br	$31, after_zeroone_140669
one_case_140668:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141004:
	ldgp	$gp, ($26)
	mov	$0, $16
code_140983:
	# done making normal call
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
	bne	$0, array_int_small_140694
code_140985:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_141002:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_140986:
	br	$31, array_int_after_140693
array_int_small_140694:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_140988
code_140989:
	jsr	$26, GCFromML
gc_check_140988:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_140701
array_init_loopto_140702:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_140701:
	bge	$1, array_init_loopto_140702
array_int_after_140693:
	stl	$3, 8($sp)
after_zeroone_140669:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141001:
	ldgp	$gp, ($26)
	mov	$0, $1
code_140994:
	# done making normal call
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_140995
code_140996:
	jsr	$26, GCFromML
gc_check_140995:
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 3105($31)
	stl	$0, ($13)
	ldl	$25, 16($sp)
	stl	$25, 4($13)
	stl	$1, 8($13)
	ldl	$25, 12($sp)
	stl	$25, 12($13)
	ldl	$25, 8($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_fill_code_137751
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
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141003:
	ldgp	$gp, ($26)
code_140998:
	# done making normal call
	ldl	$at, 8($sp)
	stl	$at, 8($sp)
code_141000:
	ldl	$0, 8($sp)
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_newVec_code_137746

	.rdata
		# -------- label,sizes,reg
	.long code_141001
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141002
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_140988
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_140995
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141003
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_141004
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
	.text
 	.align 3
	.ent String_extract_code_137741
 # arguments : [$137743,$0] [$137744,$1] [$135972,$2] [$135973,$3] [$135974,$4] 
 # results    : [$140495,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_extract_code_137741:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 20($sp)
	stl	$3, 16($sp)
	stl	$4, 12($sp)
code_141005:
funtop_140393:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141056:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141006:
	# done making normal call
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141054:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141007:
	# done making normal call
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141008
code_141009:
	jsr	$26, GCFromML
gc_check_141008:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 20($sp)
	stl	$25, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_newVec_code_137746
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
sumarm_140432:
	ldl	$25, 12($sp)
	bne	$25, sumarm_140433
code_141011:
	ldl	$25, 16($sp)
	cmpeq	$25, 0, $1
	bne	$1, one_case_140442
zero_case_140441:
	ldl	$25, 16($sp)
	cmplt	$25, 0, $1
	bne	$1, one_case_140451
zero_case_140450:
	ldl	$at, 16($sp)
	ldl	$25, 8($sp)
	cmplt	$25, $at, $1
	br	$31, after_zeroone_140452
one_case_140451:
	lda	$1, 1($31)
after_zeroone_140452:
	bne	$1, one_case_140463
zero_case_140462:
	ldl	$at, 8($sp)
	ldl	$25, 16($sp)
	cmpeq	$25, $at, $1
	bne	$1, one_case_140472
zero_case_140471:
	ldl	$at, 16($sp)
	ldl	$25, 8($sp)
	sublv	$25, $at, $4
	trapb
	# making closure call 
	ldl	$3, ($0)
	ldl	$2, 4($0)
	ldl	$1, 8($0)
	mov	$2, $0
	mov	$4, $2
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_141017:
	# done making tail call
	br	$31, after_zeroone_140473
one_case_140472:
	lda	$0, string_138122
after_zeroone_140473:
	br	$31, after_zeroone_140464
one_case_140463:
	lda	$1, mk_135053
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
after_zeroone_140464:
	mov	$0, $2
	br	$31, after_zeroone_140443
one_case_140442:
	ldl	$2, 20($sp)
after_zeroone_140443:
	mov	$2, $0
	br	$31, after_sum_140429
sumarm_140433:
	ldl	$25, 12($sp)
	ldl	$4, ($25)
	cmpeq	$4, 0, $1
	bne	$1, one_case_140515
zero_case_140514:
	cmpeq	$4, 1, $1
	bne	$1, one_case_140524
zero_case_140523:
	ldl	$25, 16($sp)
	cmplt	$25, 0, $1
	bne	$1, one_case_140533
zero_case_140532:
	cmplt	$4, 0, $1
	br	$31, after_zeroone_140534
one_case_140533:
	lda	$1, 1($31)
after_zeroone_140534:
	bne	$1, one_case_140545
zero_case_140544:
	ldl	$25, 16($sp)
	addlv	$25, $4, $1
	trapb
	ldl	$25, 8($sp)
	cmplt	$25, $1, $1
	br	$31, after_zeroone_140546
one_case_140545:
	lda	$1, 1($31)
after_zeroone_140546:
	bne	$1, one_case_140560
zero_case_140559:
	# making closure call 
	ldl	$3, ($0)
	ldl	$2, 4($0)
	ldl	$1, 8($0)
	mov	$2, $0
	mov	$4, $2
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_141031:
	# done making tail call
	br	$31, after_zeroone_140561
one_case_140560:
	lda	$1, mk_135053
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
after_zeroone_140561:
	br	$31, after_zeroone_140525
one_case_140524:
	ldl	$25, 16($sp)
	cmplt	$25, 0, $0
	bne	$0, one_case_140583
zero_case_140582:
	ldl	$25, 16($sp)
	addlv	$25, 1, $0
	trapb
	ldl	$25, 8($sp)
	cmplt	$25, $0, $0
	br	$31, after_zeroone_140584
one_case_140583:
	lda	$0, 1($31)
after_zeroone_140584:
	bne	$0, one_case_140598
zero_case_140597:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141059:
	ldgp	$gp, ($26)
code_141039:
	# done making normal call
	# int sub start
	ldl	$25, 20($sp)
	addl	$0, $25, $0
	lda	$2, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $2, $2
	# int sub end
	# making closure call 
	lda	$1, str_134220
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_141040:
	# done making tail call
	br	$31, after_zeroone_140599
one_case_140598:
	lda	$1, mk_135053
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
after_zeroone_140599:
after_zeroone_140525:
	br	$31, after_zeroone_140516
one_case_140515:
	ldl	$25, 16($sp)
	cmplt	$25, 0, $0
	bne	$0, one_case_140633
zero_case_140632:
	ldl	$at, 16($sp)
	ldl	$25, 8($sp)
	cmplt	$25, $at, $0
	br	$31, after_zeroone_140634
one_case_140633:
	lda	$0, 1($31)
after_zeroone_140634:
	bne	$0, one_case_140645
zero_case_140644:
	lda	$0, string_138122
	br	$31, after_zeroone_140646
one_case_140645:
	lda	$1, mk_135053
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
after_zeroone_140646:
after_zeroone_140516:
	br	$31, after_sum_140429
sumarm_140496:
after_sum_140429:
code_141053:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_extract_code_137741

	.rdata
		# -------- label,sizes,reg
	.long code_141054
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000440
		# -------- label,sizes,reg
	.long gc_check_141008
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000440
		# -------- label,sizes,reg
	.long code_141056
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000440
		# -------- label,sizes,reg
	.long code_141059
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000400
	.text
 	.align 3
	.ent String_HAT_code_137766
 # arguments : [$137768,$0] [$137769,$1] [$136003,$2] [$136004,$3] 
 # results    : [$140391,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_HAT_code_137766:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
	stl	$3, 8($sp)
code_141060:
funtop_140342:
	lda	$3, string_138122
	# making closure call 
	lda	$1, PLUSEstring_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141073:
	ldgp	$gp, ($26)
code_141062:
	# done making normal call
	bne	$0, one_case_140359
zero_case_140358:
	lda	$3, string_138122
	# making closure call 
	lda	$1, PLUSEstring_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141074:
	ldgp	$gp, ($26)
code_141065:
	# done making normal call
	bne	$0, one_case_140376
zero_case_140375:
	# making closure call 
	lda	$1, concat2_135218
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	ldl	$3, 8($sp)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_141068:
	# done making tail call
	mov	$0, $2
	br	$31, after_zeroone_140377
one_case_140376:
	ldl	$2, 12($sp)
after_zeroone_140377:
	mov	$2, $0
	br	$31, after_zeroone_140360
one_case_140359:
	ldl	$0, 8($sp)
after_zeroone_140360:
code_141072:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_HAT_code_137766

	.rdata
		# -------- label,sizes,reg
	.long code_141073
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141074
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent String_length_code_137771
 # arguments : [$137773,$0] [$137774,$1] [$136025,$2] [$136026,$3] 
 # results    : [$140301,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_length_code_137771:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
code_141076:
funtop_140285:
sumarm_140297:
	bne	$3, sumarm_140298
code_141077:
	ldl	$0, 12($sp)
	br	$31, after_sum_140294
sumarm_140298:
	ldl	$2, ($3)
	ldl	$25, 4($3)
	stl	$25, 8($sp)
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141084:
	ldgp	$gp, ($26)
code_141079:
	# done making normal call
	ldl	$25, 12($sp)
	addlv	$25, $0, $0
	trapb
	# making direct call 
	stl	$0, 12($sp)
	ldl	$3, 8($sp)
	br	$31, funtop_140285
code_141080:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_140294
sumarm_140302:
after_sum_140294:
code_141083:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_length_code_137771

	.rdata
		# -------- label,sizes,reg
	.long code_141084
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent String_find_code_137776
 # arguments : [$137778,$0] [$137779,$1] [$134414,$2] 
 # results    : [$140282,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_find_code_137776:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141085:
funtop_140220:
sumarm_140232:
	lda	$0, 255($31)
	cmple	$2, $0, $0
	bne	$0, nomatch_sum_140230
code_141087:
	ldl	$25, ($2)
	stl	$25, 12($sp)
	ldl	$25, 4($2)
	stl	$25, 8($sp)
	lda	$3, string_138122
	# making closure call 
	lda	$1, PLUSEstring_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141096:
	ldgp	$gp, ($26)
code_141089:
	# done making normal call
	bne	$0, one_case_140272
zero_case_140271:
	ldl	$2, 12($sp)
	br	$31, after_zeroone_140273
one_case_140272:
	# making direct call 
	ldl	$2, 8($sp)
	br	$31, funtop_140220
code_141092:
	# done making self tail call
	lda	$0, ($31)
	mov	$0, $2
after_zeroone_140273:
	mov	$2, $0
	br	$31, after_sum_140229
sumarm_140233:
nomatch_sum_140230:
	lda	$0, string_138122
after_sum_140229:
code_141095:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_find_code_137776

	.rdata
		# -------- label,sizes,reg
	.long code_141096
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent String_copyPRIME_code_137791
 # arguments : [$137793,$0] [$137794,$1] [$134399,$2] 
 # results    : [$140217,$0] 
 # destroys   :  $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $7 $6 $5 $4 $3 $2 $1 $0
String_copyPRIME_code_137791:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $4
code_141097:
funtop_140179:
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	ldl	$7, 12($1)
	cmpeq	$4, $0, $0
	bne	$0, one_case_140196
zero_case_140195:
	addl	$2, $4, $6
	addl	$4, 1, $5
	# int sub start
	addl	$4, $3, $0
	lda	$4, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $4, $4
	# int sub end
	ldl	$0, 1080($12)
	ldl	$2, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $2, $0
	bne	$0, afterMutateCheck_141102
code_141104:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141102:
	ldl	$3, 1080($12)
	mov	$7, $2
	mov	$6, $0
	stl	$2, ($3)
	stl	$0, 4($3)
	addl	$3, 12, $0
	stl	$0, 1080($12)
	addl	$7, $6, $3
	lda	$2, ($3)
	ldq_u	$0, ($3)
	mskbl	$0, $2, $0
	insbl	$4, $2, $2
	or	$2, $0, $2
	stq_u	$2, ($3)
	# making direct call 
	mov	$5, $4
	br	$31, funtop_140179
code_141113:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_140197
one_case_140196:
	lda	$0, 256($31)
after_zeroone_140197:
code_141116:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_copyPRIME_code_137791

	.rdata
		# -------- label,sizes,reg
	.long afterMutateCheck_141102
	.long 0x00000805
	.long 0x01ff0f82
	.long 0x01ff0f00
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_copy_code_137786
 # arguments : [$137788,$0] [$137789,$1] [$136043,$2] [$136044,$3] 
 # results    : [$140109,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_copy_code_137786:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 20($sp)
	stl	$3, 24($sp)
code_141117:
funtop_140092:
sumarm_140104:
	bne	$2, sumarm_140105
code_141118:
	lda	$0, 256($31)
	br	$31, after_sum_140101
sumarm_140105:
	ldl	$25, ($2)
	stl	$25, 16($sp)
	ldl	$25, 4($2)
	stl	$25, 12($sp)
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141132:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141120:
	# done making normal call
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141130:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141121:
	# done making normal call
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141122
code_141123:
	jsr	$26, GCFromML
gc_check_141122:
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 2337($31)
	stl	$0, ($13)
	ldl	$25, 16($sp)
	stl	$25, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	ldl	$25, 24($sp)
	stl	$25, 12($13)
	ldl	$25, 20($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_copyPRIME_code_137791
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
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141131:
	ldgp	$gp, ($26)
code_141125:
	# done making normal call
	ldl	$at, 8($sp)
	ldl	$25, 24($sp)
	addl	$25, $at, $0
	# making direct call 
	ldl	$2, 12($sp)
	stl	$0, 24($sp)
	br	$31, funtop_140092
code_141126:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_140101
sumarm_140110:
after_sum_140101:
code_141129:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_copy_code_137786

	.rdata
		# -------- label,sizes,reg
	.long code_141130
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000540
		# -------- label,sizes,reg
	.long gc_check_141122
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000540
		# -------- label,sizes,reg
	.long code_141131
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000440
		# -------- label,sizes,reg
	.long code_141132
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000540
	.text
 	.align 3
	.ent String_anonfun_code_137781
 # arguments : [$137783,$0] [$137784,$1] [$134361,$2] 
 # results    : [$140089,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_anonfun_code_137781:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
code_141133:
funtop_139992:
	lda	$2, ($31)
	# making closure call 
	lda	$1, length_134362
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$3, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141166:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141134:
	# done making normal call
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141135
code_141136:
	jsr	$26, GCFromML
gc_check_141135:
	cmpeq	$2, 0, $0
	bne	$0, one_case_140010
zero_case_140009:
	cmpeq	$2, 1, $0
	bne	$0, one_case_140019
zero_case_140018:
	cmplt	$31, $2, $0
	bne	$0, one_case_140028
zero_case_140027:
	lda	$1, mk_134990
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
	stl	$0, 8($sp)
	br	$31, after_zeroone_140029
one_case_140028:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141168:
	ldgp	$gp, ($26)
	mov	$0, $16
code_141144:
	# done making normal call
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
	bne	$0, array_int_small_140054
code_141146:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_141165:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_141147:
	br	$31, array_int_after_140053
array_int_small_140054:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141149
code_141150:
	jsr	$26, GCFromML
gc_check_141149:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_140061
array_init_loopto_140062:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_140061:
	bge	$1, array_init_loopto_140062
array_int_after_140053:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141155
code_141156:
	jsr	$26, GCFromML
gc_check_141155:
	stl	$3, 8($sp)
after_zeroone_140029:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_copy_code_137786
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	ldl	$25, 8($sp)
	stl	$25, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	lda	$3, ($31)
	# making closure call 
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141164:
	ldgp	$gp, ($26)
code_141158:
	# done making normal call
	ldl	$0, 8($sp)
	br	$31, after_zeroone_140020
one_case_140019:
	# making closure call 
	lda	$1, find_134413
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_141160:
	# done making tail call
after_zeroone_140020:
	br	$31, after_zeroone_140011
one_case_140010:
	lda	$0, string_138122
after_zeroone_140011:
code_141163:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137781

	.rdata
		# -------- label,sizes,reg
	.long code_141164
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long gc_check_141135
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_141165
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_141149
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_141155
	.long 0x00000805
	.long 0x00000008
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_141166
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_141168
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
	.text
 	.align 3
	.ent String_concat_code_137805
 # arguments : [$137807,$0] [$137808,$1] [$134358,$2] 
 # results    : [$139983,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_concat_code_137805:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141169:
funtop_139919:
sumarm_139931:
	lda	$0, 255($31)
	cmple	$2, $0, $0
	bne	$0, nomatch_sum_139929
code_141171:
	ldl	$1, ($2)
	ldl	$0, 4($2)
sumarm_139970:
	bne	$0, sumarm_139971
code_141172:
	mov	$1, $0
	br	$31, after_sum_139967
sumarm_139971:
nomatch_sum_139968:
	# making closure call 
	lda	$1, anonfun_134360
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_141174:
	# done making tail call
after_sum_139967:
	br	$31, after_sum_139928
sumarm_139932:
nomatch_sum_139929:
	# making closure call 
	lda	$1, anonfun_134360
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($3), 1
code_141176:
	# done making tail call
after_sum_139928:
code_141178:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_concat_code_137805

	.rdata
	.text
 	.align 3
	.ent String_length_inner_code_137810
 # arguments : [$137812,$0] [$137813,$1] [$136080,$2] [$136081,$3] 
 # results    : [$139887,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
String_length_inner_code_137810:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141181:
funtop_139871:
sumarm_139883:
	bne	$2, sumarm_139884
code_141182:
	mov	$3, $0
	br	$31, after_sum_139880
sumarm_139884:
	ldl	$1, 4($2)
	addlv	$3, 1, $0
	trapb
	# making direct call 
	mov	$1, $2
	mov	$0, $3
	br	$31, funtop_139871
code_141184:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_139880
sumarm_139888:
after_sum_139880:
code_141187:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_length_inner_code_137810

	.rdata
	.text
 	.align 3
	.ent String_implode_code_137815
 # arguments : [$137817,$0] [$137818,$1] [$134443,$2] 
 # results    : [$139850,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_implode_code_137815:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 8($sp)
code_141188:
funtop_139833:
sumarm_139845:
	ldl	$25, 8($sp)
	bne	$25, sumarm_139846
code_141189:
	lda	$0, string_138122
	br	$31, after_sum_139842
sumarm_139846:
nomatch_sum_139843:
	lda	$3, ($31)
	# making closure call 
	lda	$1, length_inner_134466
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141197:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141191:
	# done making normal call
	# making closure call 
	lda	$1, implode_135359
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
code_141193:
	# done making tail call
after_sum_139842:
code_141195:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_implode_code_137815

	.rdata
		# -------- label,sizes,reg
	.long code_141197
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent String_f_code_137825
 # arguments : [$137827,$0] [$137828,$1] [$136107,$2] [$136108,$3] 
 # results    : [$139831,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_f_code_137825:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
	stl	$2, 16($sp)
	stl	$3, 12($sp)
code_141198:
funtop_139786:
	lda	$0, -1($31)
	ldl	$25, 12($sp)
	cmpeq	$25, $0, $0
	bne	$0, one_case_139796
zero_case_139795:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141208:
	ldgp	$gp, ($26)
code_141200:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141201
code_141202:
	jsr	$26, GCFromML
gc_check_141201:
	# int sub start
	ldl	$25, 8($sp)
	addl	$0, $25, $0
	lda	$1, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $1, $1
	# int sub end
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	ldl	$25, 12($sp)
	sublv	$25, 1, $0
	trapb
	# making direct call 
	stl	$1, 16($sp)
	stl	$0, 12($sp)
	br	$31, funtop_139786
code_141204:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_139797
one_case_139796:
	ldl	$0, 16($sp)
after_zeroone_139797:
code_141207:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_f_code_137825

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141201
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000110
		# -------- label,sizes,reg
	.long code_141208
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000110
	.text
 	.align 3
	.ent String_explode_code_137820
 # arguments : [$137822,$0] [$137823,$1] [$134474,$2] 
 # results    : [$139785,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_explode_code_137820:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141209:
funtop_139757:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141210
code_141211:
	jsr	$26, GCFromML
gc_check_141210:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_f_code_137825
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $25
	stl	$25, 8($sp)
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141218:
	ldgp	$gp, ($26)
code_141213:
	# done making normal call
	sublv	$0, 1, $3
	trapb
	lda	$2, ($31)
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$4, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_141214:
	# done making tail call
code_141216:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_explode_code_137820

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141210
	.long 0x00000805
	.long 0x00000004
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_141218
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
	.text
 	.align 3
	.ent String_mapf_code_137842
 # arguments : [$137844,$0] [$137845,$1] [$134499,$2] 
 # results    : [$139734,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_mapf_code_137842:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 16($sp)
	stl	$2, 20($sp)
code_141219:
funtop_139713:
	ldl	$25, 16($sp)
	ldl	$4, ($25)
	ldl	$25, 16($sp)
	ldl	$1, 4($25)
	ldl	$25, 16($sp)
	ldl	$0, 8($25)
	ldl	$25, 16($sp)
	ldl	$25, 12($25)
	stl	$25, 12($sp)
	ldl	$25, 20($sp)
	cmpult	$25, $0, $0
	bne	$0, one_case_139730
zero_case_139729:
	lda	$0, 256($31)
	br	$31, after_zeroone_139731
one_case_139730:
	ldl	$25, 20($sp)
	addl	$25, 1, $25
	stl	$25, 8($sp)
	# int sub start
	ldl	$25, 20($sp)
	addl	$25, $1, $0
	lda	$2, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $2, $2
	# int sub end
	# making closure call 
	ldl	$3, ($4)
	ldl	$0, 4($4)
	ldl	$1, 8($4)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141240:
	ldgp	$gp, ($26)
	mov	$0, $3
code_141222:
	# done making normal call
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141226
code_141228:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141226:
	ldl	$1, 1080($12)
	ldl	$0, 12($sp)
	ldl	$2, 20($sp)
	stl	$0, ($1)
	stl	$2, 4($1)
	addl	$1, 12, $0
	stl	$0, 1080($12)
	ldl	$at, 20($sp)
	ldl	$25, 12($sp)
	addl	$25, $at, $2
	lda	$1, ($2)
	ldq_u	$0, ($2)
	mskbl	$0, $1, $0
	insbl	$3, $1, $1
	or	$1, $0, $1
	stq_u	$1, ($2)
	# making direct call 
	ldl	$at, 8($sp)
	stl	$at, 20($sp)
	br	$31, funtop_139713
code_141237:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_139731:
code_141239:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_mapf_code_137842

	.rdata
		# -------- label,sizes,reg
	.long afterMutateCheck_141226
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
		# -------- label,sizes,reg
	.long code_141240
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
	.text
 	.align 3
	.ent String_anonfun_code_137837
 # arguments : [$137839,$0] [$137840,$1] [$134490,$2] 
 # results    : [$139710,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_anonfun_code_137837:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 16($sp)
	stl	$2, 20($sp)
code_141241:
funtop_139620:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141269:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_141242:
	# done making normal call
	ldl	$25, 12($sp)
	cmpeq	$25, 0, $0
	bne	$0, one_case_139636
zero_case_139635:
	ldl	$25, 12($sp)
	cmplt	$31, $25, $0
	bne	$0, one_case_139645
zero_case_139644:
	lda	$1, mk_134990
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
	stl	$0, 8($sp)
	br	$31, after_zeroone_139646
one_case_139645:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141271:
	ldgp	$gp, ($26)
	mov	$0, $16
code_141248:
	# done making normal call
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
	bne	$0, array_int_small_139671
code_141250:
	lda	$27, alloc_bigintarray
	jsr	$26, save_regs_MLtoC
	jsr	$26, alloc_bigintarray
code_141268:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
	mov	$0, $3
code_141251:
	br	$31, array_int_after_139670
array_int_small_139671:
	sll	$16, 3, $1
	addl	$1, $31, $1
	or	$1, 2, $1
	lda	$0, 1($31)
	addl	$0, $2, $0
	s4addl	$0, $13, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141253
code_141254:
	jsr	$26, GCFromML
gc_check_141253:
	# storing tag
	stl	$1, ($13)
	addl	$13, 4, $3
	sll	$0, 2, $0
	addl	$0, $31, $0
	addl	$13, $0, $13
	subl	$2, 1, $1
	sll	$1, 2, $1
	addl	$1, $31, $1
	br	$31, array_init_loopcheck_139678
array_init_loopto_139679:
	addl	$3, $1, $0
	stl	$17, ($0)
	subl	$1, 4, $1
array_init_loopcheck_139678:
	bge	$1, array_init_loopto_139679
array_int_after_139670:
	stl	$3, 8($sp)
after_zeroone_139646:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141267:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141259:
	# done making normal call
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141260
code_141261:
	jsr	$26, GCFromML
gc_check_141260:
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 2849($31)
	stl	$0, ($13)
	ldl	$25, 16($sp)
	stl	$25, 4($13)
	ldl	$25, 20($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	ldl	$25, 8($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_mapf_code_137842
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
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141270:
	ldgp	$gp, ($26)
code_141263:
	# done making normal call
	ldl	$0, 8($sp)
	br	$31, after_zeroone_139637
one_case_139636:
	lda	$0, string_138122
after_zeroone_139637:
code_141266:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137837

	.rdata
		# -------- label,sizes,reg
	.long code_141267
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000510
		# -------- label,sizes,reg
	.long code_141268
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000500
		# -------- label,sizes,reg
	.long gc_check_141253
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000500
		# -------- label,sizes,reg
	.long gc_check_141260
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000510
		# -------- label,sizes,reg
	.long code_141269
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000500
		# -------- label,sizes,reg
	.long code_141270
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000010
		# -------- label,sizes,reg
	.long code_141271
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000500
	.text
 	.align 3
	.ent String_map_code_137832
 # arguments : [$137834,$0] [$137835,$1] [$134488,$2] 
 # results    : [$139614,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_map_code_137832:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141272:
funtop_139608:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141273
code_141274:
	jsr	$26, GCFromML
gc_check_141273:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_anonfun_code_137837
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_141277:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_map_code_137832

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141273
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_anonfun_code_137861
 # arguments : [$137863,$0] [$137864,$1] [$134513,$2] 
 # results    : [$139607,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_anonfun_code_137861:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
	stl	$2, 12($sp)
code_141278:
funtop_139578:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141287:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141279:
	# done making normal call
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141285:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141280:
	# done making normal call
	lda	$4, ($31)
	# making closure call 
	lda	$1, _135439
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$6, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	ldl	$3, 12($sp)
	mov	$6, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($6), 1
code_141282:
	# done making tail call
code_141284:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137861

	.rdata
		# -------- label,sizes,reg
	.long code_141285
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141287
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent String_translate_code_137856
 # arguments : [$137858,$0] [$137859,$1] [$134511,$2] 
 # results    : [$139572,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_translate_code_137856:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141288:
funtop_139566:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141289
code_141290:
	jsr	$26, GCFromML
gc_check_141289:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_anonfun_code_137861
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_141293:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_translate_code_137856

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141289
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_scanTok_code_137878
 # arguments : [$137880,$0] [$137881,$1] [$136175,$2] [$136176,$3] [$136177,$4] 
 # results    : [$139468,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_scanTok_code_137878:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 12($sp)
	stl	$2, 28($sp)
	stl	$3, 24($sp)
	stl	$4, 32($sp)
code_141294:
funtop_139397:
	ldl	$25, 12($sp)
	ldl	$25, ($25)
	stl	$25, 8($sp)
	ldl	$25, 12($sp)
	ldl	$25, 4($25)
	stl	$25, 20($sp)
	ldl	$25, 12($sp)
	ldl	$25, 8($25)
	stl	$25, 16($sp)
	ldl	$25, 12($sp)
	ldl	$0, 12($25)
	ldl	$25, 24($sp)
	cmplt	$25, $0, $0
	bne	$0, one_case_139414
zero_case_139413:
	ldl	$at, 24($sp)
	ldl	$25, 28($sp)
	cmpeq	$25, $at, $0
	bne	$0, one_case_139423
zero_case_139422:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 28($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141330:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141297:
	# done making normal call
	ldl	$at, 28($sp)
	ldl	$25, 24($sp)
	sublv	$25, $at, $2
	trapb
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141324:
	ldgp	$gp, ($26)
	mov	$0, $4
code_141298:
	# done making normal call
	# making closure call 
	lda	$1, unsafeSubstring_135046
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	ldl	$3, 8($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_141325:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141300:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141301
code_141302:
	jsr	$26, GCFromML
gc_check_141301:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 32($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $4
	br	$31, after_zeroone_139424
one_case_139423:
	ldl	$4, 32($sp)
after_zeroone_139424:
	mov	$4, $0
	br	$31, after_zeroone_139415
one_case_139414:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 24($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141329:
	ldgp	$gp, ($26)
code_141306:
	# done making normal call
	# int sub start
	ldl	$25, 20($sp)
	addl	$0, $25, $0
	lda	$2, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $2, $2
	# int sub end
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$3, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141326:
	ldgp	$gp, ($26)
code_141307:
	# done making normal call
	bne	$0, one_case_139490
zero_case_139489:
	ldl	$25, 24($sp)
	addlv	$25, 1, $0
	trapb
	# making direct call 
	stl	$0, 24($sp)
	br	$31, funtop_139397
code_141309:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_139491
one_case_139490:
	ldl	$25, 24($sp)
	addlv	$25, 1, $25
	stl	$25, 12($sp)
	trapb
	ldl	$at, 24($sp)
	ldl	$25, 28($sp)
	cmpeq	$25, $at, $0
	bne	$0, one_case_139514
zero_case_139513:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 28($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141331:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141312:
	# done making normal call
	ldl	$at, 28($sp)
	ldl	$25, 24($sp)
	sublv	$25, $at, $2
	trapb
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141327:
	ldgp	$gp, ($26)
	mov	$0, $4
code_141313:
	# done making normal call
	# making closure call 
	lda	$1, unsafeSubstring_135046
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	ldl	$3, 8($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_141328:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141315:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141316
code_141317:
	jsr	$26, GCFromML
gc_check_141316:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 32($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	mov	$0, $3
	br	$31, after_zeroone_139515
one_case_139514:
	ldl	$3, 32($sp)
after_zeroone_139515:
	# making closure call 
	ldl	$25, 16($sp)
	ldl	$4, ($25)
	ldl	$25, 16($sp)
	ldl	$0, 4($25)
	ldl	$25, 16($sp)
	ldl	$1, 8($25)
	ldl	$2, 12($sp)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	jsr	$31, ($4), 1
code_141320:
	# done making tail call
after_zeroone_139491:
after_zeroone_139415:
code_141322:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end String_scanTok_code_137878

	.rdata
		# -------- label,sizes,reg
	.long code_141324
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010400
		# -------- label,sizes,reg
	.long code_141325
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010000
		# -------- label,sizes,reg
	.long gc_check_141301
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00010000
		# -------- label,sizes,reg
	.long code_141326
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010540
		# -------- label,sizes,reg
	.long code_141327
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010500
		# -------- label,sizes,reg
	.long code_141328
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010100
		# -------- label,sizes,reg
	.long gc_check_141316
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00010100
		# -------- label,sizes,reg
	.long code_141329
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010550
		# -------- label,sizes,reg
	.long code_141330
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010400
		# -------- label,sizes,reg
	.long code_141331
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00010500
	.text
 	.align 3
	.ent String_skipSep_code_137883
 # arguments : [$137885,$0] [$137886,$1] [$136205,$2] [$136206,$3] 
 # results    : [$139350,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_skipSep_code_137883:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 20($sp)
	stl	$2, 28($sp)
	stl	$3, 24($sp)
code_141332:
funtop_139330:
	ldl	$25, 20($sp)
	ldl	$25, ($25)
	stl	$25, 16($sp)
	ldl	$25, 20($sp)
	ldl	$25, 4($25)
	stl	$25, 12($sp)
	ldl	$25, 20($sp)
	ldl	$25, 8($25)
	stl	$25, 8($sp)
	ldl	$25, 20($sp)
	ldl	$0, 12($25)
	ldl	$25, 28($sp)
	cmplt	$25, $0, $0
	bne	$0, one_case_139347
zero_case_139346:
	ldl	$0, 24($sp)
	br	$31, after_zeroone_139348
one_case_139347:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 28($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141344:
	ldgp	$gp, ($26)
code_141335:
	# done making normal call
	# int sub start
	ldl	$25, 12($sp)
	addl	$0, $25, $0
	lda	$2, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $2, $2
	# int sub end
	# making closure call 
	ldl	$25, 16($sp)
	ldl	$3, ($25)
	ldl	$25, 16($sp)
	ldl	$0, 4($25)
	ldl	$25, 16($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141343:
	ldgp	$gp, ($26)
code_141336:
	# done making normal call
	bne	$0, one_case_139372
zero_case_139371:
	ldl	$25, 28($sp)
	addlv	$25, 1, $3
	trapb
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$5, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	ldl	$2, 28($sp)
	ldl	$4, 24($sp)
	mov	$5, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($5), 1
code_141338:
	# done making tail call
	br	$31, after_zeroone_139373
one_case_139372:
	ldl	$25, 28($sp)
	addlv	$25, 1, $0
	trapb
	# making direct call 
	stl	$0, 28($sp)
	br	$31, funtop_139330
code_141340:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_139373:
after_zeroone_139348:
code_141342:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_skipSep_code_137883

	.rdata
		# -------- label,sizes,reg
	.long code_141343
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001410
		# -------- label,sizes,reg
	.long code_141344
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00001550
	.text
 	.align 3
	.ent String_anonfun_code_137873
 # arguments : [$137875,$0] [$137876,$1] [$134522,$2] 
 # results    : [$139329,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_anonfun_code_137873:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
	stl	$2, 12($sp)
code_141346:
funtop_139268:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141357:
	ldgp	$gp, ($26)
	mov	$0, $3
code_141347:
	# done making normal call
	addl	$13, 72, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141348
code_141349:
	jsr	$26, GCFromML
gc_check_141348:
	# allocating 2 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_skipSep_code_137883
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, 258($31)
	stl	$0, 12($13)
	addl	$13, 4, $2
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_scanTok_code_137878
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	lda	$0, 258($31)
	stl	$0, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 4-record
	lda	$0, 1825($31)
	stl	$0, ($13)
	ldl	$25, 8($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	stl	$3, 16($13)
	addl	$13, 4, $0
	addl	$13, 20, $13
	# done allocating 4 record
	stl	$0, 8($2)
	# allocating 4-record
	lda	$0, 1825($31)
	stl	$0, ($13)
	ldl	$25, 8($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	stl	$2, 12($13)
	stl	$3, 16($13)
	addl	$13, 4, $0
	addl	$13, 20, $13
	# done allocating 4 record
	stl	$0, 8($1)
	# done allocating 2 closures
	lda	$2, ($31)
	lda	$3, ($31)
	lda	$4, ($31)
	# making closure call 
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$5, $27
	jsr	$26, ($5), 1
code_141358:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141351:
	# done making normal call
	# making closure call 
	lda	$1, _135540
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
code_141353:
	# done making tail call
code_141355:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137873

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141348
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141357
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141358
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_tokens_code_137868
 # arguments : [$137870,$0] [$137871,$1] [$134520,$2] 
 # results    : [$139262,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_tokens_code_137868:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141359:
funtop_139256:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141360
code_141361:
	jsr	$26, GCFromML
gc_check_141360:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_anonfun_code_137873
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_141364:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_tokens_code_137868

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141360
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_scanTok_code_137915
 # arguments : [$137917,$0] [$137918,$1] [$136253,$2] [$136254,$3] [$136255,$4] 
 # results    : [$139166,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_scanTok_code_137915:
	.mask (1 << 26), -48
	.frame $sp, 48
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -48($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 24($sp)
	stl	$2, 32($sp)
	stl	$3, 28($sp)
	stl	$4, 36($sp)
code_141365:
funtop_139108:
	ldl	$25, 24($sp)
	ldl	$25, ($25)
	stl	$25, 8($sp)
	ldl	$25, 24($sp)
	ldl	$25, 4($25)
	stl	$25, 20($sp)
	ldl	$25, 24($sp)
	ldl	$0, 8($25)
	ldl	$25, 28($sp)
	cmplt	$25, $0, $0
	bne	$0, one_case_139123
zero_case_139122:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 32($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141397:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141367:
	# done making normal call
	ldl	$at, 32($sp)
	ldl	$25, 28($sp)
	sublv	$25, $at, $2
	trapb
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141390:
	ldgp	$gp, ($26)
	mov	$0, $4
code_141368:
	# done making normal call
	# making closure call 
	lda	$1, unsafeSubstring_135046
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	ldl	$3, 8($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_141391:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141370:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141371
code_141372:
	jsr	$26, GCFromML
gc_check_141371:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	br	$31, after_zeroone_139124
one_case_139123:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 28($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141395:
	ldgp	$gp, ($26)
code_141375:
	# done making normal call
	# int sub start
	ldl	$25, 20($sp)
	addl	$0, $25, $0
	lda	$2, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $2, $2
	# int sub end
	# making closure call 
	ldl	$25, 8($sp)
	ldl	$3, ($25)
	ldl	$25, 8($sp)
	ldl	$0, 4($25)
	ldl	$25, 8($sp)
	ldl	$1, 8($25)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141392:
	ldgp	$gp, ($26)
code_141376:
	# done making normal call
	bne	$0, one_case_139188
zero_case_139187:
	ldl	$25, 28($sp)
	addlv	$25, 1, $0
	trapb
	# making direct call 
	stl	$0, 28($sp)
	br	$31, funtop_139108
code_141378:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_139189
one_case_139188:
	ldl	$25, 28($sp)
	addlv	$25, 1, $25
	stl	$25, 16($sp)
	trapb
	ldl	$25, 28($sp)
	addlv	$25, 1, $25
	stl	$25, 12($sp)
	trapb
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 32($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141396:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141380:
	# done making normal call
	ldl	$at, 32($sp)
	ldl	$25, 28($sp)
	sublv	$25, $at, $2
	trapb
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141393:
	ldgp	$gp, ($26)
	mov	$0, $4
code_141381:
	# done making normal call
	# making closure call 
	lda	$1, unsafeSubstring_135046
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	ldl	$3, 8($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_141394:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141383:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141384
code_141385:
	jsr	$26, GCFromML
gc_check_141384:
	# allocating 2-record
	lda	$0, 785($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 36($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# making direct call 
	ldl	$at, 16($sp)
	stl	$at, 32($sp)
	ldl	$at, 12($sp)
	stl	$at, 28($sp)
	stl	$0, 36($sp)
	br	$31, funtop_139108
code_141387:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_139189:
after_zeroone_139124:
code_141389:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 48($sp)
	ret	$31, ($26), 1
	.end String_scanTok_code_137915

	.rdata
		# -------- label,sizes,reg
	.long code_141390
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00040400
		# -------- label,sizes,reg
	.long code_141391
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00040000
		# -------- label,sizes,reg
	.long gc_check_141371
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00040000
		# -------- label,sizes,reg
	.long code_141392
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00041400
		# -------- label,sizes,reg
	.long code_141393
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00041400
		# -------- label,sizes,reg
	.long code_141394
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00041000
		# -------- label,sizes,reg
	.long gc_check_141384
	.long 0x00001805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00041000
		# -------- label,sizes,reg
	.long code_141395
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00041410
		# -------- label,sizes,reg
	.long code_141396
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00041400
		# -------- label,sizes,reg
	.long code_141397
	.long 0x00001805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00040400
	.text
 	.align 3
	.ent String_anonfun_code_137910
 # arguments : [$137912,$0] [$137913,$1] [$134572,$2] 
 # results    : [$139107,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_anonfun_code_137910:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
	stl	$2, 12($sp)
code_141398:
funtop_139064:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141409:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141399:
	# done making normal call
	addl	$13, 32, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141400
code_141401:
	jsr	$26, GCFromML
gc_check_141400:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 793($31)
	stl	$0, ($13)
	ldl	$25, 8($sp)
	stl	$25, 4($13)
	ldl	$25, 12($sp)
	stl	$25, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $1
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_scanTok_code_137915
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
	lda	$4, ($31)
	# making closure call 
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$5, $27
	jsr	$26, ($5), 1
code_141410:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141403:
	# done making normal call
	# making closure call 
	lda	$1, _135540
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
code_141405:
	# done making tail call
code_141407:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137910

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141400
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141409
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141410
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_fields_code_137905
 # arguments : [$137907,$0] [$137908,$1] [$134570,$2] 
 # results    : [$139058,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_fields_code_137905:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141411:
funtop_139052:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141412
code_141413:
	jsr	$26, GCFromML
gc_check_141412:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_anonfun_code_137910
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_141416:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_fields_code_137905

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141412
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_anonfun_code_137927
 # arguments : [$137929,$0] [$137930,$1] [$136302,$2] [$136303,$3] 
 # results    : [$139051,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
String_anonfun_code_137927:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141417:
funtop_139046:
	cmpeq	$2, $3, $0
code_141419:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137927

	.rdata
	.text
 	.align 3
	.ent String_anonfun_code_137937
 # arguments : [$137939,$0] [$137940,$1] [$134606,$2] 
 # results    : [$139045,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_anonfun_code_137937:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 8($sp)
	stl	$2, 12($sp)
code_141420:
funtop_139016:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141429:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141421:
	# done making normal call
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141427:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141422:
	# done making normal call
	lda	$4, ($31)
	# making closure call 
	lda	$1, _135638
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$6, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 8($sp)
	ldl	$3, 12($sp)
	mov	$6, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($6), 1
code_141424:
	# done making tail call
code_141426:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137937

	.rdata
		# -------- label,sizes,reg
	.long code_141427
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long code_141429
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
	.text
 	.align 3
	.ent String_isPrefix_code_137932
 # arguments : [$137934,$0] [$137935,$1] [$134604,$2] 
 # results    : [$139010,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_isPrefix_code_137932:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141430:
funtop_139004:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141431
code_141432:
	jsr	$26, GCFromML
gc_check_141431:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_anonfun_code_137937
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_141435:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_isPrefix_code_137932

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141431
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_compare_code_137944
 # arguments : [$137946,$0] [$137947,$1] [$136324,$2] [$136325,$3] 
 # results    : [$139003,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_compare_code_137944:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 16($sp)
	stl	$3, 12($sp)
code_141436:
funtop_138971:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141445:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141437:
	# done making normal call
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141443:
	ldgp	$gp, ($26)
	mov	$0, $7
code_141438:
	# done making normal call
	lda	$3, ($31)
	lda	$6, ($31)
	# making closure call 
	lda	$1, cmp_135646
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$8, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	ldl	$4, 8($sp)
	ldl	$5, 12($sp)
	mov	$8, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($8), 1
code_141440:
	# done making tail call
code_141442:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_compare_code_137944

	.rdata
		# -------- label,sizes,reg
	.long code_141443
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
		# -------- label,sizes,reg
	.long code_141445
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
	.text
 	.align 3
	.ent String_anonfun_code_137954
 # arguments : [$137956,$0] [$137957,$1] [$136339,$2] [$136340,$3] 
 # results    : [$138970,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_anonfun_code_137954:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
	stl	$2, 20($sp)
	stl	$3, 16($sp)
code_141446:
funtop_138932:
	# making closure call 
	lda	$1, collate_135654
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $2
	mov	$3, $27
	jsr	$26, ($3), 1
code_141457:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_141448:
	# done making normal call
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141454:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141449:
	# done making normal call
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141455:
	ldgp	$gp, ($26)
	mov	$0, $7
code_141450:
	# done making normal call
	lda	$3, ($31)
	lda	$6, ($31)
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$8, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	ldl	$2, 20($sp)
	ldl	$4, 8($sp)
	ldl	$5, 16($sp)
	mov	$8, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($8), 1
code_141451:
	# done making tail call
code_141453:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_anonfun_code_137954

	.rdata
		# -------- label,sizes,reg
	.long code_141454
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000540
		# -------- label,sizes,reg
	.long code_141455
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000540
		# -------- label,sizes,reg
	.long code_141457
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000500
	.text
 	.align 3
	.ent String_collate_code_137949
 # arguments : [$137951,$0] [$137952,$1] [$134626,$2] 
 # results    : [$138926,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_collate_code_137949:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141458:
funtop_138920:
	addl	$13, 16, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141459
code_141460:
	jsr	$26, GCFromML
gc_check_141459:
	# allocating 1 closures
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_anonfun_code_137954
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$2, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
code_141463:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_collate_code_137949

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141459
	.long 0x00000805
	.long 0x01ff0ffc
	.long 0x01ff0ff8
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_cmp_code_137966
 # arguments : [$137968,$0] [$137969,$1] [$134645,$2] 
 # results    : [$138918,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_cmp_code_137966:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 16($sp)
	stl	$2, 20($sp)
code_141464:
funtop_138837:
	ldl	$25, 16($sp)
	ldl	$0, ($25)
	ldl	$25, 16($sp)
	ldl	$1, 4($25)
	ldl	$25, 16($sp)
	ldl	$25, 8($25)
	stl	$25, 12($sp)
	ldl	$25, 16($sp)
	ldl	$25, 12($25)
	stl	$25, 8($sp)
	ldl	$25, 20($sp)
	cmpeq	$25, $0, $0
	bne	$0, one_case_138854
zero_case_138853:
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141480:
	ldgp	$gp, ($26)
code_141466:
	# done making normal call
	# int sub start
	ldl	$25, 12($sp)
	addl	$0, $25, $0
	lda	$25, ($0)
	stl	$25, 12($sp)
	ldq_u	$at, ($0)
	ldl	$25, 12($sp)
	extbl	$at, $25, $25
	stl	$25, 12($sp)
	# int sub end
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141478:
	ldgp	$gp, ($26)
code_141467:
	# done making normal call
	# int sub start
	ldl	$25, 8($sp)
	addl	$0, $25, $0
	lda	$25, ($0)
	stl	$25, 8($sp)
	ldq_u	$at, ($0)
	ldl	$25, 8($sp)
	extbl	$at, $25, $25
	stl	$25, 8($sp)
	# int sub end
	# making closure call 
	lda	$1, GT_135684
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	ldl	$3, 8($sp)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141479:
	ldgp	$gp, ($26)
code_141469:
	# done making normal call
	bne	$0, one_case_138892
zero_case_138891:
	ldl	$at, 8($sp)
	ldl	$25, 12($sp)
	cmpeq	$25, $at, $0
	bne	$0, one_case_138901
zero_case_138900:
	lda	$0, ($31)
	br	$31, after_zeroone_138902
one_case_138901:
	ldl	$25, 20($sp)
	addlv	$25, 1, $0
	trapb
	# making direct call 
	stl	$0, 20($sp)
	br	$31, funtop_138837
code_141473:
	# done making self tail call
	lda	$0, ($31)
after_zeroone_138902:
	br	$31, after_zeroone_138893
one_case_138892:
	lda	$0, 1($31)
after_zeroone_138893:
	br	$31, after_zeroone_138855
one_case_138854:
	mov	$1, $0
after_zeroone_138855:
code_141477:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_cmp_code_137966

	.rdata
		# -------- label,sizes,reg
	.long code_141478
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000110
		# -------- label,sizes,reg
	.long code_141479
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141480
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000150
	.text
 	.align 3
	.ent String_sgtr_code_137961
 # arguments : [$137963,$0] [$137964,$1] [$136357,$2] [$136358,$3] 
 # results    : [$138836,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_sgtr_code_137961:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 16($sp)
	stl	$3, 12($sp)
code_141481:
funtop_138784:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141494:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141482:
	# done making normal call
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141493:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141483:
	# done making normal call
	addl	$13, 36, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141484
code_141485:
	jsr	$26, GCFromML
gc_check_141484:
	ldl	$25, 8($sp)
	cmplt	$25, $1, $0
	bne	$0, one_case_138807
zero_case_138806:
	mov	$1, $2
	br	$31, after_zeroone_138808
one_case_138807:
	ldl	$2, 8($sp)
after_zeroone_138808:
	ldl	$25, 8($sp)
	cmplt	$1, $25, $1
	# allocating 1 closures
	# allocating 4-record
	lda	$0, 3105($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	stl	$1, 8($13)
	ldl	$25, 16($sp)
	stl	$25, 12($13)
	ldl	$25, 12($sp)
	stl	$25, 16($13)
	addl	$13, 4, $1
	addl	$13, 20, $13
	# done allocating 4 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_cmp_code_137966
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
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($3), 1
code_141489:
	# done making tail call
code_141491:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_sgtr_code_137961

	.rdata
		# -------- label,sizes,reg
	.long code_141493
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
		# -------- label,sizes,reg
	.long gc_check_141484
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
		# -------- label,sizes,reg
	.long code_141494
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
	.text
 	.align 3
	.ent String_LTEQ_code_137979
 # arguments : [$137981,$0] [$137982,$1] [$136376,$2] [$136377,$3] 
 # results    : [$138781,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_LTEQ_code_137979:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141495:
funtop_138763:
	# making closure call 
	lda	$1, sgtr_134632
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141501:
	ldgp	$gp, ($26)
code_141496:
	# done making normal call
	bne	$0, one_case_138777
zero_case_138776:
	lda	$0, 1($31)
	br	$31, after_zeroone_138778
one_case_138777:
	lda	$0, ($31)
after_zeroone_138778:
code_141500:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_LTEQ_code_137979

	.rdata
		# -------- label,sizes,reg
	.long code_141501
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_LT_code_137984
 # arguments : [$137986,$0] [$137987,$1] [$136385,$2] [$136386,$3] 
 # results    : [$138762,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_LT_code_137984:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $5
	mov	$3, $2
code_141502:
funtop_138752:
	# making closure call 
	lda	$1, sgtr_134632
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$5, $3
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_141503:
	# done making tail call
code_141505:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_LT_code_137984

	.rdata
	.text
 	.align 3
	.ent String_GTEQ_code_137989
 # arguments : [$137991,$0] [$137992,$1] [$136393,$2] [$136394,$3] 
 # results    : [$138751,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_GTEQ_code_137989:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$2, $5
	mov	$3, $2
code_141507:
funtop_138741:
	# making closure call 
	lda	$1, LTEQ_134662
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$5, $3
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($4), 1
code_141508:
	# done making tail call
code_141510:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_GTEQ_code_137989

	.rdata
	.text
 	.align 3
	.ent String_getc_code_137999
 # arguments : [$138001,$0] [$138002,$1] [$134686,$2] 
 # results    : [$138720,$0] 
 # destroys   :  $3 $2 $1 $0
 # modifies   :  $3 $2 $1 $0
String_getc_code_137999:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	mov	$1, $0
	mov	$2, $3
code_141512:
funtop_138703:
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141513
code_141514:
	jsr	$26, GCFromML
gc_check_141513:
	ldl	$1, ($0)
	ldl	$0, 4($0)
	cmpult	$3, $0, $0
	bne	$0, one_case_138716
zero_case_138715:
	lda	$0, ($31)
	br	$31, after_zeroone_138717
one_case_138716:
	addl	$3, 1, $2
	# int sub start
	addl	$3, $1, $0
	lda	$1, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $1, $1
	# int sub end
	# allocating 2-record
	lda	$0, 17($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	stl	$2, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
after_zeroone_138717:
code_141519:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_getc_code_137999

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141513
	.long 0x00000805
	.long 0x01ff0ff1
	.long 0x01ff0ff0
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_accum_code_138008
 # arguments : [$138010,$0] [$138011,$1] [$136424,$2] [$136425,$3] 
 # results    : [$138660,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_accum_code_138008:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$1, 12($sp)
	stl	$2, 20($sp)
	stl	$3, 16($sp)
code_141520:
funtop_138599:
	ldl	$25, 12($sp)
	ldl	$25, ($25)
	stl	$25, 8($sp)
	ldl	$25, 12($sp)
	ldl	$1, 4($25)
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 20($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141537:
	ldgp	$gp, ($26)
code_141521:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141522
code_141523:
	jsr	$26, GCFromML
gc_check_141522:
sumarm_138621:
	bne	$0, sumarm_138622
code_141525:
	ldl	$at, 8($sp)
	ldl	$25, 20($sp)
	cmpult	$25, $at, $0
	bne	$0, one_case_138631
zero_case_138630:
	# making closure call 
	lda	$1, _135773
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 16($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141538:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141528:
	# done making normal call
	# making closure call 
	lda	$1, implode_134442
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141536:
	ldgp	$gp, ($26)
code_141529:
	# done making normal call
	br	$31, after_zeroone_138632
one_case_138631:
	lda	$0, ($31)
after_zeroone_138632:
	br	$31, after_sum_138618
sumarm_138622:
	ldl	$2, ($0)
	ldl	$1, 4($0)
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$2, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# making direct call 
	stl	$1, 20($sp)
	stl	$0, 16($sp)
	br	$31, funtop_138599
code_141532:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_sum_138618
sumarm_138661:
after_sum_138618:
code_141535:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_accum_code_138008

	.rdata
		# -------- label,sizes,reg
	.long gc_check_141522
	.long 0x00001005
	.long 0x00000001
	.long 0x00000000
		# stacktrace
	.long 0x00000140
		# -------- label,sizes,reg
	.long code_141536
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_141537
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
		# -------- label,sizes,reg
	.long code_141538
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_fromString_code_137994
 # arguments : [$137996,$0] [$137997,$1] [$134682,$2] 
 # results    : [$138598,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_fromString_code_137994:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
	stl	$2, 12($sp)
code_141539:
funtop_138538:
	# making closure call 
	lda	$1, size_134210
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	ldl	$2, 12($sp)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141554:
	ldgp	$gp, ($26)
	mov	$0, $2
code_141540:
	# done making normal call
	# making closure call 
	lda	$1, anonfun_134201
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141553:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141541:
	# done making normal call
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141542
code_141543:
	jsr	$26, GCFromML
gc_check_141542:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 273($31)
	stl	$0, ($13)
	ldl	$25, 12($sp)
	stl	$25, 4($13)
	ldl	$25, 8($sp)
	stl	$25, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 3-record
	lda	$0, 1561($31)
	stl	$0, ($13)
	lda	$0, String_getc_code_137999
	stl	$0, 4($13)
	lda	$0, 256($31)
	stl	$0, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $2
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# making closure call 
	lda	$1, _135753
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141555:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141546:
	# done making normal call
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141547
code_141548:
	jsr	$26, GCFromML
gc_check_141547:
	# allocating 1 closures
	# allocating 2-record
	lda	$0, 529($31)
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
	lda	$0, String_accum_code_138008
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
code_141550:
	# done making tail call
code_141552:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_fromString_code_137994

	.rdata
		# -------- label,sizes,reg
	.long code_141553
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_141542
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long gc_check_141547
	.long 0x00000805
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_141554
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000040
		# -------- label,sizes,reg
	.long code_141555
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
 	.align 3
	.ent String_fromCString_inner_code_138017
 # arguments : [$138019,$0] [$138020,$1] [$134732,$2] 
 # results    : [$138537,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
String_fromCString_inner_code_138017:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)	# push_ret
code_141557:
funtop_138531:
	lda	$1, _135817
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
code_141561:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end String_fromCString_inner_code_138017

	.rdata
	.text
 	.align 3
	.ent String_main
 # arguments : 
 # results    : [$138530,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
String_main:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	stq	$26, 0($sp)	# push_ret
code_141562:
funtop_138025:
	lda	$16, ($31)
	lda	$27, AssertMirrorPtrArray
	jsr	$26, save_regs_MLtoC
	jsr	$26, AssertMirrorPtrArray
code_141820:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_141563:
	lda	$1, string_TYC
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$5, ($0)
	lda	$1, 111($31)
	lda	$0, String_STR_c_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141569
code_141571:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141569:
	lda	$4, String_STR_c_INT
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
	# allocating 2-record
	# done allocating 2 record
	# start making constructor call
	lda	$1, option_TYC
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$3, ($0)
	lda	$1, string_TYC
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$2, ($3)
	ldl	$0, 4($3)
	mov	$2, $27
	jsr	$26, ($2), 1
code_141810:
	ldgp	$gp, ($26)
	stl	$0, 16($sp)
code_141582:
	# done making constructor call
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_134201
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, size_134210
	# done allocating 1 closures
	lda	$1, Size_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 4($0)
	lda	$1, 111($31)
	lda	$0, mk_134990
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 84, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141588
code_141590:
	subl	$13, 84, $at
	jsr	$26, GCFromML
afterMutateCheck_141588:
	lda	$4, mk_134990
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
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 32($0)
	lda	$1, 111($31)
	lda	$0, chars_135000
	stl	$1, -4($0)
	lda	$4, chars_135000
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
	lda	$0, str_134220
	# done allocating 1 closures
	lda	$1, Subscript_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 4($0)
	lda	$1, 111($31)
	lda	$0, mk_135018
	stl	$1, -4($0)
	lda	$4, mk_135018
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
	lda	$0, sub_134224
	# done allocating 1 closures
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 36($0)
	lda	$1, 111($31)
	lda	$0, unsafeSubstring_135046
	stl	$1, -4($0)
	lda	$4, unsafeSubstring_135046
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
	lda	$1, General_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$0, 8($0)
	ldl	$5, 4($0)
	lda	$1, 111($31)
	lda	$0, mk_135053
	stl	$1, -4($0)
	lda	$4, mk_135053
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
	lda	$0, substring_134234
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, extract_134254
	# done allocating 1 closures
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 40($0)
	lda	$1, 111($31)
	lda	$0, concat2_135218
	stl	$1, -4($0)
	lda	$4, concat2_135218
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
	lda	$0, HAT_134348
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, length_134362
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, find_134413
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_134360
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, concat_134357
	# done allocating 1 closures
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 52($0)
	lda	$1, 111($31)
	lda	$0, implode_135359
	stl	$1, -4($0)
	lda	$4, implode_135359
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
	lda	$0, length_inner_134466
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, implode_134442
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, explode_134473
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, map_134487
	# done allocating 1 closures
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, 48($0)
	lda	$1, ($31)
	lda	$3, 256($31)
	# making closure call 
	ldl	$4, ($2)
	ldl	$0, 4($2)
	ldl	$2, 8($2)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141811:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141654:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _135439
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141659
code_141661:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141659:
	lda	$4, _135439
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
	lda	$0, translate_134510
	# done allocating 1 closures
	lda	$5, record_138038
	lda	$3, 256($31)
	# making closure call 
	lda	$1, rev_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_141812:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141671:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _135540
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141676
code_141678:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141676:
	lda	$4, _135540
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
	lda	$0, tokens_134519
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, fields_134569
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, anonfun_134613
	# done allocating 1 closures
	# allocating 1-record
	# done allocating 1 record
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, 60($0)
	lda	$1, ($31)
	lda	$3, record_138253
	# making closure call 
	ldl	$4, ($2)
	ldl	$0, 4($2)
	ldl	$2, 8($2)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141813:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141688:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _135638
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 48, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141693
code_141695:
	subl	$13, 48, $at
	jsr	$26, GCFromML
afterMutateCheck_141693:
	lda	$4, _135638
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
	lda	$0, isPrefix_134603
	# done allocating 1 closures
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 68($0)
	lda	$1, 111($31)
	lda	$0, cmp_135646
	stl	$1, -4($0)
	lda	$4, cmp_135646
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
	lda	$0, compare_134620
	# done allocating 1 closures
	lda	$1, PreString_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 64($0)
	lda	$1, 111($31)
	lda	$0, collate_135654
	stl	$1, -4($0)
	lda	$4, collate_135654
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
	lda	$0, collate_134625
	# done allocating 1 closures
	lda	$1, Char_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$5, 40($0)
	lda	$1, 111($31)
	lda	$0, GT_135684
	stl	$1, -4($0)
	lda	$4, GT_135684
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
	lda	$0, sgtr_134632
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, LTEQ_134662
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, LT_134670
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, GTEQ_134675
	# done allocating 1 closures
	lda	$1, Char_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, 52($0)
	lda	$1, 2($31)
	lda	$3, 256($31)
	# making closure call 
	ldl	$4, ($2)
	ldl	$0, 4($2)
	ldl	$2, 8($2)
	mov	$4, $27
	jsr	$26, ($4), 1
code_141814:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141732:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _135753
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141737
code_141739:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141737:
	lda	$4, _135753
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
	lda	$5, ($31)
	lda	$3, 256($31)
	# making closure call 
	lda	$1, rev_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$4, ($1)
	ldl	$0, 4($1)
	ldl	$2, 8($1)
	mov	$5, $1
	mov	$4, $27
	jsr	$26, ($4), 1
code_141815:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141749:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _135773
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141754
code_141756:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141754:
	lda	$4, _135773
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
	lda	$0, fromString_134681
	# done allocating 1 closures
	lda	$1, Char_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, 60($0)
	# making closure call 
	lda	$1, translate_134510
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141816:
	ldgp	$gp, ($26)
	stl	$0, 12($sp)
code_141766:
	# done making normal call
	lda	$1, Char_STR_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$2, 68($0)
	# making closure call 
	lda	$1, translate_134510
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141818:
	ldgp	$gp, ($26)
	stl	$0, 8($sp)
code_141768:
	# done making normal call
	lda	$1, LibFail_r_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$0, ($0)
	ldl	$1, 4($0)
	lda	$2, string_138441
	# making closure call 
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_141819:
	ldgp	$gp, ($26)
	mov	$0, $5
code_141770:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, _135817
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141775
code_141777:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141775:
	lda	$4, _135817
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
	lda	$0, fromCString_inner_136453
	# done allocating 1 closures
	lda	$1, string_TYC
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$2, ($0)
	lda	$4, fromCString_inner_136453
	# making closure call 
	lda	$1, vararg_INT
	ldl	$0, 1092($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	mov	$2, $1
	ldl	$2, 16($sp)
	mov	$5, $27
	jsr	$26, ($5), 1
code_141817:
	ldgp	$gp, ($26)
	mov	$0, $1
code_141788:
	# done making normal call
	addl	$13, 112, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_141789
code_141790:
	jsr	$26, GCFromML
gc_check_141789:
	# allocating 3-record
	lda	$0, 1817($31)
	stl	$0, ($13)
	lda	$0, LT_134670
	stl	$0, 4($13)
	lda	$0, GTEQ_134675
	stl	$0, 8($13)
	lda	$0, sgtr_134632
	stl	$0, 12($13)
	addl	$13, 4, $0
	addl	$13, 16, $13
	# done allocating 3 record
	# allocating 23-record
	lda	$2, -327($31)
	ldah	$2, -32768($2)
	stl	$2, ($13)
	lda	$2, stringmaxsize
	ldl	$2, ($2)
	stl	$2, 4($13)
	lda	$2, size_134210
	stl	$2, 8($13)
	lda	$2, sub_134224
	stl	$2, 12($13)
	lda	$2, substring_134234
	stl	$2, 16($13)
	lda	$2, extract_134254
	stl	$2, 20($13)
	lda	$2, concat_134357
	stl	$2, 24($13)
	lda	$2, HAT_134348
	stl	$2, 28($13)
	lda	$2, str_134220
	stl	$2, 32($13)
	lda	$2, implode_134442
	stl	$2, 36($13)
	lda	$2, explode_134473
	stl	$2, 40($13)
	lda	$2, fromString_134681
	stl	$2, 44($13)
	ldl	$25, 12($sp)
	stl	$25, 48($13)
	stl	$1, 52($13)
	ldl	$25, 8($sp)
	stl	$25, 56($13)
	lda	$1, map_134487
	stl	$1, 60($13)
	lda	$1, translate_134510
	stl	$1, 64($13)
	lda	$1, tokens_134519
	stl	$1, 68($13)
	lda	$1, fields_134569
	stl	$1, 72($13)
	lda	$1, isPrefix_134603
	stl	$1, 76($13)
	lda	$1, compare_134620
	stl	$1, 80($13)
	lda	$1, collate_134625
	stl	$1, 84($13)
	lda	$1, LTEQ_134662
	stl	$1, 88($13)
	stl	$0, 92($13)
	addl	$13, 4, $5
	addl	$13, 96, $13
	# done allocating 23 record
	lda	$1, 111($31)
	lda	$0, String_STR_r_INT
	stl	$1, -4($0)
	ldl	$0, 1080($12)
	ldl	$1, 1084($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_141797
code_141799:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_141797:
	lda	$4, String_STR_r_INT
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
code_141809:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end String_main

	.rdata
		# -------- label,sizes,reg
	.long code_141810
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_141811
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141812
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141813
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141814
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141815
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141816
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141817
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long afterMutateCheck_141569
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_141588
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long afterMutateCheck_141659
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long afterMutateCheck_141676
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long afterMutateCheck_141693
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long afterMutateCheck_141737
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long afterMutateCheck_141754
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_141818
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000140
		# -------- label,sizes,reg
	.long code_141819
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000150
		# -------- label,sizes,reg
	.long afterMutateCheck_141775
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000150
		# -------- label,sizes,reg
	.long gc_check_141789
	.long 0x00001005
	.long 0x00000002
	.long 0x00000000
		# stacktrace
	.long 0x00000050
		# -------- label,sizes,reg
	.long afterMutateCheck_141797
	.long 0x00001005
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_141820
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
String_unit_CODE_END_VAL:
	.rdata
		# endgcinfo with filler for alignment
	.globl String_unit_GCTABLE_END_VAL
String_unit_GCTABLE_END_VAL:
	.long 0x00000000
	.sdata
	.align 3
	.sdata
	.align 3
	.globl String_unit_GLOBALS_BEGIN_VAL
String_unit_GLOBALS_BEGIN_VAL:
		# Global
	.long 0x00000037
	.globl String_STR_c_INT
String_STR_c_INT:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000211
record_138038:
	.long 0x00000001
	.long 0x00000000
		# static record tag
	.long 0x00000619
anonfun_134201:
	.long String_anonfun_code_137716
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
size_134210:
	.long String_size_code_137721
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
mk_134990:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
chars_135000:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
str_134220:
	.long String_str_code_137726
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
mk_135018:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
sub_134224:
	.long String_sub_code_137731
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
unsafeSubstring_135046:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
mk_135053:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
substring_134234:
	.long String_substring_code_137736
	.long 0x00000100
	.long 0x00000100
	.long 0x00000002
string_138122:
		# string size = 0
	# .ascii "" (zero length string)
		# static record tag
	.long 0x00000619
extract_134254:
	.long String_extract_code_137741
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
concat2_135218:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
HAT_134348:
	.long String_HAT_code_137766
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
length_134362:
	.long String_length_code_137771
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
find_134413:
	.long String_find_code_137776
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
anonfun_134360:
	.long String_anonfun_code_137781
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
concat_134357:
	.long String_concat_code_137805
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
implode_135359:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
length_inner_134466:
	.long String_length_inner_code_137810
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
implode_134442:
	.long String_implode_code_137815
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
explode_134473:
	.long String_explode_code_137820
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
map_134487:
	.long String_map_code_137832
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
_135439:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
translate_134510:
	.long String_translate_code_137856
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
_135540:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
tokens_134519:
	.long String_tokens_code_137868
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
fields_134569:
	.long String_fields_code_137905
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
anonfun_134613:
	.long String_anonfun_code_137927
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000109
record_138253:
	.long anonfun_134613
		# Global
	.long 0x00000037
_135638:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
isPrefix_134603:
	.long String_isPrefix_code_137932
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
cmp_135646:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
compare_134620:
	.long String_compare_code_137944
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
collate_135654:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
collate_134625:
	.long String_collate_code_137949
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
GT_135684:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
sgtr_134632:
	.long String_sgtr_code_137961
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
LTEQ_134662:
	.long String_LTEQ_code_137979
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
LT_134670:
	.long String_LT_code_137984
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
GTEQ_134675:
	.long String_GTEQ_code_137989
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
_135753:
	.long 0x00000102
	.long 0x00000102
		# Global
	.long 0x00000037
_135773:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
fromString_134681:
	.long String_fromString_code_137994
	.long 0x00000100
	.long 0x00000100
	.long 0x00000112
string_138441:
		# string size = 34
	.ascii "String.fromCString not implemented"
	.align 2
		# Global
	.long 0x00000037
_135817:
	.long 0x00000102
	.long 0x00000102
		# static record tag
	.long 0x00000619
fromCString_inner_136453:
	.long String_fromCString_inner_code_138017
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x00000037
	.globl String_STR_r_INT
String_STR_r_INT:
	.long 0x00000102
	.long 0x00000102
		# Module closure
	.long 0x00000619
	.globl String_unit_closure
String_unit_closure:
	.long String_main
	.long 0x00000000
	.long 0x00000000
	.long 0x00000311
	.globl String_unit
String_unit:
	.long String_unit_closure
	.long String_unit_closure
	.globl String_unit_GLOBALS_END_VAL
String_unit_GLOBALS_END_VAL:
	.long 0x00000000
	.long 0 #filler

	.sdata
	.align 3
	.globl String_unit_TRACE_GLOBALS_BEGIN_VAL
String_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long String_STR_r_INT
	.long _135817
	.long _135773
	.long _135753
	.long GT_135684
	.long collate_135654
	.long cmp_135646
	.long _135638
	.long _135540
	.long _135439
	.long implode_135359
	.long concat2_135218
	.long mk_135053
	.long unsafeSubstring_135046
	.long mk_135018
	.long chars_135000
	.long mk_134990
	.long String_STR_c_INT
	.globl String_unit_TRACE_GLOBALS_END_VAL
String_unit_TRACE_GLOBALS_END_VAL:
		# filler so label is defined
	.long 0x00000000
