# tilt -C mapfile-bug -fshowClosureConv -fshowRtl -fTraceDebug -fTraceDiag -fToasmMsgs -fChaitinMsgs -rMakeTable mapfile-bug
#	
# The flag MakeTable is a hack to generate the final assembler
# sans GC tables.
	
	.set noat
	.rdata
		# gcinfo
	.globl Pprtl_unit_GCTABLE_BEGIN_VAL
Pprtl_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl Pprtl_unit_CODE_END_VAL
	.globl Pprtl_unit_CODE_BEGIN_VAL
Pprtl_unit_CODE_BEGIN_VAL:
	.text
 	.align 3
	.ent Pprtl_functor_var_c_code_1194
 # arguments : [$1196,$0] [$1036,$1] 
 # results    : [$1301,$0] 
 # destroys   :  $1 $0
 # modifies   :  $1 $0
Pprtl_functor_var_c_code_1194:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_1310
	lda	$sp, 16($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -16($sp)
code_1310:
	stq	$26, 0($sp)	# push_ret
code_1307:
funtop_1297:
	lda	$0, 256($31)
code_1309:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Pprtl_functor_var_c_code_1194

	.rdata
	.text
 	.align 3
	.ent Pprtl_cmpf2s_code_1204
 # arguments : [$1206,$0] [$1207,$1] [$1083,$2] 
 # results    : [$1296,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Pprtl_cmpf2s_code_1204:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_1318
	lda	$sp, 32($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -32($sp)
code_1318:
	stq	$26, 0($sp)	# push_ret
	stl	$0, 8($sp)
	stl	$2, 16($sp)
code_1311:
funtop_1276:
	ldl	$25, ($1)
	stl	$25, 12($sp)
	ldl	$1, 4($1)
	# making closure polycall
	ldl	$2, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$2, $27
	jsr	$26, ($2), 1
code_1317:
	ldgp	$gp, ($26)
	mov	$0, $3
code_1312:
	# done making normal call
	# making closure call 
	ldl	$25, 12($sp)
	ldl	$4, ($25)
	ldl	$25, 12($sp)
	ldl	$0, 4($25)
	ldl	$25, 12($sp)
	ldl	$1, 8($25)
	ldl	$2, 16($sp)
	mov	$4, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	jsr	$31, ($4), 1
code_1313:
	# done making tail call
code_1315:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Pprtl_cmpf2s_code_1204

	.rdata
	.text
 	.align 3
	.ent Pprtl_functor_var_r_code_1199
 # arguments : [$1201,$0] [$1121,$1] [$1202,$2] [$1037,$3] 
 # results    : [$1273,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Pprtl_functor_var_r_code_1199:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_1331
	lda	$sp, 16($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -16($sp)
code_1331:
	stq	$26, 0($sp)	# push_ret
	mov	$1, $4
code_1319:
funtop_1233:
	addl	$13, 28, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_1320
code_1321:
	jsr	$26, GCFromML
gc_check_1320:
	ldl	$0, ($3)
	ldl	$2, 4($0)
	ldl	$1, ($0)
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
	lda	$0, Pprtl_cmpf2s_code_1204
	stl	$0, 4($13)
	stl	$4, 8($13)
	stl	$1, 12($13)
	addl	$13, 4, $6
	addl	$13, 16, $13
	# done allocating 3 record
	# done allocating 1 closures
	# Proj_c at label T31_TYC
	ldl	$0, 88($4)
	ldl	$4, 36($0)
	lda	$2, 2($31)
	# making closure call 
	lda	$1, vararg_INT
	ldl	$0, 1076($12)
	addl	$1, $0, $0
	ldl	$1, ($0)
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$3, 8($1)
	mov	$4, $1
	mov	$6, $4
	mov	$5, $27
	jsr	$26, ($5), 1
code_1330:
	ldgp	$gp, ($26)
	mov	$0, $1
code_1324:
	# done making normal call
	addl	$13, 8, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_1325
code_1326:
	jsr	$26, GCFromML
gc_check_1325:
	# allocating 1-record
	lda	$0, 265($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	addl	$13, 4, $0
	addl	$13, 8, $13
	# done allocating 1 record
code_1329:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Pprtl_functor_var_r_code_1199

	.rdata
	.text
 	.align 3
	.ent Pprtl_main
 # arguments : 
 # results    : [$1232,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $8 $7 $6 $5 $4 $3 $2 $1 $0
Pprtl_main:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_1337
	lda	$sp, 16($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -16($sp)
code_1337:
	stq	$26, 0($sp)	# push_ret
code_1332:
funtop_1218:
	lda	$16, ($31)
	lda	$27, AssertMirrorPtrArray
	jsr	$26, save_regs_MLtoC
	jsr	$26, AssertMirrorPtrArray
code_1336:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_1333:
	# allocating 2-record
	# done allocating 2 record
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, functor_var_r_1035
	# done allocating 1 closures
	lda	$0, 256($31)
code_1335:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Pprtl_main

	.rdata
	.text
Pprtl_unit_CODE_END_VAL:
	.rdata
		# endgcinfo with filler for alignment
	.globl Pprtl_unit_GCTABLE_END_VAL
Pprtl_unit_GCTABLE_END_VAL:
	.long 0x00000000
	.sdata
	.align 3
	.sdata
	.align 3
	.globl Pprtl_unit_GLOBALS_BEGIN_VAL
Pprtl_unit_GLOBALS_BEGIN_VAL:
		# static record tag
	.long 0x00000211
record_1225:
	.long Pprtl_functor_var_c_code_1194
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl bug_FCT_c_INT
bug_FCT_c_INT:
	.long record_1225
	.long record_1225
		# Global
	.long 0x0000006f
	.globl bug_FCT_r_INT
bug_FCT_r_INT:
	.long functor_var_r_1035
	.long functor_var_r_1035
		# static record tag
	.long 0x00000619
functor_var_r_1035:
	.long Pprtl_functor_var_r_code_1199
	.long 0x00000100
	.long 0x00000100
		# Module closure
	.long 0x00000619
	.globl Pprtl_unit_closure
Pprtl_unit_closure:
	.long Pprtl_main
	.long 0x00000000
	.long 0x00000000
	.long 0x00000311
	.globl Pprtl_unit
Pprtl_unit:
	.long Pprtl_unit_closure
	.long Pprtl_unit_closure
	.globl Pprtl_unit_GLOBALS_END_VAL
Pprtl_unit_GLOBALS_END_VAL:
	.long 0x00000000
	.long 0 #filler

	.sdata
	.align 3
	.globl Pprtl_unit_TRACE_GLOBALS_BEGIN_VAL
Pprtl_unit_TRACE_GLOBALS_BEGIN_VAL:
	.globl Pprtl_unit_TRACE_GLOBALS_END_VAL
Pprtl_unit_TRACE_GLOBALS_END_VAL:
		# filler so label is defined
	.long 0x00000000
