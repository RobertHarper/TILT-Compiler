	.set noat
	.rdata
		# gcinfo
	.globl Short_unit_GCTABLE_BEGIN_VAL
Short_unit_GCTABLE_BEGIN_VAL:
	.text
	.globl Short_unit_CODE_END_VAL
	.globl Short_unit_CODE_BEGIN_VAL
Short_unit_CODE_BEGIN_VAL:
	.text
 	.align 3
	.ent Short_mkDigit_code_1786
 # arguments : [$1788,$0] [$1789,$1] [$1298,$2] 
 # results    : [$2124,$0] 
 # destroys   :  $2 $1 $0
 # modifies   :  $2 $1 $0
Short_mkDigit_code_1786:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_2134
	lda	$sp, 16($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -16($sp)
code_2134:
	stq	$26, 0($sp)	# push_ret
code_2131:
funtop_2118:
	lda	$0, string_1835
	# int sub start
	addl	$2, $0, $0
	lda	$1, ($0)
	ldq_u	$at, ($0)
	extbl	$at, $1, $1
	# int sub end
code_2133:
	mov	$1, $0
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Short_mkDigit_code_1786

	.rdata
	.text
 	.align 3
	.ent Short_f_code_1791
 # arguments : [$1793,$0] [$1794,$1] [$1574,$2] [$1575,$3] [$1576,$4] 
 # results    : [$2089,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Short_f_code_1791:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_2151
	lda	$sp, 32($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -32($sp)
code_2151:
	stq	$26, 0($sp)	# push_ret
	stl	$4, 16($sp)
code_2135:
funtop_2042:
	cmpult	$2, 8, $0
	bne	$0, one_case_2051
zero_case_2050:
	zap	$2, 240, $25
	stl	$25, 12($sp)
	ldl	$25, 12($sp)
	srl	$25, 3, $25
	stl	$25, 12($sp)
	ldl	$25, 12($sp)
	addl	$25, $31, $25
	stl	$25, 12($sp)
	and	$2, 7, $2
	addlv	$3, 1, $25
	stl	$25, 8($sp)
	trapb
	# making closure call 
	lda	$1, mkDigit_1297
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_2150:
	ldgp	$gp, ($26)
	mov	$0, $1
code_2137:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_2138
code_2139:
	jsr	$26, GCFromML
gc_check_2138:
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# making direct call 
	ldl	$2, 12($sp)
	ldl	$3, 8($sp)
	stl	$0, 16($sp)
	br	$31, funtop_2042
code_2141:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_2052
one_case_2051:
	addlv	$3, 1, $25
	stl	$25, 8($sp)
	trapb
	# making closure call 
	lda	$1, mkDigit_1297
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_2149:
	ldgp	$gp, ($26)
	mov	$0, $1
code_2143:
	# done making normal call
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_2144
code_2145:
	jsr	$26, GCFromML
gc_check_2144:
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	ldl	$25, 8($sp)
	stl	$25, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
after_zeroone_2052:
code_2148:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Short_f_code_1791

	.rdata
		# -------- label,sizes,reg
	.long gc_check_2138
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long gc_check_2144
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_2149
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_2150
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
	.text
 	.align 3
	.ent Short_f_code_1796
 # arguments : [$1798,$0] [$1799,$1] [$1594,$2] [$1595,$3] [$1596,$4] 
 # results    : [$2013,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Short_f_code_1796:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -32($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_2172
	lda	$sp, 32($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -32($sp)
code_2172:
	stq	$26, 0($sp)	# push_ret
	stl	$4, 16($sp)
code_2152:
funtop_1963:
	cmpult	$2, 10, $0
	bne	$0, one_case_1972
zero_case_1971:
	lda	$0, 10($31)
	mov	$2, $24
	mov	$0, $25
	lda	$27, __divlu
	jsr	$23, __divlu
code_2171:
	ldgp	$gp, ($23)
	stl	$27, 12($sp)
code_2155:
	addlv	$3, 1, $25
	stl	$25, 8($sp)
	trapb
	ldl	$at, 12($sp)
	ldl	$25, 12($sp)
	s4addq	$at, $at, $0
	addq	$0, $0, $0
	subl	$2, $0, $2
	# making closure call 
	lda	$1, mkDigit_1297
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_2169:
	ldgp	$gp, ($26)
	mov	$0, $1
code_2157:
	# done making normal call
	addl	$13, 12, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_2158
code_2159:
	jsr	$26, GCFromML
gc_check_2158:
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
	# making direct call 
	ldl	$2, 12($sp)
	ldl	$3, 8($sp)
	stl	$0, 16($sp)
	br	$31, funtop_1963
code_2161:
	# done making self tail call
	lda	$0, ($31)
	br	$31, after_zeroone_1973
one_case_1972:
	addlv	$3, 1, $25
	stl	$25, 8($sp)
	trapb
	# making closure call 
	lda	$1, mkDigit_1297
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_2170:
	ldgp	$gp, ($26)
	mov	$0, $1
code_2163:
	# done making normal call
	addl	$13, 24, $at
	ldq	$14, 112($12)
	cmpule	$at, $14, $25
	bne	$25, gc_check_2164
code_2165:
	jsr	$26, GCFromML
gc_check_2164:
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	stl	$1, 4($13)
	ldl	$25, 16($sp)
	stl	$25, 8($13)
	addl	$13, 4, $1
	addl	$13, 12, $13
	# done allocating 2 record
	# allocating 2-record
	lda	$0, 529($31)
	stl	$0, ($13)
	ldl	$25, 8($sp)
	stl	$25, 4($13)
	stl	$1, 8($13)
	addl	$13, 4, $0
	addl	$13, 12, $13
	# done allocating 2 record
after_zeroone_1973:
code_2168:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end Short_f_code_1796

	.rdata
		# -------- label,sizes,reg
	.long code_2169
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long gc_check_2158
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long gc_check_2164
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_2170
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_2171
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
	.text
 	.align 3
	.ent Short_f_code_1801
 # arguments : [$1803,$0] [$1804,$1] [$1614,$2] [$1615,$3] [$1616,$4] 
 # results    : [$1934,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Short_f_code_1801:
	.mask (1 << 26), -32
	.frame $sp, 32
	.prologue 1
	ldgp	$gp, ($27)					; set gp			
	lda	$sp, -32($sp)					; reserve 8 stack slots
	ldl	ASMTMP_REG, 1072(THREADPTR_REG)
	cmpult	$sp, ASMTMP_REG, ASMTMP_REG
	beq	ASMTMP_REG, code_2189
	lda	$sp, 32($sp)
	lda	ASMTMP_REG, (zero)
	mov	RA, $25
	jsr	RA, NewStackletFromML
	lda	$sp, -32($sp)
code_2189:
	;; 
	;; arguments:
	;; $0
	;; $1
	;; $2	w	word
	;; $3	n	int
	;; $4	l	char list
	;; 
	;; stack slots:	
	;; 0, 1		return address
	;; 2		temp int holds n + 1
	;; 3		temp int holds n + 1, should hold copy of "w >> 4"
	;;		also used to convert w from word to int
	;; 4		l (arg 4)
	;; 5
	;; 6
	;; 7
	stq	RA, 0($sp)	# push_ret			; save return address
	stl	$4, 16($sp)					; save l
code_2173:
funtop_1887:
	cmpult	$2, 16, $0
	bne	$0, one_case_1896
	;; Here, w < 16
	;; let M = FFFFFFFF
zero_case_1895:
	;; Correct sequence for SRL($2, $25)
	zap	$2, 240, $25					; $25 = w & M
	srl	$25, 4, $25					; $25 = (w & M) >> 4
	addl	$25, zero, $25					; $25 = sign_extend((w & M) >> 4)
	;; generated sequence
	zap	$2, 240, $25					; $25 = w & M
	stl	$25, 12($sp)					; slot[3] = w & M  (XXX, equiv to "stl $2, 12($sp)")
	ldl	$25, 12($sp)					; $25 = sign_extend(w & M)
	srl	$25, 4, $25					; $25 = sign_extend(w & M) >> 4 (logical shift)
	stl	$25, 12($sp)					; slot[3] = (sign_extend(w & M) >> 4) & M
	ldl	$25, 12($sp)					; $25 = sign_extend((sign_extend(w & M) >> 4) & M)
	addl	$25, zero, $25					; $25 = sign_extend(sign_extend((sign_extend(w & M) >> 4) & M))
	;; end SRL($2, $25)
	stl	$25, 12($sp)					; slot[3] = w >> 4
	and	$2, 15, $2					; w &= 15
	addlv	$3, 1, $25
	stl	$25, 8($sp)					; slot[2] = n + 1
	trapb							; check overflow
	# making closure call 
	lda	$1, mkDigit_1297
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	RA, ($3), 1					; done doing mkDigit(w & 15)
code_2188:
	ldgp	$gp, (RA)					; restore gp
	mov	$0, $1						; $1 = digit
code_2175:
	# done making normal call
	addl	ALLOCPTR_REG, 12, ASMTMP_REG			; allocate 3 words
	ldq	ALLOCLIMIT_REG, 112(THREADPTR_REG)
	cmpule	ASMTMP_REG, ALLOCLIMIT_REG, $25
	bne	$25, gc_check_2176
code_2177:
	jsr	RA, GCFromML
gc_check_2176:
	# allocating 2-record					; cons cell
	lda	$0, 529(zero)					
	stl	$0, (ALLOCPTR_REG)				; cons[-1] = 529
	stl	$1, 4(ALLOCPTR_REG)				; cons[0] = digit
	ldl	$25, 16($sp)
	stl	$25, 8(ALLOCPTR_REG)				; cons[1] = l
	addl	ALLOCPTR_REG, 4, $0				; $0 = cons
	addl	ALLOCPTR_REG, 12, ALLOCPTR_REG			; done allocating cons
	# done allocating 2 record
	# making direct call 
	ldl	$2, 12($sp)					; new w = w >> 4
	ldl	$3, 8($sp)					; new n = n + 1
	stl	$0, 16($sp)					; new l = cons
	br	zero, funtop_1887
code_2179:
	# done making self tail call
	lda	$0, (zero)
	br	zero, after_zeroone_1897
	;; Here, w >= 16
one_case_1896:
	addlv	$3, 1, $25					; tmp2 = n + 1
	stl	$25, 8($sp)					; slot 2 = n + 1
	trapb							; check overflow
	# making closure call 
	lda	$1, mkDigit_1297				; $0 = mkDigit w
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	RA, ($3), 1
code_2187:
	ldgp	$gp, (RA)					; restore gp
	mov	$0, $1						; $1 = mkDigit w
code_2181:
	# done making normal call
	addl	ALLOCPTR_REG, 24, ASMTMP_REG			; coalesced allocate check
	ldq	ALLOCLIMIT_REG, 112(THREADPTR_REG)
	cmpule	ASMTMP_REG, ALLOCLIMIT_REG, $25
	bne	$25, gc_check_2182
code_2183:
	jsr	RA, GCFromML
gc_check_2182:
	# allocating 2-record					; allocate cons cell (3 words)
	lda	$0, 529(zero)
	stl	$0, (ALLOCPTR_REG)				; cons[-1] = 529
	stl	$1, 4(ALLOCPTR_REG)				; cons[0] = mkDigit w
	ldl	$25, 16($sp)
	stl	$25, 8(ALLOCPTR_REG)				; cons[1] = l
	addl	ALLOCPTR_REG, 4, $1				; $1 = cons
	addl	ALLOCPTR_REG, 12, ALLOCPTR_REG			; done allocating cons cell
	# done allocating 2 record
	# allocating 2-record					; allocate pair cell (3 words)
	lda	$0, 529(zero)
	stl	$0, (ALLOCPTR_REG)				; pair[-1] = 529
	ldl	$25, 8($sp)
	stl	$25, 4(ALLOCPTR_REG)				; pair[0] = n + 1
	stl	$1, 8(ALLOCPTR_REG)				; pair[1] = cons
	addl	ALLOCPTR_REG, 4, $0				; RESULT_REG = pair
	addl	ALLOCPTR_REG, 12, ALLOCPTR_REG			; done allocating pair
	# done allocating 2 record
after_zeroone_1897:
code_2186:
	ldq	RA, 0($sp)	# pop_ret			; restore return address
	lda	$sp, 32($sp)					; restore stack pointer
	ret	zero, (RA), 1					; branch
	.end Short_f_code_1801

	.rdata
		# -------- label,sizes,reg
	.long gc_check_2176
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long gc_check_2182
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_2187
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
		# -------- label,sizes,reg
	.long code_2188
	.long 0x00001005
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000100
	.text
 	.align 3
	.ent Short_wordToHex_code_1806
 # arguments : [$1808,$0] [$1809,$1] [$1335,$2] 
 # results    : [$1886,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Short_wordToHex_code_1806:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_2195
	lda	$sp, 16($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -16($sp)
code_2195:
	stq	$26, 0($sp)	# push_ret
code_2190:
funtop_1873:
	lda	$3, ($31)
	lda	$4, ($31)
	# making closure call 
	lda	$1, f_1337
	ldl	$5, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$5, $27
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	jsr	$31, ($5), 1
code_2191:
	# done making tail call
code_2193:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Short_wordToHex_code_1806

	.rdata
	.text
 	.align 3
	.ent Short_main
 # arguments : 
 # results    : [$1872,$0] 
 # destroys   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
 # modifies   :  $f28 $f27 $f26 $f25 $f24 $f23 $f22 $f21 $f20 $f19 $f18 $f17 $f16 $f15 $f14 $f13 $f12 $f11 $f10 $f9 $f8 $f7 $f6 $f5 $f4 $f3 $f2 $f1 $f0 $24 $23 $22 $21 $20 $19 $18 $17 $16 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 $0
Short_main:
	.mask (1 << 26), -16
	.frame $sp, 16
	.prologue 1
	ldgp	$gp, ($27)
	lda	$sp, -16($sp)
	ldl	$at, 1072($12)
	cmpult	$sp, $at, $at
	beq	$at, code_2218
	lda	$sp, 16($sp)
	lda	$at, ($31)
	mov	$26, $25
	jsr	$26, NewStackletFromML
	lda	$sp, -16($sp)
code_2218:
	stq	$26, 0($sp)	# push_ret
code_2196:
funtop_1814:
	lda	$16, ($31)
	lda	$27, AssertMirrorPtrArray
	jsr	$26, save_regs_MLtoC
	jsr	$26, AssertMirrorPtrArray
code_2217:
	ldgp	$gp, ($26)
	jsr	$26, load_regs_MLtoC
	ldgp	$gp, ($26)
code_2197:
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, mkDigit_1297
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, f_1337
	# done allocating 1 closures
	# allocating 1 closures
	# allocating 3-record
	# done allocating 3 record
	lda	$0, wordToHex_1334
	# done allocating 1 closures
	# allocating 2-record
	# done allocating 2 record
	lda	$2, -1($31)
	# making closure call 
	lda	$1, wordToHex_1334
	ldl	$3, ($1)
	ldl	$0, 4($1)
	ldl	$1, 8($1)
	mov	$3, $27
	jsr	$26, ($3), 1
code_2216:
	ldgp	$gp, ($26)
	mov	$0, $5
code_2198:
	# done making normal call
	lda	$1, 111($31)
	lda	$0, BANGbindarg_INT
	stl	$1, -4($0)
	ldl	$0, 1064($12)
	ldl	$1, 1068($12)
	addl	$0, 12, $0
	cmple	$0, $1, $0
	bne	$0, afterMutateCheck_2203
code_2205:
	subl	$13, 12, $at
	jsr	$26, GCFromML
afterMutateCheck_2203:
	lda	$4, BANGbindarg_INT
	ldl	$3, 1076($12)
	ldl	$2, 1064($12)
	mov	$4, $1
	mov	$3, $0
	stl	$1, ($2)
	stl	$0, 4($2)
	addl	$4, $3, $0
	ldl	$0, ($0)
	stl	$0, 8($2)
	addl	$2, 12, $0
	stl	$0, 1064($12)
	addl	$4, $3, $0
	stl	$5, ($0)
	lda	$0, 256($31)
code_2215:
	ldq	$26, 0($sp)	# pop_ret
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end Short_main

	.rdata
		# -------- label,sizes,reg
	.long code_2216
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long afterMutateCheck_2203
	.long 0x00000805
	.long 0x00000020
	.long 0x00000000
		# stacktrace
	.long 0x00000000
		# -------- label,sizes,reg
	.long code_2217
	.long 0x00000805
	.long 0x00000000
	.long 0x00000000
		# stacktrace
	.long 0x00000000
	.text
Short_unit_CODE_END_VAL:
	.rdata
		# endgcinfo with filler for alignment
	.globl Short_unit_GCTABLE_END_VAL
Short_unit_GCTABLE_END_VAL:
	.long 0x00000000
	.sdata
	.align 3
	.sdata
	.align 3
	.globl Short_unit_GLOBALS_BEGIN_VAL
Short_unit_GLOBALS_BEGIN_VAL:
		# Global
	.long 0x0000006f
	.globl Format_STR_c_INT
Format_STR_c_INT:
	.long 0x00000100
	.long 0x00000100
	.long 0x00000082
string_1835:
		# string size = 16
	.ascii "0123456789abcdef"
	.align 2
		# static record tag
	.long 0x00000619
mkDigit_1297:
	.long Short_mkDigit_code_1786
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
f_1337:
	.long Short_f_code_1801
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000619
wordToHex_1334:
	.long Short_wordToHex_code_1806
	.long 0x00000100
	.long 0x00000100
		# Global
	.long 0x0000006f
	.globl bindTuple_INT
bindTuple_INT:
	.long 0x00000100
	.long 0x00000100
		# static record tag
	.long 0x00000311
record_1859:
	.long 0x00000100
	.long wordToHex_1334
		# Global
	.long 0x0000006f
	.globl Format_STR_r_INT
Format_STR_r_INT:
	.long record_1859
	.long record_1859
		# Global
	.long 0x00000037
	.globl BANGbindarg_INT
BANGbindarg_INT:
	.long 0x00000102
	.long 0x00000102
		# Module closure
	.long 0x00000619
	.globl Short_unit_closure
Short_unit_closure:
	.long Short_main
	.long 0x00000000
	.long 0x00000000
	.long 0x00000311
	.globl Short_unit
Short_unit:
	.long Short_unit_closure
	.long Short_unit_closure
	.globl Short_unit_GLOBALS_END_VAL
Short_unit_GLOBALS_END_VAL:
	.long 0x00000000
	.long 0 #filler

	.sdata
	.align 3
	.globl Short_unit_TRACE_GLOBALS_BEGIN_VAL
Short_unit_TRACE_GLOBALS_BEGIN_VAL:
	.long BANGbindarg_INT
	.globl Short_unit_TRACE_GLOBALS_END_VAL
Short_unit_TRACE_GLOBALS_END_VAL:
		# filler so label is defined
	.long 0x00000000
