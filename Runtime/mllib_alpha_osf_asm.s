#include "general.h"
	.text	
	.align	4
	.globl	ml_input
	.globl  ml_input_gcentry
	.globl	ml_lookahead
	.globl  ml_lookahead_gcentry

#define R_heap_limit	$10
#define R_heap_ptr	$11

 # ---------------------------------------
 # first C arg = integer descriptor
 # second C arg = num of chars to read
 # -----------------------------------------
	.ent	ml_input
ml_input:
.set noat
	ldgp    $gp, ($27)	# fix own gp
	lda	$sp, -32($sp)	# allocate frame
	stq	$26, 0($sp)	# own return address
	lda	$at, 32($31)    # put stack frame size in stack/self-describing
	stq	$at, 8($sp)	#
	stq	$16, 16($sp)	# save 1st arg
	stq	$17, 24($sp)	# save 2ndt arg
	lda	$at, 36(R_heap_ptr)    # heap pointer + min string size -> $at
	addlv	$at, $17, $at   # $at + variable string size     -> $at
	cmpule	R_heap_limit, $at, $at
	beq	$at, ml_input_gcret
ml_input_dogc:
	lda	R_heap_limit, 36($17)    # req size into hlimit
	bsr	$26, gc_raw
ml_input_gcret:
	lda	$at, cur_alloc_pointer
	stl	R_heap_ptr, ($at)
	lda	$at, cur_alloc_limit
	stl	R_heap_limit, ($at)
	ldq	$16, 16($sp)	# restore 1st arg
	ldq	$17, 24($sp)	# restore 2nd arg
	lda     $27, mla_input	# call mla_input with 2 args in $16, $17 preserved from entry
	bsr     mla_input
	ldq	$26, 0($sp)
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
.set at
	.end	ml_input
.data
	# -------- hand-crafted GC entry for input(see stack.c)
        # -------- label,sizes,reg
ml_input_gcentry:
	.long ml_input_gcret
#ifdef GCTABLE_HASENTRYID
	.long 99998
#define ml_input_gcentrysize 6
#else
#define ml_input_gcentrysize 5
#endif
	.long (ml_input_gcentrysize + (8<<10) + (0<<19) + (0<<28))
	.long ((65535<<16) + 65535)	# should be safe to use all callee-save for now
	.long ((65535<<16) + 65535)	# should be safe to use all callee-save for now
		# stacktrace    no traceable in this frame
	.long 0
		# bytedata      no data
.text

 # ---------------------------------------
 # first C arg = integer descriptor
 # ---------------------------------------
	.ent	ml_lookahead
ml_lookahead:
 # set gp, allocate some stack space, save args
.set noat
	ldgp    $gp, ($27)	# get own GP
	lda	$sp, -32($sp)	# allocate frame
	stq	$26, 0($sp)	# own return address
	lda	$at, 32($31)    # put stack frame size
	stq	$at, 8($sp)	#     in stack
	stq	$16, 16($sp)	# save 1st arg
	lda	$at, 36(R_heap_ptr)    # room to store a 0 or 1 char string
	cmpule	R_heap_limit, $at, $at
	beq	$at, ml_lookahead_gcret
ml_lookahead_dogc:
	lda	R_heap_limit, 36($31)
	bsr	$26, gc_raw
ml_lookahead_gcret:
	lda	$at, cur_alloc_pointer
	stl	R_heap_ptr, ($at)
	lda	$at, cur_alloc_limit
	stl	R_heap_limit, ($at)
	lda     $27, mla_lookahead
	ldq	$16, 16($sp)	# restore 1st arg
	bsr     mla_lookahead	# call C routine with one argument in $16 preserved from entry
	ldq	$26, 0($sp)	# fetch return address
	lda	$sp, 32($sp)	# deallocate frame
	ret	$31, ($26), 1
.set at
	.end	ml_lookahead
.data
	# -------- hand-crafted GC entry for lookahead(see stack.c)
        # -------- label,sizes,reg
ml_lookahead_gcentry:
	.long ml_lookahead_gcret
#ifdef GCTABLE_HASENTRYID
	.long 99999
#define ml_lookahead_gcentrysize 6
#else
#define ml_lookahead_gcentrysize 5
#endif
	.long (ml_lookahead_gcentrysize + (8<<10) + (0<<19) + (0<<28))
	.long ((65535<<16) + 65535)	# should be safe to use all callee-save for now
	.long ((65535<<16) + 65535)	# should be safe to use all callee-save for now
		# stacktrace    no traceable in this frame
	.long 0
		# bytedata      no data
.text


