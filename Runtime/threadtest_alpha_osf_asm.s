	.globl client_entry
	.globl client_entry2
	.data
$$finalmsg:
	.align 8
	.ascii "THE ANSWER IS %d.\n\000"
$$finalmsg2:
	.align 8
	.ascii "THE OTHER ANSWER IS %d.\n\000"
$$rawmsg2:
	.align 8
	.ascii "PRINT.\n\000"
$$rawmsg4:
	.align 8
	.ascii "OTHER_PRINT.\n\000"
$$rawmsg3:
	.align 8
	.ascii "-------------GCING_---------------.\n\000"
	.text
 #
 # pause
 #
 # arguments: $17
 # results  : $0 
 # return   : $18
	.ent pause
pause:
	beq	$17, pausedone
	subl	$17, 1, $17
	br	pause
pausedone:
	ret	$31, ($18), 1
	.end
 #
 # count
 #
 # arguments: $16
 # results  : $0 
 # return   : $14
	.ent count
count:
	.set noat
	cmpult	$25, $26, $at		# do a GC check
	bne	$at, normal
	lda     $1, 1($31)
	bsr	$26, gc_raw
	ldgp	$gp, 0($at)
/*
	stl	$1, NOTINML	     # set NOTINML
	stl	$31, NOTINML	     # clear NOTINML
*/
	.set at
normal:	
	stl	$31, -64($sp)
	lda	$sp, -64($sp)
	
	stl	$14, 16($sp)
	stl	$16, 0($sp)
	beq	$16, basecase

	lda	$17, 500000($31)       # pause in ML
	bsr	$18, pause
	
	lda     $1, 1($31)
	stl	$1, NOTINML	     # set NOTINML

	stl	$25, 32($sp)	     # save special regs
	stl	$26, 48($sp)
	
	lda	$17, 100000($31)       # pause NOT in ML
	bsr	$18, pause
	
	mov	$13,  $16
	jsr	$26, printf
	ldgp	$gp, 0($26)

	ldl	$14, 16($sp)         # restore regs
	ldl	$16, 0($sp)
	ldl	$25, 32($sp)
	ldl	$26, 48($sp)
	
	stl	$31, NOTINML         # clear NOTINML
	
	subl	$16, 1, $16
	bsr	$14, count
	stl	$31, 0($sp)
	ldl	$14, 16($sp)
	addl	$0, 1, $0
	lda	$sp, 64($sp)
	ret	$31, ($14), 1
basecase:
	lda     $1, 1($31)
	stl	$1, NOTINML	     # set NOTINML

	stl	$25, 32($sp)	     # save special regs
	stl	$26, 48($sp)
	
	mov	$13,  $16
	jsr	$26, printf
	ldgp	$gp, 0($26)

	ldl	$14, 16($sp)         # restore regs
	ldl	$25, 32($sp)
	ldl	$26, 48($sp)
	
	stl	$31, NOTINML         # clear NOTINML
	
	lda	$0, 100($31)
	lda	$sp, 64($sp)
	ret	$31, ($14), 1
client_entry:
	lda	$sp, -64($sp)
	stl	$8, 0($sp)

	lda	$16, 20($31)
	lda	$13, $$rawmsg2
	jsr	$14, count

	lda     $1, 1($31)
	stl	$1, NOTINML	     # set NOTINML
	
	lda	$16, $$finalmsg
	mov	$0, $17
	jsr	$26, printf
	ldgp	$gp, 0($26)

	stl	$31, NOTINML	     # set NOTINML
	
	ldl	$8, 0($sp)
	lda	$sp, 64($sp)

	lda     $1, 1($31)
	stl	$1, NOTINML	     # set NOTINML
	
	ret	$31, ($8), 1
client_entry2:
	lda	$sp, -64($sp)
	stl	$8, 0($sp)

	lda	$16, 25($31)
	lda	$13, $$rawmsg4
	jsr	$14, count

	lda     $1, 1($31)
	stl	$1, NOTINML	     # set NOTINML
	
	lda	$16, $$finalmsg2
	mov	$0, $17
	jsr	$26, printf
	ldgp	$gp, 0($26)

	stl	$31, NOTINML	     # set NOTINML
	
	ldl	$8, 0($sp)
	lda	$sp, 64($sp)

	lda     $1, 1($31)
	stl	$1, NOTINML	     # set NOTINML
	
	ret	$31, ($8), 1
	.end
	.data
L37419:
	.long ((0<<16) + 4)
	.long ((0<<16) + 0)

	
