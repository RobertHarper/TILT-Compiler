	.verstamp	3 11
	.extern	_iob 0
	.data
	.align	8
$$storage:
	.long	0
	.long	0
	.long 	0
	.long 	0
  # tag for an int array:	 4 + 8 * wordlen  strlen = 20
	.long	44
$$rawmsg:
	.ascii "Inside mlclient now.\000"
	.align 	4
  # tag for a record of an int and a bytearray:	 0 + wordlen << 27 + mask << 3
	.long	268435968
$$msg:	
	.long	20
	.long	$$rawmsg
$$gcdone:
	.ascii   "mlclient: %d RETURNED FROM GC.\000"
	.text
	.align	4
 	.globl	ml_client
	.ent	ml_client 2
 # gets 3 arguments:	fake stack, alloc ptr val, alloc limit val
ml_client:
	ldgp	$gp, 0($27)
 # allocate some space for self and save some stuff
	lda	$sp, -32($sp)
	stq	$26, 0($sp)
	stq	$16, 8($sp)
	stq	$17, 16($sp)
	stq	$18, 24($sp)
 # print a msg using printf
 #	lda	$16, $$rawmsg
 #	jsr	$26, printf
 # print using our output
 	lda	$16, 1($31)
 	lda	$17, $$msg
 	jsr	$26, ml_output
 # restore out own args
	ldq	$26, 0($sp)
	ldq	$16, 8($sp)
	ldq	$17, 16($sp)
	ldq	$18, 24($sp)
 # mov src dest
 	mov	$17, $22
 	mov	$18, $23
 # save self stack and switch to passed in stack
	stq	$sp, $$storage
	lda	$sp, 0($16)
 # pass regmask and allocsize to gc_raw routine
	lda	$16, 555($31)
	lda	$17, 888($31)
	lda	$19, 42($31)
	jsr	$26, gc_raw
 # returned from gc
	lda	$16, $$gcdone
	mov	$19, $17
 	jsr	$26, printf
 # switch back to own stack
	ldq	$sp, $$storage
	ldq	$26, 0($sp)
	lda	$sp, 32($sp)
	ret	$31, ($26), 1
	.end	ml_client
