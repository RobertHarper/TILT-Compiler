	.text	
	.align	4
 	.globl	start_client
 	.globl	start_client_retadd_val
	.globl  global_exnrec
        .globl  GetRpcc
	.globl	raise_exception_raw
	.globl	Overflow_exncon
	.globl	Divide_exncon

 # ----------------------------------------------------------------------------	
 # one might call getrpcc twice and take the different between the two results
 # to obtain the cycles used; remember to subtract 5 from the result
 # return the rpcc in standard result register $0 (a 32-bit-quantity)
 # GetRpcc returns the contents of the cycle count for this process
 # this is some multiple(in range 1..16) of the number of cycles
 # ----------------------------------------------------------------------------	
        .ent GetRpcc
GetRpcc:
        rpcc    $0
        sll     $0,   32,  $1
        addq    $0,   $1,  $0
        srl     $0,   32,  $0
        ret     $31, ($26), 1
        .end GetRpcc

 # ------------------------here comes thread_spawn_raw-------------------------
 # return address comes in temp register
 # rpv seems to hold parameter
 # need to check XXXX
 # ----------------------------------------------------------------------------
	.ent	thread_spawn_raw
thread_spawn_raw:	
.set noat
	lda	$sp, -320($sp)	# allocate big frame
	stq	$at, 0($sp)	# save return address
	stq	$gp, 272($sp)	# save caller's gp
	stq	$27, 280($sp)	# save Rpv why?
.set at
	ldgp	$gp, 0($27)	# fix own gp

.set noat
				 # save int registers on the stack
	stq	$0, 16($sp)
	stq	$1, 24($sp)
	stq	$2, 32($sp)
	stq	$3, 40($sp)
	stq	$4, 48($sp)
	stq	$5, 56($sp)
	stq	$6, 64($sp)
	stq	$7, 72($sp)
	stq	$8, 80($sp)
	stq	$9, 88($sp)
	stq	$10, 96($sp)
	stq	$11, 104($sp)
	stq	$12, 112($sp)
	stq	$13, 120($sp)
	stq	$14, 128($sp)
	stq	$15, 136($sp)
	stq	$16, 144($sp)
	stq	$17, 152($sp)
	stq	$18, 160($sp)
	stq	$19, 168($sp)
	stq	$20, 176($sp)
	stq	$21, 184($sp)
	stq	$22, 192($sp)
	stq	$23, 200($sp)
	stq	$24, 208($sp)
	stq	$25, 216($sp)
	stq	$26, 224($sp)
	stq	$27, 232($sp)
	stq	$28, 240($sp)
	stq	$29, 248($sp)
	stq	$30, 256($sp)
.set at
	lda	$16, 16($sp)		# pass address of saved int register as 1st arg
	ldq	$17, 280($sp)		# pass saved Rpv as 2nd arg
	lda     $27, thread_spawn
	jsr	$26, thread_spawn	# jump to thread_spawn with Rpv set
	stq	$0, 288($sp)		# save result
	ldgp	$gp, 0($26)		# restore own gp
.set noat
					# restore int regs
	ldq	$0, 16($sp)
	ldq	$1, 24($sp)
	ldq	$2, 32($sp)
	ldq	$3, 40($sp)
	ldq	$4, 48($sp)
	ldq	$5, 56($sp)
	ldq	$6, 64($sp)
	ldq	$7, 72($sp)
	ldq	$8, 80($sp)
	ldq	$9, 88($sp)
	ldq	$10, 96($sp)
	ldq	$11, 104($sp)
	ldq	$12, 112($sp)
	ldq	$13, 120($sp)
	ldq	$14, 128($sp)
	ldq	$15, 136($sp)
	ldq	$16, 144($sp)
	ldq	$17, 152($sp)
	ldq	$18, 160($sp)
	ldq	$19, 168($sp)
	ldq	$20, 176($sp)
	ldq	$21, 184($sp)
	ldq	$22, 192($sp)
	ldq	$23, 200($sp)
	ldq	$24, 208($sp)
	ldq	$25, 216($sp)
	ldq	$26, 224($sp)
	ldq	$27, 232($sp)
	ldq	$28, 240($sp)
	ldq	$29, 248($sp)
	ldq	$30, 256($sp)
.set at
	ldq	$27, 288($sp)		# put result in Rpv
	lda	$sp, 320($sp)		# deallocate frame
	ret	$31, ($27), 1		# jump to result of call to thread_spawn
	.end	thread_spawn_raw

	
 # ------------------------ start_client  -------------------------------------
 # first C arg = new stack
 # second C arg = alloc ptr val
 # third C arg = alloc limit val
 # fourth C arg = client_entry (array of starting addresss)
 # fifth C arg = number of starting address in array client_entry
 # ----------------------------------------------------------------------------
	.ent	start_client 
 # gets 5 arguments:	new stack, alloc ptr val, alloc limit val, client_entry(start_adds), num_add
start_client:
 	ldgp	$gp, 0($27)	# get self gp
	lda	$sp, -320($sp)	# allocate frame
	stq	$26, 0($sp)	# save return address
	stq	$16, 8($sp)	# save all 5 args
	stq	$17, 16($sp)
	stq	$18, 24($sp)
	stq	$19, 32($sp)
	stq	$20, 40($sp)
	ldq	$11, 16($sp)    # initialize heap ptr   outside loop
	ldq	$10, 24($sp)	# initizlize heap limit outside loop
	stq	$31, 48($sp)	# initialize current thunk to run to 0
thunk_loop:
 # nuke regs for debugging
	lda	$0,  1200($31)
	lda	$1,  1201($31)
	lda	$2,  1202($31)
	lda	$3,  1203($31)
	lda	$4,  1204($31)
	lda	$5,  1205($31)
	lda	$6,  1206($31)
	lda	$7,  1207($31)
	lda	$8,  1208($31)
	ldq	$27, 32($sp)	# fetch start_client array address
	ldq	$19, 48($sp)	# fetch current thunk counter
	s4addq	$19, $27, $27	# compute array item address
	ldl	$27, ($27)	# fetch current thunk address
 # save self stack and switch to passed in stack, install global handler
	ldq	$16, 8($sp)	# fetch stack argument
 	stq	$sp, -8($16)	# save own stack pointer on new stack
 	lda	$sp, -8($16)	# set SP to new stack, below where old SP was saved
	lda	$9, global_exnrec
	stl	$31, NOTINML
	jsr	$26,  ($27)
start_client_retadd_val:	
	br	$26, dummy
dummy:	ldgp	$gp, 0($26)
 # returned from client
 	ldq	$sp, 0($sp)	 # switch back to own stack
	ldq	$16, 40($sp)
	ldq	$17, 48($sp)
	addq	$17, 1, $17
	stq	$17, 48($sp)
	cmplt	$17, $16, $18
	bne	$18, thunk_loop
 # we are done, let's save some regs for end diagnosis
	stq	$0, 64($sp)
	stq	$1, 72($sp)
	stq	$2, 80($sp)
	stq	$3, 88($sp)
	stq	$4, 96($sp)
	stq	$5, 104($sp)
	stq	$6, 112($sp)
	stq	$7, 120($sp)
	stq	$8, 128($sp)
	stq	$9, 136($sp)
	stq	$10, 144($sp)
	stq	$11, 152($sp)
	stq	$12, 160($sp)
	stq	$13, 168($sp)
	stq	$14, 176($sp)
	stq	$15, 184($sp)
	stq	$16, 192($sp)
	stq	$17, 200($sp)
	stq	$18, 208($sp)
	stq	$19, 216($sp)
	stq	$20, 224($sp)
	stq	$21, 232($sp)
	stq	$22, 240($sp)
	stq	$23, 248($sp)
	stq	$24, 256($sp)
	stq	$25, 264($sp)
	stq	$26, 272($sp)
	stq	$27, 280($sp)
 # call thread_finish
	lda	$16, 64($sp)
	jsr	thread_finish
	lda	$16, $$errormsg
	jsr	printf
	jsr	abort
	ldq	$26, 0($sp)
	lda	$sp, 320($sp)
	ret	$31, ($26), 1
	.end	start_client

 # ------------------------------------------------------------
 # global_exnhandler when all else fails
 # saves all registers and calls C function toplevel_exnhandler
 # ------------------------------------------------------------
	.ent	global_exnhandler
global_exnhandler:
	br	$gp, global_exn_handler_dummy
global_exn_handler_dummy:	
	ldgp	$gp, 0($gp)
	lda	$sp, -320($sp)
.set noat
	stq	$0, 16($sp)
	stq	$1, 24($sp)
	stq	$2, 32($sp)
	stq	$3, 40($sp)
	stq	$4, 48($sp)
	stq	$5, 56($sp)
	stq	$6, 64($sp)
	stq	$7, 72($sp)
	stq	$8, 80($sp)
	stq	$9, 88($sp)
	stq	$10, 96($sp)
	stq	$11, 104($sp)
	stq	$12, 112($sp)
	stq	$13, 120($sp)
	stq	$14, 128($sp)
	stq	$15, 136($sp)
	stq	$16, 144($sp)
	stq	$17, 152($sp)
	stq	$18, 160($sp)
	stq	$19, 168($sp)
	stq	$20, 176($sp)
	stq	$21, 184($sp)
	stq	$22, 192($sp)
	stq	$23, 200($sp)
	stq	$24, 208($sp)
	stq	$25, 216($sp)
	stq	$26, 224($sp)
	stq	$27, 232($sp)
	stq	$28, 240($sp)
	stq	$29, 248($sp)
	stq	$30, 256($sp)	
	stq	$31, 264($sp)
	lda	$16, 16($sp)
	lda	$27, toplevel_exnhandler
	bsr	toplevel_exnhandler
	jsr	abort
	.end	global_exnhandler

 # ------------------------------------------------------------
 # first C arg = where regs are
 # second C arg = exn argument
 # third C arg = exn code handler address
 # ------------------------------------------------------------
	.ent	raise_exception_raw
raise_exception_raw:
	lda	$sp, -64($sp)	# allocate frame
	stq	$16, 8($sp)	# save where regs are
	stq	$17, 16($sp)	# save the exn value
	stq	$18, 24($sp)	# save handler address
	br	$gp, restore_dummy
restore_dummy:	
	ldgp	$gp, 0($gp)	# get own gp
.set noat
				# restore address from argument
	ldq	$0, 0($16)
	ldq	$1, 8($16)
	ldq	$2, 16($16)
	ldq	$3, 24($16)
	ldq	$4, 32($16)
	ldq	$5, 40($16)
	ldq	$6, 48($16)
	ldq	$7, 56($16)
	ldq	$8, 64($16)
	ldq	$9, 72($16)	# exn ptr must be restored
	ldq	$10, 80($16)	# alloc limit must be restored
	ldq	$11, 88($16)	# alloc ptr must be restored
	ldq	$12, 96($16)
	ldq	$13, 104($16)
	ldq	$14, 112($16)
	ldq	$15, 120($16)
				# regs 16 to 21 are C arg regs and need not be restored
	ldq	$22, 176($16)	
	ldq	$23, 184($16)
	ldq	$24, 192($16)	
	ldq	$25, 200($16)	
				# skip exn argument
	ldq	$27, 216($16)
	ldq	$28, 224($16)
				# $29 is gp
				# $30 is sp
				# $31 is zero
	
	ldq	$26, 16($sp)	# restore exn arg
	ldq	$at, 24($sp)	# restore where to go
	ldq	$sp, 240($16)	# restore SP from saved regs at end
	mov	$at, $27
	jmp	$31, ($at), 1
.set at
	.end	raise_exception_raw

	.data
Overflow_exncon:
	.long	Prelude_31_Overflow_1
Divide_exncon:
	.long	Prelude_57_Div_1

	
	.data

 # a triple to represent the top-level exn record
	.align 4
	.long   (3 << 27) + (0 << 3) + 0
global_exnrec:
	.long	global_exnhandler
	.long   0
	.long	0

	.align 4
$$errormsg:
	.ascii "Thread_finish returned!!!\n\000"

	.align 4
$$global_exnmsg:
	.ascii "Runtime Error: Uncaught exception!!!\n\000"
	

