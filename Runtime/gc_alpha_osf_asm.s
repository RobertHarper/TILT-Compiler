	.text	
	.align	4
	.globl	gc_raw
	.globl	float_alloc_raw
	.globl	int_alloc_raw
	.globl	ptr_alloc_raw
	.globl	context_restore
	.globl	old_alloc
	.globl	cur_alloc_ptr
	
#define R_at2 $25
#define R_heap_limit $10
#define R_heap_ptr $11	
		
 # ----------------- gc_raw -----------------------------
 # return address comes in normal return address register
 # request size come in at heap limit
 # ------------------------------------------------------
	.ent	gc_raw
	.frame $sp, 640, $26
	.prologue 0
gc_raw:
	lda	$sp, -640($sp)	# allocate big frame
	stq	$gp, 600($sp)	# save caller's gp
	br	$gp, dummy	
dummy:	
	ldgp	$gp, 0($gp)	# compute correct gp for self
.set noat
	stq	$26, 0($sp)		# save return address
	stq	R_heap_limit, 8($sp)	# save req_size = heaplimit

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
	addl	$30, 640, $at   # we need to save the sp of the caller of GC
	stq	$at, 256($sp)
	stq	$31, 264($sp)
				# save all fp but otherwise not used by GC
	stt	$f0, 320($sp)   
	stt	$f1, 328($sp)
	stt	$f2, 336($sp)
	stt	$f3, 344($sp)
	stt	$f4, 352($sp)
	stt	$f5, 360($sp)
	stt	$f6, 368($sp)
	stt	$f7, 376($sp)
	stt	$f8, 384($sp)
	stt	$f9, 392($sp)
	stt	$f10, 400($sp)
	stt	$f11, 408($sp)
	stt	$f12, 416($sp)
	stt	$f13, 424($sp)
	stt	$f14, 432($sp)
	stt	$f15, 440($sp)
	stt	$f16, 448($sp)
	stt	$f17, 456($sp)
	stt	$f18, 464($sp)
	stt	$f19, 472($sp)
	stt	$f20, 480($sp)
	stt	$f21, 488($sp)
	stt	$f22, 496($sp)
	stt	$f23, 504($sp)
	stt	$f24, 512($sp)
	stt	$f25, 520($sp)
	stt	$f26, 528($sp)
	stt	$f27, 536($sp)
	stt	$f28, 544($sp)
	stt	$f29, 552($sp)
	stt	$f30, 560($sp)
	stt	$f31, 568($sp)
.set at
	lda	$16, 16($sp)	# saved int regs
	lda	$17, 640($sp)   # sp of caller of gc_raw
	ldq	$18, 0($sp)     # ret_add
	ldq	$19, 8($sp)     # req_size
	lda	$20, ($31)      # not a majorGC
	jsr	$26, gc_handler
	ldgp	$gp, 0($26)
.set noat
	ldt	$f0, 320($sp)   # save all fp but otherwise not used by GC
	ldt	$f1, 328($sp)
	ldt	$f2, 336($sp)
	ldt	$f3, 344($sp)
	ldt	$f4, 352($sp)
	ldt	$f5, 360($sp)
	ldt	$f6, 368($sp)
	ldt	$f7, 376($sp)
	ldt	$f8, 384($sp)
	ldt	$f9, 392($sp)
	ldt	$f10, 400($sp)
	ldt	$f11, 408($sp)
	ldt	$f12, 416($sp)
	ldt	$f13, 424($sp)
	ldt	$f14, 432($sp)
	ldt	$f15, 440($sp)
	ldt	$f16, 448($sp)
	ldt	$f17, 456($sp)
	ldt	$f18, 464($sp)
	ldt	$f19, 472($sp)
	ldt	$f20, 480($sp)
	ldt	$f21, 488($sp)
	ldt	$f22, 496($sp)
	ldt	$f23, 504($sp)
	ldt	$f24, 512($sp)
	ldt	$f25, 520($sp)
	ldt	$f26, 528($sp)
	ldt	$f27, 536($sp)
	ldt	$f28, 544($sp)
	ldt	$f29, 552($sp)
	ldt	$f30, 560($sp)
	ldt	$f31, 568($sp)
		
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
				# we do not need to or want to restore $30
				# for it contains the sp of the caller
	ldq	$31, 264($sp)

	ldq	$26, 0($sp)	# fetch return address
	ldq	$gp, 600($sp)	# restore caller's gp
	lda	$sp, 640($sp)	# deallocate frame
	ret	$31, ($26), 1
.set at
	.end	gc_raw

 # --------------- float_alloc_raw ----------------------
 # return address comes in normal return address	register
 # array logical size come in at temp register
 # array initial value come in at ftemp register
 # optional profile tag comes in at alloc_limit register
 # ------------------------------------------------------
	.ent	float_alloc_raw 
	.frame $sp, 640, $26
	.prologue 0
float_alloc_raw:
	lda	$sp, -640($sp)	# allocate big frame
	stq	$gp, 600($sp)	# save caller gp
	br	$gp, float_alloc_raw_dummy
float_alloc_raw_dummy:	
	ldgp	$gp, 0($gp)	# compute correct gp for self
.set noat
	stq	$26, 0($sp)		# save return address
	stq	$at, 8($sp)		# alloc logical size = $at
	stq	R_heap_limit, 584($sp)	# optional profile tag = alloc_limit

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
	addl	$30, 640, $at   # we need to save the sp of the caller of GC
	stq	$at, 256($sp)
	stq	$31, 264($sp)
	
				# save all fp but otherwise not used by GC
	stt	$f0, 320($sp)   
	stt	$f1, 328($sp)
	stt	$f2, 336($sp)
	stt	$f3, 344($sp)
	stt	$f4, 352($sp)
	stt	$f5, 360($sp)
	stt	$f6, 368($sp)
	stt	$f7, 376($sp)
	stt	$f8, 384($sp)
	stt	$f9, 392($sp)
	stt	$f10, 400($sp)
	stt	$f11, 408($sp)
	stt	$f12, 416($sp)
	stt	$f13, 424($sp)
	stt	$f14, 432($sp)
	stt	$f15, 440($sp)
	stt	$f16, 448($sp)
	stt	$f17, 456($sp)
	stt	$f18, 464($sp)
	stt	$f19, 472($sp)
	stt	$f20, 480($sp)
	stt	$f21, 488($sp)
	stt	$f22, 496($sp)
	stt	$f23, 504($sp)
	stt	$f24, 512($sp)
	stt	$f25, 520($sp)
	stt	$f26, 528($sp)
	stt	$f27, 536($sp)
	stt	$f28, 544($sp)
	stt	$f29, 552($sp)
	stt	$f30, 560($sp)
	stt	$f31, 568($sp)
.set at
	lda	$16, 16($sp)	# saved regs
	lda	$17, 640($sp)   # sp of caller of float_alloc
	ldq	$18, 0($sp)     # ret_add
	ldq	$19, 8($sp)     # float_alloc logical size
	fmov    $f30, $f20      # initial value -- note 1 GP reserved
	ldq	$21, 584($sp)   # profile tag
	jsr	$26, float_alloc
	ldgp	$gp, 0($26)	# fix self gp
	stq	$0,  576($sp)	# save result
.set noat
				# restore all fp but otherwise not used by GC
	ldt	$f0, 320($sp)   
	ldt	$f1, 328($sp)
	ldt	$f2, 336($sp)
	ldt	$f3, 344($sp)
	ldt	$f4, 352($sp)
	ldt	$f5, 360($sp)
	ldt	$f6, 368($sp)
	ldt	$f7, 376($sp)
	ldt	$f8, 384($sp)
	ldt	$f9, 392($sp)
	ldt	$f10, 400($sp)
	ldt	$f11, 408($sp)
	ldt	$f12, 416($sp)
	ldt	$f13, 424($sp)
	ldt	$f14, 432($sp)
	ldt	$f15, 440($sp)
	ldt	$f16, 448($sp)
	ldt	$f17, 456($sp)
	ldt	$f18, 464($sp)
	ldt	$f19, 472($sp)
	ldt	$f20, 480($sp)
	ldt	$f21, 488($sp)
	ldt	$f22, 496($sp)
	ldt	$f23, 504($sp)
	ldt	$f24, 512($sp)
	ldt	$f25, 520($sp)
	ldt	$f26, 528($sp)
	ldt	$f27, 536($sp)
	ldt	$f28, 544($sp)
	ldt	$f29, 552($sp)
	ldt	$f30, 560($sp)
	ldt	$f31, 568($sp)
		
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
				# we do not need to or want to restore $30
				# for it contains the sp of the caller
	ldq	$31, 264($sp)
	ldq	$at,  576($sp)  # get result and return in temp
	ldq	$26, 0($sp)	# load return address
	ldq	$gp, 600($sp)
	lda	$sp, 640($sp)   # deallocate stack frame
	ret	$31, ($26), 1
.set at
	.end	float_alloc_raw

 # ----------------- old_alloc --------------------------
 # return address comes in $26
 # request size come in at heap limit
 # ------------------------------------------------------
	.ent	old_alloc
	.frame $sp, 640, $at
.set noat
old_alloc:
	stq	$gp, -8($sp)    # save caller gp
	br	$gp, old_alloc_dummy
old_alloc_dummy:
	ldgp	$gp, 0($gp)     # get self gp so we can access globals
				# return address passed in at $26

	stq	$0, -32($sp)    # we some scratch regs
	stq	$27, -24($sp)    # we some scratch regs
	stl	R_heap_limit, -16($sp)	# save req size
	
 	lda	$0, old_alloc_limit
 	ldl	$27, 0($0)      # at this point $27 is top/limit
	lda	$0, old_alloc_ptr
	ldl	$at, 0($0)			# at this point $at is ptr/start
	addl	R_heap_limit, $at, R_heap_limit	# heaplimit contains address to check against limit
	

	cmpule	$27, R_heap_limit, $at
	beq     $at, old_alloc_ok
old_alloc_bad:
        jsr	abort
.globl old_alloc_ok
old_alloc_ok:
	# we must make sure we won't be asked to old_alloc more than we can give
	# it suffices to make sure that there is less space in the nursery than
	# in the old alloc area
	subl	$27, R_heap_limit, $27   # $27 contains space left in old_alloc area
	lda	$0, fromheap
	ldl	$0, 0($0)		 # $0 is fromheap
	ldl	R_heap_limit, 8($0)      # at this point R_heap_limit is restored to heaplimit
	subl	R_heap_limit, $11, $at	 # $at has less space between limit and at
	cmple   $at, $27, $at
	bne	$at, no_limit_reset
	addl	$11, $27, R_heap_limit
no_limit_reset:
	ldl	$at, -16($sp)
	subl	R_heap_limit, $at, R_heap_limit
	subl	R_heap_limit, $at, R_heap_limit
	# return old_alloc_ptr at R_heap_ptr;  code retrieves normal from cur_alloc_ptr
	lda	$0, cur_alloc_ptr
	stl	R_heap_ptr, 0($0)
	lda	$0, old_alloc_ptr
	ldl	R_heap_ptr, 0($0)		# at this point $at is ptr/start
	ldq	$27, -24($sp)			# restore scractch regs
	ldq	$0, -32($sp)
	ldq	$gp, -8($sp)
	ret	$31, ($26), 1
.set at
	.end	old_alloc


 # ----------------- int_alloc_raw ----------------------
 # return address comes in normal return address register
 # array logical size come in at temp register
 # array initial value come in at temp2 register
 # optional profile tag comes in at alloc_limit register
 # ------------------------------------------------------
	.ent	int_alloc_raw
	.frame $sp, 640, $26
	.prologue 0
int_alloc_raw:
	lda	$sp, -640($sp)	# allocate big frame
	stq	$gp, 600($sp)	# save caller gp
	br	$gp, int_alloc_raw_dummy
int_alloc_raw_dummy:	
	ldgp	$gp, 0($gp)	# get correct gp for self
	
.set noat
	stq	$26, 0($sp)	# return address
	stq	$at, 8($sp)	# alloc logical size
	stq	R_at2, 584($sp)	# value to fill array from Rat2

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
	addl	$30, 640, $at   # we need to save the sp of the caller of GC
	stq	$at, 256($sp)
	stq	$31, 264($sp)

	stt	$f0, 320($sp)   # save all fp but otherwise not used by GC
	stt	$f1, 328($sp)
	stt	$f2, 336($sp)
	stt	$f3, 344($sp)
	stt	$f4, 352($sp)
	stt	$f5, 360($sp)
	stt	$f6, 368($sp)
	stt	$f7, 376($sp)
	stt	$f8, 384($sp)
	stt	$f9, 392($sp)
	stt	$f10, 400($sp)
	stt	$f11, 408($sp)
	stt	$f12, 416($sp)
	stt	$f13, 424($sp)
	stt	$f14, 432($sp)
	stt	$f15, 440($sp)
	stt	$f16, 448($sp)
	stt	$f17, 456($sp)
	stt	$f18, 464($sp)
	stt	$f19, 472($sp)
	stt	$f20, 480($sp)
	stt	$f21, 488($sp)
	stt	$f22, 496($sp)
	stt	$f23, 504($sp)
	stt	$f24, 512($sp)
	stt	$f25, 520($sp)
	stt	$f26, 528($sp)
	stt	$f27, 536($sp)
	stt	$f28, 544($sp)
	stt	$f29, 552($sp)
	stt	$f30, 560($sp)
	stt	$f31, 568($sp)
.set at
	lda	$16, 16($sp)	# saved regs
	lda	$17, 640($sp)   # sp of caller of gc_raw
	ldq	$18, 0($sp)     # ret_add
	ldq	$19, 8($sp)     # int_alloc logical size
	ldq     $20, 584($sp)   # initial int value
	mov	$31, $21        # no profile tag yet
	jsr	$26, int_alloc
	ldgp	$gp, 0($26)	# fix self gp
	stq	$0,  576($sp)	# save result
.set noat
				# restore all fp but otherwise not used by GC
	ldt	$f0, 320($sp)
	ldt	$f1, 328($sp)
	ldt	$f2, 336($sp)
	ldt	$f3, 344($sp)
	ldt	$f4, 352($sp)
	ldt	$f5, 360($sp)
	ldt	$f6, 368($sp)
	ldt	$f7, 376($sp)
	ldt	$f8, 384($sp)
	ldt	$f9, 392($sp)
	ldt	$f10, 400($sp)
	ldt	$f11, 408($sp)
	ldt	$f12, 416($sp)
	ldt	$f13, 424($sp)
	ldt	$f14, 432($sp)
	ldt	$f15, 440($sp)
	ldt	$f16, 448($sp)
	ldt	$f17, 456($sp)
	ldt	$f18, 464($sp)
	ldt	$f19, 472($sp)
	ldt	$f20, 480($sp)
	ldt	$f21, 488($sp)
	ldt	$f22, 496($sp)
	ldt	$f23, 504($sp)
	ldt	$f24, 512($sp)
	ldt	$f25, 520($sp)
	ldt	$f26, 528($sp)
	ldt	$f27, 536($sp)
	ldt	$f28, 544($sp)
	ldt	$f29, 552($sp)
	ldt	$f30, 560($sp)
	ldt	$f31, 568($sp)
		
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
				# we do not need to or want to restore $30
				# for it contains the sp of the caller
	ldq	$31, 264($sp)
	ldq	$at,  576($sp)  # get result and return in temp
	ldq	$26, 0($sp)	# load return address
	ldq	$gp, 600($sp)	# restore caller gp
	lda	$sp, 640($sp)   # deallocate stack frame
	ret	$31, ($26), 1
.set at
	.end	int_alloc_raw

 # ----------------- ptr_alloc_raw ----------------------
 # return address comes in normal return address register
 # array logical size come in at temp register
 # array initial value come in at temp2 register
 # optional profile tag comes in at alloc_limit register
 # ------------------------------------------------------
	.ent	ptr_alloc_raw 
	.frame $sp, 640, $26
	.prologue 0
.set noat
ptr_alloc_raw:
	lda	$sp, -640($sp)	# allocate big frame
	stq	$gp, 600($sp)	# save caller gp
	br	$gp, ptr_alloc_raw_dummy
ptr_alloc_raw_dummy:		
	ldgp	$gp, 0($gp)	# get correct gp for self
	
	stq	$26, 0($sp)	# return address
	stq	$at, 8($sp)	# alloc logical size
	stq	R_at2, 584($sp)	# value to fill array from Rat2

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
	addl	$30, 640, $at   # we need to save the sp of the caller of GC
	stq	$at, 256($sp)
	stq	$31, 264($sp)
				# save all fp but otherwise not used by GC
	stt	$f0, 320($sp)
	stt	$f1, 328($sp)
	stt	$f2, 336($sp)
	stt	$f3, 344($sp)
	stt	$f4, 352($sp)
	stt	$f5, 360($sp)
	stt	$f6, 368($sp)
	stt	$f7, 376($sp)
	stt	$f8, 384($sp)
	stt	$f9, 392($sp)
	stt	$f10, 400($sp)
	stt	$f11, 408($sp)
	stt	$f12, 416($sp)
	stt	$f13, 424($sp)
	stt	$f14, 432($sp)
	stt	$f15, 440($sp)
	stt	$f16, 448($sp)
	stt	$f17, 456($sp)
	stt	$f18, 464($sp)
	stt	$f19, 472($sp)
	stt	$f20, 480($sp)
	stt	$f21, 488($sp)
	stt	$f22, 496($sp)
	stt	$f23, 504($sp)
	stt	$f24, 512($sp)
	stt	$f25, 520($sp)
	stt	$f26, 528($sp)
	stt	$f27, 536($sp)
	stt	$f28, 544($sp)
	stt	$f29, 552($sp)
	stt	$f30, 560($sp)
	stt	$f31, 568($sp)
.set at
	lda	$16, 16($sp)	# saved regs
	lda	$17, 640($sp)   # sp of caller of gc_raw
	ldq	$18, 0($sp)     # ret_add
	ldq	$19, 8($sp)     # ptr_alloc logical size
	ldq     $20, 584($sp)   # initial int value
	mov	$31, $21        # no profile tag yet
	jsr	$26, ptr_alloc
	ldgp	$gp, 0($26)	# fix gp for self
	stq	$0,  576($sp)	# save result
.set noat
				# restore all fp but otherwise not used by GC
	ldt	$f0, 320($sp)
	ldt	$f1, 328($sp)
	ldt	$f2, 336($sp)
	ldt	$f3, 344($sp)
	ldt	$f4, 352($sp)
	ldt	$f5, 360($sp)
	ldt	$f6, 368($sp)
	ldt	$f7, 376($sp)
	ldt	$f8, 384($sp)
	ldt	$f9, 392($sp)
	ldt	$f10, 400($sp)
	ldt	$f11, 408($sp)
	ldt	$f12, 416($sp)
	ldt	$f13, 424($sp)
	ldt	$f14, 432($sp)
	ldt	$f15, 440($sp)
	ldt	$f16, 448($sp)
	ldt	$f17, 456($sp)
	ldt	$f18, 464($sp)
	ldt	$f19, 472($sp)
	ldt	$f20, 480($sp)
	ldt	$f21, 488($sp)
	ldt	$f22, 496($sp)
	ldt	$f23, 504($sp)
	ldt	$f24, 512($sp)
	ldt	$f25, 520($sp)
	ldt	$f26, 528($sp)
	ldt	$f27, 536($sp)
	ldt	$f28, 544($sp)
	ldt	$f29, 552($sp)
	ldt	$f30, 560($sp)
	ldt	$f31, 568($sp)
		
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
				# we do not need to or want to restore $30
				# for it contains the sp of the caller
	ldq	$31, 264($sp)
	ldq	$at,  576($sp)  # get result and return in temp
	ldq	$26, 0($sp)	# load return address
	ldq	$gp, 600($sp)
	lda	$sp, 640($sp)   # deallocate stack frame
	ret	$31, ($26), 1
.set at
	.end	ptr_alloc_raw

 # -----------------
 # need to check XXX
 # -----------------
	.ent	context_restore	
context_restore:	
	br	$gp, dummy_context_restore
dummy_context_restore:	
	ldgp	$gp, 0($gp)
	lda	$sp, -64($sp)
	stq	$16, 8($sp)
	stq	$17, 16($sp)
.set noat
	mov	$16, $at
	ldq	$0, 0($at)
	ldq	$1, 8($at)
	ldq	$2, 16($at)
	ldq	$3, 24($at)
	ldq	$4, 32($at)
	ldq	$5, 40($at)
	ldq	$6, 48($at)
	ldq	$7, 56($at)
	ldq	$8, 64($at)
	ldq	$9, 72($at)
	ldq	$10, 80($at)
	ldq	$11, 88($at)
	ldq	$12, 96($at)
	ldq	$13, 104($at)
	ldq	$14, 112($at)
	ldq	$15, 120($at)
	ldq	$16, 128($at)
	ldq	$17, 136($at)
	ldq	$18, 144($at)
	ldq	$19, 152($at)
	ldq	$20, 160($at)
	ldq	$21, 168($at)
	ldq	$22, 176($at)
	ldq	$23, 184($at)
	ldq	$24, 192($at)
	ldq	$25, 200($at)
	ldq	$26, 208($at)
	ldq	$27, 216($at)
				# we do not need to or want to restore $28 = $at
				# for we are using it
	ldq	$29, 232($at)
				# we do not need to or want to restore $30
				# for it contains the sp of the caller
	ldq	$31, 248($at)

	ldq	$at, 16($sp)	# get return address
	ldq	$sp, 8($sp)	# get first arg again
	ldq	$sp, 240($sp)	# get ml-caller sp

	ret	$31, ($at), 1
.set at
	.end	context_restore	


.data
cur_alloc_ptr:	
	.long	0
	.long	0