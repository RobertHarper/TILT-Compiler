structure Divmult
  :> DIVMULT where DA = Decalpha =
  struct
    open Decalpha
    open Machine
    open Core
    structure MU = Decalphautils
    structure DA = Decalpha

    val N = 32

    val error = fn s => Util.error "divmult.sml" s
    val i2w = TilWord32.fromInt
    val w2i = TilWord32.toInt
    val one64 = TilWord64.one
    val one32 = TilWord32.one
    val zero32 = TilWord32.zero
    val opt_on = ref true;
    val debug = ref false
    fun wshift(v,amount) =
	if (amount >= 0) then TilWord32.lshift(v,amount)
	    else TilWord32.rshiftl(v,~amount)

    fun iseven32(l) = TilWord32.andb(l,0w1) = zero32
    fun isodd32(l) = not(iseven32 l)
    fun split32(l) =
      if (iseven32 l)
	then case split32 (TilWord32.udiv(l,i2w 2)) of
	  (a,b) => (a+1,b)
      else (0,l)
    fun binpow64(l : int) = TilWord64.lshift(one64,l)
    fun binpow32(l : int) = wshift(one32,l)
    fun logceil(0w0) = error "logceil of zero!!!"
      | logceil(0w1) = 0
      | logceil(n) = 1 + logceil(TilWord32.uplus(wshift(n,~1),
					    if (iseven32 n)
						then zero32 else one32))
    fun logfloor(0w0) = error "logfloor of zero!!!"
      | logfloor(0w1) = 0
      | logfloor(n) = 1 + logfloor(TilWord32.udiv(n,0w2))
    fun isbinpow32(l) = l = binpow32(logceil l)

    fun abs32 arg =
      if ((TilWord32.andb(binpow32(31),arg)) = zero32)
	then arg
      else
	TilWord32.uplus(TilWord32.notb(arg),one32)

    fun load32 (immed, Rdest) =
          let
            val w65535 = i2w 65535
	    val low    = w2i (TilWord32.andb(immed, w65535))
            val high   = w2i (wshift(immed, ~16))
            val low'   = if (low > 32767) then low - 65536 else low
            val high'  = if (high > 32767) then high - 65536 else high
	  in
	    (LOADI (LDA, Rdest, low', Rzero)) ::
	    (if (low' >= 0) then
	       if (high' <> 0) then [LOADI(LDAH,Rdest,high',Rdest)] else nil
	     else
	       if (high' = ~1) then
		 nil
	       else
		 if (high' <> 32767) then
		   [LOADI (LDAH, Rdest, high' + 1, Rdest)]
		 else
		   [LOADI (LDAH, Rdest, ~32768, Rdest)])
	  end

    fun Choose_Multiplier(d32,prec) =
      let
	val l = logceil d32
	val d64 = TilWord64.fromUnsignedHalf d32
	val sh_post' = l
	val mlow' = TilWord64.udiv(binpow64(N+l),d64)
	val mhigh' = TilWord64.udiv(TilWord64.uplus(binpow64(N+l),binpow64(N+l-prec)),d64)
	fun loop (mlow,mhigh,sh_post) =
	  let
	    val mlow_half = TilWord64.rshiftl(mlow,1)
	    val mhigh_half = TilWord64.rshiftl(mhigh,1)
	  in
	    if (TilWord64.ult(mlow_half,mhigh_half) andalso (sh_post>0))
	      then loop(mlow_half,mhigh_half,sh_post-1)
	    else
	      (mhigh,sh_post,l)
	  end
      in
	loop(mlow',mhigh',sh_post')
      end



    val goodfactors =
      let
	fun helper 4 = map i2w [9,7,5,3,17,15]
	  | helper n = (TilWord32.uplus(binpow32 n,one32))::
	    (TilWord32.uminus(binpow32 n,one32))::(helper (n-1))
      in
	helper 31
      end

    fun instr2cycle (DA.INTOP(i,_,_,_)) =
      (case i of
	 DA.SUBL => 1
       | DA.ADDL => 1
       | DA.ADDLV => 1
       | DA.SUBLV => 1
       | DA.S4ADDL => 1
       | DA.S4SUBL => 1
       | DA.S8ADDL => 1
       | DA.S8SUBL => 1
       | DA.SUBQ => 1
       | DA.ADDQ => 1
       | DA.ADDQV => 1
       | DA.SUBQV => 1
       | DA.S4ADDQ => 1
       | DA.S4SUBQ => 1
       | DA.S8ADDQ => 1
       | DA.S8SUBQ => 1
       | DA.OR => 1
       | DA.SLL => 2
       | _ => 10000)
      | instr2cycle _ = 10000
    fun instrlist_cost arg = foldr ((op +) : int * int -> int) 0
	                           (map instr2cycle arg)

    fun quad_mult_convert' (reg as (F _),d : TilWord32.word,dest) =
	error "quad_mult_convert' called with float reg"
      | quad_mult_convert' (reg,0w0,dest)  = [INTOP(OR, Rzero, REGop Rzero, dest)]
      | quad_mult_convert' (reg,0w1,dest)  = [INTOP(OR, reg, REGop Rzero,dest)]
      | quad_mult_convert' (reg,0wxffffffff,dest) = [INTOP(SUBQ,Rzero, REGop reg, dest)]
      | quad_mult_convert' (reg,0w2,dest)  = [INTOP(ADDQ,reg, REGop reg, dest)]
      | quad_mult_convert' (reg,0w3,dest)  = [INTOP(S4SUBQ,reg, REGop reg, dest)]
      | quad_mult_convert' (reg,0w4,dest)  = [INTOP(S4ADDQ,reg, REGop Rzero, dest)]
      | quad_mult_convert' (reg,0w5,dest)  = [INTOP(S4ADDQ,reg, REGop reg, dest)]
      | quad_mult_convert' (reg,0w7,dest)  = [INTOP(S8SUBQ,reg, REGop reg, dest)]
      | quad_mult_convert' (reg,0w8,dest)  = [INTOP(S8ADDQ,reg, REGop Rzero, dest)]
      | quad_mult_convert' (reg,0w9,dest)  = [INTOP(S8ADDQ,reg, REGop reg, dest)]
      | quad_mult_convert' (reg,0w15,dest) = [INTOP(S4ADDQ,reg,REGop Rzero,dest),
					   INTOP(S4SUBQ,dest, REGop reg, dest)]
      | quad_mult_convert' (reg,0w17,dest) = [INTOP(S4ADDQ,reg,REGop Rzero,dest),
					   INTOP(S4ADDQ,dest, REGop reg, dest)]
      | quad_mult_convert' (reg,0w31,dest) = [INTOP(S4ADDQ,reg,REGop Rzero,dest),
					   INTOP(S8SUBQ,dest, REGop reg, dest)]
      | quad_mult_convert' (reg,0w33,dest) = [INTOP(S4ADDQ,reg,REGop Rzero,dest),
					   INTOP(S8ADDQ,dest, REGop reg, dest)]
      | quad_mult_convert' (reg,0w63,dest) = [INTOP(S8ADDQ,reg,REGop Rzero,dest),
					   INTOP(S8SUBQ,dest, REGop reg, dest)]
      | quad_mult_convert' (reg,0w65,dest) = [INTOP(S8ADDQ,reg,REGop Rzero,dest),
					   INTOP(S8ADDQ,dest, REGop reg, dest)]
      | quad_mult_convert' (reg,n,dest_reg) =
	if (isbinpow32(n))
	  then
	    [INTOP(SLL,reg, IMMop (logceil n), dest_reg)]
	else
	  let
	    val temp1 = freshIreg()
	  in
	    if (iseven32(n))
	      then
		let
		  val (exp,odd) = split32 n
		in
		  (quad_mult_convert'(reg,odd,temp1)) @
		  (quad_mult_convert'(temp1,binpow32 exp,dest_reg))
		end
	    else
	      if (isbinpow32(TilWord32.uplus(n,0w1)))
		then
		  [INTOP(SLL,reg,IMMop (logceil n),temp1),
		   INTOP(SUBQ,temp1,REGop reg,dest_reg)]
	      else
		if (isbinpow32(TilWord32.uminus(n,0w1)))
		  then [INTOP(SLL,reg,IMMop (logfloor n),temp1),
			INTOP(ADDQ,reg,REGop temp1,dest_reg)]
		else (* this is the general case of not easily factorable *)
		  (quadodd_mult_convert(reg,n,dest_reg))
	  end
      and (* mutual recursive funs *)
        quadodd_mult_convert (reg as (F _),d : TilWord32.word,dest_reg) =
	error "quadodd__mult_convert called with float reg"
      | quadodd_mult_convert (reg as (R _),n,dest_reg) =
	let
	  val temp1 = freshIreg();
	  fun factorseq f =
	    if (TilWord32.umod(n,f) = 0w0)
	      then
		SOME((quad_mult_convert' (reg,TilWord32.udiv(n,f),temp1)) @
		     (quad_mult_convert' (temp1,f,dest_reg)))
	    else NONE
	  fun bestseq [] = NONE
	    | bestseq (NONE::b) = bestseq b
	    | bestseq ((SOME a)::b) =
	      case (bestseq b) of
		NONE => SOME a
	      | (SOME bb) => SOME(if (instrlist_cost a <= instrlist_cost bb)
				    then a else bb)
	  fun ExtendedBinaryMethod() =
	    let
	      val temp = freshIreg();
	      val addseq = SOME(quad_mult_convert'(reg,TilWord32.uminus(n,0w1),dest_reg)
				@ [INTOP(ADDQ,dest_reg,REGop reg,dest_reg)])
	      val subseq = SOME(quad_mult_convert'(reg,TilWord32.uplus(n,0w1),dest_reg)
				@ [INTOP(SUBQ,dest_reg,REGop reg,dest_reg)])
	      val add3seq = SOME(quad_mult_convert'(reg,TilWord32.uminus(n,0w3),dest_reg)
				 @ quad_mult_convert'(reg,0w3,temp)
				 @ [INTOP(ADDQ,dest_reg,REGop temp,dest_reg)])
	      val sub3seq = SOME(quad_mult_convert'(reg,TilWord32.uplus(n,0w3),dest_reg)
				 @ quad_mult_convert'(reg,0w3,temp) @
				 [INTOP(SUBQ,dest_reg,REGop temp,dest_reg)])
	      val mult4sub1seq =
		if (TilWord32.umod(TilWord32.uplus(n,0w1),0w4) = 0w0)
		  then
		    SOME(quad_mult_convert'(reg,TilWord32.udiv(TilWord32.uplus(n,0w1),0w4),dest_reg)
			 @ [INTOP(S4SUBQ,dest_reg,REGop reg,dest_reg)])
		else NONE
	      val mult4add1seq =
		if (TilWord32.umod(TilWord32.uminus(n,one32),i2w 4) = zero32)
		  then
		    SOME(quad_mult_convert'(reg,TilWord32.udiv(TilWord32.uminus(n,one32),i2w 4),dest_reg)
			 @ [INTOP(S4ADDQ,dest_reg,REGop reg,dest_reg)])
		else NONE
	      val mult8sub1seq =
		if (TilWord32.umod(TilWord32.uplus(n,one32),0w8) = 0w0)
		  then
		    SOME(quad_mult_convert'(reg,TilWord32.udiv(TilWord32.uplus(n,0w1),0w8),dest_reg)
			 @ [INTOP(S8SUBQ,dest_reg,REGop reg,dest_reg)])
		else NONE
	      val mult8add1seq =
		if (TilWord32.umod(TilWord32.uminus(n,one32),0w8) = zero32)
		  then
		    SOME(quad_mult_convert'(reg,TilWord32.udiv(TilWord32.uminus(n,one32),i2w 8),
					   dest_reg)
			 @ [INTOP(S8ADDQ,dest_reg,REGop reg,dest_reg)])
		else NONE
	    in
	      case (bestseq [addseq,subseq,add3seq,sub3seq,
			  mult4add1seq,mult4sub1seq,
			  mult8add1seq,mult8sub1seq]) of
		NONE => error "no working sequences"
	      | SOME seq => seq
	    end
	  fun SimpleBinaryMethod() =
	    (quad_mult_convert'(reg,TilWord32.uminus(n,one32),dest_reg))
	     @ [INTOP(ADDQ,dest_reg,REGop reg,dest_reg)]
	in
	  case (bestseq (map factorseq goodfactors)) of
	    SOME seq => seq
	  | NONE => if (TilWord32.ulte(n,0wxffff))
		      then ExtendedBinaryMethod()
		    else SimpleBinaryMethod()
	end


    fun raw_unsigned_div_conv (reg as (F _),d,dest_reg) =
      error "unsignded_div_convert called with float reg"
      | raw_unsigned_div_conv (reg as (R _),0w0,dest_reg) =
	error "unsignded_div_convert called with zero divisor"
      | raw_unsigned_div_conv (reg as (R _),d,dest_reg) =
	let
	  val (m',sh_post',l) = Choose_Multiplier(d,N)
	  val (sh_pre,m,sh_post) =
	    if ((iseven32 d) andalso (TilWord64.ugte(m',binpow64(N))))
	      then
		let
		  fun loop (e,d) = if (TilWord32.umod(d,0w2) = 0w0)
				     then loop(e+1,TilWord32.udiv(d,0w2))
				   else
				     (e,d)
		  val (e,d_odd) = loop(0,d)
		  val (m,sh_post,_) = Choose_Multiplier(d_odd,N-e)
		in
		  (e,m,sh_post)
		end
	    else
	      (0,m',sh_post')
	  val instrs =
	    if (binpow32(logceil(d)) = d)
	      then [INTOP(SRL,reg,IMMop l,dest_reg)]
	    else
	      let
		val t1 = freshIreg()
		val t2 = freshIreg()
		val t3 = freshIreg()
	      in
		if (TilWord64.ugte(m,binpow64(N)))
		  then
(*		    (load32 (TilWord64.toUnsignedHalf m,t1)) @
		    [(INTOP(MULQ,reg,  REGop t1,                    t2))] *)
		    (quad_mult_convert'(reg,TilWord64.toUnsignedHalf m,t2)) @
		     [(INTOP(SUBL,reg,  REGop t2,                    t3)),
		     (INTOP(SRL, t3,   IMMop 1,                     t3)),
		     (INTOP(ADDL,t3,   REGop t2,                    t3)),
		     (INTOP(SRL, t3,   IMMop (sh_post-1),           dest_reg))]
		else
		  if (sh_pre = 0)
		     then
(*		       (load32 (TilWord64.toUnsignedHalf m,t1)) @
                        [(INTOP(MULQ,t1,   REGop reg,                 t3)), *)
			(quad_mult_convert'(reg,TilWord64.toUnsignedHalf m,t3)) @
			[(INTOP(SRL, t3,   IMMop sh_post,            dest_reg)),
			(INTOP(SRL, t3,   IMMop sh_post,            dest_reg))]
		   else
(*		  (load32 (TilWord64.toUnsignedHalf m,t1)) @ *)
		     [(INTOP(SRL, reg,  IMMop sh_pre,               t2))] @
		     (quad_mult_convert'(t2,TilWord64.toUnsignedHalf m,t2)) @
(*		     [(INTOP(MULQ,t1,   REGop t2,                   t3)), *)
                     [(INTOP(SRL, t3,   IMMop sh_post,              dest_reg))]
	      end
	  val instrs = map SPECIFIC instrs
	in
	  (app (fn i => print (msInstruction ("", i))) instrs);
	  instrs
	end

      fun raw_signed_div_conv (reg as (F _),d : TilWord32.word,dest_reg) =
	error "raw_signed_div_conv called with float reg"
	| raw_signed_div_conv (reg as (R _),0w0,dest_reg) =
	  error "raw_signed_div_conv called with 0 divisor"
	| raw_signed_div_conv (reg as (R _),d,dest_reg) =
	  let
	    val d_abs = abs32 d
	    val (m,sh_post,l) = Choose_Multiplier(d_abs,N-1)
(* val _ = print (if !debug then ("sh_post is " ^ (makestring sh_post) ^ "\n") else "") *)

	    val instrs =
	      if (d = 0w1)
		then [INTOP(OR,reg,REGop Rzero,dest_reg)]
	      else
		if (d = 0wxffffffff)
		  then
		    [INTOP(SUBLV,Rzero,REGop reg,dest_reg),
		     TRAPB]
		else
		  let
		    val lowhalfmask = TilWord64.uminus(binpow64(16),one64)
		    val lowm64  = TilWord64.andb(m,lowhalfmask)
		    val highm64 = TilWord64.andb(TilWord64.rshiftl(m,16),lowhalfmask)
		    val t1 = freshIreg()
		    val t2 = freshIreg()
		    val t3 = freshIreg()
		    val t4 = freshIreg()
		    val t5 = freshIreg()
		    val neginstr =
		      if (TilWord32.andb(d,binpow32(31)) = 0w0)
			then nil
		      else
			[INTOP(SUBL,Rzero,REGop dest_reg,dest_reg)]
		  in
		    if (d_abs = binpow32(l))
		      then [INTOP(SRA,reg,IMMop(l-1),t1),
			    INTOP(SRL,t1,IMMop(N-l),t2),
			    INTOP(ADDL,reg,REGop t2,t3),
			    INTOP(SRA,t3,IMMop l,dest_reg)] @ neginstr
		    else
		      if (TilWord64.ult(m,binpow64(N-1)))
			then
(*			  [LOADI(LDA, t1,(TilWord64.word64ToInt lowm64), Rzero),
			   LOADI(LDAH,t1,(TilWord64.word64ToInt highm64),t1),
			   INTOP(MULQ,t1,REGop reg,t2)] *)
			  (quad_mult_convert'(reg,TilWord64.toUnsignedHalf m,t2)) @

			  [INTOP(SRA,t2,IMMop (32+sh_post),t3),
			   INTOP(CMPLT,reg,IMMop 0, t4),
			   INTOP(ADDL,t3,REGop t4,dest_reg)] @ neginstr
		      else
(*			[LOADI(LDA, t1,(TilWord64.word64ToInt lowm64), Rzero),
			 LOADI(LDAH,t1,(TilWord64.word64ToInt highm64),t1),
			 INTOP(MULQ,t1,REGop reg,t2), *)
                         (quad_mult_convert'(reg,TilWord64.toUnsignedHalf m,t2)) @
		         [INTOP(SRA,t2,IMMop 32,t2),
			 INTOP(ADDL,t2,REGop reg,t3),
			 INTOP(SRA,t3,IMMop sh_post,t4),
			 INTOP(CMPLT,reg,IMMop 0, t5),
			 INTOP(ADDL,t4,REGop t5,dest_reg)] @ neginstr
		end
             val instrs = map SPECIFIC instrs
	  in
	    instrs
	  end

    fun quad_mult_convert arg = map SPECIFIC (quad_mult_convert' arg)
    fun signed_div_convert arg =
      if (!opt_on)
	then (raw_signed_div_conv arg)
      else
	nil

    fun unsigned_div_convert arg =
      if (!opt_on)
	then (raw_unsigned_div_conv arg)
      else
	nil


  end;





