structure TilWord64 : TILWORD =
 struct

  structure W = TilWord32
  val error = fn s => Util.error "word64.sml" s

  type word32 = W.word
  type word = word32 * word32           (* high bits * low bits *)
  val wordsize = 64
  val zero = (W.zero,W.zero)
  val one = (W.zero,W.one)
  val neg_one = (W.neg_one,W.neg_one)
  val low_mask = (W.zero,W.one)
  val high_mask = (W.one,W.zero)
  val most_neg = (W.most_neg, W.zero)
  val ten = (W.zero, W.fromInt 10)

  (* ------ EQUALITY OPERATIONS --------- *)
  fun equal((x1,x2):word,(y1,y2)) = (x1 = y1) andalso (x2 = y2)
  fun nequal(x:word,y) = not(equal(x,y))


  (* ----------- LOGICAL OPERATIONS  --------------- *)
  fun notb (h,l) = (W.notb h, W.notb l)
  fun andb ((h1, l1), (h2, l2)) = (W.andb (h1, h2), W.andb (l1, l2))
  fun orb ((h1, l1), (h2, l2)) = (W.orb (h1, h2), W.orb (l1, l2))
  fun xorb ((h1, l1), (h2, l2)) = (W.xorb (h1, h2), W.xorb (l1, l2))
  fun lshift ((h,l), shift : int) =
       if shift >= 32 then
	   lshift ((l, W.zero), shift - 32)
       else
	   let val lowlow = W.lshift (l, shift)
	       val lowhigh = W.rshiftl (l, 32 - shift)
	       val highhigh = W.lshift (h, shift)
	   in (W.orb (highhigh, lowhigh), lowlow)
	   end
  fun rshifta ((high, low):word, shift:int):word =
       if shift >= 32 then
	rshifta ((W.rshifta (high, 32), high), shift - 32)
       else
	let val highhigh = W.rshifta (high, shift)
	    val highlow = W.lshift (high, 32 - shift)
	    val lowlow = W.rshiftl (low, shift)
	in (highhigh, W.orb (highlow, lowlow))
	end
  fun rshiftl ((high, low):word, shift:int):word =
       if shift >= 32 then
	rshiftl ((0wx0, high), shift - 32)
       else
	let val highhigh = W.rshiftl (high, shift)
	    val highlow = W.lshift (high, 32 - shift)
	    val lowlow = W.rshiftl (low, shift)
	in (highhigh, W.orb (highlow, lowlow))
	end


  (* ------- UNSIGNED OPERATIONS ------------- *)
  fun uplus' ((h1, l1), (h2, l2)) =
        let val (over_l,l) = W.uplus'(l1,l2)
	    val (over_h,h) = W.uplus'(h1,h2)
	    val (over_h',h) = W.uplus'(h,over_l)
	    val over = W.uplus'(over_h,over_h')
        in (over, (h, l))
        end
  fun uplus(arg1,arg2) = #2(uplus'(arg1,arg2))
  fun unegate (n:word):word = uplus(one,notb n)  (* does not check for overflow *)
  fun uminus(n1,n2) = uplus(n1, unegate n2)
  fun umult'((h1, l1), (h2, l2)) : word * word = 
      let 
	  val (low_high,low_low) = W.umult'(l1,l2)
	  val (med1_high,med1_low) = W.umult'(l1,h2)
	  val (med2_high,med2_low) = W.umult'(h1,l2)
	  val high = W.umult'(h1,h2)
          (* ---------- now compute final results ------ *)
	  val (med_high,med_low) = uplus(uplus((W.zero,low_high),(W.zero,med1_low)),
					 (W.zero,med2_low))
	  val high' = uplus((W.zero,med_high),high)
	  val low' = (med_low,low_low)
      in (high',low')
      end
  fun umult(arg1,arg2) = #2(umult'(arg1,arg2))
  fun ult((h1, l1), (h2, l2)) = W.ult(h1,h2) orelse (W.equal(h1,h2) andalso (W.ult(l1,l2)))
  fun ulte((h1, l1), (h2, l2)) = W.ult(h1,h2) orelse (W.equal(h1,h2) andalso (W.ulte(l1,l2)))
  fun ugt(a,b) = not(ulte(a,b))
  fun ugte(a,b) = not(ult(a,b))
  fun udiv(a,b) = 
      let
	  val one32 : W.word = 0wx1
	  fun high_bit ((high, _):word) = W.equal(W.one,W.rshiftl (high, W.wordsize - 1))
	  fun find_shift (n1, n2) =
	      if (ult(n1,n2) orelse high_bit n2) 
		  then 0
	      else 1 + find_shift (n1, lshift (n2, 1))

	  (* div_loop invariant: rshiftl (n2, shifts) = divisor. *)
	  fun div_loop (n1, n2, 0) = if ult(n1,n2) then zero else one
	    | div_loop (n1, n2, shifts) =
	      if ult(n1,n2) then
		  div_loop (n1, rshiftl (n2, 1), shifts-1)
	      else
		  uplus(lshift (one, shifts),
			div_loop (uminus(n1,n2), rshiftl (n2, 1), shifts-1))
		  
      in
	  if b = zero then raise Div
	  else
	      let val shift = find_shift (a,b)
	      in div_loop (a, lshift (b, shift), shift)
	      end
      end
  fun umod(a,b) = uminus(a,umult(b,udiv(a,b)))

  (* ------- SIGNED OPERATIONS ------------- *)
  fun sign arg = if equal(arg,zero) then 0
		 else if equal(rshiftl(arg,wordsize-1),zero) then 1 else ~1
  fun slt(a as (ah,al),b as (bh,bl)) = 
      (case (sign a, sign b) of
	   (~1,~1) => ult(a,b)
	 | (~1,_) => true
	 | (0,1) => true
	 | (0,_) => false
	 | (1,1) => ult(a,b)
	 | (1,_) => false
	 | _ => error "sign returned a number other than -1, 0, or 1")
  fun slte(a,b) = slt(a,b) orelse equal(a,b)
  fun sgt(a,b) = not(slte(a,b))
  fun sgte(a,b) = not(slt(a,b))
  fun splus ((h1,l1),(h2,l2)) = 
      let 
	  val (low_high,low_low) = W.uplus'(l1,l2)
	  val (high_high,high_low) = W.uplus'(h1,h2)
	  val (high_high',high_low) = W.uplus'(low_high,high_low)
      in if (W.equal(high_high, W.zero) andalso
	     W.equal(high_high', W.zero))
	     then (high_low,low_low)
	 else raise Overflow
      end
  fun snegate arg = if (equal(arg,most_neg))
			 then raise Overflow
		     else splus(one,notb arg)
  fun absolute x = if (equal(x,most_neg)) 
		       then raise Overflow
		   else if (sign x >= 0) then x else snegate x
  fun sminus(a as (h1,l1) : word, b as (h2,l2) : word) = 
      let fun doit() = splus(a,unegate b)
      in  if (equal(b,neg_one))
	      then if (sign a = 1) then raise Overflow else doit()
	  else doit()
      end
  fun smult(a as (ah,al),b as (bh,bl)) = 
      let
	  val (sa,sb) = (sign a, sign b)
	  fun help(a,b) = (* both a and b are positive now *)
	      let val (high,low) = umult'(a,b)
	      in if (equal(high,zero) andalso (sign low = 1))
		     then low
		 else raise Overflow
	      end
      in case (sa,sb) of
	  ((0,_) | (_,0)) => zero
	| (1,1) => help(a,b)
	| (~1,1) => smult(b,a)
	| (1,~1) => if (equal(a,one)) then b else snegate(help(a,snegate b))
	| (~1,~1) => help(snegate a,snegate b)
	| _ => error "sign did not return -1, 0, or 1"
      end
  fun sdiv(a,b) = 
      (case (sign b = 0, equal(b,most_neg), equal(a,most_neg)) of
	   (true,_,_) => raise Div
	 | (_,true,_) => if (sign a <= 0) then zero else neg_one
	 | (_,_,true) => if (sign b > 0) then (splus(sdiv(splus(a,b),b),one))
			 else (splus(sdiv(sminus(a,b),b),one))
	 | (false,false,false) => 
	       let 
		   val (sa,sb) = (sign a, sign b)
		   val flip = (sa * sb = ~1)
		   val pa = if (sa = ~1) then snegate a else a
		   val pb = if (sb = ~1) then snegate b else b
		   val pq = udiv(pa,pb)
		   val divisible = equal(pa, umult(pq,pb))
		   val pq' = if (not divisible andalso flip) 
				 then splus(pq,one) else pq
	       in if flip then snegate pq' else pq
	       end)
  fun smod(a,b) = sminus(a,smult(b,sdiv(a,b)))
  fun squot _ = raise Util.UNIMP
  fun srem _ = raise Util.UNIMP

  (* ----- Conversion Operations ------ *)
  fun fromInt i = 
      let val low = W.fromInt i
	  val high = if (W.sign low = ~1) then W.neg_one else W.zero
      in (high,low)
      end
  fun toInt(high,low) = 
      (case (W.equal(high,W.zero),W.equal(high,W.neg_one),W.sign low) of
	   (true,_,(0 | 1)) => W.toInt low
	 | (_,true,~1) => W.toInt low
	 | _ => raise Overflow)

  fun fromHexString s = raise Util.UNIMP
  fun fromDecimalString str = 
      let 
	  val (sign,chars) = (case (String.explode str) of
				  (#"~" :: rest) => (neg_one,rest)
				| all => (one,all))
	  fun digit d = if (Char.isDigit d) 
			    then fromInt(ord d - ord #"0")
			else error "fromDecimalString called with non-decimal string"
	  fun loop acc [] = acc
	    | loop acc (a::b) = loop (splus(smult(ten,acc),digit a)) b
      in smult(sign,loop zero chars)
      end
  fun toHexString (high,low) = W.toHexString(high) ^ W.toHexString(low)
  fun toDecimalString w =
      let
	  val is_neg = sign w = ~1
	  val (is_least,w) = if (equal(w,most_neg)) 
				 then (true,absolute(splus(w,one))) else (false,absolute w)
	  fun loop w = if (equal(w,zero)) then []
		       else let val q = udiv(w,ten)
				val r = umod(w,ten)
				val digit = chr(ord #"0" + (toInt r))
			    in digit :: (loop q)
			    end
	  val res = if (equal(w,zero)) then [#"0"] else rev(loop w)
      in
	  implode (if is_neg then #"-"::res else res)
      end

(*

  fun toString (w1,w2) = 
      let 
	  val s1 = Word32.toString w1
	  val s2 = Word32.toString w1
	  fun extend str = if (Int.<(size s1,8))
			       then extend ("0" ^ str)
			   else str
      in (extend s1) ^ (extend s2)
      end
*)
 end (* struct *)
