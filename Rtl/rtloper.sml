functor MakeOperations(structure Rtl : RTL) : OPERATIONS = 
  struct
    
    open Rtl
    val error = fn s => Util.error "rtloper.sml" s

    structure W = TilWord32
    type w32 = W.word
    val i2w = W.fromInt
    val w2i = W.toInt;
    val wzero = i2w 0;
    val wneg1 = i2w ~1;
    val wone = i2w 1;

    (* positive shift disp is left shift; 
     shifting left is same for unsigned/signed *)
    fun bitshift(v,disp) = if (disp >= 0) 
                               then W.lshift(v,disp)
                           else W.rshiftl(v,~disp)
    fun abitshift(v,disp) = if (disp >= 0) 
				then W.lshift(v,disp)
                           else W.rshiftl(v,~disp)

    fun str_to_real s = 
	case (Real.fromString s) of
	    NONE => error "Bug in call to str_to_real"
	  | (SOME r) => r

    datatype hard_exn = OKAY | OVERFLOW | DIVIDEZERO
    local
      val cur_hard_exn = ref OKAY;
    in
      fun get_exn() = !cur_hard_exn;
      fun okay() = (get_exn() = OKAY)
      fun reset_exn() = cur_hard_exn := OKAY
      fun raise_overflow() = if okay() then (cur_hard_exn := OVERFLOW) else ()
      fun raise_dividezero() = if okay() then (cur_hard_exn := DIVIDEZERO) else ()
    end;


     fun getsign a = if (wzero = a) then 0
		     else (if (wzero = bitshift(a,~31)) then 1 else ~1)


     fun plusop((a1,a2),(b1,b2),tflag)   = 
	let val temp = W.uplus(a2,b2);
            val (s1,s2,s3) = (getsign(a2),getsign(b2),getsign(temp))
	    val nooverflow = not (s1 = s2 andalso (s1 <> 0) andalso s1 <> s3)
	    val res = (wzero,temp)
        in if (nooverflow) then res
	   else (if tflag then (raise_overflow(); res) 
			else (error("COMPILER_MADE_OVERFLOW" ^ 
			(W.toDecimalString a2) ^ " + " ^ (W.toDecimalString b2))))
	end

     fun minusop((a1,a2),(b1,b2),tflag)   = 
	 let val temp = W.uminus(a2,b2);
            val (s1,s2,s3) = (getsign(a2),getsign(b2),getsign(temp))
	    val nooverflow = not (s1 <> s2 andalso (s1 <> 0) andalso s1 <> s3)
	    val res = (wzero,temp)
        in if (nooverflow) then res
	   else (if tflag then (raise_overflow(); res) 
			else (error("COMPILER_MADE_OVERFLOW" ^ 
			(W.toDecimalString a2) ^ " - " ^ (W.toDecimalString b2))))
	end


fun multop((a1,a2),(b1,b2),tflag)   = 
  let val temp = W.umult(a2,b2);
    val nooverflow = (a2 = wzero) orelse W.udiv(temp,a2) = b2
    val res = (wzero,temp)
  in if (nooverflow) then res
     else (if tflag then (raise_overflow(); res) 
	   else (error "COMPILER_MADE_OVERFLOW"))
  end

(* need to check for overflow if you divide minint by -1 *)

    fun divop((a1,a2),(b1,b2),tflag) = 
        if b2=wzero then 
	     if tflag
	     then (raise_dividezero(); (wzero,wzero))
	     else (error "COMPILER_MADE_DIVIDEZERO")
        else if (a2 = wneg1) andalso (b2 = wneg1) then
	       if tflag
	       then (raise_overflow(); (wzero,wzero))
	       else (error "COMPILER_MADE_OVERFLOW ON DIV")
	else (wzero,W.udiv(a2,b2))


     fun word_to_real w : real = 
	 let 
	     val wmod : int = 256 * 256;
	     val bottom : int = w2i(W.umod(w,i2w wmod));
	     val top : int = w2i(W.udiv(w,i2w wmod));
         in (real bottom) + (real wmod) * (real top)
         end 
     fun sword_to_real w : real = 
	 let 
	     val wmod : int = 256 * 256;
	     val bottom : int = w2i(W.umod(w,i2w wmod));
	     val top : int = w2i(W.udiv(w,i2w wmod));
	     val neg : real = if (top > 128 * 256) 
				  then (real wmod) * (real wmod)
			      else 0.0
         in (real bottom) + (real wmod) * (real top) - neg
         end

     fun long_to_real (w1,w2) = 
	 let val lmod = 4294967296.0  (* 256^4 *)
	     val r1 : real = word_to_real w1
	     val r2 : real = word_to_real w2
	 in
	     (r1 * lmod) + r2
	 end

     (* currying seems to cause a bug in the compiler to show *)
     fun cmpiu_to_fun EQ (a,b) = (a = b)
       | cmpiu_to_fun NE (a,b) = not (a = b)
       | cmpiu_to_fun LE (a,b) = W.ult(a,b)
       | cmpiu_to_fun LT (a,b) = W.ulte(a,b)
       | cmpiu_to_fun GE (a,b) = W.ugte(a,b)
       | cmpiu_to_fun GT (a,b) = W.ugt(a,b)
       | cmpiu_to_fun LBC (a,b) = (W.andb(a,wone) = wzero)
       | cmpiu_to_fun LBS (a,b) = (W.andb(a,wone) = wone)
	 
     fun cmpis_to_fun EQ (a,b) = (a = b)
       | cmpis_to_fun NE (a,b) = not (a = b)
       | cmpis_to_fun LE (a,b) = (sword_to_real a) <= (sword_to_real b)
       | cmpis_to_fun LT (a,b) = (sword_to_real a) <  (sword_to_real b)
       | cmpis_to_fun GE (a,b) = (sword_to_real a) >= (sword_to_real b)
       | cmpis_to_fun GT (a,b) = (sword_to_real a) >  (sword_to_real b)
       | cmpis_to_fun LBC (a,b) = (W.andb(a,wone) = wzero)
       | cmpis_to_fun LBS (a,b) = (W.andb(a,wone) = wone)
	 
     fun cmpf_to_fun EQ (a : real,b) = Real.==(a,b)
       | cmpf_to_fun NE (a,b) = not (Real.==(a,b))
       | cmpf_to_fun LE (a,b) = a <= b
       | cmpf_to_fun LT (a,b) = a < b
       | cmpf_to_fun GE (a,b) = a >= b
       | cmpf_to_fun GT (a,b) = a > b
       | cmpf_to_fun LBC (a,b) = error "floating point LBC makes no sense"
       | cmpf_to_fun LBS (a,b) = error "floating point LBS makes no sense"
	 
     fun bool_to_ireg_val(f) = (wzero,(i2w (if f then 1 else 0)));
	 
     (* not quite what the Alpha architecture specifies, since top 32-bits of
      ints aren't used in this interpreter....*)

     fun bool_to_freg_val(f) = if f then (wzero,wneg1) else (wzero,wzero)
	 
     fun longs_to_bool (a,b) = not ((a = wzero) andalso (b = wzero));
     fun notop (ah,al)   = (wzero,W.notb al)
     fun andop ((_,a),(_,b)) = (wzero,W.andb(a,b))
     fun orop  ((_,a),(_,b)) = (wzero,W.orb(a,b))
     fun xorop ((_,a),(_,b)) = (wzero,W.xorb(a,b))
	 
     fun low5(high,low) = w2i(W.umod(low,i2w 32));
	 
     fun sraop ((a,b),sh) = (wzero, abitshift (b,~(low5(sh))))
     fun srlop ((a,b),sh) = (wzero, bitshift  (b,~(low5(sh))))
     fun sllop ((a,b),sh) = (wzero, bitshift  (b,low5(sh)))
	 
	 
	 
     exception RTL_HALTED;
     
     
  end;
  
  
