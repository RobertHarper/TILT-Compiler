(*$import Prelude REAL Ieee StringCvt Math64 General Bool Int RealFormat NumScan *)
(* real64.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Real64 :> REAL where type real = real = 
  struct
(*    structure I = InlineT.DfltInt *)

    structure Math = Math64
    val real_logb  : real -> int = fn arg => Ccall(real_logb, arg)

    infix 4 == !=
    type real = real

    val plus : int * int -> int = op +
    val minus : int * int -> int = op -
    val negate : int -> int = ~
    val gt : int * int -> bool = op >
    val lt : int * int -> bool = op <

(*
    val ~ = InlineT.Real64.~
    val op +  = InlineT.Real64.+
    val op -  = InlineT.Real64.-
    val op *  = InlineT.Real64.*
    val op /  = InlineT.Real64./
*)
    val ~ : real -> real = ~
    val op + : real * real -> real = op +
    val op - : real * real -> real = op -
    val op * : real * real -> real = op *
    val op / : real * real -> real = op /
    fun *+(a,b,c) = a*b+c
    fun *-(a,b,c) = a*b-c

(*
    val op >  = InlineT.Real64.>
    val op <  = InlineT.Real64.<
    val op >= = InlineT.Real64.>=
    val op <= = InlineT.Real64.<=

    val op == = InlineT.Real64.==
    val op != = InlineT.Real64.!=
*)
    val op >  : real * real -> bool = op >
    val op <  : real * real -> bool = op <
    val op >= : real * real -> bool = op >=
    val op <= : real * real -> bool = op <=
    val op == : real * real -> bool = float_eq
    val op != : real * real -> bool = float_neq

    fun unordered(x,y) = Bool.not(x>y orelse x <= y)
    fun ?= (x, y) = (x == y) orelse unordered(x, y)

    fun real_scalb (x, k) = raise Fail "scalb and real_scalb not implemented: multiarg C fun..."

  (* The next three values are computed laboriously, partly to
   * avoid problems with inaccurate string->float conversions
   * in the compiler itself.
   *)
    val maxFinite = let
	  fun f(x,i) = if i=1023 then x else f(x*2.0, plus(i, 1))
	  val y = f(1.0,0)
	  fun g(z, y, 0) = z
	    | g(z, y, i) = g(z+y, y*0.5, minus(i, 1))
	  in
	    g(0.0,y,53)
	  end

    val minNormalPos = let
	  fun f(x) = let
		val y = x * 0.5
		in
		  if real_logb y = ~1023 then x else f y
		end
	  in
	    f 1.0
	  end

    val minPos = let
	  fun f(x) = let
		val y = x * 0.5
		in
		  if y == 0.0 then x else f y
                end
	  in
	    f minNormalPos
	  end


    val posInf = maxFinite * maxFinite
    val negInf = ~posInf

    fun isFinite x = negInf < x andalso x < posInf
    fun isNan x = Bool.not(x==x)
    fun isNormal x = (case real_logb x
	   of ~1023 => (x != 0.0)
	    | 1024 => false
	    | _ => true
	  (* end case *))

    val floor = fn x => if isNormal x then floor x
			else if x==0.0 then 0 
			     else if isNan x then raise General.Domain
				  else raise General.Overflow

    fun trunc n = if n < 0.0 then negate(floor(~n)) else floor n
    fun ceil n = negate(floor(~n))
    fun round x = floor(x+0.5)  (* bug: does not do round-to-nearest *)
    val abs : real -> real = abs_float
    val fromInt : int -> real = real

    (* bug: operates correctly but slowly *)
    fun fromLargeInt(x : Int32.int) =
       let val i = Int32.quot(x,2)
           val j = Int32.-(x,Int32.+(i,i))
           val i' = Int32.toInt i
	   val j' = Int32.toInt j
        in fromInt(i')*2.0+fromInt(j')
       end    

     (* bug: only one rounding mode implemented *)
    fun toInt IEEEReal.TO_NEGINF = floor
      | toInt _ = raise Fail "toInt supports only NEGINF rounding mode now"

      (* bug: doesn't support full range of large ints *)
    fun toLargeInt mode x = Int32.fromInt(toInt mode x)

    fun toLarge x = x
    fun fromLarge _ x = x       

    fun sign x = if (x < 0.0) then ~1 else if (x > 0.0) then 1 
                  else if isNan x then raise Domain else 0
    fun signBit x = (* Bug: negative zero not handled properly *)
	real_scalb(x, negate(real_logb x)) < 0.0

    fun sameSign (x, y) = signBit x = signBit y

    fun copySign(x,y) = (* may not work if x is Nan *)
           if sameSign(x,y) then x else ~x

    fun compare(x,y) = if x<y then General.LESS else if x>y then General.GREATER
                       else if x == y then General.EQUAL 
			    else raise IEEEReal.Unordered
    
    fun compareReal(x,y) = 
           if x<y then IEEEReal.LESS else if x>y then IEEEReal.GREATER
                       else if x == y then IEEEReal.EQUAL 
			    else IEEEReal.UNORDERED
    

(** This proably needs to be reorganized **)
    fun class x =  (* does not distinguish between quiet and signalling NaN *)
      if signBit x
       then if x>negInf then if x == 0.0 then IEEEReal.ZERO
	                     else if real_logb x = ~1023
			          then IEEEReal.SUBNORMAL
			          else IEEEReal.NORMAL
	                else if x==x then IEEEReal.INF
			             else IEEEReal.NAN IEEEReal.QUIET
       else if x<posInf then if x == 0.0 then IEEEReal.ZERO
	                     else if real_logb x = ~1023
			          then IEEEReal.SUBNORMAL
			          else IEEEReal.NORMAL
	                else if x==x then IEEEReal.INF
			             else IEEEReal.NAN IEEEReal.QUIET

    val radix = 2
    val precision = 52

    val two_to_the_54 = 18014398509481984.0

    val two_to_the_neg_1000 =
      let fun f(i,x) = if i=0 then x else f(minus(i,1), x*0.5)
       in f(1000, 1.0)
      end

    fun toManExp x = 
      case real_logb x
	of ~1023 => if x==0.0 then {man=x,exp=0}
		    else let val {man=m,exp=e} = toManExp(x*1048576.0)
		              in {man=m,exp=minus(e,20)}
			 end
         | 1024 => {man=x,exp=0}
         | i => {man=real_scalb(x,negate i),exp=i}

    fun fromManExp {man=m,exp=e:int} =
      if (m >= 0.5 andalso m <= 1.0  orelse m <= ~0.5 andalso m >= ~1.0)
	then if gt(e, 1020)
	  then if gt(e, 1050) then if m>0.0 then posInf else negInf
	       else let fun f(i,x) = if i=0 then x else f(minus(i,1),x+x)
		       in f(minus(e,1020),  real_scalb(m,1020))
		      end
	  else if lt(e, negate 1020)
	       then if lt(e, negate 1200) then 0.0
		 else let fun f(i,x) = if i=0 then x else f(minus(i,1), x*0.5)
		       in f(minus(1020,e), real_scalb(m,negate 1020))
		      end
	       else real_scalb(m,e)  (* This is the common case! *)
      else let val {man=m',exp=e'} = toManExp m
            in fromManExp{man=m', exp=plus(e',e)}
           end

  (* This is the IEEE double-precision maxint *)
    val maxint = 4503599627370496.0


    local
    (* realround mode x returns x rounded to the nearest integer using the
     * given rounding mode.
     * May be applied to inf's and nan's.
     *)
      fun realround mode x = let
            val saveMode = IEEEReal.getRoundingMode ()
            in
              IEEEReal.setRoundingMode mode;
              if x>=0.0 then x+maxint-maxint else x-maxint+maxint
                before IEEEReal.setRoundingMode saveMode
            end
    in
    val realFloor = realround IEEEReal.TO_NEGINF
    val realCeil = realround IEEEReal.TO_POSINF
    val realTrunc = realround IEEEReal.TO_ZERO
    end
(*
    fun realFloor _ = raise Fail "Real.realFloor unimplemented"
    fun realCeil _ = raise Fail "Real.realCeil unimplemented"
    fun realTrunc _ = raise Fail "Real.realTrunc unimplemented"
*)

  (* realround(x) returns x rounded to some nearby integer, almost always
   * the nearest integer.
   *  May be applied to inf's and nan's.
   *)
    fun realround x = if x>=0.0 then x+maxint-maxint else x-maxint+maxint

  (* whole and split could be implemented more efficiently if we had
   * control over the rounding mode; but for now we don't.
   *)
    fun whole x = if x>0.0 
		    then if x > 0.5
		      then x-0.5+maxint-maxint
		      else whole(x+1.0)-1.0
	          else if x<0.0
                    then if x < ~0.5
		      then x+0.5-maxint+maxint
		      else whole(x-1.0)+1.0
	          else x

    fun split x = let val w = whole x 
                      val f = x-w
		   in if abs(f)==1.0
		     then {whole=w+f,frac=0.0}
		     else {whole=w, frac=f} 
		  end

    fun realMod x = let
	  val f = x - whole x
	  in
	    if abs f == 1.0 then 0.0 else f
	  end
    nonfix rem
    fun rem(x,y) = y * #frac(split(x/y))

    fun checkFloat x = if x>negInf andalso x<posInf then x
                       else if isNan x then raise General.Div
			 else raise General.Overflow

(** NOTE logb and scalb are also defined in math64.sml; do we need both??? **)
    fun logb x = (case real_logb x
	   of ~1023 => (* denormalized number *)
		minus(real_logb(x * two_to_the_54), 54)
	    | i => i
	  (* end case *))

(*
  (* This function is IEEE double-precision specific;
     we do not apply it to inf's and nan's *)
    fun scalb (x, k) = if lessu(I.+(k,1022),2046)
	  then Assembly.A.scalb(x,k)
          else let val k1 = I.div(k, 2)
	    in
	      scalb(scalb(x, k1), I.-(k, k1))
	    end
*)
    fun scalb (x, k) = raise Fail "scalb and real_scalb not implemented: multiarg C fun..."
(*
if lt(plus(k,1022),2046)
	  then Assembly.A.scalb(x,k)
          else let val k1 = div(k, 2)
	    in
	      scalb(scalb(x, k1), minus(k, k1))
	    end
*)
  
    fun nextAfter _ = raise Fail "Real.nextAfter unimplemented"

    fun min(x,y) = if x<y orelse isNan y then x else y
    fun max(x,y) = if x>y orelse isNan y then x else y

    fun toDecimal _ = raise Fail "Real.toDecimal unimplemented"
    fun fromDecimal _ = raise Fail "Real.fromDecimal unimplemented"

    val fmt = RealFormat.fmtReal
    val toString = fmt (StringCvt.GEN NONE)
    val scan = NumScan.scanReal
    val fromString = StringCvt.scanString scan

  end (* Real64 *)

structure Real = Real64
structure LargeReal = Real64

(*
 * $Log$
# Revision 1.5  2000/09/12  18:54:32  swasey
# Changes for cutoff compilation
# 
 * Revision 1.4  1999/09/22 15:45:09  pscheng
 * *** empty log message ***
 *
 * Revision 1.3  1999/04/15 18:52:56  pscheng
 * *** empty log message ***
 *
# Revision 1.2  1998/04/06  21:17:40  pscheng
# update: Typeof_c, dependent arrow/record types
#
# Revision 1.1  1998/03/09  19:52:50  pscheng
# added basis
#
 * Revision 1.6  1997/07/08  19:06:26  jhr
 *   Fixed bug in REal64.isNormal on 0.0
 *
 * Revision 1.5  1997/07/07  17:27:57  jhr
 *   Added missing basis functions to REAL and stubs to Real64.
 *
 * Revision 1.4  1997/05/29  14:44:26  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.3  1997/02/11  15:15:46  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.2  1997/02/10  14:28:09  george
 *   isNormal for 0.0 returned false! When the unbiased exponent is ~1023
 *   and explict check for 0.0 must be performed.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:16  george
 *   Version 109.24
 *
 *)
