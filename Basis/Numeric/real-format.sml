(* real-format.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Code for converting from real (IEEE 64-bit floating-point) to string.
 * This ought to be replaced with David Gay's conversion algorithm.
 *
 *)

structure RealFormat : sig

    val fmtReal : StringCvt.realfmt -> real -> string

  end = struct
    val float_eq = TiltPrim.float_eq
    val float_neq = TiltPrim.float_neq
    val floor = TiltPrim.float2int
    val floor = 
      fn r =>
      let val a = TiltPrim.float2int r
      in  if (TiltPrim.int2float a) <= r then a
	  else TiltPrim.iminus(a,1)
      end

    val int32touint32 = TiltPrim.int32touint32
    val unsafe_vsub = TiltPrim.unsafe_vsub8

    infix 4 == !=

    val plus : int * int -> int = op +
    val minus : int * int -> int = op -
    val negate : int -> int = ~
    val gt : int * int -> bool = op >
    val lt : int * int -> bool = op <
    val gte : int * int -> bool = op >=
    val lte : int * int -> bool = op <=

    val ~ : real -> real = ~
    val op + : real * real -> real = op +
    val op - : real * real -> real = op -
    val op * : real * real -> real = op *
    val op / : real * real -> real = op /
    val op >  : real * real -> bool = op >
    val op <  : real * real -> bool = op <
    val op >= : real * real -> bool = op >=
    val op <= : real * real -> bool = op <=
    val op == : real * real -> bool = float_eq
    val op != : real * real -> bool = float_neq


    val op^  = String.^
    val implode = String.implode
    val concat = String.concat
    val size = String.size


    fun inc i = plus(i, 1)
    fun dec i = minus(i, 1)
    fun min (i, j) = if lt(i, j) then i else j
    fun max (i, j) = if gt(i, j) then i else j

    val atoi = (NumFormat.fmtInt StringCvt.DEC)

    fun zeroLPad (s, wid) = StringCvt.padLeft #"0" wid s
    fun zeroRPad (s, wid) = StringCvt.padRight #"0" wid s

    fun mkDigit d = unsafe_vsub("0123456789", int32touint32 d)

  (* decompose a non-zero real into a list of at most maxPrec significant digits
   * (the first digit non-zero), and integer exponent. The return value
   *   (a::b::c..., exp)
   * is produced from real argument
   *   a.bc... * (10 ^^ exp)
   * If the list would consist of all 9's, the list consisting of 1 followed by
   * all 0's is returned instead.
   *)
    val maxPrec = 15
    fun decompose (f, e, precisionFn) = let
	  fun scaleUp (x, e) =
		if (x < 1.0) then scaleUp(10.0*x, dec e) else (x, e)
	  fun scaleDn (x, e) =
		if (x >= 10.0) then scaleDn(0.1*x, inc e) else (x, e)
	  fun mkdigits (f, 0, odd) = ([], if f < 5.0 then 0
					  else if f > 5.0 then 1
					  else odd)
	    | mkdigits (f, i, _) = let 
		val d = floor f
		val (digits, carry) = mkdigits (10.0 * (f - real d), dec i,
						PreInt.imod(d,2))
		val (digit, c) = (case (d, carry)
		       of (9, 1) => (0, 1)
			| _ => (plus(d, carry), 0)
		      (* end case *))
		in
		  (digit::digits, c)
		end
	  val (f, e) = if (f < 1.0)
		  then scaleUp (f, e)
		else if (f >= 10.0)
		  then scaleDn (f, e)
		  else (f, e)
	  val (digits, carry) = mkdigits(f, max(0, min(precisionFn e, maxPrec)),0)
	  in
	    case carry
	     of 0 => (digits, e)
	      | _ => (1::digits, inc e)
	    (* end case *)
          end

    fun realFFormat (r, prec) = let
	  fun pf e = plus(e, inc prec)
	  fun rtoa (digits, e) = let
		fun doFrac (_, 0, n, l) = PreString.revImplode(n, l)
		  | doFrac ([], p, n, l) = doFrac([], dec p, inc n, #"0"::l)
		  | doFrac (hd::tl, p, n, l) =
		      doFrac(tl, dec p, inc n, (mkDigit hd) :: l)
		fun doWhole ([], e, n, l) = if gte(e, 0)
			then doWhole ([], dec e, inc n, #"0" :: l)
		      else if prec = 0
			then PreString.revImplode(n, l)
			else doFrac ([], prec, inc n, #"." :: l)
		  | doWhole (arg as (hd::tl), e, n, l) = if gte(e, 0)
			then doWhole(tl, dec e, inc n, (mkDigit hd) :: l)
		      else if prec = 0
			then PreString.revImplode(n, l)
			else doFrac(arg, prec, inc n, #"." :: l)
		fun doZeros (_, 0, n, l) = PreString.revImplode(n, l)
		  | doZeros (1, p, n, l) = doFrac(digits, p, n, l)
		  | doZeros (e, p, n, l) = doZeros(dec e, dec p, inc n, #"0" :: l)
		in
		  if gte(e, 0)
		    then doWhole(digits, e, 0, [])
		  else if (prec = 0)
		    then "0"
		    else doZeros (negate e, prec, 2, [#".", #"0"])
		end
	  in
	    if lt(prec, 0) then raise Size else ();
	    if (r < 0.0)
	      then {sign = "~", mantissa = rtoa(decompose(~r, 0, pf))}
	    else if (r > 0.0)
	      then {sign="", mantissa = rtoa(decompose(r, 0, pf))}
	    else if (prec = 0)
	      then {sign="", mantissa = "0"}
	      else {sign="", mantissa = zeroRPad("0.", plus(prec, 2))}
	  end (* realFFormat *)

    fun realEFormat (r, prec) = let
	  fun pf _ = inc prec
	  fun rtoa (sign, (digits, e)) = let
		fun mkRes (m, e) = {sign = sign, mantissa = m, exp = e}
		fun doFrac (_, 0, l)  = implode(List.rev l)
		  | doFrac ([], n, l) = zeroRPad(implode(List.rev l), n)
		  | doFrac (hd::tl, n, l) = doFrac (tl, dec n, (mkDigit hd) :: l)
		in
		  if (prec = 0)
		    then mkRes(String.str(mkDigit(List.hd digits)), e)
		    else mkRes(
		      doFrac(List.tl digits, prec, [#".", mkDigit(List.hd digits)]), e)
		end
	  in
	    if lt(prec, 0) then raise Size else ();
	    if (r < 0.0)
	      then rtoa ("~", decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa ("", decompose(r, 0, pf))
	    else if (prec = 0)
	      then {sign = "", mantissa = "0", exp = 0}
	      else {sign = "", mantissa = zeroRPad("0.", plus(prec, 2)), exp = 0}
	  end (* realEFormat *)

    fun realGFormat (r, prec) = let
	  fun pf _ = prec
	  fun rtoa (sign, (digits, e)) = let
		fun mkRes (w, f, e) = {sign = sign, whole = w, frac = f, exp = e}
		fun doFrac [] = []
		  | doFrac (0::tl) = (case doFrac tl
		       of [] => []
			| rest => #"0" :: rest
		      (* end case *))
		  | doFrac (hd::tl) = (mkDigit hd) :: (doFrac tl)
		fun doWhole ([], e, wh) =
		      if gte(e, 0)
			then doWhole([], dec e, #"0"::wh)
			else mkRes(implode(List.rev wh), "", NONE)
		  | doWhole (arg as (hd::tl), e, wh) =
		      if gte(e, 0)
			then doWhole(tl, dec e, (mkDigit hd)::wh)
			else mkRes(implode(List.rev wh), implode(doFrac arg), NONE)
		in
		  if lt(e, ~4) orelse gte(e, prec)
		    then mkRes(
		      String.str(mkDigit(List.hd digits)),
		      implode(doFrac(List.tl digits)), SOME e)
		  else if gte(e, 0)
		    then doWhole(digits, e, [])
		    else let
		      val frac = implode(doFrac digits)
		      in
			mkRes("0", zeroLPad(frac, plus(size frac, minus(~1, e))), NONE)
		      end
		end
	  in
	    if lt(prec, 1) then raise Size else ();
	    if (r < 0.0)
	      then rtoa("~", decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa("", decompose(r, 0, pf))
	      else {sign="", whole="0", frac="", exp=NONE}
	  end (* realGFormat *)

   val infinity = let fun bigger x = let val y = x*x
				     in if y>x then bigger y else x
				     end
                   in bigger 100.0
                  end

   fun fmtInfNan x =
        if x==infinity then "inf"
        else if x == ~infinity then "~inf"
        else "nan"

  (* convert a real number to a string of the form [~]d.dddE[~]dd, where
   * the precision (number of fractional digits) is specified by the
   * second argument.
   *)
    fun realToSciStr prec r =
	if ~infinity < r andalso r < infinity
	then let
	  val {sign, mantissa, exp} = realEFormat (r, prec)
	  in
	    (* minimum size exponent string, no padding *)
	    concat[sign, mantissa, "E", atoi exp]
	  end
        else fmtInfNan r

  (* convert a real number to a string of the form [~]ddd.ddd, where
   * the precision (number of fractional digits) is specified by the
   * second argument.
   *)
    fun realToFixStr prec x =
	if ~infinity < x andalso x < infinity
	then let
	  val {sign, mantissa} = realFFormat (x, prec)
	  in
	    sign^mantissa
	  end
        else fmtInfNan x

      fun realToGenStr prec r = 
	if ~infinity < r andalso r < infinity
	then let
  	  val {sign, whole, frac, exp} = realGFormat(r, prec)
	  val frac = if (frac = "") then frac else "." ^ frac
	  val expStr = (case exp
 		 of NONE => ""
 		  | (SOME e) => "E" ^ atoi e
  		(* end case *))
  	  in
  	    concat[sign, whole, frac, expStr]
  	  end
        else fmtInfNan r

    fun fmtReal (StringCvt.SCI NONE) = realToSciStr 6
      | fmtReal (StringCvt.SCI(SOME prec)) = realToSciStr prec
      | fmtReal (StringCvt.FIX NONE) = realToFixStr 6
      | fmtReal (StringCvt.FIX(SOME prec)) = realToFixStr prec
      | fmtReal (StringCvt.GEN NONE) = realToGenStr 12
      | fmtReal (StringCvt.GEN(SOME prec)) = realToGenStr prec
      | fmtReal StringCvt.EXACT =
	raise TiltExn.LibFail "RealFormat: fmtReal: EXACT not supported"

  end

