(* format.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *
 * TODO
 *   - field widths in scan
 *   - add PREC of (int * fmt_item) constructor to allow dynamic control of
 *     precision.
 *   - precision in %d, %s, ...
 *   - * flag in scan (checks, but doesn't scan input)
 *   - %n specifier in scan
 *)

structure Format :> FORMAT =
  struct

    structure SS = Substring
    structure SC = StringCvt

    open FmtFields

    exception BadFmtList

    fun padLeft (str, pad) = SC.padLeft #" " pad str
    fun padRight (str, pad) = SC.padRight #" " pad str
    fun zeroLPad (str, pad) = SC.padLeft #"0" pad str
    fun zeroRPad (str, pad) = SC.padRight #"0" pad str

  (* int to string conversions (for positive integers only) *)
    local
      val (maxInt8, maxInt10, maxInt16) = (case LargeInt.maxInt
	     of (SOME n) => let
		  val maxP1 = LargeWord.fromLargeInt n + 0w1
		  in
		    ( LargeWord.fmt SC.OCT maxP1,
		      LargeWord.fmt SC.DEC maxP1,
		      LargeWord.fmt SC.HEX maxP1
		    )
		  end
	      | NONE => ("", "", "")
	    (* end case *))
    in
    datatype posint = PosInt of LargeInt.int | MaxInt
    fun intToOctal MaxInt = maxInt8
      | intToOctal (PosInt i) = LargeInt.fmt SC.OCT i
    fun intToStr MaxInt = maxInt10
      | intToStr (PosInt i) = LargeInt.toString i
    fun intToHex MaxInt = maxInt16
      | intToHex (PosInt i) = LargeInt.fmt SC.HEX i
    fun intToHeX i =
	  String.implode (
	    CharVector.foldr (fn (c, l) => Char.toUpper c :: l) [] (intToHex i))
    end (* local *)

    fun compileFormat str = let
	  val split = SS.splitl (fn #"%" => false | _ => true)
	  fun scan (ss, l) =
		if (SS.isEmpty ss)
		  then rev l
		  else let val (ss1, ss2) = split ss
		    in
		      case (SS.getc ss2)
		       of (SOME(#"%", ss')) => let val (field, ss3) = scanField ss'
			    in
			      scan(ss3, field::(Raw ss1)::l)
			    end
			| _ => rev((Raw ss1)::l)
		      (* end case *)
		    end
	  in
	    scan (Substring.all str, [])
	  end

    fun format s = let
	  val fmts = compileFormat s
	  fun doField (flags, wid, ty, arg) = let
		fun padFn s = (case (#ljust flags, wid)
		       of (_, NoPad) => s
			| (false, Wid i) => padLeft(s, i)
			| (true, Wid i) => padRight(s, i)
		      (* end case *))
		fun zeroPadFn (sign, s) = (case wid
		       of NoPad => raise BadFormat
			| (Wid i) => zeroLPad(s, i - (String.size sign))
		      (* end case *))
		fun negate i = ((PosInt(~i)) handle _ => MaxInt)
		fun doSign i = (case (i < 0, #sign flags, #neg_char flags)
		       of (false, AlwaysSign, _) => ("+", PosInt i)
			| (false, BlankSign, _) => (" ", PosInt i)
			| (false, _, _) => ("", PosInt i)
			| (true, _, TildeSign) => ("~", negate i)
			| (true, _, _) => ("-", negate i)
		      (* end case *))
		fun doRealSign sign = (case (sign, #sign flags, #neg_char flags)
		       of (false, AlwaysSign, _) => "+"
			| (false, BlankSign, _) => " "
			| (false, _, _) => ""
			| (true, _, TildeSign) => "~"
			| (true, _, _) => "-"
		      (* end case *))
		fun doExpSign (exp, isCap) = let
		      val e = if isCap then "E" else "e"
		      fun mkExp e = zeroLPad(Int.toString e, 2)
		      in
			case (exp < 0, #neg_char flags)
			 of (false, _) => [e, mkExp exp]
			  | (true, TildeSign) => [e, "~", mkExp(~exp)]
			  | (true, _) => [e, "-", mkExp(~exp)]
			(* end case *)
		      end
		fun octal i = let
		      val (sign, i) = doSign i
		      val sign = if (#base flags) then sign^"0" else sign
		      val s = intToOctal i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
		fun decimal i = let
		      val (sign, i) = doSign i
		      val s = intToStr i
		      in
			if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
		          else padFn (sign ^ s)
		      end
		fun hexidecimal i = let
		      val (sign, i) = doSign i
		      val sign = if (#base flags) then sign^"0x" else sign
		      val s = intToHex i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
	        fun capHexidecimal i = let
		      val (sign, i) = doSign i
		      val sign = if (#base flags) then sign^"0X" else sign
		      val s = intToHeX i
		      in
		        if (#zero_pad flags)
			  then sign ^ zeroPadFn(sign, s)
			  else padFn (sign ^ s)
		      end
		in
		  case (ty, arg)
		   of (OctalField, LINT i) => octal i
		    | (OctalField, INT i) => octal(Int.toLarge i)
		    | (IntField, LINT i) => decimal i
		    | (IntField, INT i) => decimal(Int.toLarge i)
		    | (HexField, LINT i) => hexidecimal i
		    | (HexField, INT i) => hexidecimal(Int.toLarge i)
		    | (CapHexField, LINT i) => capHexidecimal i
		    | (CapHexField, INT i) => capHexidecimal(Int.toLarge i)
		    | (CharField, CHR c) => padFn(String.str c)
		    | (BoolField, BOOL false) => padFn "false"
		    | (BoolField, BOOL true) => padFn "true"
		    | (StrField, ATOM s) => padFn(Atom.toString s)
		    | (StrField, STR s) => padFn s
		    | (RealField{prec, format=F_Format}, REAL r) => let
		        val {sign, mantissa} = RealFormat.realFFormat(r, prec)
		        val sign = doRealSign sign
		        in
		          if ((prec = 0) andalso (#base flags))
			    then padFn(concat[sign, mantissa, "."])
			    else padFn(sign ^ mantissa)
		        end
		    | (RealField{prec, format=E_Format isCap}, REAL r) => let
		        val {sign, mantissa, exp} = RealFormat.realEFormat(r, prec)
		        val sign = doRealSign sign
		        val expStr = doExpSign(exp, isCap)
		        in
		          if ((prec = 0) andalso (#base flags))
			    then padFn(concat(sign :: mantissa :: "." :: expStr))
			    else padFn(concat(sign :: mantissa :: expStr))
		        end
		    | (RealField{prec, format=G_Format isCap}, REAL r) => let
		        val prec = if (prec = 0) then 1 else prec
		        val {sign, whole, frac, exp} =
			      RealFormat.realGFormat(r, prec)
		        val sign = doRealSign sign
		        val expStr = (case exp
			       of SOME e => doExpSign(e, isCap)
			        | NONE => [])
		        val num = if (#base flags)
			        then let
			          val diff = prec - ((size whole) + (size frac))
			          in
				    if (diff > 0)
				      then zeroRPad(frac, (size frac)+diff)
				      else frac
			          end
			      else if (frac = "")
			        then ""
			        else ("." ^ frac)
		        in
		          padFn(concat(sign::whole::frac::expStr))
		        end
		    | (_, LEFT(w, arg)) => let
		        val flags = {
			        sign = (#sign flags), neg_char = (#neg_char flags),
			        zero_pad = (#zero_pad flags), base = (#base flags),
			        ljust = true, large = false
			      }
		        in
			  doField (flags, Wid w, ty, arg)
		        end
		    | (_, RIGHT(w, arg)) => doField (flags, Wid w, ty, arg)
		    | _ => raise BadFmtList
		  (* end case *)
		end
	  fun doArgs ([], [], l) = SS.concat(rev l)
	    | doArgs ((Raw s)::rf, args, l) = doArgs(rf, args, s::l)
	    | doArgs (Field(flags, wid, ty)::rf, arg::ra, l) =
		doArgs (rf, ra, SS.all (doField (flags, wid, ty, arg)) :: l)
	    | doArgs _ = raise BadFmtList
	  in
	    fn args => doArgs (fmts, args, [])
	  end (* format *)

    fun formatf fmt = let
	  val f = format fmt
	  in
	    fn consumer => fn args => consumer(f args)
	  end

  end (* Format *)
