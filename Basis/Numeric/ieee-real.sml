(* ieee-real.sml
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *)

structure IEEEReal :> IEEE_REAL =
  struct

    exception Unordered

    datatype real_order = LESS | EQUAL | GREATER | UNORDERED

    datatype nan_mode = QUIET | SIGNALLING

    datatype float_class
      = NAN of nan_mode
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL

    datatype rounding_mode = datatype TiltFc.rounding_mode

    fun setRoundingMode (r:rounding_mode) : unit =
	let val (_,p) = TiltFc.getfc()
	    val () = TiltFc.setfc(r,p)
	in  ()
	end

    fun getRoundingMode () : rounding_mode =
	#1(TiltFc.getfc())

    type decimal_approx = {
	kind : float_class,
	sign : bool,
	digits : int list,
	exp : int
      }

    fun toString {kind, sign, digits, exp} = let
	  fun fmtExp 0 = []
	    | fmtExp i = ["E", Int.toString i]
	  fun fmtDigits ([], tail) = tail
	    | fmtDigits (d::r, tail) = (Int.toString d) :: fmtDigits(r, tail)
	  in
	    case (sign, kind, digits)
	     of (true, ZERO, _) => "~0.0"
	      | (false, ZERO, _) => "0.0"
	      | (true, NORMAL, []) => "~0.0"
	      | (true, SUBNORMAL, []) => "~0.0"
	      | (false, NORMAL, []) => "0.0"
	      | (false, SUBNORMAL, []) => "0.0"
	      | (true, NORMAL, _) =>
		  String.concat("~0." :: fmtDigits(digits, fmtExp exp))
	      | (true, SUBNORMAL, _) =>
		  String.concat("~0." :: fmtDigits(digits, fmtExp exp))
	      | (false, NORMAL, _) =>
		  String.concat("0." :: fmtDigits(digits, fmtExp exp))
	      | (false, SUBNORMAL, _) =>
		  String.concat("0." :: fmtDigits(digits, fmtExp exp))
	      | (true, INF, _) => "~inf"
	      | (false, INF, _) => "inf"
	      | (_, NAN _, []) => "nan"
	      | (_, NAN _, _) => String.concat("nan(" :: fmtDigits(digits, [")"]))
	    (* end case *)
	  end

(** TODO: implement fromString **)
    fun fromString s = NONE

  end;


