(*$import IEEE String Int *)
(* ieee-real.sml
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *)

structure IEEEReal : IEEE_REAL =
  struct

  (* this may cause portability problems to 64-bit systems *)
(*    structure Int = Int31 *)

    exception Unordered

    datatype real_order = LESS | EQUAL | GREATER | UNORDERED

    datatype nan_mode = QUIET | SIGNALLING

    datatype float_class
      = NAN of nan_mode
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL

    datatype rounding_mode
      = TO_NEAREST
      | TO_NEGINF
      | TO_POSINF
      | TO_ZERO

    fun intToRM 0 = TO_NEAREST
      | intToRM 1 = TO_ZERO
      | intToRM 2 = TO_POSINF
      | intToRM 3 = TO_NEGINF

(*
    val ctlRoundingMode : int option -> int =
	    CInterface.c_function "SMLNJ-Math" "ctlRoundingMode"
    fun setRoundingMode' m = (ctlRoundingMode (SOME m); ()) 
*)
    fun setRoundingMode' m = (Ccall(setRoundingMode,m); ())
    fun getRoundingMode'() = Ccall(getRoundingMode,0)

    fun setRoundingMode TO_NEAREST	= setRoundingMode' 0
      | setRoundingMode TO_ZERO		= setRoundingMode' 1
      | setRoundingMode TO_POSINF	= setRoundingMode' 2
      | setRoundingMode TO_NEGINF	= setRoundingMode' 3
    (* dummy arg *)
    fun getRoundingMode () = intToRM (getRoundingMode' ())

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


(*
 * $Log$
# Revision 1.3  99/09/22  15:45:08  pscheng
# *** empty log message ***
# 
# Revision 1.2  1998/04/06  21:17:39  pscheng
# update: Typeof_c, dependent arrow/record types
#
# Revision 1.1  1998/03/09  19:52:34  pscheng
# added basis
#
 * Revision 1.2  1997/05/29  14:44:22  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
