(*$import Prelude *)
(* sigs/ieee-real-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *)

signature IEEE_REAL =
  sig

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

    val setRoundingMode : rounding_mode -> unit
    val getRoundingMode : unit -> rounding_mode

    type decimal_approx = {
	kind : float_class,
	sign : bool,
	digits : int list,
	exp : int
      }

    val toString   : decimal_approx -> string
    val fromString : string -> decimal_approx option

  end;


(*
 * $Log$
# Revision 1.1  98/03/09  19:52:33  pscheng
# added basis
# 
 * Revision 1.2  1997/05/29  14:44:22  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
