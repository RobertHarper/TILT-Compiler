(*$import Prelude PreInt StringCvt *)
(* integer-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)



signature INTEGER =
  sig

    eqtype int

    val toLarge   : int -> PreLargeInt.int
    val fromLarge : PreLargeInt.int -> int
	
    val toInt     : int -> PreInt.int
    val fromInt   : PreInt.int -> int

    val precision : PreInt.int option
    val minInt : int option
    val maxInt : int option

    val ~ : int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val + : int * int -> int
    val - : int * int -> int
	
    val compare : int * int -> order
    val >  : int * int -> bool
    val >= : int * int -> bool
    val <  : int * int -> bool
    val <= : int * int -> bool

    val abs : int -> int
    val min : int * int -> int
    val max : int * int -> int

    val sign     : int -> PreInt.int
    val sameSign : int * int -> bool

    val fmt  : StringCvt.radix -> int -> string
    val toString   : int -> string
    val fromString : string -> int option
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader

  end;



