(*$import Firstlude TiltPrim Prelude StringCvt *)
(* time-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TIME =
  sig

    eqtype time

    exception Time

    val zeroTime : time

    val fromReal : real -> time
    val toReal   : time -> real

    val toSeconds        : time -> int
    val toMilliseconds   : time -> int
    val toMicroseconds   : time -> int
    val fromSeconds      : int -> time
    val fromMilliseconds : int -> time
    val fromMicroseconds : int -> time

    val +  : time * time -> time
    val -  : time * time -> time

    val compare : time * time -> order

    val <  : time * time -> bool
    val <= : time * time -> bool
    val >  : time * time -> bool
    val >= : time * time -> bool

    val now : unit -> time

    val fmt : int -> time -> string
    val toString   : time -> string
    val fromString : string -> time option
    val scan : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader

  end (* TIME *)

