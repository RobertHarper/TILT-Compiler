(*$import Prelude StringCvt *)
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
    val fromSeconds      : int -> time
    val toMilliseconds   : time -> int
    val fromMilliseconds : int -> time
    val toMicroseconds   : time -> int
    val fromMicroseconds : int -> time

    val +  : (time * time) -> time
    val -  : (time * time) -> time

    val compare : (time * time) -> order

    val <  : (time * time) -> bool
    val <= : (time * time) -> bool
    val >  : (time * time) -> bool
    val >= : (time * time) -> bool

    val now : unit -> time

    val toString   : time -> string
    val fromString : string -> time option
    val fmt : int -> time -> string
    val scan : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader

  end (* TIME *)

(*
 * $Log$
# Revision 1.1  98/03/09  19:53:10  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
