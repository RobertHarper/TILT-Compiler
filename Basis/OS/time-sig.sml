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

(*
 * $Log$
# Revision 1.3  2001/12/13  16:31:25  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/11/27  22:36:38  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:53:10  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
