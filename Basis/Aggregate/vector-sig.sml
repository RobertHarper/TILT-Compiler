(*$import Prelude *)
(* vector-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature VECTOR = 
  sig
    eqtype 'a vector

    val maxLen   : int

    val fromList : 'a list -> 'a vector
    val tabulate : int * (int -> 'a) -> 'a vector

    val length   : 'a vector -> int
    val sub      : 'a vector * int -> 'a 
    val extract  : ('a vector * int * int option) -> 'a vector
    val concat   : 'a vector list -> 'a vector

    val app    : ('a -> unit) -> 'a vector -> unit
    val map    : ('a -> 'b) -> 'a vector -> 'b vector
    val foldl  : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b
    val foldr  : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b

    val appi   :
	  ((int * 'a) -> unit) -> ('a vector * int * int option) -> unit
    val mapi   :
	  ((int * 'a) -> 'b) -> ('a vector * int * int option) -> 'b vector
    val foldli :
	  ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int * int option) -> 'b
    val foldri :
	  ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int * int option) -> 'b

  end

(*
 * $Log$
# Revision 1.1  98/03/09  19:50:19  pscheng
# added basis
# 
 * Revision 1.2  1997/05/29  14:44:30  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
