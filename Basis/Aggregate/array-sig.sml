(*$import Prelude *)
(* array-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature ARRAY =
  sig
    type 'a array
    type 'a vector

    val maxLen   : int

    val array    : int * 'a -> 'a array
    val tabulate : int * (int -> 'a) -> 'a array
    val fromList : 'a list -> 'a array
    val array0   : 'a array

    val length   : 'a array -> int
    val sub      : 'a array * int -> 'a
    val update   : 'a array * int * 'a -> unit
    val extract  : ('a array * int * int option) -> 'a vector

    val copy : {
	    src : 'a array, si : int, len : int option,
	    dst : 'a array, di : int
	  } -> unit
    val copyVec : {
	    src : 'a vector, si : int, len : int option,
	    dst : 'a array, di : int
	  } -> unit

    val app    : ('a -> unit) -> 'a array -> unit
    val foldl  : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldr  : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val modify : ('a -> 'a) -> 'a array -> unit

    val appi    : ((int * 'a) -> unit) -> ('a array * int * int option) -> unit
    val foldli  : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b
    val foldri  : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b
    val modifyi : ((int * 'a) -> 'a) -> ('a array * int * int option) -> unit

  end

(*
 * $Log$
# Revision 1.1  98/03/09  19:50:07  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:12  george
 *   Version 109.24
 *
 *)
