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
    val extract  : 'a vector * int * int option -> 'a vector
    val concat   : 'a vector list -> 'a vector

    val mapi   : (int * 'a -> 'b) -> 'a vector * int * int option -> 'b vector
    val map    : ('a -> 'b) -> 'a vector -> 'b vector
	
    val appi   : (int * 'a -> unit) -> 'a vector * int * int option -> unit
    val app    : ('a -> unit) -> 'a vector -> unit
	
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector * int * int option -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector * int * int option -> 'b

    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b

  end

(*
 * $Log$
# Revision 1.3  2001/12/13  16:31:17  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/11/27  22:36:22  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:19  pscheng
 * added basis
 *
 * Revision 1.2  1997/05/29  14:44:30  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
