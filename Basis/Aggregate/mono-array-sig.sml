(*$import Prelude MONO_VECTOR *)
(* mono-array-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Generic interface for monomorphic array structures.
 *
 *)

signature MONO_ARRAY =
  sig

    eqtype array
    type elem
    structure Vector : MONO_VECTOR
	(* sharing type Vector.elem = elem -- Holds for TILT but not part of this signature *)

    val maxLen : int

  (* array creation functions *)
    val array    : int * elem -> array
    val fromList : elem list -> array
    val tabulate : int * (int -> elem) -> array

    val length   : array -> int
    val sub      : array * int -> elem
    val update   : array * int * elem -> unit
    val extract  : array * int * int option -> Vector.vector

    val copy : {
	    src : array, si : int, len : int option,
	    dst : array, di : int
	  } -> unit
    val copyVec : {
	    src : Vector.vector, si : int, len : int option,
	    dst : array, di : int
	  } -> unit

    val appi    : (int * elem -> unit) -> array * int * int option -> unit
    val app     : (elem -> unit) -> array -> unit
	
    val foldli  : (int * elem * 'a -> 'a) -> 'a -> array * int * int option -> 'a
    val foldri  : (int * elem * 'a -> 'a) -> 'a -> array * int * int option -> 'a
	  
    val foldl   : (elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldr   : (elem * 'a -> 'a) -> 'a -> array -> 'a
	
    val modifyi : (int * elem -> elem) -> array * int * int option -> unit

    val modify  : (elem -> elem) -> array -> unit

  end

(*
 * $Log$
# Revision 1.3  2001/12/13  16:31:17  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/11/27  22:36:21  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:17  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
