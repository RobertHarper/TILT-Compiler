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

