(*$import Firstlude TiltPrim Prelude *)
(* mono-vector-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Generic interface for monomorphic vector structures.
 *
 *)

signature MONO_VECTOR =
  sig

    type vector
    type elem

    val maxLen : int

  (* vector creation functions *)
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector

    val length   : vector -> int
    val sub      : vector * int -> elem
    val extract  : vector * int * int option -> vector
    val concat   : vector list -> vector

    val mapi   : (int * elem -> elem) -> vector * int * int option -> vector
    val map    : (elem -> elem) -> vector -> vector
	
    val appi   : (int * elem -> unit) -> vector * int * int option -> unit
    val app    : (elem -> unit) -> vector -> unit
	
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a

    val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a

  end

