(*$import Prelude *)
(* mono-vector-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Generic interface for monomorphic vector structures.
 *
 *)

signature MONO_VECTOR =
  sig

    type elem
    type vector

    val maxLen : int

  (* vector creation functions *)
    val fromList : elem list -> vector
    val tabulate : (int * (int -> elem)) -> vector

    val length   : vector -> int
    val sub      : (vector * int) -> elem
    val extract  : (vector * int * int option) -> vector
    val concat   : vector list -> vector

    val app    : (elem -> unit) -> vector -> unit
    val map    : (elem -> elem) -> vector -> vector
    val foldl  : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
    val foldr  : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a

    val appi
	  : ((int * elem) -> unit) -> (vector * int * int option) -> unit
    val mapi
	  : ((int * elem) -> elem) -> (vector * int * int option) -> vector
    val foldli
	  : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a
    val foldri
	  : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a

  end

(*
 * $Log$
# Revision 1.1  98/03/09  19:50:18  pscheng
# added basis
# 
 * Revision 1.2  1997/05/29  14:44:23  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
