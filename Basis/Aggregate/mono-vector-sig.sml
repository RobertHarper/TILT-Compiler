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

(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:21  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:18  pscheng
 * added basis
 *
 * Revision 1.2  1997/05/29  14:44:23  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
