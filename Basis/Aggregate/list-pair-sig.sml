(*$import Prelude *)
(* list-pair-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * If lists are of unequal length, the excess elements from the
 * tail of the longer one are ignored. No exception is raised.
 *
 *)

signature LIST_PAIR =
  sig

    val zip    : 'a list * 'b list -> ('a * 'b) list
    val unzip  : ('a * 'b) list -> 'a list * 'b list
    val map    : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val app    : ('a * 'b -> unit) -> 'a list * 'b list -> unit
    val foldl  : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldr  : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val all    : ('a * 'b -> bool) -> 'a list * 'b list -> bool
    val exists : ('a * 'b -> bool) -> 'a list * 'b list -> bool

  end (* signature LIST_PAIR *)

(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:20  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:13  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
