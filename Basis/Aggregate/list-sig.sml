(*$import Prelude *)
(* list-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature LIST =
  sig

    datatype 'a list = nil | :: of 'a * 'a list

    exception Empty

    val null : 'a list -> bool 
    val length : 'a list -> int 
    val @    : 'a list * 'a list -> 'a list
    val hd   : 'a list -> 'a                (* raises Empty *)
    val tl   : 'a list -> 'a list           (* raises Empty *)
    val last : 'a list -> 'a                (* raises Empty *)

    val getItem : 'a list -> ('a * 'a list) option

    val nth  : 'a list * int -> 'a       (* raises Subscript *)
    val take : 'a list * int -> 'a list  (* raises Subscript *)
    val drop : 'a list * int -> 'a list  (* raises Subscript *)

    val rev : 'a list -> 'a list 

    val concat    : 'a list list -> 'a list
    val revAppend : 'a list * 'a list -> 'a list

    val app        : ('a -> unit) -> 'a list -> unit
    val map        : ('a -> 'b) -> 'a list -> 'b list
    val mapPartial : ('a -> 'b option) -> 'a list -> 'b list

    val find      : ('a -> bool) -> 'a list -> 'a option
    val filter    : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool) -> 'a list -> ('a list * 'a list)

    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

    val exists : ('a -> bool) -> 'a list -> bool
    val all    : ('a -> bool) -> 'a list -> bool

    val tabulate : int * (int -> 'a) -> 'a list   (* raises Size *)

  end (* signature LIST *)

(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:21  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:50:15  pscheng
 * added basis
 *
 * Revision 1.2  1997/06/02  19:15:01  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
