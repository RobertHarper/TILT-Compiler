(*$import Prelude *)
(* general-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature GENERAL =
  sig

    type unit
    type exn

    exception Bind
    exception Match
    exception Subscript
    exception Size
    exception Overflow
    exception Chr
    exception Div
    exception Domain
    exception Span

    exception Fail of string

    datatype order = LESS | EQUAL | GREATER

    val = : ''a * ''a -> bool
    val <> : ''a * ''a -> bool 

    val !  : 'a ref -> 'a
    val := : ('a ref * 'a) -> unit

    val o      : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val before : ('a * 'b) -> 'a
    val ignore : 'a -> unit

    val exnName : exn -> string
    val exnMessage: exn -> string

  end


