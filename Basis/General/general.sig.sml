(* general-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature GENERAL =
  sig

    eqtype unit
    type exn

    exception Bind
    exception Chr
    exception Div
    exception Domain
    exception Fail of string
    exception Match
    exception Overflow
    exception Size
    exception Span
    exception Subscript

    val exnName : exn -> string
    val exnMessage: exn -> string

    datatype order = LESS | EQUAL | GREATER

    val !  : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val o  : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c

    val before : 'a * unit -> 'a
    val ignore : 'a -> unit

  end


