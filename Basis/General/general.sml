(* general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure General :> GENERAL where type unit = unit
			       and type exn = exn =
  struct

    type unit = unit
    type exn = exn

    exception Bind
    exception Chr
    exception Div = Div  (* from Tiltexn *)
    exception Domain
    exception Fail of string
    exception Match
    exception Overflow = Overflow (* from Tiltexn *)
    exception Size
    exception Span
    exception Subscript

    val exnName = exnName
    val exnMessage = exnMessage

    datatype order = LESS | EQUAL | GREATER

    val ! = !
    val := = := 
    val o = o

    val before = before
    val ignore = ignore

  end (* structure General *)
