(* general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure General :> GENERAL where type unit = unit
			       and type exn = exn
			       and type order = order =
  struct

    type unit = unit
    type exn = exn

    exception Bind = Bind
    exception Chr = Chr
    exception Div = Div
    exception Domain = Domain
    exception Fail = Fail
    exception Match = Match
    exception Overflow = Overflow
    exception Size = Size
    exception Span = Span
    exception Subscript = Subscript

    val exnName = exnName
    val exnMessage = exnMessage

    datatype order = datatype order

    val ! = fn x => !x
    val op := = fn (x,y) => x := y
    val op o = op o

    val op before = (op before)
    val ignore = ignore

  end (* structure General *)
