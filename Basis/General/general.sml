(*$import Prelude GENERAL *)
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
    exception Match = Match
    exception Subscript = Subscript
    exception Size = Size
    exception Overflow = Overflow
    exception Chr = Chr
    exception Div = Div
    exception Domain = Domain
    exception Span = Span
    exception Fail = Fail
    datatype order = datatype order

    fun equal(x,y) = x = y
    fun nequal(x,y) = x <> y
    val op = = equal
    val op <> = nequal

    val ! = fn x => !x
    val op := = fn (x,y) => x := y

    val op o = op o
    val op before = (op before)
    val ignore = ignore

    val exnName = exnName
    val exnMessage = exnMessage

  end (* structure General *)
