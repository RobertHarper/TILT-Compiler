(*$import Prelude GENERAL *)
(* general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

extern exnName : (exn, string) -->
extern exnMessage : (exn, string) -->

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

    exception Fail of string

    datatype order = datatype order

    fun equal(x,y) = x = y
    fun nequal(x,y) = x <> y
    val op = = equal
    val op <> = nequal

    val ! = fn x => !x
    val op := = fn (x,y) => x := y


    fun f o g = fn x => f(g x)
    fun a before b = a
    fun ignore _ = ()

    val exnName : exn -> string = fn exn => exnName exn
    val exnMessage: exn -> string = fn exn => exnMessage exn

  end (* structure General *)

open General