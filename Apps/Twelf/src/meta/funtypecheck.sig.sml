(* Type checking for functional proof term calculus *)
(* Author: Carsten Schuermann *)

signature FUNTYPECHECK = 
sig
  structure FunSyn : FUNSYN
  structure StateSyn : STATESYN

  exception Error of string

  val isFor : FunSyn.IntSyn.dctx * FunSyn.For -> unit
  val check : FunSyn.Pro * FunSyn.For -> unit    
  val checkSub : FunSyn.lfctx * FunSyn.IntSyn.Sub * FunSyn.lfctx -> unit

  val isState : StateSyn.State -> unit
end (* Signature FUNTYPECHECK *)       

