(*$import FUNSYN STATESYN *)
(* Type checking for functional proof term calculus *)
(* Author: Carsten Schuermann *)

signature FUNTYPECHECK = 
sig
  structure FunSyn : FUNSYN
  structure StateSyn : STATESYN

  exception Error of string

  val isFor : FunSyn.IntSyn.Dec FunSyn.IntSyn.Ctx(*FunSyn.IntSyn.dctx*) * FunSyn.For -> unit
  val check : FunSyn.Pro * FunSyn.For -> unit    
  val checkSub : FunSyn.LFDec FunSyn.IntSyn.Ctx(*FunSyn.lfctx*) * FunSyn.IntSyn.Sub * FunSyn.LFDec FunSyn.IntSyn.Ctx(*FunSyn.lfctx*) -> unit

  val isState : StateSyn.State -> unit
end (* Signature FUNTYPECHECK *)       

