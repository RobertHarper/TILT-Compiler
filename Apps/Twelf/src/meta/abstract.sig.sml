(*$import Prelude INTSYN FUNSYN STATESYN *)
(* Meta Theorem Prover abstraction : Version 1.3 *)
(* Author: Frank Pfenning, Carsten Schuermann *)

signature MTPABSTRACT =
sig
  structure IntSyn : INTSYN
  structure FunSyn : FUNSYN
  structure StateSyn : STATESYN

  exception Error of string

  datatype ApproxFor =			(* Approximat formula *)
    Head of IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * (FunSyn.For * IntSyn.Sub) * int	(* AF ::= F [s] *)
  | Block of (IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Sub * int * IntSyn.Dec list) * ApproxFor
					(*  | (t, G2), AF *)


  val weaken :IntSyn.Dec IntSyn.Ctx (*IntSyn.dctx*) * int (*IntSyn.cid*) -> IntSyn.Sub
  val raiseType : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Exp -> IntSyn.Exp 
      
  val abstractSub : IntSyn.Sub * StateSyn.Tag IntSyn.Ctx * (IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * StateSyn.Tag IntSyn.Ctx) * IntSyn.Sub * StateSyn.Tag IntSyn.Ctx
        -> ((IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * StateSyn.Tag IntSyn.Ctx) * IntSyn.Sub)
  val abstractSub' : (IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * StateSyn.Tag IntSyn.Ctx) * IntSyn.Sub * StateSyn.Tag IntSyn.Ctx
        -> ((IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * StateSyn.Tag IntSyn.Ctx) * IntSyn.Sub)


  val abstractApproxFor : ApproxFor -> FunSyn.For
end;  (* signature MTPABSTRACT *)
