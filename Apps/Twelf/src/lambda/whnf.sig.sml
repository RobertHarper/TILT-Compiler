(*$import INTSYN *)
(* Weak Head-Normal Forms *)
(* Authors: Frank Pfenning, Carsten Schuermann *)

signature WHNF =
sig
  structure IntSyn : INTSYN

  (* Patterns *)
  val isPatSub : IntSyn.Sub -> bool
  val dotEta   : IntSyn.Front * IntSyn.Sub -> IntSyn.Sub

  exception Eta
  val etaContract : IntSyn.Exp -> int	(* can raise Eta *)

  (* Weak head normalization *)
  val whnf : (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*)
  val expandDef : (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*)
  val etaExpandRoot : IntSyn.Exp -> IntSyn.Exp
  val whnfEta : ((IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*)) -> ((IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*))
  val lowerEVar : IntSyn.Exp -> IntSyn.Exp

  (* Full normalization *)
  val normalize: (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> IntSyn.Exp
  val normalizeDec: IntSyn.Dec * IntSyn.Sub -> IntSyn.Dec
  val normalizeCtx: IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) -> IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*)

  (* Inverting substitutions *)
  val invert : IntSyn.Sub -> IntSyn.Sub
  val strengthen: IntSyn.Sub * IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) -> IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) 
  val isId : IntSyn.Sub -> bool

  val cloInv : IntSyn.Exp * IntSyn.Sub -> IntSyn.Exp
  val compInv : IntSyn.Sub * IntSyn.Sub -> IntSyn.Sub
end; (* signature WHNF *)
