(*$import INTSYN *)
(* Unification *)
(* Author: Frank Pfenning, Carsten Schuermann *)

signature UNIFY =
sig

  structure IntSyn : INTSYN

  exception Unify of string
	
  val unify : (IntSyn.Dec IntSyn.Ctx)(*IntSyn.dctx*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> unit	(* raises Unify *)
  val unifyW : (IntSyn.Dec IntSyn.Ctx)(*IntSyn.dctx*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> unit (* raises Unify *)

  (* unifiable (G, Us,Us') will instantiate EVars as an effect *)
  val unifiable : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> bool

  (* unifiable' (G, Us,Us') is like unifiable, but returns NONE for
     success and SOME(msg) for failure *)
  val unifiable' :IntSyn.Dec IntSyn.Ctx (*IntSyn.dctx*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> string option

end;  (* signature UNIFY *)
