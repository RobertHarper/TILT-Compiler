(*$import INTSYN *)
(* Type Checking *)
(* Author: Carsten Schuermann *)

signature TYPECHECK =
sig
    structure IntSyn : INTSYN

    exception Error of string

    val check : IntSyn.Exp * IntSyn.Exp  -> unit
    val checkDec : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * (IntSyn.Dec * IntSyn.Sub) -> unit 
    val infer : IntSyn.Exp -> IntSyn.Exp
    val infer' : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Exp -> IntSyn.Exp
    val typeCheck : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * (IntSyn.Exp * IntSyn.Exp) -> unit
    val typeCheckCtx : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) -> unit
end;  (* signature TYPECHECK *)
