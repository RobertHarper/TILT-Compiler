(* Type Checking *)
(* Author: Carsten Schuermann *)

signature TYPECHECK =
sig
    structure IntSyn : INTSYN

    exception Error of string

    val check : IntSyn.Exp * IntSyn.Exp  -> unit
    val checkDec : IntSyn.dctx * (IntSyn.Dec * IntSyn.Sub) -> unit 
    val infer : IntSyn.Exp -> IntSyn.Exp
    val infer' : IntSyn.dctx * IntSyn.Exp -> IntSyn.Exp
    val typeCheck : IntSyn.dctx * (IntSyn.Exp * IntSyn.Exp) -> unit
    val typeCheckCtx : IntSyn.dctx -> unit
end;  (* signature TYPECHECK *)
