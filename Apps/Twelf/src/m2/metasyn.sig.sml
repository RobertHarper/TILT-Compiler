(*$import INTSYN *)
(* Meta syntax *)
(* Author: Carsten Schuermann *)

signature METASYN =
sig
  structure IntSyn : INTSYN

  datatype Mode =			(* Mode                       *)
    Bot					(* M ::= Bot                  *)
  | Top					(*     | Top                  *)

  datatype Prefix =			(* Prefix P := *)
    Prefix of IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*)		(* G   declarations           *)
            * Mode IntSyn.Ctx		(* Mtx modes                  *)
            * int IntSyn.Ctx		(* Btx splitting depths       *)

  datatype State =			(* State S :=                 *)
    State of string			(*             [name]         *)
             * Prefix			(*             G; Mtx; Btx    *)
             * IntSyn.Exp		(*             |- V           *)

  datatype Sgn =			(* Interface signature        *)
    SgnEmpty				(* IS ::= .                   *)
  | ConDec of IntSyn.ConDec * Sgn       (*      | c:V, IS             *)

  val createAtomConst : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Head -> (IntSyn.Exp * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*))
  val createAtomBVar : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * int -> (IntSyn.Exp * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*))
end; (* signature METASYN *)
