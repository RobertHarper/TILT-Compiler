(*$import Prelude FUNSYN *)
(* Converter from relational representation to a functional
   representation of proof terms *)
(* Author: Carsten Schuermann *)

signature RELFUN = 
sig
  structure FunSyn : FUNSYN

  exception Error of string

  val convertFor : int (*FunSyn.IntSyn.cid*) list -> FunSyn.For
  val convertPro : int (*FunSyn.IntSyn.cid*) list -> FunSyn.Pro
end (* Signature RELFUN *)       


