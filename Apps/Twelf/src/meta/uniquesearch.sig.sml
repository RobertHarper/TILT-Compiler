(*$import INTSYN FUNSYN STATESYN *)
(* Basic search engine: Version 1.3*)
(* Author: Carsten Schuermann *)

signature UNIQUESEARCH = 
sig
  structure IntSyn : INTSYN
  structure FunSyn : FUNSYN
  structure StateSyn : STATESYN

  exception Error of string

  (*type acctype = IntSyn.Exp*)

  val searchEx : int * StateSyn.FunSyn.IntSyn.Exp list
      * (IntSyn.Exp list -> IntSyn.Exp list) -> IntSyn.Exp list
end;  (* signature SEARCH *)