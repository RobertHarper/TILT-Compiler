(*$import INTSYN *)
(* Convertibility Modulo Beta and Eta *)
(* Author: Frank Pfenning, Carsten Schuermann *)

signature CONV =
sig
  structure IntSyn : INTSYN
	
  val conv : (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> bool
  val convDec : (IntSyn.Dec * IntSyn.Sub) * (IntSyn.Dec * IntSyn.Sub)-> bool
end;  (* signature CONV *)
