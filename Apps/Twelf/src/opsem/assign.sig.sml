(*$import INTSYN *)
(* Assignment *)
(* Author: Larry Greenfield *)

signature ASSIGN =
sig
  structure IntSyn : INTSYN

  exception Assign of string

  (* raises Assign, instantiates EVars as effect *)
  val assign : (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> unit

  (* assigniable (Us,Us') will instantiate EVars as an effect *)
  val assignable : (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) * (IntSyn.Exp * IntSyn.Sub)(*IntSyn.eclo*) -> bool
end;
