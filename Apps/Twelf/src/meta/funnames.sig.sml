(*$import FUNSYN *)
(* Names of Constants and Variables *)
(* Author: Carsten Schuermann *)

signature FUNNAMES =
sig

  structure FunSyn : FUNSYN

  exception Error of string

  (* Constant names and fixities *)
  val reset : unit -> unit

  val installName : string * int (*FunSyn.lemma*) -> unit
  val nameLookup : string -> int (*FunSyn.lemma*) option
  val constName : int (*FunSyn.lemma*) -> string	(* will mark if shadowed *)

end;  (* signature NAMES *)
