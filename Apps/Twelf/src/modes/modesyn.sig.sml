(*$import INTSYN *)
(* Mode Syntax *)
(* Author: Carsten Schuermann *)
(* Modified: Frank Pfenning *)

signature MODESYN =
sig

  structure IntSyn : INTSYN

  exception Error of string

  datatype Mode = Plus | Star | Minus 
  datatype ModeSpine = Mnil | Mapp of Marg * ModeSpine
  and Marg = Marg of Mode * string option

  val reset : unit -> unit
  val installMode : (int (*IntSyn.cid*) * ModeSpine) -> unit 
  val modeLookup : int (*IntSyn.cid*) -> ModeSpine option

  val modeEqual : Mode * Mode -> bool
  val modeToString : Mode -> string
end;  (* signature MODESYN *)
