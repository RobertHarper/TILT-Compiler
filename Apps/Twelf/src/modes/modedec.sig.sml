(*$import Prelude MODESYN PATHS LambdaStructs *)
(* Modes: short and long forms *)
(* Author: Carsten Schuermann *)
(* Modified: Frank Pfenning *)

signature MODEDEC =
sig

  structure ModeSyn : MODESYN
  structure Paths : PATHS

  exception  Error of string

  val shortToFull : int (*IntSyn.cid*) * ModeSyn.ModeSpine * Paths.region -> ModeSyn.ModeSpine
  val checkFull : int (*IntSyn.cid*) * ModeSyn.ModeSpine * Paths.region -> unit
 
end;  (* signature MODEDEC *)
