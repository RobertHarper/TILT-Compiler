(* Initialization *)
(* Author: Carsten Schuermann *)

signature INIT = 
sig
  structure MetaSyn : METASYN

  exception Error of string

  val init : MetaSyn.IntSyn.cid list -> MetaSyn.State list
end;  (* signature INIT *)
