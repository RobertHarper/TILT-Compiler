(*$import Prelude INTSYN *)
(* Meta Prover *)
(* Author: Carsten Schuermann *)

signature PROVER =
sig
  structure IntSyn : INTSYN

  exception Error of string 

  val init   : (int * int (*IntSyn.cid*) list) -> unit
  val auto   : unit -> unit
  val print  : unit -> unit
  val install: (IntSyn.ConDec -> int (*IntSyn.cid*)) -> unit
end;  (* signature PROVER *)
