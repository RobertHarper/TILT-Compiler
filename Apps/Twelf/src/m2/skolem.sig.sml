(*$import Prelude INTSYN *)
(* Skolem administration *)
(* Author: Carsten Schuermann *)

signature SKOLEM =
sig
  structure IntSyn : INTSYN

  val install: int (*IntSyn.cid*) list -> unit
end;  (* signature SKOLEM *)
