(*$import Prelude INTSYN *)
(* Subordination *)
(* Author: Carsten Schuermann *)

signature SUBORDINATE =
sig
  structure IntSyn : INTSYN
    
  val reset : unit -> unit
  val install : int(*IntSyn.cid*) -> unit

  val below : int(*IntSyn.cid*) * int(*IntSyn.cid*) -> bool (* transitive closure *)
  val belowEq : int(*IntSyn.cid*) * int(*IntSyn.cid*) -> bool	(* refl. transitive closure *)
  val equiv : int(*IntSyn.cid*) * int(*IntSyn.cid*) -> bool (* mutual dependency *)

  val weaken : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * int (*IntSyn.cid*) -> IntSyn.Sub
end;  (* signature SUBORDINATE *)
