(*$import Prelude INTSYN PATHS *)
(* Origins of Declarations *)
(* Author: Frank Pfenning *)

signature ORIGINS =
sig

  structure IntSyn : INTSYN
  structure Paths : PATHS

  (* val reset : unit -> unit *)
  val installOrigin : int (*IntSyn.cid*) * (string * Paths.occConDec option) -> unit
  val originLookup : int (*IntSyn.cid*) -> (string * Paths.occConDec option)

end;  (* signature ORIGINS *)
