(*$import INTSYN COMPSYN *)
(* Printer for Compiled Syntax *)
(* Author: Iliano Cervesato *)

signature CPRINT =
sig

  structure IntSyn: INTSYN
  structure CompSyn: COMPSYN

  val goalToString: string
      -> IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * CompSyn.Goal -> string
  val clauseToString: string
      -> IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * CompSyn.ResGoal -> string
  val sProgToString: unit -> string
  val dProgToString: CompSyn.DProg -> string

end; (* signature CPRINT *)
