(*$import INTSYN FUNSYN *)
(* State definition for Proof Search *)
(* Author: Carsten Schuermann *)

signature STATESYN =
sig
  structure IntSyn : INTSYN
  structure FunSyn : FUNSYN

  datatype Order =	       	        (* Orders                     *)
    Arg of (IntSyn.Exp * IntSyn.Sub) * 
           (IntSyn.Exp * IntSyn.Sub)	(* O ::= U[s] : V[s]          *)
  | Lex of Order list			(*     | (O1 .. On)           *)
  | Simul of Order list			(*     | {O1 .. On}           *)
  | All of FunSyn.IntSyn.Dec * Order  	(*     | {{D}} O              *)
  | And of Order * Order		(*     | O1 ^ O2              *)


  datatype Info =
    Splits of int
  | RL 
  | RLdone
    
  datatype Tag = 
    Parameter of int (*FunSyn.label*) option
  | Lemma of Info
  | None

  datatype State =			(* S = <n, (G, B), (IH, OH), d, O, H, F> *)
    State of int			(* Part of theorem                   *)
	   * (FunSyn.IntSyn.Dec FunSyn.IntSyn.Ctx(*FunSyn.IntSyn.dctx*)	(* Context of Hypothesis, in general not named *)
           * Tag FunSyn.IntSyn.Ctx) (* Status information *)
           * (FunSyn.For * Order)	(* Induction hypothesis, order       *)
           * int			(* length of meta context            *)
           * Order			(* Current order *)
           * (int * FunSyn.For) list	(* History of residual lemmas *)
           * FunSyn.For			(* Formula *)

  val orderSub : Order * FunSyn.IntSyn.Sub -> Order  
  val decrease : Tag -> Tag
  val splitDepth : Info -> int

  val normalizeOrder : Order -> Order
  val convOrder : Order * Order -> bool

  val normalizeTag : Tag * FunSyn.IntSyn.Sub -> Tag
end; (* signature STATESYN *)
