(*$import FUNSYN STATESYN *)
(* Inference: Version 1.3 *)
(* Author: Carsten Schuermann *)

signature INFERENCE = 
sig
  structure FunSyn : FUNSYN
  structure StateSyn : STATESYN

  exception Error of string

  type operator

  val expand : StateSyn.State -> operator 
  val apply : operator -> StateSyn.State
  val menu : operator -> string
end; (* signature Inference *)


