(*$import INTSYN *)
(* Termination Order *)
(* Author: Carsten Schuermann *)

signature ORDER =
sig

  structure IntSyn : INTSYN

  exception Error of string

  datatype 'a Order =	       	        (* Orders                     *)
      Arg of 'a				(* O ::= x                    *)
    | Lex of 'a Order list              (*     | {O1 .. On}           *)
    | Simul of 'a Order list            (*     | [O1 .. On]           *)

  datatype Mutual =			(* Termination ordering       *)
      Empty				(* O ::= No order specified   *)
    | LE of int (*IntSyn.cid*) * Mutual		(*     | mutual dependencies  *)
    | LT of int (*IntSyn.cid*) * Mutual		(*     | lex order for  -     *)

  datatype TDec =			(* Termination declaration *)
      TDec of int Order * Mutual

  val reset : unit -> unit
  val install : int (*IntSyn.cid*) * TDec -> unit 
  val selLookup : int (*IntSyn.cid*) -> int Order
  val mutLookup : int (*IntSyn.cid*) -> Mutual
  val closure : int (*IntSyn.cid*) -> int (*IntSyn.cid*) list

end;  (* signature ORDER *)
