(*$import Prelude TopLevel STATESYN *)
(* Basic search engine: Version 1.3*)
(* Author: Carsten Schuermann *)

signature MTPSEARCH = 
sig
  structure StateSyn : STATESYN

  exception Error of string

  val searchEx : int * StateSyn.FunSyn.IntSyn.Exp list
(*      * (StateSyn.FunSyn.IntSyn.Exp * StateSyn.FunSyn.IntSyn.Sub) *)
      * (int -> unit) -> unit
end;  (* signature SEARCH *)
