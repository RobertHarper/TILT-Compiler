(*$import Prelude METASYN *)
(* Lemma *)
(* Author: Carsten Schuermann *)

signature LEMMA = 
sig
  structure MetaSyn : METASYN
    
  exception Error of string

  val apply : MetaSyn.State * int (*MetaSyn.IntSyn.cid*) -> MetaSyn.State 
end;  (* signature LEMMA *)
