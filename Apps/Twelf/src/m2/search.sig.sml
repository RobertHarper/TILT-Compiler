(* Basic search engine *)
(* Author: Carsten Schuermann *)

signature SEARCH = 
sig
  structure MetaSyn : METASYN

  exception Error of string

  val searchEx : 
      MetaSyn.IntSyn.dctx * MetaSyn.IntSyn.Exp list
      * (MetaSyn.IntSyn.Exp * MetaSyn.IntSyn.Sub)
      * (unit -> unit)
      -> MetaSyn.State list
    
  val searchAll : 
      MetaSyn.IntSyn.dctx * MetaSyn.IntSyn.Exp list
      * (MetaSyn.IntSyn.Exp * MetaSyn.IntSyn.Sub)
      * (MetaSyn.State list -> MetaSyn.State list)
      -> MetaSyn.State list
end;  (* signature SEARCH *)
