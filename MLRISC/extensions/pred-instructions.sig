(*
 * This signature matches an instruction set that provides full predication
 *)

signature PREDICATED_INSTRUCTIONS =
sig
   include INSTRUCTIONS
   
   type predicate  (* basically says implement it however you want to *)

end
