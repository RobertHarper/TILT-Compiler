(*
 * Signature for an SSA optimization phase
 *)
signature SSA_OPTIMIZATION =
sig

   structure SSA : SSA 

   val optimize : SSA.ssa -> SSA.ssa

end
