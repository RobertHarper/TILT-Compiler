
(*
 * This is Reif and Tarjan's algorithm (SIAM J Computing 1981) 
 * for computing approximate birthpoints for expressions.   
 * For each basic block B,
 *   idef(B) = { v | v is defined on some path between B's idom and B }
 *)
signature MLRISC_IDEFS =
sig

   structure Dom : DOMINATOR_TREE
   structure CFG : CONTROL_FLOW_GRAPH
   structure I   : INSTRUCTIONS
      sharing CFG.I = I

   val idefs : 
       (I.instruction -> I.C.register list * I.C.register list) ->
       CFG.cfg ->
       { idefuse     : unit -> (RegSet.regset * RegSet.regset) Array.array,
         ipostdefuse : unit -> (RegSet.regset * RegSet.regset) Array.array
       }
end
