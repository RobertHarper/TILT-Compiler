
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

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:36  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:03  pscheng
# *** empty log message ***
#
 *)
