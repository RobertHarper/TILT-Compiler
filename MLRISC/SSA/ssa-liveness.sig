(*
 * Compute liveness information from an SSA graph.
 *)
signature SSA_LIVENESS =
sig

   structure SSA : SSA

   (* Compute live out information for each basic block in the SSA *)
   val liveOut : SSA.ssa -> RegSet.regset Array.array

   (* Is variable v live out at block b? *)
   val isLiveOut : SSA.ssa -> {v:SSA.value,b:SSA.block} -> bool

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:15:03  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:26  pscheng
# *** empty log message ***
#
 *)
