signature STATIC_BRANCH_PREDICTION = 
sig

   structure IR : MLRISC_IR

   val profile : { branchProb     : IR.CFG.block -> int,
                   loopMultiplier : int
                 } -> IR.IR -> unit

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:39  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:08  pscheng
# *** empty log message ***
#
 *)
