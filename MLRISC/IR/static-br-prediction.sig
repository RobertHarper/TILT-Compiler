signature STATIC_BRANCH_PREDICTION = 
sig

   structure IR : MLRISC_IR

   val profile : { branchProb     : IR.CFG.block -> int,
                   loopMultiplier : int
                 } -> IR.IR -> unit

end
