signature SSA2CFG =
sig

   structure SSA : SSA
   structure CFG : CONTROL_FLOW_GRAPH
     sharing SSA.CFG = CFG

   val buildCFG : SSA.ssa -> CFG.cfg 

end
