signature CFG2SSA =
sig

   structure SSA : SSA
   structure CFG : CONTROL_FLOW_GRAPH
      sharing SSA.CFG = CFG

   (* Build an SSA graph from a CFG.
    * Optionally perform copy propagation during construction.
    *)
   val buildSSA : 
       { copyPropagation : bool, (* perform copy prop.? *) 
         keepName        : bool, (* keep around original names? *)
         semiPruned      : bool  (* use semi-pruned instead of pruned form? *)
       } -> CFG.cfg * (CFG.cfg -> SSA.dom) -> SSA.ssa

end
