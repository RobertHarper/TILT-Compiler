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

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:42  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:41  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:10  pscheng
# *** empty log message ***
#
 *)
