signature SSA2CFG =
sig

   structure SSA : SSA
   structure CFG : CONTROL_FLOW_GRAPH
     sharing SSA.CFG = CFG

   val buildCFG : SSA.ssa -> CFG.cfg 

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:45  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:15  pscheng
# *** empty log message ***
#
 *)
