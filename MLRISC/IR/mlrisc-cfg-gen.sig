(*
 * Emit code and build a CFG
 *)

signature CONTROL_FLOW_GRAPH_GEN =
sig

   structure CFG     : CONTROL_FLOW_GRAPH
   structure Emitter : CODE_EMITTER
       sharing Emitter.I = CFG.I
       sharing Emitter.P = CFG.P

   (*
    * This creates an emitter which can be used to build a CFG
    *)
   val emitter : CFG.cfg -> Emitter.emitter

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:40  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:24  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:05:53  pscheng
# *** empty log message ***
#
 *)
