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
