signature LIVENESS_ANALYSIS =
sig
  
   structure CFG : CONTROL_FLOW_GRAPH
   structure I   : INSTRUCTIONS
       sharing CFG.I = I

   val liveness : 
       { cfg     : CFG.cfg,
         liveOut : CFG.block Graph.node -> RegSet.regset,
         defUse  : CFG.block Graph.node -> RegSet.regset * RegSet.regset
       } -> unit

   val getLiveness : CFG.cfg -> Graph.node_id -> RegSet.regset * RegSet.regset

end

functor LivenessAnalysisFn(CFG : CONTROL_FLOW_GRAPH) : LIVENESS_ANALYSIS =
struct

   structure CFG = CFG
   structure I   = CFG.I
   structure A   = Annotations
   structure S   = RegSet
   structure G   = Graph

   val {put : (S.regset * S.regset) * A.annotations -> A.annotations,
        get, 
        rmv} = A.new() 

   structure Liveness =
      DataflowFn
         (struct
              structure CFG   = CFG
              type domain     = S.regset
              val  forward    = false
              val  bot        = S.empty
              val  ==         = S.==
              val  join       = S.union
              val  op +       = S.+
              val  op -       = S.-
              type dataflow_info = 
                  { liveOut : CFG.block Graph.node -> S.regset,
                    defUse  : CFG.block Graph.node -> S.regset * S.regset
                  }

              fun prologue(cfg,{defUse,liveOut}) (b,b') =
                  let val (def,use) = defUse(b,b')
                      val live_out  = liveOut(b,b')
                  in  { input    = live_out,
	                output   = (live_out - def) + use,
	                transfer = fn live_out => (live_out - def) + use
                      }
                  end

              fun epilogue _ { node = (_,CFG.BLOCK{annotations,...}), 
                               input=liveOut, output=liveIn } = 
                  annotations := put((liveIn,liveOut),!annotations)
         end
        )

   fun liveness {cfg,liveOut,defUse} = 
      (Liveness.analyze(cfg,{liveOut=liveOut,defUse=defUse}); ())

   fun getLiveness (G.GRAPH cfg) b = 
       let val CFG.BLOCK{annotations,...} = #node_info cfg b
       in  case get(!annotations) of
              SOME x => x
           |  NONE => (S.empty,S.empty)
       end

end

