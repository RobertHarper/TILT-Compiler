functor MLRISCGlue
   (structure Asm  : EMITTER_NEW
    structure F  : FLOWGRAPH
    structure P  : INSN_PROPERTIES
       sharing P.I = Asm.I = F.I
       sharing F.P = Asm.P
    val copyProp : F.cluster -> F.cluster
    val branchProb : P.I.instruction -> int
    val patchBranch : {instr:P.I.instruction,backwards:bool} -> 
          P.I.instruction list
   ) : MLRISC_GLUE =
struct

   structure F = F
   structure I = F.I
   structure B = F.B
 
   val viewer  = MLRISC_ViewStyle.viewer
   val mlrisc  = MLRISC_Control.mlrisc
   val phases  = MLRISC_Control.mlrisc_phases
   val view_IR = MLRISC_Control.view_IR
   val verbose = MLRISC_Control.verbose

   fun error msg = MLRiscErrorMsg.impossible("MLRISCGlue."^msg)

   structure GraphViewer = GraphViewerFn(AllDisplaysFn(val viewer = viewer))

   structure FormatInsn = FormatInstructionFn(Asm)

   structure CFG = ControlFlowGraphFn
      (structure I = I
       structure B = B
       structure P = F.P
       structure W = FixedPointFn(val decimal_bits = 8)
       structure GraphImpl = DirectedGraph
       structure Asm = Asm
      )

   structure CFG2Cluster = CFG2ClusterFn
      (structure CFG = CFG
       structure F   = F
       val patchBranch = patchBranch
      )

   structure Cluster2CFG = Cluster2CFGFn
      (structure CFG = CFG
       structure F   = F
       structure P   = P
      )
       
   structure Dom = DominatorTreeFn(DirectedGraph)

   structure CDG = ControlDependenceGraphFn
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure Loop = LoopStructureFn
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure Util = CFGUtilFn
      (structure CFG = CFG
       structure P   = P
      )

   structure IR = MLRISC_IRFn
      (structure CFG         = CFG
       structure CDG         = CDG
       structure Loop        = Loop
       structure GraphViewer = GraphViewer
       structure Util        = Util
      )

   structure Guess = StaticBranchPredictionFn(IR)
      
   structure Liveness = LivenessAnalysisFn(CFG)

   structure Reshape = ReshapeBranchesFn(structure IR = IR
                                         structure P  = P)

   fun view phase ir = if !view_IR then IR.view phase ir else ()

   val ssaParams = {copyPropagation=false,keepName=true,semiPruned=false} 

   fun optimize cluster =
   let datatype rep = IR of IR.IR
                    | CLUSTER of F.cluster
       fun doPhase "copy-prop" (CLUSTER c) = CLUSTER(copyProp c)
         | doPhase "cluster->cfg" (CLUSTER c) = IR(Cluster2CFG.cluster2cfg c)
         | doPhase "cfg->cluster" (IR cfg) = 
            CLUSTER(CFG2Cluster.cfg2cluster{cfg=cfg,relayout=false})
         | doPhase "guess" (r as IR ir) =
            let fun prob(CFG.BLOCK{insns,...}) = 
                    case !insns of
                       [] => 100
                    |  jmp::_ => branchProb jmp
            in  Guess.profile {loopMultiplier=10,branchProb=prob} ir; r
            end
         | doPhase "reshape"   (r as IR ir) = (Reshape.reshapeBranches ir; r)
         | doPhase "view-cfg"  (r as IR ir) = (view "cfg" ir; r)
         | doPhase "view-dom"  (r as IR ir) = (view "dom" ir; r)
         | doPhase "view-pdom" (r as IR ir) = (view "pdom" ir; r)
         | doPhase "view-doms" (r as IR ir) = (view "doms" ir; r)
         | doPhase "view-cdg"  (r as IR ir) = (view "cdg" ir; r)
         | doPhase "view-loop" (r as IR ir) = (view "loop" ir; r)
         | doPhase phase _ = error(phase)
       fun doPhases [] (CLUSTER c) = c
         | doPhases [] _ = error "cluster needed"
         | doPhases (phase::phases) ir = 
            (if !verbose then print("["^phase^"]\n") else (); 
             doPhases phases (doPhase phase ir))
   in  doPhases (!phases) (CLUSTER cluster)
   end

   fun codegen cluster = if !mlrisc then optimize cluster else cluster

end
