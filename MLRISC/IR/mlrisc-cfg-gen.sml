functor ControlFlowGraphGenFn
   (structure CFG     : CONTROL_FLOW_GRAPH
    structure Emitter : CODE_EMITTER
    structure P       : INSN_PROPERTIES
        sharing CFG.I = Emitter.I = P.I
        sharing CFG.P = Emitter.P
        sharing CFG.B = Emitter.B
   ) : CONTROL_FLOW_GRAPH_GEN =
struct

   structure CFG     = CFG
   structure Props   = P
   structure I       = CFG.I
   structure B       = CFG.B
   structure P       = CFG.P
   structure G       = Graph
   structure W       = CFG.W
   structure Emitter = Emitter

   fun error msg = MLRiscErrorMsg.impossible("ControlFlowGraphGen." ^ msg)

   fun warning msg = print("Warning: "^msg^"\n")

   fun emitter(CFG as G.GRAPH cfg) = 
   let val currentBlock = ref NONE : CFG.block option ref
       val newBlocks    = ref [] : CFG.block list ref 
       val blkName      = ref B.default
       val _            = CFG.init CFG
       val [ENTRY]      = #entries cfg ()
       val [EXIT]       = #exits cfg ()
       exception NotFound
       val labelMap = Intmap.new(43,NotFound)
       val newLabel = Intmap.add labelMap

       fun newBlock() = 
             let val id = #new_id cfg ()
                 val b  = CFG.newBlock(id,!blkName)
             in  currentBlock := SOME b; 
                 newBlocks := b :: !newBlocks;
                 #add_node cfg (id,b);
                 b 
             end

       fun getBlock() = 
           case !currentBlock of
              NONE   => newBlock()
           |  SOME b => b

       fun newPseudoOpBlock() =
            (case !currentBlock of
                SOME(b as CFG.BLOCK{insns=ref [],...}) => b
             |  _ => newBlock()
            )  

       fun insertOp p = 
            let val CFG.BLOCK{data,...} = newPseudoOpBlock()
            in  data := !data @ [p] end

       fun defineLabel(l as Label.Label{id,...}) = 
           let val b as CFG.BLOCK{labels,...} = newPseudoOpBlock()
           in  labels := l :: !labels;
               newLabel(id,b)
           end

       fun pseudoOp p = insertOp(CFG.PSEUDO p)

       fun exitBlock liveOut = 
           let val CFG.BLOCK{annotations,...} = getBlock()
           in  annotations := CFG.LIVEOUT liveOut :: !annotations
           end

       fun comment msg = 
           let val CFG.BLOCK{annotations,...} = getBlock()
           in  annotations := Annotations.COMMENT msg :: !annotations
           end

       fun blockName name = blkName := name

       fun blockName name = blkName := name

       fun entryLabel(l as Label.Label{id,...}) = 
       let val b as CFG.BLOCK{id=j,labels,...} = newPseudoOpBlock()
       in  labels := l :: !labels;
           newLabel(id,b);
           #add_edge cfg (ENTRY,j,CFG.EDGE{k=CFG.ENTRY,a=ref [],
                                           w=ref W.zero})
       end
       fun emitInstr i =
           let val CFG.BLOCK{insns,...} = getBlock()
           in  insns := i :: !insns;
               if Props.instrKind i = Props.IK_JUMP then
                  currentBlock := NONE
               else () 
           end
       fun finish() =
           let  fun nextBlock(CFG.BLOCK{id,data=ref [],...}::_) = id
                  | nextBlock _ = error "nextBlock"
                fun target (Label.Label{id,...}) = 
                    let val CFG.BLOCK{id,...} = Intmap.map labelMap id
                    in  id end
                fun addEdges [] = ()
                  | addEdges(CFG.BLOCK{id,insns,...}::blocks) =
                    (case !insns of
                       [] => fallsThru(id,blocks)
                     | instr::_ =>
                        if Props.instrKind instr = Props.IK_JUMP then
                           jump(id,Props.branchTargets instr,blocks)
                        else 
                          fallsThru(id,blocks);
                     addEdges blocks
                    )
                and fallsThru(i,CFG.BLOCK{id=j,data,...}::_) =
                      (case !data of
                          [] => ()
                       |  _  => warning("falls thru into pseudo ops: "^
                                        Int.toString i^" -> "^Int.toString j)
                       ;
                       #add_edge cfg (i,j,CFG.EDGE{k=CFG.FALLSTHRU,
                                                   w=ref W.zero,
                                                   a=ref []
                                                  })
                      )
                  | fallsThru(i,[]) =
                       error("missing return in block "^Int.toString i)
                and jump(i,[Props.ESCAPES],_) =
                        #add_edge cfg (i,EXIT,CFG.EDGE{k=CFG.EXIT,
                                                       w=ref W.zero,
                                                       a=ref []
                                                      })
                  | jump(i,[Props.LABELLED L],_) =
                       #add_edge cfg (i,target L,CFG.EDGE{k=CFG.JUMP,
                                                          w=ref W.zero,
                                                          a=ref []})
                  | jump(i,[Props.LABELLED L,Props.FALLTHROUGH],bs) =
                       (#add_edge cfg (i,target L,CFG.EDGE{k=CFG.BRANCH true,
                                                           w=ref W.zero,
                                                           a=ref []
                                                          });
                        #add_edge cfg (i,nextBlock bs,CFG.EDGE
                                                          {k=CFG.BRANCH false,
                                                           w=ref W.zero,
                                                           a=ref []
                                                          })
                       )
                  | jump(i,targets,_) =
                       let fun f(n,[]) = ()
                             | f(n,Props.LABELLED L::targets) =
                             (#add_edge cfg (i,target L,CFG.EDGE
                                                        {k=CFG.SWITCH n,
                                                         w=ref W.zero,
                                                         a=ref []});
                              f(n+1,targets))
                             | f _ = error "jump"
                       in  f(0,targets) end
           in  addEdges(rev(!newBlocks))
           end

    in
        {  init        = fn _ => (),
           defineLabel = defineLabel,
           entryLabel  = entryLabel,
           pseudoOp    = pseudoOp,
           emitInstr   = emitInstr,
           exitBlock   = exitBlock,
           blockName   = blockName,
           comment     = comment,
           finish      = finish
        } 
    end  

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:25  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:05:54  pscheng
# *** empty log message ***
#
 *)
