(*
 * This module constructs an SSA graph from a CFG.
 * It can use either pruned or semi-pruned SSA form.
 * The latter is probably faster to construct but may consume more space.
 *)

functor CFG2SSAFn
   (structure SSA      : SSA
    structure Liveness : LIVENESS_ANALYSIS
       sharing SSA.CFG = Liveness.CFG
   ) : CFG2SSA =
struct

   structure SSA  = SSA
   structure CFG  = SSA.CFG
   structure I    = SSA.I
   structure SP   = SSA.SP
   structure E    = SSAExp
   structure C    = I.C
   structure G    = Graph
   structure A    = Array
   structure R    = RegSet
   structure DA   = DynamicArray
   structure SSA' = StaticSingleAssignmentFormFn(SSA.Dom)
   structure CP   = SSACopiesFn(SP)

   fun error msg = MLRiscErrorMsg.impossible("CFG2SSA."^msg)

   (*
    * Construct an SSA graph from a CFG and dominator tree
    *) 
   fun buildSSA {copyPropagation,keepName,semiPruned} 
           (CFG as G.GRAPH cfg,computeDom) =
   let val Dom as G.GRAPH dom = computeDom CFG
       val SSA as G.GRAPH ssa = SSA.newSSA(CFG,computeDom)
     
       val N               = #capacity cfg ()
       val ENTRY           = hd(#entries cfg ()) 
       val EXIT            = hd(#exits cfg ())
       val getLiveness     = Liveness.getLiveness CFG
       val updateCellClass = SSA.updateCellClass SSA
       val DU              = A.array(N,[])
       val mustDef         = SP.mustDef

       (*
        * Is the definition pinned
        *)
       fun isPinnedDef r = List.exists (fn r' => r = r') mustDef 

       (*
        * Check for initial and terminal blocks
        *)
       fun isInitial b = List.exists(fn (i,_,_) => i=ENTRY) (#in_edges cfg b)
       fun isTerminal b = List.exists(fn (_,j,_) => j=EXIT) (#out_edges cfg b)

       (*
        *  v is in localLiveIn iff a use of v in some block is not defined
        *  in the same block 
        *)
       val localLiveIn = BitSet.create(if semiPruned then C.maxCell() else 1)

       (*
        * Extracts the registers 
        *) 
       fun getRegs(SP.REG(r,c)::du,regs) = getRegs(du,r::regs)
         | getRegs(SP.FIX(r,c)::du,regs) = getRegs(du,r::regs)
         | getRegs(SP.MEM r::du,regs)    = getRegs(du,r::regs)
         | getRegs(SP.OPN _::du,regs)    = getRegs(du,regs)
         | getRegs([],regs) = regs

       (*
        * Initialization.  Compute the DU array and the
        * localLiveIn set.
        *)
       fun initialize() = 
       let fun getLiveOut(_,block) = 
               R.fromList(C.cellsetToRegs(CFG.regmap CFG,CFG.liveOut block))
           val getOperands = SP.operands{regmap  = CFG.reglookup CFG,
                                         immed   = SSA.immed SSA,
                                         label   = SSA.label SSA,
                                         operand = SSA.operand SSA
                                        }
           fun defUse(b,b' as CFG.BLOCK{insns,...}) =
           let fun getRegs([],regs) = regs
                 | getRegs(SP.REG(r,c)::du,regs) = 
                    (updateCellClass(r,c); getRegs(du,r::regs))
                 | getRegs(SP.FIX(r,c)::du,regs) = 
                    (updateCellClass(r,c); getRegs(du,r::regs))
                 | getRegs(SP.MEM r::du,regs) = 
                    (updateCellClass(r,C.MEM); getRegs(du,r::regs))
                 | getRegs(SP.OPN _::du,regs) = getRegs(du,regs)
               fun scan([],du,def,use) = (du,def,use)
                 | scan(insn::insns,du,def,use) =
                   let val (d,u) = getOperands insn
                       val d'    = R.fromList(getRegs(d,[]))
                       val u'    = R.fromList(getRegs(u,[]))
                       val use   = R.+(R.-(use,d'),u')
                       val def   = R.-(R.+(def,d'),u')
                   in  scan(insns,(d,u)::du,def,use)
                   end
               val liveOut = getLiveOut(b,b')
               val (du,def,use) = scan(!insns,[],R.empty,liveOut)
           in  A.update(DU,b,du);
               if semiPruned then 
                  R.app (fn r => BitSet.set(localLiveIn,r)) use else ();  
               (def,use)
           end
       in  Liveness.liveness{cfg=CFG,defUse=defUse,liveOut=getLiveOut}
       end

       (*
        * Is variable v considered live at block b?
        *)
       fun isLiveSemiPruned(v,b) = BitSet.contains(localLiveIn,v) 
       fun isLivePruned(v,b) = 
           let val (liveIn,_) = getLiveness b
           in  R.contains(liveIn,v) end

       (*
        * Return the relevant definitions in a block
        *) 
       fun getDefsPruned(b,_) = 
       let val defs = foldr (fn ((d,_),l) => getRegs(d,l)) [] (A.sub(DU,b))
           val (_,liveOut) = getLiveness b
       in   (* List.filter 
               (fn r => BitSet.contains(localLiveIn,r)) (R.sort defs) *)
           R.toList(R.*(R.fromList defs,liveOut))
       end

       fun getDefsSemiPruned(b,_) = 
       let val defs = foldr (fn ((d,_),l) => getRegs(d,l)) [] (A.sub(DU,b))
       in  List.filter 
               (fn r => BitSet.contains(localLiveIn,r)) (R.sort defs) 
       end

       (*
        * Add edges to the SSA graph
        *)
       fun addEdges(n,uses) =  
          app (fn r => if r >= 0 then #add_edge ssa (r,n,r)
               else ()) uses

       fun addEdges'(n,uses) =  
          app (fn SP.REG(r,_) => #add_edge ssa (r,n,r)
                | SP.FIX(r,_) => #add_edge ssa (r,n,r)
                | SP.MEM r    => #add_edge ssa (r,n,r)
                | SP.OPN _ => ()) uses

       (* 
        * Insert phi nodes into a basic block 
        *)
       fun insertPhi{block,in_edges,phis=[]} = ()
         | insertPhi{block=(_,CFG.BLOCK{kind=CFG.STOP,...}),...} = ()
         | insertPhi{block=(b,_),in_edges,phis} =
           let val preds = map (fn (j,_,_) => j) in_edges
               fun addPhi(t',t,s) =
                   (#add_node ssa(t,SSA.PHI{preds=preds,t'=t',t=t,s=s,b=b});
                    addEdges(t,s))
           in  app addPhi phis
           end

       fun getval(SP.OPN v)    = v
         | getval(SP.REG(r,_)) = r
         | getval(SP.MEM r)    = r
         | getval(SP.FIX(r,_)) = r
       val getvals = map getval 

       (*
        * Rename a block 
        *)
       fun renameStmt _ (_,CFG.BLOCK{kind=CFG.START,...}) = ()
         | renameStmt _ (_,CFG.BLOCK{kind=CFG.STOP,...}) = ()
         | renameStmt {rename,copy} (b,CFG.BLOCK{insns,...}) = 
       let fun addOp(e,i,defs,uses,pos) =
           let val {defs,uses} = rename{defs=defs,uses=uses}
               val n  = (case defs of
                           r :: _ => r
                        |  []     => C.newReg())
               val n' = SSA.OP{e=e,i=i,t=defs,s=uses,b=b,p=pos}
           in  #add_node ssa (n,n');
               addEdges(n,uses)
           end
           fun scan(i::insns,pos,(d,u)::DU) =
               let val defs = getvals d
                   val uses = getvals u
                   val e = SP.exp i
                   val e = if List.exists isPinnedDef defs then
                              E.PINNED e else e
               in  case (e,copyPropagation) of 
                     (E.COPY,true) => copy{dst=defs,src=uses}
                   | (E.COPY,false) =>
                        let val (defs,uses) = CP.simplifyCopy'(defs,uses)
                        in  case defs of
                               [] => ()
                            |  _  => addOp(e,i,defs,uses,pos)
                        end
                        (* must be last if it is a control flow op *)
                   | ((E.BRANCH _ | E.JMP _ | E.RET),_) => 
                         addOp(e,i,defs,uses,12345678)
                   | _ => addOp(e,i,defs,uses,pos)
                   ;
                   scan(insns,pos+1,DU)
               end
             | scan _ = ()

           (*
            * Insert a source node into the ssa graph
            *)
           fun addSource() = 
               let val (liveIn,_) = getLiveness b
                   val defs' = R.toList liveIn
                   val {defs,...} = rename{defs=defs',uses=[]}
                   val n    = (case defs of
                                 r :: _ => r
                               | []     => C.newReg())
               in  #add_node ssa (n,SSA.SOURCE{t=defs,t'=defs',b=b});
                   #set_entries ssa (n:: #entries ssa ())
               end
                        
           (*
            * Insert a sink node into the ssa graph
            *)
           fun addSink() = 
               let val (_,liveOut) = getLiveness b
                   val uses' = R.toList liveOut
                   val {uses,...} = rename{defs=[],uses=uses'}
                   val n = C.newReg()
               in  #add_node ssa (n,SSA.SINK{s=uses,s'=uses',b=b});
                   addEdges(n,uses);
                   #set_exits ssa (n :: #exits ssa ())
               end
                   
       in  if isInitial(b) then addSource() else ();
           scan(rev(!insns),0,A.sub(DU,b));
           if isTerminal(b) then addSink() else ()
       end

       (*
        * Compute the SSA graph
        *)
       fun computeSSA() =
           SSA'.compute_ssa Dom
           { max_var     = C.maxCell(),
             defs        = if semiPruned then getDefsSemiPruned 
                           else getDefsPruned,
             is_live     = if semiPruned then isLiveSemiPruned
                           else isLivePruned,
             rename_var  = SSA.newRenamedVar 
                                (if keepName then SOME(A.array(C.maxCell(),0)) 
                                 else NONE) SSA,
             rename_stmt = renameStmt,
             insert_phi  = insertPhi
           }

       (* 
        * Fix up the uninitialized node 
        *)
       fun fixupUninit() =
       let val defSite = SSA.defSite SSA
           fun fixUp i =
               case #node_info ssa i of 
                 SSA.SOURCE{b,...} =>
                   if b = ENTRY then fixUninit(i,b)
                   else ()
               |  _ => ()
           and fixUninit(uninit,b) =
               let val regs = ref []
                   fun add(r,_,_) =
                       if defSite r = uninit then regs := r :: !regs else ()
                   val _ = #forall_edges ssa add
                   val regs = R.sort(!regs)
               in  #add_node ssa (uninit,SSA.SOURCE{b=b,t=regs,t'=regs})
               end
       in  app fixUp (#entries ssa ()) 
       end

   in  initialize();
       computeSSA();
       fixupUninit();
       SSA
   end 

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:42  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:12  pscheng
# *** empty log message ***
#
 *)
