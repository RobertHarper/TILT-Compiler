(*
 * Module to rebuild a CFG from an SSA.
 *)

functor SSA2CFGFn
   (structure SSA : SSA
    structure Liveness : SSA_LIVENESS
    structure Util : CFG_UTIL
    structure P   : INSN_PROPERTIES
       sharing SSA.I = P.I
       sharing SSA.CFG = Util.CFG
       sharing Liveness.SSA = SSA
    val input_repairs : int ref
    val output_repairs : int ref
    val conflict_repairs : int ref
    val liveout_repairs : int ref
    val ssa_copies : int ref
    val ssa_copies' : int ref
   ) : SSA2CFG =
struct

   structure SSA = SSA
   structure CFG = SSA.CFG   
   structure Dom = SSA.Dom 
   structure I   = SSA.I
   structure SP  = SSA.SP
   structure E   = SSAExp
   structure C   = I.C
   structure G   = Graph
   structure A   = Array
   structure H   = HashArray
   structure R   = RegSet
   structure CP  = SSACopiesFn(SP)
   structure DJ  = DJGraphFn(Dom)

   fun error msg = MLRiscErrorMsg.impossible("SSA2CFG."^msg) 

   (*
    * Rebuild a CFG from an SSA graph
    *)
   fun buildCFG(SSA as G.GRAPH ssa) =
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
       val Dom as G.GRAPH dom = SSA.dom SSA
       val N           = #capacity cfg ()
       val M           = #capacity ssa ()
       val show_val    = SSA.show_val SSA
       val show_op     = SSA.show_op SSA
       val nodes       = SSA.nodes SSA
       val ENTRY       = hd(#entries cfg ())
       val cellClass   = SSA.cellClass SSA
       val getOperands = SP.operands{ regmap  = CFG.reglookup CFG,
                                      immed   = SSA.immed SSA,
                                      label   = SSA.label SSA,
                                      operand = SSA.operand SSA
                                    }
       val rewriteOperands = SP.rewriteOperands{const=SSA.const SSA}
       val DU = A.array(N,[])            (* def/use information per block *)

               (* merge sets per block
                * register r in merge set(b) implies that b in IDF(defsite(r)) 
                * and r is a targeted register.
                *)
       val mergeSets = A.array(N,[])     

               (* values that must be repaired since it may be
                * overwritten.
                *)
       val mustRepair = H.array(M,false)  

               (* ssa name -> original register map *)
       val targetTo = H.array'(M,fn r => r) 

               (* target definition of v to r in the transformed program *)
       fun target(v,r) = H.update(targetTo,v,r)
       fun targetAll(vs,rs) = ListPair.app target (vs,rs)

       (*
        * Collect physical registers (we try to preserve these names)  
        *)
       val firstPseudo = C.firstPseudo
       fun isPhysical r = r < firstPseudo

       (*
        * Initialization.  
        * (1) Compute def/use informations for all instructions.
        * (2) For each instruction that has to be fixed,
        *     compute its def sites.
        * (3) Update the targetTo map.
        *)
       fun initialize() =
       let val fixedDefSites = H.array(N,[])  (* def sites of fixed registers *)
           fun computeDU(b,_) =
           let val {source,phis,ops,sink} = A.sub(nodes,b)
                    (*
                     * Extract fixed registers.
                     * A register r is fixed if it is  
                     *  (i) a source operand and the instruction must use 
                     *      that register.
                     *  (ii) a destination and the instruction must use 
                     *      that register, or if it is a physical register.
                     *)
               fun getFixed([],du,fixed) = (rev du,fixed)
                 | getFixed((_,SSA.OP{i,e,s,t,...})::ops,du,fixed) = 
                     let val (defs,uses) = getOperands i
                         val (defs,uses) = 
                               case e of
                                  E.COPY => CP.simplifyCopy(defs,uses)
                               |  _      => (defs,uses)
                               (* uses *)
                         fun f([],[],fixed) = fixed
                           | f(SP.OPN _::l,s::ss,fixed) = f(l,ss,fixed)
                           | f(SP.REG(r,_)::l,s::ss,fixed) = f(l,ss,fixed)
                           | f(SP.FIX(r,_)::l,s::ss,fixed) = f(l,ss,r::fixed)
                           | f(SP.MEM _::l,s::ss,fixed) = f(l,ss,fixed)
                           | f _ = error "getFixed"
                               (* defs *)
                         fun g([],[],fixed) = fixed
                           | g(SP.OPN _::l,t::ts,fixed) = g(l,ts,fixed)
                           | g(SP.MEM _::l,t::ts,fixed) = g(l,ts,fixed)
                           | g(SP.REG(r,c)::l,t::ts,fixed) =
                               if isPhysical r then
                                  (target(t,r); g(l,ts,r::fixed))
                               else g(l,ts,fixed)
                           | g(SP.FIX(r,c)::l,t::ts,fixed) = 
                                (target(t,r); g(l,ts,r::fixed))
                           | g _ = error "getFixed"
                         val fixed = f(uses,s,fixed)
                         val fixed = g(defs,t,fixed)
                     in  getFixed(ops,(defs,uses)::du,fixed)
                     end
                 | getFixed((_,SSA.PHI{s,t,t',...})::ops,du,fixed) = 
                     if isPhysical t' then
                        (target(t,t'); getFixed(ops,du,t'::fixed))
                     else
                         getFixed(ops,du,fixed)
                 | getFixed((_,SSA.SOURCE{t',t,...})::ops,du,fixed) = 
                     (targetAll(t,t'); getFixed(ops,du,t'@fixed))
                 | getFixed((_,SSA.SINK{s',s,...})::ops,du,fixed) = 
                     (getFixed(ops,du,fixed))
               val (_,fixed)  = getFixed(source,[],[])
               val (_,fixed)  = getFixed(phis,[],fixed)
               val (du,fixed) = getFixed(ops,[],fixed)
               val (_,fixed)  = getFixed(sink,[],fixed)
           in  A.update(DU,b,du);
               R.app (fn r => H.update(fixedDefSites,r,
                                 b::H.sub(fixedDefSites,r))) 
                      (R.fromList fixed)
           end

           val _ = #forall_nodes cfg computeDU

           val {IDFs,...} = DJ.dj_graph Dom

       in  H.appi(fn (v,defsites) =>
                  let val bs = IDFs defsites
                      fun add b = A.update(mergeSets,b,v::A.sub(mergeSets,b))
                  in  (* print("[ IDF(defsite("^show_val v^")) = IDF"^
                             R.toString(R.fromList defsites)^" = "^
                             R.toString(R.fromList bs)^" ]\n") *)
                      app add bs
                  end
                 ) (fixedDefSites,0,NONE)
       end

       val _ = initialize()

       (* 
        * mapping from physical name to current ssa name
        *)
       val avail = H.array(N,[]) 

       fun pos(SSA.PHI{b,...}) = b
         | pos(SSA.OP{b,...}) = b
         | pos(SSA.SOURCE{b,...}) = b
         | pos(SSA.SINK{b,...}) = b

       fun defInfo v = let val i = #node_info ssa v
                       in  show_op i^" b"^Int.toString(pos(i)) end

       fun REPAIR_MSG(block,ssa_op,s,v,r) =
           print("[ Block "^Int.toString block^" must repair "^show_val s
                 ^" defined in "^defInfo s^" used in "^show_op ssa_op
                 ^" ]\n")

       fun REPAIRING_MSG(block,d,d',r) =
           print("[ Block "^Int.toString block^" inserting copy "
                ^show_val d^" (r"^Int.toString r
                ^")-> v"^Int.toString d'^" ]\n") 

       fun REPAIRING_PHI_MSG(block,d,d',r) =
           print("[ Block "^Int.toString block^" inserting (phi) copy "
                ^show_val d^" (r"^Int.toString r
                ^")-> v"^Int.toString d'^" ]\n") 

       fun LIVEOUT_MSG(block,d,d',r) =
           print("[ Block "^Int.toString block^" liveout copy "
                ^show_val d^" (r"^Int.toString r
                ^")-> v"^Int.toString d'^" ]\n") 

       (*
        * Phase 1.  Locate potiential conflicts and mark all repair locations
        *)
       fun findConflicts block =
       let val trail = ref [] 
           fun pop r = 
               case H.sub(avail,r) of
                 [] => error "findConflicts.pop"
               | _::x => H.update(avail,r,x)

               (* register r is now aliased to ssa name v *) 
           fun alias(r,v) = 
               (trail := r :: !trail; 
                H.update(avail,r,v::H.sub(avail,r)))

           fun kill r' = 
               ((* print("[ Killing "^show_val r'
                         ^" b"^Int.toString block^" ]\n"); *)
                alias(r',~1))

           fun look v = 
               case H.sub(avail,v) of
                  [] => v
               |  v'::_  => v'

           (* update the alias table *)
           fun define t =
               let val r = H.sub(targetTo,t)
               in  if t = r then () else alias(r,t) end

           (* check whether the current value for s is available.
            * If not, mark the repair table.
            *)
           fun use ssa_op s  =
               let val r = H.sub(targetTo,s)
               in  if s = r then () 
                   else  
                     (case H.sub(avail,r) of
                         v::_ => if v <> s then 
                                    (REPAIR_MSG(block,ssa_op,s,v,r); 
                                     H.update(mustRepair,s,true))
                                  else ()
                     |  _ => ())
               end
     
           (*
            * Scan instructions and look for conflicts
            *)
           fun chkOp(_,SSA.SOURCE{t,...})    = app define t
             | chkOp(_,x as SSA.SINK{s,...}) = app (use x) s
             | chkOp(_,SSA.PHI{t,...})       = define t
             | chkOp(_,x as SSA.OP{s,t,...}) = (app (use x) s; app define t)

           (* Scan phi nodes in the CFG successor blocks 
            * and look for conflicts 
            *)
           fun chkPhis b' =
               let val {phis,...} = A.sub(nodes,b')
                   fun f((i,x as SSA.PHI{s,preds,...})::ops) = 
                       let fun g(p::preds,s::ss) = 
                               if p = block then use x s else g(preds,ss)
                             | g _ = ()
                       in  g(preds,s); f ops end
                     | f _ = ()
               in  f phis end

           (*
            * Extract the set of definitions from a list of phi nodes
            *)
           fun getPhiDefs phis =
           let fun f((_,SSA.PHI{t,...})::ops,defs) = 
                   f(ops,H.sub(targetTo,t)::defs)
                 | f(_,defs) = defs
           in  f(phis,[])
           end

           val {source,phis,ops,sink} = A.sub(nodes,block) 
           fun killMergeSet block =
               let val mergeSet = R.fromList(A.sub(mergeSets,block))
                   val phiDefs  = R.fromList(getPhiDefs phis)
                   val killSet  = R.-(mergeSet,phiDefs)
               in  R.app kill killSet
               end
       in  app chkOp source;               (* check source nodes *)
           app chkOp phis;                 (* check phi definitions *)
           killMergeSet block;             (* eliminate fixed regs! *)
           app chkOp ops;                  (* check ssa ops *)
           app chkPhis (#succ cfg block);  (* check phi uses *)
           app chkOp sink;                 (* check phi nodes *)
           app findConflicts (#succ dom block);
           app pop (!trail)
       end

       val _ = findConflicts ENTRY 

       (*
        * renaming table
        *)
       val V             = SSA.maxVar SSA
       val renamingTable = A.array(V,[])
       val isLiveOut     = Liveness.isLiveOut SSA

       (* 
        * Phase 2.  Walk the dominator tree and rename all instructions.
        * Retarget instructions back to their original.
        *)
       fun rename block =
       let val trail = ref []
           fun pop v = 
               case A.sub(renamingTable,v) of 
                  _::rest => A.update(renamingTable,v,rest)
               |  [] => error "rename.pop"

           fun renameAllUses{from,to} =
               (trail := from :: !trail;
                A.update(renamingTable,from,to::A.sub(renamingTable,from)))

           fun look v =
               case A.sub(renamingTable,v) of
                  [] => H.sub(targetTo,v)
               |  v'::_ => v'

           (* 
            * Rename the src of an ssa ops, generate renaming
            * copies if a src is fixed.   Make sure that we do not
            * generate duplicated copies if the original instruction
            * has two of more operands to the same register.
            *)     
           fun renameUses(src,uses) = 
           let fun f(s::ss,SP.OPN _::uses,src,copies) = 
                    f(ss,uses,s::src,copies)
                 | f(s::ss,SP.REG _::uses,src,copies) =
                    f(ss,uses,look s::src,copies)   
                 | f(s::ss,SP.FIX(r,c)::uses,src,copies) =
                    f(ss,uses,r::src,newCopy(c,r,look s,copies))
                 | f(s::ss,SP.MEM _::uses,src,copies) =
                    f(ss,uses,s::src,copies)
                 | f(_,_,src,copies) = (rev src,SP.copies copies)
                   (* Generate a copy only if it is not already present *)
               and newCopy(c,r,s,copies) =
                   if r = s orelse
                      List.exists (fn {class,dst,src} => dst = r) copies
                   then copies
                   else (input_repairs := !input_repairs + 1;
                         {class=c,dst=r,src=s}::copies)
           in  f(src,uses,[],[]) end

           (* 
            * Rename the dst of an ssa op, generate renaming
            * copies if a dst is fixed, or if repair is necessary because
            * of output- or anti- dependence conflicts.
            *)     
           fun renameDefs(dst,defs) = 
           let fun f(d::ds,def::defs,dst,copies) =
                   let    (* target of instruction *)
                       val t = H.sub(targetTo,d) 
                          (* repair fixed definition 
                           *  d -- ssa name for the new value
                           *  r -- real target of the instruction
                           *  t -- target of the abstraction instruction
                           *  (in the worst case all three could be different!)
                           *)
                       val (r,copies,c) = (* fixed register fix up *)
                         case def of
                           SP.OPN _ => error "renameDefs"
                         | SP.REG(_,c) => (t,copies,c)
                         | SP.FIX(r,c) => 
                             if r = t then (r,copies,c)
                             else
                             (output_repairs := !output_repairs + 1;
                              (r,{class=c,dst=t,src=r}::copies,c))
                         | SP.MEM _    => (t,copies,C.MEM)
                          (* repair overwritten value *)
                       val copies = 
                           if H.sub(mustRepair,d) then
                              let val d' = C.newReg()
                              in  REPAIRING_MSG(block,d,d',r);
                                  renameAllUses{from=d,to=d'};
                                  conflict_repairs := !conflict_repairs + 1;
                                  {class=c,dst=d',src=r}::copies
                              end
                           else copies
                   in  f(ds,defs,r::dst,copies)
                   end
                 | f(_,_,dst,copies) = (rev dst,SP.copies copies)
           in  f(dst,defs,[],[])
           end
      
           (* 
            * Rename the operands of ssa ops.
            * As side effects, copies may be generated.
            *)     
           fun renameOps([],[],insns) = insns
             | renameOps((_,SSA.OP{i,s,t,...})::ops,(d,u)::DU,insns) = 
               let val (s,copies)  = renameUses(s,u)
                   val (t,copies') = renameDefs(t,d)
                   val i'          = rewriteOperands{instr=i,dst=t,src=s}
               in  renameOps(ops,DU,copies' @ [i'] @ copies @ insns)
               end
             | renameOps((_,SSA.SOURCE{t,t',...})::ops,DU,insns) = 
               let fun renameSrc(t::ts,t'::ts',copies) =
                       let val (copies,r) = 
                              if H.sub(mustRepair,t) then
                              let val d' = C.newReg()
                                  val c  = cellClass t
                              in  REPAIRING_MSG(block,t,d',t');
                                  renameAllUses{from=t,to=d'};
                                  conflict_repairs := !conflict_repairs + 1;
                                  ({class=c,dst=d',src=t'}::copies,d')
                              end
                              else (copies,t')
                       in  renameAllUses{from=t,to=r};
                           renameSrc(ts,ts',copies)
                       end
                     | renameSrc(_,_,copies) = copies
                   val copies = renameSrc(t,t',[])
               in  renameOps(ops,DU,SP.copies copies@insns) end
             | renameOps((_,SSA.SINK{s,s',...})::ops,DU,insns) = 
               let fun cp(s::ss,s'::ss',cps) = 
                         cp(ss,ss',{class=cellClass s,src=look s,dst=s'}::cps)
                     | cp(_,_,cps) = cps
                   val copies = cp(s,s',[]) 
               in  renameOps(ops,DU,SP.copies copies @ insns) end
             | renameOps _ = error "renameOps"

           (*
            * Insert copies for phi nodes.
            * Also rename t <- s if t is live out on the current block.
            *)
           fun insertPhiCopies(block) =
           let fun collectCopies([],copies) = copies
                 | collectCopies((_,b',_)::es,copies) =
                   let val {phis,...} = A.sub(nodes,b') 
                       val copies = collectPhis(phis,copies)
                   in  collectCopies(es,copies) end
               and collectPhis((_,SSA.PHI{preds,s,t,t',...})::phis,copies) =
                   let fun findSrc(p::ps,s::ss) = 
                            if p = block then look s else findSrc(ps,ss)
                         | findSrc _ = error "findSrc"
                       val s = findSrc(preds,s)  (* real source *)
                       val c = cellClass t
                       val r = H.sub(targetTo,t) (* real target *)
                       val _ = ssa_copies' := !ssa_copies' + 1
                       val copies = if r = s then copies 
                                    else (ssa_copies := !ssa_copies + 1;
                                          {class=c,dst=r,src=s}::copies)
                       val copies = (* repair conflicts *)
                           if H.sub(mustRepair,t) then
                              let val t'' = C.newReg()
                              in  REPAIRING_PHI_MSG(block,t,t'',r);
                                  conflict_repairs := !conflict_repairs + 1;
                                  renameAllUses{from=t,to=t''};
                                  {class=c,dst=t'',src=r}::copies
                              end
                           else copies
                       val copies = (* fix liveout problem *)
                           if r <> s andalso isLiveOut{v=t,b=block} then
                              let val t'' = C.newReg()
                              in  LIVEOUT_MSG(block,t,t'',r);
                                  renameAllUses{from=t,to=t''};
                                  liveout_repairs := !liveout_repairs + 1;
                                  {class=c,dst=t'',src=s}::copies
                              end
                           else copies
                   in  collectPhis(phis,copies)
                   end
                 | collectPhis(_,copies) = copies
               val copies = collectCopies(#out_edges cfg block,[])
           in  SP.copies copies
           end
          
           (* Generate the new instructions *) 
           val {source,phis,ops,sink} = A.sub(nodes,block)
           val instrs     = renameOps(source,[],[])
           val instrs     = renameOps(ops,A.sub(DU,block),instrs)
           val sinkCopies = renameOps(sink,[],[])
           val copies     = insertPhiCopies(block)

           (* Put the copies before the jmp instruction (if any) *)
           val allInstrs  = case instrs of
                              [] => copies@sinkCopies
                           |  jmp::rest => if P.instrKind jmp = P.IK_JUMP then
                                              jmp::copies@sinkCopies@rest
                                           else copies@sinkCopies@instrs
           val CFG.BLOCK{insns,...} = #node_info cfg block
       in  insns := allInstrs;
           app rename (#succ dom block);
           app pop (!trail)
       end

       val _ = rename ENTRY
       val _ = Util.removeUnreachableCode CFG
       val _ = Util.mergeAllEdges CFG

   in  CFG
   end

end

