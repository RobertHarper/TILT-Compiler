(*
 * Global code motion algorithm in SSA form.
 * This is Cliff Click's algorithm.
 * I've generalized it to take into account of execution frequencies.
 *
 * -- Allen
 *)

functor SSAGlobalCodeMotionFn(SSA : SSA) : SSA_OPTIMIZATION =
struct

   structure SSA  = SSA
   structure CFG  = SSA.CFG
   structure Dom  = SSA.Dom
   structure SP   = SSA.SP
   structure E    = SSAExp
   structure W    = CFG.W
   structure G    = Graph
   structure A    = Array
   structure S    = BitSet

   fun error msg = MLRiscErrorMsg.impossible("SSAGlobalCodeMotion."^msg)

   fun optimize(SSA as G.GRAPH ssa) =
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
       val Dom as G.GRAPH dom = SSA.dom SSA
       val _                  = SSA.changed SSA
       val M                  = #capacity ssa ()
       val N                  = #capacity cfg ()
       val visited            = S.create M
       val dom_methods        = Dom.methods Dom
       val dom_level          = #dom_level dom_methods
       val dom_lca            = #dom_lca dom_methods
       val idom               = #idom dom_methods
       val dominates          = #dominates dom_methods
       val pos                = A.array(M,~1)  (* position for an instruction *)
       val ENTRY              = hd(#entries cfg ())
       val EXIT               = hd(#exits cfg ())
       val entry_pos          = A.array(N,~1)
       val show_op            = SSA.show_op SSA
       val move               = SSA.move SSA
       val move_to_back       = A.array(N,[])

       (*
        * Compute the real entry of each dominator node.
        * The real entry E of node X is the node that is
        * immediately dominated by ENTRY and which dominates X.
        * This is needed since we can have multiple entry points in
        * a cluster/CFG.
        *)
       fun computeEntry (X,E) =
           (A.update(entry_pos,X,E);
            app (fn (_,Y,_) => computeEntry(Y,if X = ENTRY then Y else E))
               (#out_edges dom X)
           )
       val _ = computeEntry(ENTRY,ENTRY)

       fun pinned_up(SSA.OP{e=E.LOAD _,b,i,...}) = (SP.isNonSafeRead(i),b)
         | pinned_up(SSA.OP{e,b,i,...})   = (E.can'tMoveUp(e),b)
         | pinned_up(SSA.PHI{b,...})    = (true,b)
         | pinned_up(SSA.SOURCE{b,...}) = (true,b)
         | pinned_up(SSA.SINK{b,...})   = (true,b)

       fun pinned_down(SSA.OP{e,b,...})   = (E.can'tMoveDown(e),b)
         | pinned_down(SSA.PHI{b,...})    = (true,b)
         | pinned_down(SSA.SOURCE{b,...}) = (true,b)
         | pinned_down(SSA.SINK{b,...})   = (true,b)

       fun pos'n'depth i = let val pos   = A.sub(pos,i)
                               val depth = dom_level pos
                           in  (pos,depth) end

       (* 
        * For each value, try to schedule it as early as possible.
        * This is constrained by the position of an intruction's operand.
        *)
       fun scheduleEarly(i) = 
           if S.markAndTest(visited,i) then pos'n'depth i
           else let val (pinned,b) = pinned_up(#node_info ssa i)
                    val _ = A.update(pos,i,b) 
                    fun scan([],pos_i,depth_i) = 
                           if pinned then (b,dom_level b)
                           else (pos_i,depth_i)
                      | scan((j,_,_)::es,pos_i,depth_i) =
                        let val (pos_j,depth_j) = scheduleEarly j
                        in  if depth_j > depth_i then
                               scan(es,pos_j,depth_j)
                            else
                               scan(es,pos_i,depth_i)
                        end
                    val pos_i   = A.sub(entry_pos,b)
                    val depth_i = dom_level pos_i
                    val (pos_i,depth_i) = 
                        scan(#in_edges ssa i,pos_i,depth_i)
                in  A.update(pos,i,pos_i);
                    (pos_i,depth_i)
                end

       fun pass1(n,n') =
           let val (pinned,b) = pinned_up n'
           in  if pinned then
                  (S.set(visited,n); 
                   A.update(pos,n,b);
                   app (fn (j,_,_) => (scheduleEarly j;())) (#in_edges ssa n))
               else ()
           end

       fun dump(i',b,earliest,latest,best) = 
           print(show_op i'^" orig="^Int.toString b^
                 " earliest="^Int.toString earliest^
                 " latest="^Int.toString latest^
                 " best="^Int.toString best^
                 "\n")

       fun freq block =
           let val CFG.BLOCK{freq,...} = #node_info cfg block
           in  !freq end

       (*
        * Schedule an instruction as late as possible.
        *)  
       fun scheduleLate(_,SSA.PHI{b,...}) = b
         | scheduleLate(_,SSA.SINK{b,...}) = b
         | scheduleLate(_,SSA.SOURCE{b,...}) = b
         | scheduleLate(i,i' as SSA.OP{b,e,...}) = 
           if S.markAndTest(visited,i) then A.sub(pos,i)
           else 
           let val pinned = E.can'tMoveDown e
               fun scan([],~1) = b
                 | scan([],latest) = latest
                 | scan((_,j,r)::es,latest) = 
                   let val j'    = #node_info ssa j
                       val pos_j = scheduleLate(j,j')
                       val pos_j = 
                           case j' of
                              SSA.PHI{preds,s,...} =>
                                 let fun find(b::bs,s::ss,bs') = 
                                           if s = r then find(bs,ss,b::bs') 
                                           else find(bs,ss,bs')
                                       | find(_,_,bs') = bs'
                                 in  find(preds,s,[]) end
                           |  _ => [pos_j]
                       fun lca([],latest) = latest
                         | lca(u::use,~1) = lca(use,u)
                         | lca(~1::use,latest) = lca(use,latest)
                         | lca(u::use,latest) = lca(use,dom_lca(u,latest))
                       val latest = lca(pos_j,latest)
                   in  scan(es,latest) end
               val earliest = A.sub(pos,i)
               val latest = scan(#out_edges ssa i,if pinned then b else ~1)
               (* if latest = ~1 then the instruction is dead *)
               fun find_best(pos,best_pos,best_freq) =
                    if pos = earliest then best_pos
                    else let val CFG.BLOCK{freq,...} = #node_info cfg pos
                                 handle e => (print("pos="^Int.toString pos
                                          ^" earliest="^Int.toString earliest
                                          ^" lastest="^Int.toString latest
                                                ^" "^show_op i'^"\n"); raise e)
                         in  if W.<(!freq,best_freq) then
                                find_best(idom pos,pos,!freq)
                             else
                                find_best(idom pos,best_pos,best_freq)
                         end
               val best = if earliest = ~1 then ~1 
                          else if latest = ~1 then ~1
                          else find_best(latest,latest,freq latest)
           in
               A.update(pos,i,best);
               if best = ~1 then ()
               else if best <> b then 
                      (dump(i',b,earliest,latest,best);
                       if dominates(best,b) then
                           A.update(move_to_back,best,
                             (i,i')::A.sub(move_to_back,best))
                       else move{i=(i,i'),block=best})
               else ();
               best
           end

       fun pass2(n,n') =  
             let val (pinned,b) = pinned_down n'
             in  if pinned then
                    (S.set(visited,n);
                     app (fn (_,j,_) => (scheduleLate(j,#node_info ssa j);())) 
                         (#out_edges ssa n))
                 else ()
             end

       fun pass3(n,n') = (scheduleLate(n,n'); ())
   
       (* 
        * Move instructions to their proper places
        *)
       fun moveInstructions() = 
          A.appi(fn (b,insns) => 
                  app (fn(i,i') => move{i=(i,i'),block=b}) insns)
             (move_to_back,0,NONE)

   in
       #forall_nodes ssa pass1; (* schedule earliest *)
       S.clear visited;
       #forall_nodes ssa pass2; (* schedule latest *)
       #forall_nodes ssa pass3; (* schedule source instructions *)
       moveInstructions();      (* move instructions *)
       SSA
   end
end

