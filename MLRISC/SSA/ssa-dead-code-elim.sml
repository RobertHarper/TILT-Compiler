(*
 * Dead code elimination.
 *)
functor SSADeadCodeElimFn(SSA : SSA) : SSA_OPTIMIZATION =
struct

   structure SSA  = SSA
   structure E    = SSAExp
   structure G    = Graph 
   structure S    = BitSet
   structure A    = Array

   fun optimize (SSA as G.GRAPH ssa) = 
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
       val live     = S.create (SSA.C.maxCell())
       val defSite  = SSA.defSite SSA
       val show_op  = SSA.show_op SSA
       val show_val = SSA.show_val SSA
     
       fun mark [] = ()
         | mark ((i,insn)::WL) =
           let val src = case insn of SSA.OP{s,...} => s
                                    | SSA.PHI{s,...} => s
                                    | SSA.SOURCE _ => []
                                    | SSA.SINK{s,...} => s
           in  markSrc(src,WL) end

       and markSrc([],WL) = mark WL
         | markSrc(r::rs,WL) =
           if r < 0 then markSrc(rs,WL)
           else let val i = defSite r
                in  if S.markAndTest(live,i) then markSrc(rs,WL)
                    else markSrc(rs,(i,#node_info ssa i)::WL)
                end

           (* 
            * All control flow instructions, and stores are not removed 
            *)
       fun findRoots() =
           let fun find([],roots) = roots
                 | find((_,SSA.PHI _)::ops,roots) = find(ops,roots)
                 | find((_,SSA.SOURCE _)::ops,roots) = find(ops,roots)
                 | find((i as (n,SSA.OP{e,...}))::ops,roots) = 
                      if E.isPinned e then
                         (S.set(live,n); find(ops,i::roots))
                      else find(ops,roots) 
                 | find((i as (n,SSA.SINK _))::ops,roots) =
                      (S.set(live,n); find(ops,i::roots))
           in  find(#nodes ssa (),[])
           end

      fun removeDeadCode() =
          #forall_nodes ssa (fn (n,n') => 
               if S.contains(live,n) then () else 
                  (print("[ DCE: "^show_op n'^"]\n"); #remove_node ssa n))

      val _ = mark(findRoots())
      val n = #order ssa ()
      val _ = removeDeadCode()
   in if #order ssa () < n then SSA.changed SSA else ();
      SSA
   end
end
 
(*
 * $Log$
# Revision 1.1  99/02/17  21:14:52  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:20  pscheng
# *** empty log message ***
#
 *) 
