(*
 * This module removes redundant computations and branches, and 
 * folds constants using global value numbering.
 *)
functor SSAGVNFn(structure GVN : SSA_GLOBAL_VALUE_NUMBERING
                 val leaveBehindCopy : bool
                ) : SSA_OPTIMIZATION =
struct

   structure SSA  = GVN.SSA
   structure CFG  = SSA.CFG
   structure E    = SSAExp
   structure G    = Graph
   structure A    = Array
   structure H    = HashArray

   fun optimize (SSA as G.GRAPH ssa) =
   let val VN = GVN.computeValueNumbers SSA
       val Dom as G.GRAPH dom = SSA.dom SSA
       val CFG as G.GRAPH cfg = SSA.cfg SSA
       val setBranch          = SSA.setBranch SSA
       val replace            = if leaveBehindCopy then SSA.replaceByCopy SSA
                                else SSA.replaceAllUses SSA
       val foldConstant       = SSA.foldConstant SSA
       val show_op            = SSA.show_op SSA
       val nodes              = SSA.nodes SSA
       val _                  = SSA.changed SSA

       exception NoPredicate

       val REG       = A.array(SSA.maxVar SSA,~1)
       val CONST     = A.array(SSA.operands SSA,~1)
       val PREDICATE = H.array'(30,fn _ => raise NoPredicate)

       fun walk b =
       let val regTrail = ref []
           val constTrail = ref []

           fun define(t) = 
               let val vn = A.sub(VN,t)
               in  if vn >= 0 then defineReg(t,vn)
                   else defineConst(t,vn)
               end

           and defineReg(t,vn) =
               let val t' = A.sub(REG,vn)
               in  if t' = ~1 orelse 
                      t <> t' andalso not(replace{from=t,to=t'}) then
                     (regTrail := vn :: !regTrail;
                      A.update(REG,vn,t))
                   else ()
               end

           and defineConst(t,vn) =
               let val vn' = ~1-vn
                   val t' = A.sub(CONST,vn')
               in  if t' = ~1 then
                     (foldConstant{value=t,const=vn};
                      constTrail := vn' :: !constTrail;
                      A.update(CONST,vn',t))
                   else if
                      t <> t' andalso not(replace{from=t,to=t'}) then
                     (constTrail := vn' :: !constTrail;
                      A.update(CONST,vn',t))
                   else ()
               end handle _ => ()
             
           fun eliminateRedundantBranch(i,i',t) =
               let val vn = A.sub(VN,t)
               in  setTarget(i,i',H.sub(PREDICATE,vn),vn)
                   handle NoPredicate => ();
                   vn
               end
           and setTarget(i,i',cond,vn) =
               (print("Eliminating: "^show_op i'
                   ^" "^Bool.toString cond^" vn="^Int.toString vn^"\n");
                setBranch{jmp=(i,i'),cond=cond}
               )
                       
           fun scan ([],br) = br
             | scan ((i,i')::ops,br) =
               (case i' of
                 SSA.OP{e=(E.BRANCH _),t=[t],...} =>
                    scan(ops,eliminateRedundantBranch(i,i',t))
               | SSA.OP{e=(E.JMP _ | E.RET | E.COPY | E.CALL _),...} => 
                    scan(ops,br)
               | SSA.OP{t,...} =>  (app define t; scan(ops,br))
               | SSA.PHI{t,...} => (define t; scan(ops,br))
               | _ => scan(ops,br))

           val {phis,ops,...} = A.sub(nodes,b)
           val _  = scan(phis,GVN.top)
           val br = scan(ops,GVN.top)

           fun doChildren [] = ()
             | doChildren ((_,j,_)::es) = 
               let val old = SOME(H.sub(PREDICATE,br)) 
                                handle NoPredicate => NONE
                   val _ = addPredicate j
                   val _ = walk j
                   val _ = case old of
                              NONE => H.remove(PREDICATE,br)
                           |  SOME x => H.update(PREDICATE,br,x)
               in  doChildren es
               end

           and addPredicate(j) =
               case #in_edges cfg j of
                  [(i',j,CFG.EDGE{k=CFG.BRANCH cond,...})] =>
                       if i' = b then H.update(PREDICATE,br,cond)
                       else ()
               |  _ => ()

      in doChildren(#out_edges dom b);
           app (fn vn => A.update(REG,vn,~1)) (!regTrail);
           app (fn vn => A.update(CONST,vn,~1)) (!constTrail)
       end

   in  walk(hd(#entries dom ()));
       SSA
   end

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:49  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:02  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:26  pscheng
# *** empty log message ***
#
 *)
