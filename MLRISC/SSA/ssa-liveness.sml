(*
 * Compute liveness on an SSA graph.
 *)
functor SSALivenessFn(SSA : SSA) : SSA_LIVENESS =
struct

   structure SSA = SSA
   structure Dom = SSA.Dom
   structure G   = Graph
   structure A   = Array
   structure R   = RegSet

   fun error msg = MLRiscErrorMsg.impossible("SSALiveness."^msg)

   structure Liveness = DataflowFn
     (struct
         structure CFG = SSA.CFG
         type domain     = R.regset
         val  forward    = false
         val  bot        = R.empty
         val  ==         = R.==
         val  join       = R.union
         val  op +       = R.+
         val  op -       = R.-
         type dataflow_info = {ssa     : SSA.ssa, 
                               liveout : domain A.array
                              }

         fun prologue(CFG as G.GRAPH cfg,{ssa,liveout}) =
         let val nodes = SSA.nodes ssa
             fun rmvOpn([],s) = s
                | rmvOpn(s::ss,s') = rmvOpn(ss,if s < 0 then s' else s::s')
             fun genKill([],gen,kill) = (gen,kill)
               | genKill((_,i)::ops,gen,kill) =
                 let val (def,use) = case i of
                           SSA.OP{t,s,...} => (t,rmvOpn(s,[]))
                        |  SSA.PHI{t,...}  => ([t],[])
                        |  SSA.SINK{s,...} => ([],s)
                        |  SSA.SOURCE{t,...} => (t,[])
                     val def = R.fromList(def)
                     val use = R.fromList(use)
                 in  genKill(ops,(gen - def) + use, kill + def)
                 end
             fun process(b,_) =
             let fun phiGen([],gen) = R.fromList gen
                   | phiGen((_,j,_)::es,gen) =
                     let val {phis,...} = A.sub(nodes,j)
                         fun f((_,SSA.PHI{s,preds,...})::phis,gen) =
                                f(phis,g(s,preds,gen))
                           | f(_,gen) = gen
                         and g(s::ss,p::preds,gen) =
                             if p = b then s::gen
                             else g(ss,preds,gen)
                           | g(_,_,gen) = gen
                     in  phiGen(es,f(phis,gen)) end
                 val {ops,phis,sink,source} = A.sub(nodes,b)
                 val (liveout1,_) = genKill(sink,R.empty,R.empty)
                 val liveout2     = phiGen(#out_edges cfg b,[])
                 val liveout      = liveout1 + liveout2
                 val (gen,kill)   = genKill(rev ops,liveout,R.empty)
                 val (gen,kill)   = genKill(phis,gen,kill)
                 val (gen,kill)   = genKill(source,gen,kill)
             in  { input    = liveout,
                   output   = (liveout - kill) + gen,
                   transfer = fn liveout => (liveout - kill) + gen
                 }
             end
        in  process
        end

        fun epilogue(cfg,{ssa,liveout}) { node=(b,_), input, output } =
             A.update(liveout,b,input)
     end
    )

   fun liveOut ssa = 
   let val CFG as G.GRAPH cfg = SSA.cfg ssa
       val N       = #capacity cfg ()
       val liveout = A.array(N,R.empty) 
   in  Liveness.analyze(CFG,{ssa=ssa,liveout=liveout});
       liveout
   end

   fun isLiveOut(SSA as G.GRAPH ssa) =
   let val {dominates,strictly_dominates,...} = Dom.methods(SSA.dom SSA) 
       val show_op = SSA.show_op SSA
       val show_val = SSA.show_val SSA
       fun defBlock(SSA.PHI{b,...}) = b
         | defBlock(SSA.OP{b,...}) = b
         | defBlock(SSA.SOURCE{b,...}) = b
         | defBlock(SSA.SINK{b,...}) = b
       fun liveOut{v,b} =
           dominates(defBlock(#node_info ssa v),b) andalso
           let fun domUse(_,i,_) = 
                     case #node_info ssa i of
                       SSA.OP{b=b',...} => strictly_dominates(b,b')
                     | SSA.SINK{b=b',...} => dominates(b,b')
                     | SSA.SOURCE _ => error "isLiveOut"
                     | SSA.PHI{preds,s,...} =>
                       let fun find(p::ps,s::ss) =
                               s = v andalso dominates(b,p) orelse find(ps,ss)
                             | find _ = false
                       in  find(preds,s) end
               fun domUse'(p,q,r) = 
                   let val x = domUse(p,q,r)
                   in  if x then 
                          let val i' = #node_info ssa v
                              val q' = #node_info ssa q 
                          in  print("[ BB"^Int.toString b^" "^
                                    show_val v^" "^show_op i'^" (b"^
                                    Int.toString(defBlock i')^
                                    ") used by "^show_op q'^" (b"^
                                    Int.toString(defBlock q')^") ]\n")
                          end else ();
                       x
                   end
           in  List.exists domUse' (#out_edges ssa v)
           end
   in  liveOut
   end

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:49  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:04  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:27  pscheng
# *** empty log message ***
#
 *)
