functor StaticBranchPredictionFn(IR : MLRISC_IR) : STATIC_BRANCH_PREDICTION =
struct

   structure IR   = IR
   structure Loop = IR.Loop
   structure CFG  = IR.CFG
   structure I    = IR.I
   structure G    = Graph
   structure W    = CFG.W
   structure S    = BitSet
   structure A    = Array
   structure H    = HashArray

   infix ==

   val op +   = W.+
   val op -   = W.-
   val op div = W.div
   val op ==  = W.==

   fun profile {loopMultiplier,branchProb} IR =
   let val CFG as G.GRAPH cfg  = IR
       val L   as G.GRAPH loop = IR.loop IR
       val [ENTRY]             = #entries cfg ()
       val N                   = #capacity cfg ()
       val marked              = S.create N
       val number_of_entries   = length(#out_edges cfg ENTRY)     
       val entry_weight        = W.scale(W.fromInt 100,number_of_entries)

       val likely_exits        = H.array(N,[])
       val exit_counts         = H.array(N,0)
       val entry_edges         = A.tabulate(N,#in_edges cfg)
       val header_of           = A.array(N,ENTRY)
       val TIMES               = 20

             (* is an edge unlikely? *)
       fun is_unlikely_edge (i,j,CFG.EDGE{k=CFG.BRANCH true,...}) = 
             branchProb(#node_info cfg i) = 0
         | is_unlikely_edge _ = false

       fun is_exit_edge (e as (i,j,_)) = 
            List.exists (fn (i',j',_) => i = i' andalso j = j')
               (H.sub(likely_exits,A.sub(header_of,i)))

       val sum = List.foldr (fn ((_,_,CFG.EDGE{w,...}),m) => !w + m) W.zero

       fun exit_weight_of i = 
       let val h = A.sub(header_of,i)
           val CFG.BLOCK{freq=ref w, ...}= #node_info cfg h
       in  w div (loopMultiplier * H.sub(exit_counts,h))
       end
     
       fun preprocess(header,Loop.LOOP{backedges,exits,loop_nodes,...}) = 
       let fun is_backedge (i,j,_) = 
              List.exists(fn (i',j',_) => i = i' andalso j = j') backedges
           val real_exits = List.filter (fn e => not(is_unlikely_edge e)) exits
       in  H.update(likely_exits,header,real_exits);
           H.update(exit_counts,header,length real_exits);
           A.update(entry_edges,header,
              List.filter (fn e => not(is_backedge e)) (#in_edges cfg header));
           app (fn i => A.update(header_of,i,header)) (header::loop_nodes)
       end

       fun propagate(0,_) = ()
         | propagate(n,[]) = ()
         | propagate(n,i::worklist) =
       let val _ = S.reset(marked,i)
           val CFG.BLOCK{freq,...} = #node_info cfg i
           val old_weight = !freq
           val new_weight = sum(A.sub(entry_edges,i))
           val new_weight = if i = ENTRY then entry_weight
                            else if #has_node loop i then 
                               W.scale(new_weight,loopMultiplier) 
                            else new_weight
       in  if old_weight == new_weight then
                propagate(n,worklist)
           else (freq := new_weight;
                 propagate_edge_weight(#out_edges cfg i,new_weight,[]);
                 propagate'(n,#out_edges cfg i,worklist)
                )
       end

       and propagate'(n,[],worklist) = propagate(n,worklist)
         | propagate'(n,(i,j,_)::es,worklist) =
           if S.markAndTest(marked,j) then 
                propagate'(n,es,worklist)
           else propagate'(Int.-(n,1),es,j::worklist)

       and propagate_edge_weight([],W,es') = process_non_exits(W,es')
         | propagate_edge_weight((e as (i,_,CFG.EDGE{w,...}))::es,W,es') =
           if is_exit_edge e then 
              let val exit_weight = exit_weight_of(A.sub(header_of,i))
              in  w := exit_weight; 
                  propagate_edge_weight(es,W-exit_weight,es')
              end
           else
              propagate_edge_weight(es,W,e::es')

       and process_non_exits(W,[]) = ()
         | process_non_exits(W,[(_,_,CFG.EDGE{w,...})]) = w := W
         | process_non_exits(W,es as 
                               [(i,_,CFG.EDGE{w,k=CFG.BRANCH b,...}),
                                (_,_,CFG.EDGE{w=w',k=CFG.BRANCH _,...})]) =
           let val (w_F,w_T) = if b then (w',w) else (w,w')
               val p = branchProb(#node_info cfg i)
           in  w_F := W.scale(W,Int.-(100,p)) div 100;
               w_T := W.scale(W,p) div 100
           end
         | process_non_exits(W,es) = divide_evenly(W,es)

       and divide_evenly(W,es) = 
           let val W' = W div (length es)
           in  app (fn (_,_,CFG.EDGE{w,...}) => w := W') es
           end

   in
       #forall_nodes loop preprocess;
       propagate(TIMES * N, [ENTRY])
   end

end
