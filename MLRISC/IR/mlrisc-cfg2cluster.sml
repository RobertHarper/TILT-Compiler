(*
 *  Convert the new control flow graph format back into the old cluster format
 *)

signature CFG2CLUSTER =
sig
   structure CFG : CONTROL_FLOW_GRAPH
   structure F   : FLOWGRAPH
      sharing CFG.I = F.I
      sharing CFG.P = F.P
      sharing CFG.B = F.B

   (* 
    * If relayout is true, then always use the layout algorithm.
    * Otherwise, try to preserve the original layout if possible.
    *)
   val cfg2cluster : { cfg      : CFG.cfg,
                       relayout : bool   
                     } -> F.cluster

end 

functor CFG2ClusterFn
   (structure CFG  : CONTROL_FLOW_GRAPH
    structure F    : FLOWGRAPH
       sharing CFG.I = F.I
       sharing CFG.P = F.P
       sharing CFG.B = F.B
    val patchBranch : {instr:CFG.I.instruction, backwards:bool} -> 
                         CFG.I.instruction list
   ) : CFG2CLUSTER =
struct

    structure CFG      = CFG
    structure W        = CFG.W
    structure F        = F
    structure G        = Graph
    structure Q        = PriorityQueue
    structure Set      = BitSet
    structure A        = Array

    fun error msg = MLRiscErrorMsg.impossible("CFG2Cluster."^msg)

    fun pseudo_op (CFG.LABEL l) = F.LABEL l
      | pseudo_op (CFG.PSEUDO p) = F.PSEUDO p

        (* create a new BBLOCK with id i *)
    fun bblock M (i,b as 
                    CFG.BLOCK{kind,name,annotations,insns,labels,data,...}) =
    let val labels = map F.LABEL (!labels)
    in  case kind of
           CFG.STOP => map pseudo_op (!data)
        |  _ =>
        let val block = F.BBLOCK{blknum  =i,
                                 name    =name,
                                 insns   =ref(! insns),
                                 liveIn  =ref F.C.empty,
                                 liveOut =ref (CFG.liveOut b),
                                 pred    =ref [],
                                 succ    =ref []
                                }
       in  A.update(M,i,block); 
           map pseudo_op (!data) @ labels @ [block]
       end
    end

    fun bblock' (M,M',M'') =
    let val bblock = bblock M
    in  fn (i,b as CFG.BLOCK{id,...}) =>
           let val block = bblock(i,b) 
           in  A.update(M',i,id); A.update(M'',id,i); block end
    end

        (* create a new ENTRY with id i *)
    fun entry(M,i) =
    let val entry = F.ENTRY{succ=ref [], blknum=i}
    in  A.update(M,i,entry); 
        entry
    end

    fun entry'(M,M',M'',i,id) =
    let val entry = entry(M,i)
    in  A.update(M',i,id); A.update(M'',id,i); entry
    end

        (* create a new EXIT with id i *)
    fun exit(M,i) = 
    let val exit = F.EXIT{pred=ref [], blknum=i}
    in  A.update(M,i,exit); 
        exit
    end

    fun exit'(M,M',M'',i,id) =
    let val exit = exit(M,i)
    in  A.update(M',i,id); A.update(M'',id,i); exit
    end

    fun id_of(F.BBLOCK{blknum,...}) = blknum
      | id_of(F.ENTRY{blknum,...})  = blknum
      | id_of(F.EXIT{blknum,...})   = blknum

    fun delete_preentries (ENTRY,G.GRAPH cfg) =
    let fun remove (ENTRY,i,_) =
            let val CFG.BLOCK{kind,insns,...} = #node_info cfg i
            in  if kind = CFG.FUNCTION_ENTRY then
                   let val out_edges = #out_edges cfg i
                       val out_edges' = map (fn (i,j,e)=>(ENTRY,j,e)) out_edges 
                   in  case !insns of
                          [] => ()
                       |  _  => error "delete_preentries";
                       app (#add_edge cfg) out_edges';
                       #remove_node cfg i
                   end
                else ()
            end
    in  app remove (#out_edges cfg ENTRY)
    end

    fun remove_entry_to_exit (ENTRY,EXIT,CFG) =
        Graph.remove_edge CFG (ENTRY,EXIT)

       (*
        * Convert cfg -> cluster, assuming the layout is unchanged
        *)
    fun computeOldLayout (CFG as G.GRAPH cfg) =
    let val M       = #capacity cfg ()
        val [ENTRY] = #entries cfg ()
        val [EXIT]  = #exits cfg ()
        val regmap  = CFG.regmap CFG
        val _       = delete_preentries(ENTRY,CFG)
        val _       = remove_entry_to_exit(ENTRY,EXIT,CFG)
        val A       = A.array(M,F.ORDERED [])
        val nodes   = List.filter(fn (i,CFG.BLOCK{kind,...}) => 
                           i <> ENTRY andalso i <> EXIT andalso 
                           kind <> CFG.FUNCTION_ENTRY)
                                 (#nodes cfg ())
        val blocks  = List.concat(
                        map (bblock A) (nodes @ [(EXIT,#node_info cfg EXIT)]))
        val entry   = entry (A,ENTRY)
        val exit    = exit (A,EXIT)
        fun succs i = map (fn i => A.sub(A,i)) (#succ cfg i)
        fun preds i = map (fn i => A.sub(A,i)) (#pred cfg i)
        fun set_links(F.BBLOCK{blknum,pred,succ,insns,...}) = 
                  (pred := preds blknum; succ := succs blknum)
          | set_links(F.ENTRY{blknum,succ,...}) = succ := succs blknum
          | set_links(F.EXIT{blknum,pred,...})  = pred := preds blknum
          | set_links _ = ()
        val _ = A.app set_links A
    in  F.CLUSTER{ blkCounter= ref M,
                   regmap    = regmap,
                   blocks    = blocks,
                   entry     = entry,
                   exit      = exit
                 }
    end

       (*
        * Convert cfg -> cluster, while computing a new code layout.
        *)
    fun computeNewLayout (CFG as G.GRAPH cfg) =
    let val M        = #capacity cfg ()
        val [ENTRY]  = #entries cfg ()
        val [EXIT]   = #exits cfg ()
        val _        = delete_preentries(ENTRY,CFG)
        val CFG.INFO{firstBlock,regmap,...} = #graph_info cfg
        val A        = A.array(M,F.ORDERED []) (* new id -> F.block *)
        val A'       = A.array(M,~1)           (* new id -> old id *)
        val A''      = A.array(M,~1)           (* old id -> new id *)
        val min_pred = A.array(M,10000000)
        val in_degs  = A.tabulate(M,fn i => length(#in_edges cfg i))
        val nodes    = GraphTopsort.topsort CFG (ENTRY::map #1 (#nodes cfg ()))

        fun higher_freq(i,j) =
            let val CFG.BLOCK{freq=w1,...} = #node_info cfg i
                val CFG.BLOCK{freq=w2,...} = #node_info cfg j
            in  W.>(!w1,!w2) 
            end

        fun older(i,j) = A.sub(min_pred,i) < A.sub(min_pred,j)

        val marked  = Set.create M
        val node_queue = Q.create (* older *) higher_freq
        val insert_node = Q.insert node_queue

        fun node b = (b,#node_info cfg b)
        
        val make_a_block = bblock' (A,A',A'')
        fun make_block(id,B as CFG.BLOCK{id=i,
                               insns=ref [],data,labels,...}) = 
              (case #in_edges cfg i of
                  [] => map pseudo_op (!data) @ map F.LABEL (!labels)
               |  _  => make_a_block(id,B) 
              )
          | make_block(id,B) = make_a_block(id,B)

        fun update_succs (id,[])      = ()
          | update_succs (id,((i,j,_)::es)) = 
            let val count = A.sub(in_degs,j) - 1
            in  A.update(min_pred,j,Int.min(id,A.sub(min_pred,j)));
                A.update(in_degs,j,count);
                if count = 0 andalso
                   j <> EXIT andalso
                   (case CFG.fallsThruFrom(CFG,j) of SOME _ => false 
                                                   | NONE => true) then
                   insert_node j
                else ();
                update_succs(id,es)
            end
         
        fun layout(id,(i,B),waiting,blocks) =
            if Set.markAndTest(marked,i) then
                 layout_all(id,waiting,blocks)
            else let val blocks = make_block(id,B)::blocks
                 in  update_succs(id,#out_edges cfg i);
                     case CFG.fallsThruTo(CFG,i) of
                        SOME j => layout(id+1,node j,waiting,blocks)
                     |  NONE   => layout_all(id+1,waiting,blocks)
                 end

        and layout_all(id,waiting,blocks) =
          if Q.isEmpty node_queue then
             layout_waiting(id,waiting,blocks) 
          else
             let val b = Q.deleteMin node_queue
             in  layout(id,node b,waiting,blocks)
             end

        and layout_waiting(id,[],blocks) = 
               (id,List.concat(rev blocks))
          | layout_waiting(id,n::waiting,blocks) =  
              case CFG.fallsThruFrom(CFG,n) of
                 SOME _ => layout_waiting(id,waiting,blocks)
              |  NONE   => layout(id,node n,waiting,blocks)

        val _ = Set.set(marked,ENTRY)
        val _ = Set.set(marked,EXIT)
        val (id,blocks) = layout_all(0,(!firstBlock)::nodes,[])
        (*val _ = print("M="^Int.toString M^ " id="^Int.toString id^"\n")*)

        val exit    = exit'(A,A',A'',id,EXIT)
        val entry   = entry'(A,A',A'',id+1,ENTRY)
        val blocks  = blocks @ bblock A (EXIT,#node_info cfg EXIT)
        fun succs i = map (fn i=> A.sub(A,A.sub(A'',i)))
                                 (#succ cfg (A.sub(A',i)))
        fun preds i = map (fn i=> A.sub(A,A.sub(A'',i))) 
                                 (#pred cfg (A.sub(A',i)))


        fun set_links(F.BBLOCK{blknum,pred,succ,insns,...}) = 
            let fun isBackwardBranch(F.BBLOCK{blknum=next,...}::bs) =
                      next <= blknum orelse isBackwardBranch bs
                  | isBackwardBranch(_::bs) = isBackwardBranch bs
                  | isBackwardBranch []     = false
            in  pred := preds blknum; 
                succ := succs blknum;
                case !insns of
                  [] => ()
                | jmp::rest => insns := 
                     patchBranch{instr=jmp,backwards=isBackwardBranch(!succ)}
                    @rest
            end
          | set_links(F.ENTRY{blknum,succ,...}) = succ := succs blknum
          | set_links(F.EXIT{blknum,pred,...})  = pred := preds blknum
          | set_links _ = ()
        val _ = A.app set_links A
    in  F.CLUSTER{ blkCounter= ref(id+2),
                   regmap    = regmap,
                   blocks    = blocks,
                   entry     = entry,
                   exit      = exit
                 }
    end

    fun cfg2cluster {cfg=CFG as G.GRAPH cfg,relayout} =
    let val CFG.INFO{reorder,...} = #graph_info cfg
    in  if !reorder orelse relayout then computeNewLayout CFG
        else computeOldLayout CFG
    end

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:41  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:34  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:01  pscheng
# *** empty log message ***
#
 *)
