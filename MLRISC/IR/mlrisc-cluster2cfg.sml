(*
 *  Convert the old cluster format into the new control flow graph format
 *
 *)

signature CLUSTER2CFG =
sig
   structure CFG : CONTROL_FLOW_GRAPH
   structure F   : FLOWGRAPH
      sharing CFG.I = F.I
      sharing CFG.P = F.P
      sharing CFG.B = F.B

   val cluster2cfg : F.cluster -> CFG.cfg

end 

functor Cluster2CFGFn
   (structure CFG : CONTROL_FLOW_GRAPH 
    structure F   : FLOWGRAPH
    structure P   : INSN_PROPERTIES
       sharing CFG.I = F.I = P.I 
       sharing CFG.P = F.P
       sharing CFG.B = F.B
   ) : CLUSTER2CFG =
struct

    structure CFG = CFG
    structure I   = CFG.I
    structure F   = F
    structure G   = Graph
    structure W   = CFG.W

    fun error msg = MLRiscErrorMsg.impossible ("Cluster2CFG."^msg)

    fun cluster2cfg(F.CLUSTER{ blocks, entry, exit, regmap,blkCounter, ...})= 
    let fun id(F.BBLOCK{blknum, ...}) = blknum
          | id(F.ENTRY {blknum, ...}) = blknum
          | id(F.EXIT {blknum, ...})  = blknum
          | id _                      = error "id"

        fun first_block (F.BBLOCK{blknum,...}::_) = blknum
          | first_block (F.ORDERED bs::bs')       = first_block(bs @ bs')
          | first_block (_::bs)                   = first_block bs
          | first_block []                        = error "first_block"

        val info = CFG.INFO { regmap       = regmap,
                              firstBlock   = ref(first_block blocks),
                              reorder      = ref false,
                              annotations  = ref []
                             }

        val CFG as G.GRAPH cfg = CFG.cfg(info)

        val F.ENTRY{ blknum = ENTRY, ... } = entry
        val F.EXIT{ blknum = EXIT, ... }   = exit

        fun is_exit []             = false
          | is_exit (F.EXIT _::bs) = true
          | is_exit (_::bs)        = is_exit bs

            (* Add a list of blocks into the CFG *) 
        fun add(F.ENTRY e::rest,Ps,Ls)     = add_entry(e,Ps,Ls,rest)
          | add(F.EXIT e::rest,Ps,Ls)      = add_exit(e,Ps,Ls,rest)
          | add(F.BBLOCK b::rest,Ps,Ls)    = add_block(b,rev Ps,Ls,rest)
          | add((F.PSEUDO p)::rest,Ps,Ls)  = 
              add(rest,CFG.PSEUDO p::map CFG.LABEL Ls@Ps,[])
          | add((F.LABEL l)::rest,Ps,Ls)   = add(rest,Ps,l::Ls)
          | add(F.ORDERED bs::rest,Ps,Ls)  = add(bs@rest,Ps,Ls)
          | add([],Ps,Ls)                  = finish(Ps,Ls)

            (* Insert an entry node *)
        and add_entry({blknum, succ}, [], [], rest) =
              ( #add_node cfg (blknum,CFG.newStart blknum);
                #set_entries cfg [blknum];
                app (fn j => add_edge(blknum, j, CFG.JUMP)) (map id (!succ));
                add(rest, [], [])
              )
          | add_entry _ = error "add_entry"

            (* Insert an exit node *)
        and add_exit({blknum, pred}, [], [], rest) = 
              ( #add_node cfg (blknum, CFG.newStop blknum);
                #set_exits cfg [blknum];
                add(rest, [], [])
              )
  
            (* Insert an normal basic block *)
       and add_block({blknum,name,liveIn,liveOut,succ,pred,insns},
                     Ps,Ls,rest) =
           let val bb = CFG.BLOCK{id    = blknum,
                                  kind  = CFG.NORMAL,
                                  labels= ref Ls,
                                  freq  = ref W.zero,
                                  data  = ref Ps,
                                  insns = insns,
                                  name  = name,
                                  annotations=ref [CFG.LIVEOUT (!liveOut)]
                                 }
           in  #add_node cfg (blknum,bb);
               add_edges(blknum, !succ, !insns);
               (*
               add_call_edges(blknum, !callSucc);
               add_return_edge(blknum, !returnSucc);
                *)
               add(rest, [], [])
           end

            (* Finished insertion *)
       and finish([],[]) = ()
         | finish(Ps,[]) = 
               let val CFG.BLOCK{data,labels,...} = #node_info cfg EXIT
               in  data := Ps @ !data
               end
         | finish _ = error "finish"

            (* Insert one edge into the flowgraph *)
       and add_edge(i,j,k) =
           let val k = if ENTRY = i then CFG.ENTRY
                       else if EXIT = j then CFG.EXIT
                       else k
           in  #add_edge cfg (i,j,CFG.EDGE{k=k,w=ref W.zero,a=ref []})
           end

            (* Add edges into the flowgraph *)
       and add_edges (i, succs, insns) = 
           let fun is_fallsthru (j,yes,no) =
                   if j = i + 1 then
                      (case insns of
                         jmp::_ => (case P.instrKind jmp of
                                      P.IK_JUMP => no
                                    | _         => yes
                                   )
                      |  _ => yes)
                   else no
               fun add_branch(i,j,k) =
                   if j = i + 1 then
                     ( add_edge(i,j,CFG.BRANCH false);
                       add_edge(i,k,CFG.BRANCH true))
                   else if k = i + 1 then
                     ( add_edge(i,k,CFG.BRANCH false);
                       add_edge(i,j,CFG.BRANCH true))
                   else error "add_branch"
               fun add_switch(i,_,[]) = ()
                 | add_switch(i,k,j::js) =
                   (add_edge(i,j,CFG.SWITCH k); add_switch(i,k+1,js))
           in  case map id succs of
                 []    => ()
               | [j]   => add_edge(i,j,is_fallsthru(j,CFG.FALLSTHRU,CFG.JUMP))
               | [j,k] => add_branch(i,j,k)
               | js    => add_switch(i,0,js)
           end

           fun insert_postentries () = 
           let fun add_edge(i,j) =
                   #add_edge cfg 
                 (i,j,CFG.EDGE{k=CFG.ENTRY,w=ref W.zero,a=ref []})
               fun postentry(i,j,e) = 
                   case #in_edges cfg j of 
                     [_] => () (* only edge from ENTRY, don't bother *)
                   | _   => 
                   let val k = #new_id cfg ()
                   in  #add_node cfg (k,CFG.newFunctionEntry(k));
                       CFG.removeEdge CFG (i,j,e);
                       add_edge(i, k);
                       add_edge(k, j)
                   end
                val entries = #out_edges cfg ENTRY
           in   app postentry entries
           end

           (* add edge from entry to exit *)
           fun insert_entry_to_exit () = add_edge (ENTRY,EXIT,CFG.JUMP)
    in 
        add(entry::exit::blocks,[],[]);
        insert_postentries(); 
        insert_entry_to_exit(); 
        CFG
    end

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:35  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:02  pscheng
# *** empty log message ***
#
 *)
