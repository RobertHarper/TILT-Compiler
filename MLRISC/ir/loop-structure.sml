functor LoopStructureFn (structure GraphImpl : GRAPH_IMPLEMENTATION
                         structure Dom       : DOMINATOR_TREE)
    : LOOP_STRUCTURE =
struct
 
   structure G   = Graph
   structure GI  = GraphImpl
   structure Dom = Dom
   structure A   = Array

   datatype ('n,'e,'g) loop = 
      LOOP of { nesting    : int,
                header     : G.node_id,
                loop_nodes : G.node_id list,
                backedges  : 'e G.edge list,
                exits      : 'e G.edge list
              }

   datatype ('n,'e,'g) loop_info = 
       INFO of { dom : ('n,'e,'g) Dom.dominator_tree }

   type ('n,'e,'g) loop_structure = 
         (('n,'e,'g) loop, unit, ('n,'e,'g) loop_info) Graph.graph 

   fun loop_structure DOM =
   let val info               = INFO{ dom = DOM }
       val LS as G.GRAPH ls   = GI.graph ("Loop structure",info,10) 
       val G.GRAPH cfg        = Dom.cfg DOM
       val G.GRAPH dom        = DOM
       val N                  = #capacity dom ()
       val M                  = Dom.methods DOM
       val dominates          = #dominates M
       val [ENTRY]            = #entries cfg ()
       val headers            = A.array(N,~1)

       fun find_loops header level i =
       let val backedges = List.filter (fn (j,i,_) => dominates(i,j)) 
                                (#in_edges cfg i)
           val is_header = case backedges of [] => i = ENTRY
                                           | _  => true
       in  if is_header 
           then (* i is now the new loop header *)
           let    (* find all nested loops first *)
               val _ = app (find_loops i (level+1)) (#succ dom i)
               fun find_loop_nodes((j,_,_)::es,nodes) = 
                   let val h = A.sub(headers,j)
                   in  if h < 0 then 
                       (A.update(headers,j,i);
                        find_loop_nodes(#in_edges cfg j,
                           find_loop_nodes(es,j::nodes)))
                       else if h = i then
                           find_loop_nodes(es,nodes)
                       else (A.update(headers,h,i);
                             A.update(headers,j,i);
                              find_loop_nodes(#in_edges cfg h,
                               find_loop_nodes(es,nodes)))
                   end
                 | find_loop_nodes([],nodes) = nodes
               fun find_entry_loop_nodes() =
                  map #1 (List.filter (fn (i,n) => A.sub(headers,i) = ~1)
                            (#nodes cfg ()))

               fun find_exits(header,[],exits) = exits
                 | find_exits(header,i::is,exits) =
                   let fun f((e as (i,j,_))::es,exits) =
                         if A.sub(headers,j) = header then f(es,exits)
                         else f(es,e::exits)
                         | f([], exits) = exits
                   in  find_exits(header,is,f(#out_edges cfg i,exits)) end
               val _     = A.update(headers,i,i)
               val nodes = if i = ENTRY then
                              find_entry_loop_nodes()
                           else
                              find_loop_nodes(backedges,[])
               val exits = if i = ENTRY then [] 
                           else find_exits(i,i::nodes,[])
               val loop  = LOOP { nesting    = level,
                                  header     = i,
                                  backedges  = backedges,
                                  loop_nodes = nodes,
                                  exits      = exits
                                }
           in  #add_node ls (i,loop);
               if i = ENTRY then () else #add_edge ls (header,i,()) 
           end
           else app (find_loops header level) (#succ dom i)
       end
   in  
       find_loops ENTRY 0 ENTRY;
       #set_entries ls [ENTRY];
       LS
   end

   fun nesting_level(G.GRAPH L) =
       let val INFO{dom=G.GRAPH dom,...} = #graph_info L
           val N                         = #capacity dom ()
           val levels                    = A.array(N,0)
           fun tabulate(_,LOOP{nesting,header,loop_nodes,...}) =
               (A.update(levels,header,nesting);
                app (fn i => A.update(levels,i,nesting)) loop_nodes)
       in  #forall_nodes L tabulate;
           levels
       end

   fun header(G.GRAPH L) = 
       let val INFO{dom=G.GRAPH dom,...} = #graph_info L
           val N                         = #capacity dom ()
           val headers                   = A.array(N,0)
           fun tabulate(_,LOOP{header,loop_nodes,...}) =
               (A.update(headers,header,header);
                app (fn i => A.update(headers,i,header)) loop_nodes)
       in  #forall_nodes L tabulate;
           headers
       end

end    

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:14  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:48  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:29  pscheng
# *** empty log message ***
#
 *)
