(*
 * Compute Tarjan's dominator derived graph from a dominator tree
 *)

signature DERIVED_GRAPH =
sig
   structure Dom : DOMINATOR_TREE
   type ('n,'e) derived_graph = ('n,'e Graph.edge,unit) Graph.graph

   val derived_graph :   (* O(n+e) *)
        ('n,'e,'g) Dom.dominator_tree -> ('n,'e) derived_graph
end

functor DerivedGraphFn
   (structure Dom : DOMINATOR_TREE 
    structure GraphImpl : GRAPH_IMPLEMENTATION): DERIVED_GRAPH =
struct
   structure Dom = Dom
   structure G   = Graph
   structure GI  = GraphImpl
   structure A   = Array

   type ('n,'e) derived_graph = ('n,'e Graph.edge,unit) Graph.graph

   fun derived_graph (Dom as G.GRAPH dom) =
   let val N              = #capacity dom ()
       val D as G.GRAPH d = GI.graph("derived graph",(),N) 
       val G.GRAPH cfg    = Dom.cfg Dom
       val ancestors      = A.array(Dom.max_levels Dom,0)
       fun dfs lvl i = 
       let val _ = A.update(ancestors,lvl,i)
           val _ = #add_node d (i,#node_info cfg i)
           fun add_edge (e as (i,j,_)) =
               let val Dom.DOM{level,...} = #node_info dom j
               in if lvl < level then 
                     #add_edge d (i,j,e)  (* i idom j ! *)
                  else
                     #add_edge d (A.sub(ancestors,level),j,e)
               end
       in  app add_edge (#out_edges cfg i);
           app (dfs (lvl+1)) (#succ dom i)
       end
       
   in  app (dfs 0) (#entries dom ());
       #set_entries d (#entries dom ());
       D
   end
   
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:39  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:15  pscheng
# *** empty log message ***
#
 *)
