(*
 * Acyclic subgraph adaptor
 *)

signature ACYCLIC_SUBGRAPH_VIEW = 
sig

     (* Acyclic node induced subgraph *)
   val acyclic_view : Graph.node_id list ->
                      ('n,'e,'g) Graph.graph -> 
                      ('n,'e,'g) Graph.graph 
end

structure AcyclicSubgraphView : ACYCLIC_SUBGRAPH_VIEW =
struct

   structure G = Graph
   structure A = HashArray
   structure S = Subgraph_P_View

   fun acyclic_view nodes (G as G.GRAPH g) =
   let val ord = A.array(#capacity g (),~1)
       fun order(i,[]) = ()
         | order(i,n::ns) = (A.update(ord,n,i); order(i+1,ns))
       val _ = order(0,nodes)
       fun node_p i = A.sub(ord,i) >= 0 
       fun edge_p (i,j) = 
           let val i = A.sub(ord,i)
           in  i >= 0 andalso i < A.sub(ord,j) end
   in  S.subgraph_p_view nodes node_p edge_p G
   end

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:59  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:45  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:15  pscheng
# *** empty log message ***
#
 *)
