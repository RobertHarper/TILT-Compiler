structure GraphIsCyclic : GRAPH_IS_CYCLIC = 
struct

   structure G = Graph

   exception Cyclic

   (*
    * Cyclic test
    *)
   fun is_cyclic (G.GRAPH G) = 
   let val N       = #capacity G () 
       val visited = BitSet.create N
       val done    = BitSet.create N
       fun dfs i =
          if BitSet.markAndTest(visited,i) then
             if BitSet.contains(done,i) then ()
             else raise Cyclic
          else 
             (dfsSucc(#out_edges G i);
              BitSet.set(done,i))
       and dfs'(i,_) = dfs i
       and dfsSucc [] = ()
         | dfsSucc((_,j,_)::es) = (dfs j; dfsSucc es)
   in
       (#forall_nodes G dfs'; false) handle Cyclic => true
   end

end

(* 
 * $Log$
# Revision 1.2  2001/12/13  16:32:02  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:53  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:22  pscheng
# *** empty log message ***
#
 *)
