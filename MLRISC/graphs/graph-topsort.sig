signature GRAPH_TOPOLOGICAL_SORT = 
sig

      (* topological sort *)

   val topsort : ('n,'e,'g) Graph.graph -> 
		    Graph.node_id list -> Graph.node_id list

end

(* 
 * $Log$
# Revision 1.1  99/02/17  21:15:55  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:24  pscheng
# *** empty log message ***
#
 *)
