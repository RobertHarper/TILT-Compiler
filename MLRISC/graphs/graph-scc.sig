signature GRAPH_STRONGLY_CONNECTED_COMPONENTS = 
sig

      (* strongly connected components *)

   val scc : ('n,'e,'g) Graph.graph -> 
		    (Graph.node_id list * 'a -> 'a) -> 'a -> 'a

end

(* 
 * $Log$
# Revision 1.1  99/02/17  21:15:54  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:23  pscheng
# *** empty log message ***
#
 *)
