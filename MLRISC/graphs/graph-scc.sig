signature GRAPH_STRONGLY_CONNECTED_COMPONENTS = 
sig

      (* strongly connected components *)

   val scc : ('n,'e,'g) Graph.graph -> 
		    (Graph.node_id list * 'a -> 'a) -> 'a -> 'a

end
