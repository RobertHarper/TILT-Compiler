signature GRAPH_SIMPLE_CYCLES = 
sig

      (* enumerate all simple cycles *)

   val cycles : ('n,'e,'g) Graph.graph -> 
		    ('e Graph.edge list * 'a -> 'a) -> 'a -> 'a

end
