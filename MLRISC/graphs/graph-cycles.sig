signature GRAPH_SIMPLE_CYCLES = 
sig

      (* enumerate all simple cycles *)

   val cycles : ('n,'e,'g) Graph.graph -> 
		    ('e Graph.edge list * 'a -> 'a) -> 'a -> 'a

end

(* 
 * $Log$
# Revision 1.1  99/02/17  21:15:50  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:20  pscheng
# *** empty log message ***
#
 *)
