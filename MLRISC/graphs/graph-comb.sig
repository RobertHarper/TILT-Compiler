signature GRAPH_COMBINATIONS = 
sig

   (* disjoint union *)
   val sum   : ('n,'e,'g) Graph.graph * ('n,'e,'g) Graph.graph ->
                  ('n,'e,'g) Graph.graph
   val union : ('n,'e,'g) Graph.graph list -> ('n,'e,'g) Graph.graph
   val sums  : ('n,'e,'g) Graph.graph list -> ('n,'e,'g) Graph.graph

end

(* 
 * $Log$
# Revision 1.1  99/02/17  21:15:48  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:19  pscheng
# *** empty log message ***
#
 *)
