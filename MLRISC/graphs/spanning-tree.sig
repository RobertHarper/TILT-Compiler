signature MIN_COST_SPANNING_TREE =
sig

   exception Unconnected

   val spanning_tree : { weight    : 'e Graph.edge -> 'w,
                         <         : 'w * 'w -> bool
                       } -> ('n, 'e, 'g) Graph.graph 
                         -> ('e Graph.edge * 'x -> 'x) -> 'x -> 'x
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:10  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:42  pscheng
# *** empty log message ***
#
 *)
