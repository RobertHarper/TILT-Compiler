structure GraphCombinations : GRAPH_COMBINATIONS = 
struct

   structure G       = Graph
   structure Union   = UnionGraphView
   structure Rename  = RenamedGraphView
   structure Rev     = ReversedGraphView

   (*
    * Disjoint union
    *)
   fun my_union (a,b) = Union.union_view (fn (x,y) => x) (a, b)
   fun sum (A as G.GRAPH a, B as G.GRAPH b) =
      my_union(A, Rename.rename_view (#capacity a ()) B)
   fun union []     = raise G.Graph "union"
     | union [a]    = a
     | union [a,b]  = my_union(a,b)
     | union (a::b) = my_union(a,union b)

   fun sums []     = raise G.Graph "sums"
     | sums [a]    = a
     | sums [a,b]  = sum(a,b)
     | sums (a::b) = sum(a,sums b)

end

(* 
 * $Log$
# Revision 1.2  2001/12/13  16:32:00  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:49  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:19  pscheng
# *** empty log message ***
#
 *)

