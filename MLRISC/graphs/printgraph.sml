(*
 *  Print a graph 
 *)

signature PRINT_GRAPH =
sig

   val toString : ('n,'e,'g) Graph.graph -> string

end

structure PrintGraph : PRINT_GRAPH =
struct

   structure G = Graph

   fun toString (G.GRAPH G) =
   let fun showEdges es = 
          String.concat(
             map (fn (i,j,_) => Int.toString i^" -> "^Int.toString j^"\n") es)
       fun showNodes ns = 
          String.concat(map (fn n => Int.toString n^" ") ns)^"\n"
   in     
       #name G ^ "\n" ^
       "nodes: "^showNodes(map #1 (#nodes G ()))^
       "edges:\n"^showEdges(#edges G ())^
       "entry edges:\n"^
           showEdges(List.concat(map (#entry_edges G o #1) (#nodes G ())))^ 
       "exit edges:\n"^
           showEdges(List.concat(map (#exit_edges G o #1) (#nodes G ())))^ 
       "entries: "^showNodes(#entries G ())^
       "exits: "^showNodes(#exits G ())
   end
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:05  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:05  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:35  pscheng
# *** empty log message ***
#
 *)
