signature DOMINANCE_FRONTIERS =
sig

   structure Dom : DOMINATOR_TREE

   type dominance_frontiers = Graph.node_id list Array.array

   val DFs : ('n,'e,'g) Dom.dominator_tree -> dominance_frontiers

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:13  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:40  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:16  pscheng
# *** empty log message ***
#
 *)
