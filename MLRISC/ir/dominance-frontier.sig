signature DOMINANCE_FRONTIERS =
sig

   structure Dom : DOMINATOR_TREE

   type dominance_frontiers = Graph.node_id list Array.array

   val DFs : ('n,'e,'g) Dom.dominator_tree -> dominance_frontiers

end
