(*
 *  Use the DJ-graph algorithm to compute dominance frontiers
 *  and iterated-dominance frontiers in O(n) time.
 *  Here, the dominator tree is treated as a DJ-graph.
 *)

signature DJ_GRAPH =
sig

    structure Dom : DOMINATOR_TREE

    type ('n,'e,'g) dj_graph = ('n,'e,'g) Dom.dominator_tree

    val dj_graph : ('n,'e,'g) dj_graph -> 
        {  DF   : Graph.node_id -> Graph.node_id list,
           IDF  : Graph.node_id -> Graph.node_id list,
           IDFs : Graph.node_id list -> Graph.node_id list
        }
end

(*
 * $Log
 *)
