signature GRAPH_DEPTH_FIRST_SEARCH = 
sig

   (* depth first search *)

   val dfs : (Graph.node_id -> unit) ->
             ('e Graph.edge -> unit) -> 
             ('n,'e,'g) Graph.graph  -> 
             Graph.node_id list -> unit

   val dfs_fold : { node_f     : Graph.node_id * 'a -> 'b,
                    node_unit  : 'b,
                    edge_f     : 'e Graph.edge * 'b * 'a -> 'a,
                    edge_unit  : 'a,
                    graph_f    : 'b * 'c -> 'c,
                    graph_unit : 'c
                  } -> ('n,'e,'g) Graph.graph -> Graph.node_id list -> 'c
                     
      (* preorder/postorder numbering *)
   val preorder_numbering  : ('n,'e,'g) Graph.graph -> int -> int Array.array
   val postorder_numbering : ('n,'e,'g) Graph.graph -> int -> int Array.array

end
