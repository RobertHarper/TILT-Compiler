signature MIN_COST_SPANNING_TREE =
sig

   exception Unconnected

   val spanning_tree : { weight    : 'e Graph.edge -> 'w,
                         <         : 'w * 'w -> bool
                       } -> ('n, 'e, 'g) Graph.graph 
                         -> ('e Graph.edge * 'x -> 'x) -> 'x -> 'x
end
