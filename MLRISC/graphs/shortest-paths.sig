signature SINGLE_SOURCE_SHORTEST_PATHS =
sig

   val single_source_shortest_paths :
                 { weight : 'e Graph.edge -> 'w,
                   <      : 'w * 'w -> bool,
                   +      : 'w * 'w -> 'w,
                   zero   : 'w,
                   inf    : 'w 
                 } -> 
                 ('n,'e,'g') Graph.graph -> 
                 Graph.node_id -> 
                 { dist : 'w Array.array,
                   pred :  Graph.node_id Array.array
                 }
end

signature ALL_PAIRS_SHORTEST_PATHS =
sig

end
