structure Dijkstra : SINGLE_SOURCE_SHORTEST_PATHS =
struct

   structure Q = NodePriorityQueueFn(Array)
   structure G = Graph
   structure A = Array

   fun single_source_shortest_paths { weight, <, +, inf, zero } =
   let fun dijkstra (G' as G.GRAPH G) s =
       let val N    = #capacity G ()
           val dist = A.array(N, inf)
           val pi   = A.array(N, ~1)
           val _    = A.update(dist,s,zero)
           fun less(i,j) = A.sub(dist,i) < A.sub(dist,j)
           val Q  = Q.fromGraph less G'
           fun relax(e as (u,v,_)) =
           let val d_v = A.sub(dist,v)
               val d_x = A.sub(dist,u) + weight e
           in  if d_x < d_v then 
                  (A.update(dist,v,d_x); 
                   A.update(pi,v,u); 
                   Q.decreaseWeight(Q,v))
               else ()
           end
       in  
           (while true do
              app relax (#out_edges G (Q.deleteMin Q))
           ) handle Q.EmptyPriorityQueue => ();
           { dist = dist,
             pred = pi
           }
       end
   in
       dijkstra
   end
        
end
