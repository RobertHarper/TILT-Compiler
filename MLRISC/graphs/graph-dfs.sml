structure GraphDFS : GRAPH_DEPTH_FIRST_SEARCH = 
struct

   structure G = Graph
   structure A = Array

   (*
    * Depth first search
    *)
   fun dfs f g (G.GRAPH G) roots =
   let val visited = BitSet.create(#capacity G ())
       val out_edges = #out_edges G
       fun traverse n =
           if BitSet.markAndTest(visited,n) then ()
           else (f n; app traverse_edge (out_edges n))
       and traverse_edge (e as (_,n,_)) =
           if BitSet.markAndTest(visited,n) then ()
           else (f n; g e; app traverse_edge (out_edges n)) 
   in  app traverse roots end

   fun dfs_fold { node_f  = f,
                  edge_f  = g,
                  graph_f = h,
                  node_unit = nu,
                  edge_unit = eu,
                  graph_unit = u
                } (G.GRAPH G) roots =
   let val visited = BitSet.create(#capacity G ())
       val out_edges = #out_edges G
       fun fold_node (n, nu) =
             if BitSet.markAndTest(visited,n) then nu
             else f(n,fold_edges(out_edges n,eu))
       and fold_edges ([], eu) = eu
         | fold_edges ((e as (_,n,_))::es, eu) =
             if BitSet.contains(visited,n) then fold_edges(es,eu)
             else g(e,fold_node(n,nu),fold_edges(es,eu))
       fun fold_nodes []      = u
         | fold_nodes (n::ns) =
             if BitSet.contains(visited,n) then fold_nodes ns
             else h(fold_node(n, nu), fold_nodes ns)
   in 
       fold_nodes roots
   end

   (*
    *  Reachability
    *)
   fun reachables G = 
       dfs_fold { node_f     = op::,
                  node_unit  = [],
                  edge_f     = fn (_,a,b) => a @ b,
                  edge_unit  = [],
                  graph_f    = op @,
                  graph_unit = [] } G


   (* 
    * Closure 
    *)

   (*
    * Topological sort
    *)
   fun topsort (G.GRAPH G) roots = 
   let val visited = BitSet.create(#capacity G ())
       val succ    = #succ G
       fun dfs (n, list) =
          if BitSet.markAndTest(visited,n) then list
          else dfs'(n,succ n,list)
       and dfs'(x,[],list)    = x::list
         | dfs'(x,n::ns,list) = dfs'(x,ns,dfs(n,list))
       and dfs''([], list)    = list
         | dfs''(n::ns, list) = dfs''(ns,dfs(n,list))
   in
       dfs''(roots,[])
   end

   fun preorder_numbering (G.GRAPH G) root =
   let val N = #capacity G ()
       val P = A.array(N,~1)
       fun f(i,n) = 
           if A.sub(P,i) = ~1 then
              let fun g([],n) = n 
                    | g((_,j,_)::es,n) = g(es,f(j,n))
              in  A.update(P,i,n); g(#out_edges G i,n+1) end
           else n
   in  f(root,0); P end

   fun postorder_numbering (G.GRAPH G) root =
   let val N = #capacity G ()
       val P = A.array(N,~2)
       fun f (i,n) = 
           if A.sub(P,i) = ~2 then
              let fun g([],n) = n
                    | g((_,j,_)::es,n) = g(es,f(j,n))
                  val _ = A.update(P,i,~1)
                  val n =  g(#out_edges G i,n) 
              in  A.update(P,i,n); n+1
              end
           else n
   in  f(root,0); P end
end


