(*
 *  Tarjan's algorithm
 *)
structure GraphSCC : GRAPH_STRONGLY_CONNECTED_COMPONENTS =
struct

   structure G = Graph
   structure A = Array

   fun scc (G.GRAPH G) process S =
   let val N = #capacity G ()
       val onstack = BitSet.create N
       val dfsnum = A.array(N,~1)
       fun dfs(v,num,stack,S) =
       let val dfsnum_v = num
           fun f([],num,stack,low_v,S) = (num,stack,low_v,S)
             | f((_,w,_)::es,num,stack,low_v,S) =
               let val dfsnum_w = A.sub(dfsnum,w)
               in  if dfsnum_w = ~1 then
                     let val (num,stack,dfsnum_w,low_w,S) = dfs(w,num,stack,S)
                     in  f(es,num,stack,Int.min(low_v,low_w),S) end
                   else
                     if dfsnum_w < dfsnum_v andalso BitSet.contains(onstack,w)
                     then f(es,num,stack,Int.min(dfsnum_w,low_v),S)
                   else
                      f(es,num,stack,low_v,S)
               end
           val _ = A.update(dfsnum,v,dfsnum_v)
           val _ = BitSet.set(onstack,v)
           val (num,stack,low_v,S) = 
                  f(#out_edges G v,num+1,v::stack,dfsnum_v,S)
           fun pop([],SCC,S) = ([],S)
             | pop(x::stack,SCC,S) =
                 let val SCC = x::SCC
                     val _   = BitSet.reset(onstack,x)
                 in  if x = v then (stack,process(SCC,S)) 
                     else pop(stack,SCC,S)
                 end
           val (stack,S) = if low_v = dfsnum_v then pop(stack,[],S) 
                           else (stack,S)
       in  (num,stack,dfsnum_v,low_v,S)
       end
       fun dfsAll([],S) = S
         | dfsAll((n,_)::nodes,S) =
           if A.sub(dfsnum,n) = ~1 then
              let val (_,_,_,_,S) = dfs(n,0,[],S)
              in  dfsAll(nodes,S) end
           else dfsAll(nodes,S)
   in  dfsAll(#nodes G (),S)
   end

end

