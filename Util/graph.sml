(*$Graph: NODE DIRECTEDGRAPH *)

functor Graph(A : NODE) : DIRECTEDGRAPH =
struct
   open A

   (* graph representation: 
            hashtable: used to hash vertices into integers,
	    nodes: used to unhash integers to vertices,
	    count: count of vertices,
	    edges: edges are represented as an adjacency structure.
	           For each vertex v, we keep a list of vertices l
		   such that for all v' in l, there is an edge from v
		   to v'.
      Invariants: count= length (nodes) = length(edges) *)
		   
 datatype graph = GRAPH of {hashtable : int A.hash_table,
			    nodes : node Array.array ref,
	                    count : int ref,
			    edges : int list Array.array ref}

  val hint = 128

  fun empty n = GRAPH {hashtable=A.make hint,
		       nodes = ref (Array.array(hint,n)),
		       count=ref 0,
		       edges = ref (Array.array(hint,nil))}

  (* apply f to integers from i...j-1 *)

  fun for (i,j,f) =
        if i<j then (f i; for (i+1,j,f)) else ()

  fun fill_array (a,a') =
      for (0,Array.length a,fn i => Array.update(a',i,Array.sub(a,i)))

  fun copy_array a = 
       let val a' = Array.array (Array.length a,
				 Array.sub (a,0))
       in fill_array(a,a'); a'
       end
      
  fun copy (GRAPH {hashtable,nodes,count,edges}) =
      GRAPH{hashtable=A.copy hashtable,
	    nodes=ref (copy_array(!nodes)),
	    count=ref (!count),
	    edges=ref (copy_array(!edges))}

  fun nodecount (GRAPH {count,...}) = !count

  fun nodes (GRAPH {nodes=ref nodes,count=ref count,...}) = 
      let fun f i = if i<count then Array.sub(nodes,i):: f(i+1)
                    else nil
      in f 0
      end
	      
  fun edges (GRAPH{edges,...}) i = Array.sub(!edges,i)

  exception Hash
  fun hash (GRAPH {hashtable,count,...}) node =
       case A.find hashtable node
       of NONE => raise Hash
	| SOME i => i
 
  fun unhash (GRAPH {nodes,count,...}) i =
       if i>=0 andalso i< !count then Array.sub(!nodes,i) else raise Hash

  fun addhash (GRAPH{hashtable,count,nodes as ref n,
		     edges as ref e,...},node) = 
	let open Array
	in case A.find hashtable node
	   of SOME i => i
	    | NONE =>
		 let val num = !count
		 in count := !count + 1;
		    A.insert hashtable (node,num);
		    if num<length n then
			update(n,num,node)
		    else let val n' = array(length n * 2+1,node)
			     val e' = array(length n * 2+1,nil)
			 in fill_array(n,n');
			    fill_array(e,e');
			    nodes := n';
			    edges := e'
			 end;
		    num
                  end
          end (* let open Array *)
 
   fun insert_node (g,n1) = (addhash(g,n1); ())

   fun insert_edge (g as GRAPH {edges,...},(n1,n2)) =
       let val index_n1 = addhash(g,n1)
	   val index_n2 = addhash(g,n2)
           fun find (h :: t) = if h=index_n2 then true else find t
             | find nil =  false
	   fun insert l =
	       if find l then l
	       else index_n2 :: l
       in Array.update(!edges,index_n1,insert(Array.sub(!edges,index_n1)))
       end

  (* reverse graph in O(V+E) time *)

  fun rev (g as GRAPH{edges=ref e,count=ref count,...}) =
       let val g' as GRAPH {edges=edges',...} = copy g
	   val e' = Array.array(count,nil)
           (* scan: for all edges of the form (i,j), insert (j,i) into
	            the new edge array *)
	   fun scan i =
	       let fun f (h::t) =
	                 (Array.update(e',h, i :: Array.sub(e',h)); f t)
                     | f nil = ()
               in f (Array.sub(e,i))
	       end
       in for (0,count,scan);
	  edges' := e';
	  g'
       end

  fun dfs' (g,visited,start) =
    let fun dfs node =
	     if Array.sub(visited,node) then nil
             else (Array.update(visited,node,true);
		   node :: foldr (op @) nil (map dfs (edges g node)))
    in dfs start
    end

  fun dfs_excluding_node (g as GRAPH{edges,count,...}) {exclude,start} =
     let val visited = Array.array(!count,false)
	 val _ = Array.update(visited,exclude,true)
     in dfs'(g,visited,start)
     end

  fun dfs (g as GRAPH{count,...}) start =
     let val visited = Array.array(!count,false)
     in dfs'(g,visited,start)
     end

  fun reachable (g as GRAPH{count,...}) start_nodes =
     let val visited = Array.array(!count,false)
	 val hash = hash g
     in app (fn node => (dfs' (g,visited,hash node); ())) start_nodes;
	fn i => Array.sub(visited,i)
     end


  fun postfix_dfs' (g,visited,start) =
    let fun dfs node =
	     if Array.sub(visited,node) then nil
             else (Array.update(visited,node,true);
		   (foldr (op @) nil (map dfs (edges g node)))) @ [node]
    in dfs start
    end

 fun sc_components print_node (g as GRAPH{count,...}) =
   let val visited = Array.array (!count, false)
       val nodelistlist = map (fn n => postfix_dfs' (g, visited, n)) 
	                      (map (hash g) (nodes g))
       val nodelist = List.rev (foldr (op @) nil nodelistlist)
       val _ = app (print_node o (unhash g)) nodelist
       val gtranspose = rev g
       val visited = Array.array (!count, false)
       val components = map (fn n => dfs' (gtranspose, visited, n)) nodelist
       fun remove_nils [] = []
         | remove_nils (nil::xs) = remove_nils xs
         | remove_nils (x::xs) = x :: (remove_nils xs)
   in
       remove_nils components
   end

  val app = fn f => fn (g as GRAPH {edges=ref edges,nodes=ref nodes,...}) =>
      for (0,Array.length(edges),
	   fn i =>
	     let val n1 = Array.sub(nodes,i)
		 val n1pair = (n1,i)
	     in app (fn n2 => f (n1pair,(Array.sub(nodes,n2),n2)))
		    (Array.sub(edges,i))
	     end)
end
