(*$import Prelude TopLevel GRAPH Int TextIO String List Array Util HashString HashTableFn *)

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

   fun delete_edge (g as GRAPH {edges,...},(n1,n2)) =
       let val index_n1 = addhash(g,n1)
	   val index_n2 = addhash(g,n2)
           fun remove (h :: t) = if h=index_n2 then t else h :: (remove t)
             | remove [] = []
       in Array.update(!edges,index_n1,remove(Array.sub(!edges,index_n1)))
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

  local
      datatype color = White | Black | Gray
      val noParent = ~1
      type parent = int Array.array	(* maps each node to it's parent or noParent *)
      type path = int list		(* [n1,...,nk] is a path from n1 to nk *)

      (* cyclePath : parent * int * int -> path
       * Given the first and last nodes of a cycle, use parent links to compute
       * the path from first to last.
       *)
      fun cyclePath (parent,first,last) =
	  let	      
	      (* The only way that a parent link can be noParent is if last = first.
	       * The loop is structured to deal with that case.
	       *)
	      fun loop u acc =
		  if u = first then acc
		  else let val v = Array.sub(parent, u)
		       in  loop v (v::acc)
		       end
	  in
	      loop last [last]
	  end

      exception Cycle of path
      fun foundCycle x = raise Cycle (cyclePath x)

      fun dfsVisit (G as (edges, color, parent), u) =
	  let val _ = Array.update(color,u,Gray)
	      val edgeList = Array.sub(edges,u)
	      fun mapper v = case Array.sub(color,v)
			       of White => (Array.update(parent,v,u);
					    dfsVisit (G,v))
				| Gray => foundCycle (parent,v,u)
				| Black => ()
	  in
	      app mapper edgeList;
	      Array.update(color,u,Black)
	  end

      fun dfs (G as (edges, color, parent)) =
	  for (0, Array.length edges,
	       fn v => if Array.sub(color, v) = White
			   then dfsVisit (G,v)
		       else ())
  in
      (* findCycle : graph -> node list *)
      fun findCycle (g as GRAPH {nodes=ref nodes, edges=ref edges,...}) =
	  let val count = Array.length edges
	      val color = Array.array(count, White)
	      val parent = Array.array(count, noParent)
	  in
	      (dfs (edges,color,parent); nil)
	      handle Cycle c => map (fn i => Array.sub(nodes, i)) c
	  end
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

structure Dag :> DAG = 
struct

    val error = fn str => Util.error "graph.sml" str
    exception HashUnknownNode
    local
	structure HashKey =
	    struct
		type hash_key = string
		val hashVal = HashString.hashString 
		val sameKey = (op = : string * string -> bool)
	    end
	structure HashTable = HashTableFn(HashKey)
	structure Node = 
	    struct
		type node = string
		open HashTable
		fun make i = HashTable.mkTable(i,HashUnknownNode)
	    end
    in
	structure Graph = Graph(Node)
    end			

    structure StringMap = Util.StringMap
    structure StringOrderedSet = Util.StringOrderedSet
	
    type node = string
    exception UnknownNode of node
    exception Cycle of node list
    type 'a graph = {main        : Graph.graph,                      (* Primary info *)
		     attributes : (int * 'a) StringMap.map ref,  
		     cached      : bool ref,                         (* Cache up-to-date? *)
		     reverse     : Graph.graph ref,                  (* Cached info *)
		     upInfo      : {ancestorWeight : int,
				    ancestors : StringOrderedSet.set}
		                      StringMap.map ref,
		     downInfo    : {descendentWeight : int}
		                      StringMap.map ref}

    fun empty () = {main = Graph.empty "", 
		    attributes = ref StringMap.empty,
		    cached = ref false,
		    reverse = ref (Graph.empty ""),
		    upInfo = ref StringMap.empty,
		    downInfo = ref StringMap.empty}

    fun copy ({main,attributes,...} : 'a graph) : 'a graph = 
	{main = Graph.copy main,
	 attributes = ref (!attributes),
	 cached = ref false,
	 reverse = ref (Graph.empty ""),
	 upInfo = ref StringMap.empty,
	 downInfo = ref StringMap.empty}

    fun flush ({cached, ...} : 'a graph) = cached := false

    (* Modifications to the graph require flushing the cached information *)
    fun insert_node (g as {main, attributes, ...} : 'a graph, node, weight, attr) = 
	(flush g;
	 Graph.insert_node(main, node); 
	 attributes := (StringMap.insert(!attributes, node, (weight, attr))))
	 
    fun insert_edge (g as {main, ...} : 'a graph, src, dest) = 
	(flush g;
	 Graph.insert_edge(main, (src, dest)))

    fun has_edge (g as {main, ...} : 'a graph, src, dest) =
	let val edges = Graph.edges main (Graph.hash main src)
	    val hash = Graph.hash main dest
	in  List.exists (fn h => h = hash) edges
	end
    
    (* Queries requiring only inspection of non-cached infomation *)
    fun isNode ({attributes, ...} : 'a graph, node) = 
	(case StringMap.find(!attributes, node) of
	     NONE => false
	   | SOME _ => true)
    fun nodeWeight ({attributes, ...} : 'a graph, node) = 
	(case StringMap.find(!attributes, node) of
	     NONE => raise (UnknownNode node)
	   | SOME (w,_) => w)
    fun nodeAttribute ({attributes, ...} : 'a graph, node) = 
	(case StringMap.find(!attributes, node) of
	     NONE => raise (UnknownNode node)
	   | SOME (_,a) => a)
    fun nodes ({main,...} : 'a graph) = Graph.nodes main
    fun numNodes ({main,attributes,...} : 'a graph) = 
	let val l1 = length(Graph.nodes main)
	    val l2 = StringMap.numItems (!attributes)
	    val _ = if (l1=l2) then ()
		    else (print "numNodes detected inconsistency l1 = ";
			  print (Int.toString l1); print "  l2 = ";
			  print (Int.toString l2); print "\n")
	in  l1
	end
    fun children (g as {main,...} : 'a graph, node : string) = 
	let val edges = Graph.edges main (Graph.hash main node)
	in  map (Graph.unhash main) edges
	end
    val children' = children
    fun numChildren(g as {main,...} : 'a graph, node : string) = 
	length(Graph.edges main (Graph.hash main node))
    fun numEdges(g as {main,...} : 'a graph) = 
	foldl (fn (n,acc) => acc + (numChildren(g,n))) 0 (nodes g)

    (* checkAcyclic : 'a graph -> unit.  May raise Cycle. *)
    fun checkAcyclic ({main,...} : 'a graph) =
	case (Graph.findCycle main)
	  of nil => ()
	   | nodes => raise Cycle nodes
	      
    fun refresh({cached = ref true, ...} : 'a graph) = ()
      | refresh(g as {main, cached, reverse, upInfo, downInfo, ...} : 'a graph) = 
	let val _ = checkAcyclic g
	    val _ = reverse := Graph.rev main
	    val _ = upInfo := StringMap.empty
	    val _ = downInfo := StringMap.empty
	    fun get_upInfo node = 
		(case (StringMap.find(!upInfo, node)) of
		     SOME pw_a => pw_a
		   | NONE => 
			 let 	
			     val this_size = nodeWeight(g,node)
			     fun mapper hashedNode = 
				 let val node = Graph.unhash main hashedNode
				     val {ancestors,ancestorWeight} = get_upInfo node
				 in  (node, ancestors, ancestorWeight)
				 end
			     val parents_info = (map mapper 
						 (Graph.edges (!reverse) (Graph.hash main node)))
			     fun folder ((node,anc,size),(acc_anc,acc_size)) = 
				 let val size = Int.max(size + this_size, acc_size)
				     val acc_anc = StringOrderedSet.append(anc,acc_anc)
				     val acc_anc = StringOrderedSet.cons(node,acc_anc)
				 in  (acc_anc,size)
				 end
			     val i = foldl folder (StringOrderedSet.empty, 0) parents_info
			     val i = {ancestors = #1 i, ancestorWeight = #2 i}
			     val _ = upInfo := (StringMap.insert(!upInfo,node,i))
			 in  i
			 end)
	    fun get_downInfo node = 
		(case (StringMap.find(!downInfo, node)) of
		     SOME di => di
		   | NONE => 
			 let 	
			     val this_size = nodeWeight(g,node)
			     fun mapper hashedNode = 
				 let val node = Graph.unhash main hashedNode
				     val {descendentWeight} = get_downInfo node
				 in  (node, descendentWeight)
				 end
			     val children_info = (map mapper 
						  (Graph.edges main (Graph.hash main node)))
			     fun folder ((node,size),acc_size) = 
				 Int.max(size + this_size, acc_size)
			     val i = foldl folder 0 children_info
			     val i = {descendentWeight = i}
			     val _ = downInfo := (StringMap.insert(!downInfo,node,i))
			 in  i
			 end)
	in  map get_upInfo (Graph.nodes main); 
	    map get_downInfo (Graph.nodes main); 
	    cached := true
	end

    (* Queries requiring the cached parts of the graph *)
    fun ancestorWeight (g as {upInfo, ...} : 'a graph, node) = 
	(refresh g;
	 case StringMap.find(!upInfo, node) of
	     NONE => raise (UnknownNode node)
	   | SOME {ancestorWeight,...} => ancestorWeight)
    fun descendentWeight (g as {downInfo, ...} : 'a graph, node) : int = 
	(refresh g;
	 case StringMap.find(!downInfo, node) of
	     NONE => raise (UnknownNode node)
	   | SOME {descendentWeight,...} => descendentWeight)
    fun parents (g as {main, reverse,...} : 'a graph, node) = 
	let val _ = refresh g
	    val edges = Graph.edges (!reverse) (Graph.hash main node)
	in  map (Graph.unhash main) edges
	end
    fun ancestors (g as {upInfo,...} : 'a graph, node) = 
	(refresh g; 
	 (case (StringMap.find(!upInfo, node)) of
	      NONE => raise (UnknownNode node)
	    | SOME {ancestors = a,... } => StringOrderedSet.toList a))

    datatype nodeStatus = Black  (* Completed nodes whose outgoing edges will not be shown *)
	                | Gray   (* Working nodes which will be shown in boxes *)
	                | White  (* Nodes that can be worked on if there are no incoming edges *)

    fun makeDot {graph = g, out, status} =
	let val _ = TextIO.output (out, "digraph G {\n")
	    val _ = TextIO.output (out, "  size = \"8,8\"\n")
	    val _ = TextIO.output (out, "  rankdir = LR\n") 
	    val _ = TextIO.output (out, "  concentrate = true\n")  
	    fun escape "Graph" = "_Graph"
	      | escape "GRAPH" = "_GRAPH"
	      | escape str = implode (map (fn #"-" => #"_"
	                                    | c => c) (explode str))
	    val maxDescendentWeight = foldl Int.max 0 (map (fn n => descendentWeight(g,n)) (nodes g))
	    fun doNode parent =
		let val children = children(g,parent)
		    val parentName = escape parent
		    val _ = TextIO.output (out, "  " ^ parentName ^ 
					   " [label=\"" ^ parent ^"\"");
		    val weight = nodeWeight(g,parent) 
		    val _ = if (weight > 100000)
				then TextIO.output (out, ", color=red")
			    else if (weight > 50000)
				then TextIO.output (out, ", color=purple")
			    else if (weight > 10000)
				then TextIO.output (out, ", color=blue")
			    else TextIO.output (out, ", color=black")
		    val status = status parent
		    val _ = (case status of
				 White => ()
			       | Gray => TextIO.output (out, ", shape=box, peripheries=4")
			       | Black => ())
		    val _ = TextIO.output (out, "];\n")
		    fun apper child = 
			let val childName = escape child
			    val childNodeWeight = nodeWeight(g, child)
			    val childAncestorWeight = ancestorWeight(g, child)
			    val parentAncestorWeight = ancestorWeight(g, parent)
			    val percentUpMax = (real parentAncestorWeight) / 
				               (real (childAncestorWeight - childNodeWeight))
			    val percentDownMax = (real (descendentWeight(g,parent))) /
						 (real maxDescendentWeight)
			in  TextIO.output (out, "  " ^ parentName ^ " -> " ^ childName);
			    (case status of
				 White => ()
			       | Gray => ()
			       | Black => TextIO.output (out, " [color=gray]"));
			    TextIO.output (out, ";\n") 
			end
		in  app apper children
		end
	    val _ = app doNode (nodes g)
	    val _ = TextIO.output (out, "}\n") 
	in  ()
	end


    fun removeTransitive (g : 'a graph) = 
	let val g as {main,...} : 'a graph = copy g
	    fun removeEdge (parent : node) (child : node) = 
		let val unhashedChild = Graph.hash main child
		    val _ = Graph.delete_edge(main,(parent,child))
		in  if (Graph.reachable main [parent] unhashedChild)
			then ()
		    else Graph.insert_edge(main,(parent,child))
		end
	    fun doNode parent = 
		let val children = children(g,parent)
		in  app (removeEdge parent) children
		end
	    val _ = app doNode (nodes g)
	    val _ = flush g
	in  g
	end

    fun collapse (g : 'a graph, {maxWeight, maxParents, maxChildren}) = 
	let val g' : 'a graph = empty()
	    fun isRemovable node = 
		let val parents = parents(g, node)
		    val children = children(g, node)
		    val weight = nodeWeight(g, node)
		in  (weight < maxWeight) andalso
		    (length parents <= maxParents) andalso
		    (length children <= maxChildren)
		end
	    fun addNodes n = if (isRemovable n)
				 then ()
			     else insert_node(g', n, nodeWeight(g,n), nodeAttribute(g,n))
	    fun getUpwardDownward getLayer n = 
		let val nextLayer = getLayer(g,n)
		    fun folder (n,acc) = if (isRemovable n)
						  then (getUpwardDownward getLayer n) @ acc
					      else n :: acc
		in  foldl folder [] nextLayer
		end
	    fun addEdges n = 
		if (isRemovable n)
		    then let val parents = getUpwardDownward parents n
			     val children = getUpwardDownward children n
			 in  app (fn src => app (fn dest => insert_edge(g',src,dest)) children) parents
			 end
		else let val children = children(g, n)
		     in  app (fn c => if (isNode(g', c))
					  then insert_edge(g',n,c)
				      else ()) children
		     end
				
	    val originalNodes = nodes g
	    val _ = app addNodes originalNodes  (* First, add all nodes that will be eventually there. *)
	    val _ = app addEdges originalNodes  (* Now, add all edges including the additional ones
						   generated by the removal of certain nodes. *)
	    val gV = numNodes g
	    val gE = numEdges g
	    val gV' =  numNodes g'
	    val gE' = numEdges g'
	    val _ = (print "Original graph has "; 
		     print (Int.toString gV); print " nodes and ";
		     print (Int.toString gE); print " edges\n";
		     print "Collapsed graph has "; 
		     print (Int.toString gV'); print " nodes and ";
		     print (Int.toString gE'); print " edges\n")
	in  g'
	end
end
