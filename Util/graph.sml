functor Graph(A : NODE) :> DIRECTEDGRAPH where type node = A.node =
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

functor Dag (structure Node : NODE
	     val dummy : Node.node
	     val toString : Node.node -> string
	     structure Map : ORD_MAP
		 where type Key.ord_key = Node.node
	     structure OrderedSet : ORDERED_SET
		 where type item = Node.node)
    :> DAG
	where type node = Node.node =
struct

    val error = fn str => Util.error "graph.sml" str

    structure Graph = Graph(Node)

    type node = Node.node
    exception UnknownNode of node
    exception Cycle of node list
    type 'a graph =
	{(* Primary info. *)
	 main       : Graph.graph,
	 attributes : 'a Map.map ref,
	 cached     : bool ref,		(* Cache up-to-date? *)

	 (* Cached info. *)
	 reverse    : Graph.graph ref,
	 downInfo   : {descendants : OrderedSet.set} Map.map ref}

    fun empty () : 'a graph =
	{main = Graph.empty dummy,
	 attributes = ref Map.empty,
	 cached = ref false,
	 reverse = ref (Graph.empty dummy),
	 downInfo = ref Map.empty}

    fun copy ({main,attributes,...} : 'a graph) : 'a graph =
	{main = Graph.copy main,
	 attributes = ref (!attributes),
	 cached = ref false,
	 reverse = ref (Graph.empty dummy),
	 downInfo = ref Map.empty}

    fun flush ({cached, ...} : 'a graph) : unit = cached := false

    (* Modifications to the graph require flushing the cached information *)
    fun insert_node (g as {main, attributes, ...} : 'a graph, node : node,
		     attr : 'a) : unit =
	(flush g;
	 Graph.insert_node(main, node);
	 attributes := (Map.insert(!attributes, node, attr)))

    fun insert_edge (g as {main, ...} : 'a graph, src : node,
		     dest : node) : unit =
	(flush g;
	 Graph.insert_edge(main, (src, dest)))

    (* Queries requiring only inspection of non-cached information *)
    val nodes : 'a graph -> node list = fn g => Graph.nodes (#main g)
    val numNodes : 'a graph -> int = fn g => Graph.nodecount (#main g)
    fun numChildren ({main,...} : 'a graph, node : node) : int =
	length(Graph.edges main (Graph.hash main node))
    fun numEdges(g as {main,...} : 'a graph) : int =
	foldl (fn (n,acc) => acc + (numChildren(g,n))) 0 (nodes g)
    fun has_node ({attributes, ...} : 'a graph, node : node) : bool =
	(case Map.find(!attributes, node) of
	     NONE => false
	   | SOME _ => true)
    fun has_edge (g as {main, ...} : 'a graph, src : node, dest : node) : bool =
	let val edges = Graph.edges main (Graph.hash main src)
	    val hash = Graph.hash main dest
	in  List.exists (fn h => h = hash) edges
	end
    fun nodeAttribute ({attributes, ...} : 'a graph, node : node) : 'a =
	(case Map.find(!attributes, node) of
	     NONE => raise (UnknownNode node)
	   | SOME a => a)
    fun children (g as {main,...} : 'a graph, node : node) : node list =
	let val edges = Graph.edges main (Graph.hash main node)
	in  map (Graph.unhash main) edges
	end
    fun reachable' (g : Graph.graph) (nodes : node list) : OrderedSet.set =
	let val edges : node -> node list =
		let val h = Graph.hash g
		    val e = Graph.edges g
		    val u = Graph.unhash g
		in  (map u) o e o h
		end
	    fun add (n : node, acc : OrderedSet.set) : OrderedSet.set =
		if OrderedSet.member(n,acc) then acc
		else OrderedSet.cons(n, foldl add acc (edges n))
	in  foldl add OrderedSet.empty nodes
	end
    fun reachable (g : 'a graph, nodes : node list) : OrderedSet.set =
	reachable' (#main g) nodes
    fun descendants' (g : 'a graph, nodes : node list) : OrderedSet.set =
	reachable (g, List.concat (map (fn n => children (g,n)) nodes))

    (* checkAcyclic : 'a graph -> unit.  May raise Cycle. *)
    fun checkAcyclic ({main,...} : 'a graph) =
	case (Graph.findCycle main)
	  of nil => ()
	   | nodes => raise Cycle nodes

    fun refresh({cached = ref true, ...} : 'a graph) = ()
      | refresh(g as {main, cached, reverse, downInfo, ...} : 'a graph) =
	let val _ = checkAcyclic g
	    val _ = reverse := Graph.rev main
	    val _ = downInfo := Map.empty
	    fun get_downInfo node =
		(case (Map.find(!downInfo, node)) of
		     SOME di => di
		   | NONE =>
			 let
			     fun mapper hashedNode =
				 let val node = Graph.unhash main hashedNode
				     val {descendants} = get_downInfo node
				 in  (node, descendants)
				 end
			     val children_info = (map mapper
						  (Graph.edges main (Graph.hash main node)))
			     fun folder ((node,desc),acc) =
				 let val acc = OrderedSet.append(desc,acc)
				     val acc = OrderedSet.cons(node,acc)
				 in  acc
				 end
			     val i = foldl folder OrderedSet.empty children_info
			     val i = {descendants = i}
			     val _ = downInfo := (Map.insert(!downInfo,node,i))
			 in  i
			 end)
	in  app (ignore o get_downInfo) (Graph.nodes main);
	    cached := true
	end

    fun parents (g as {main, reverse,...} : 'a graph, node : node) : node list =
	let val _ = refresh g
	    val edges = Graph.edges (!reverse) (Graph.hash main node)
	in  map (Graph.unhash main) edges
	end
    fun descendants (g : 'a graph, node : node) : OrderedSet.set =
	(refresh g;
	 (case Map.find (!(#downInfo g), node)
	    of NONE => raise UnknownNode node
	     | SOME {descendants = d, ...} => d))

    val descendants = fn x => OrderedSet.toList (descendants x)
    val descendants' = fn x => OrderedSet.toList (descendants' x)
    val reachable = fn x => OrderedSet.toList (reachable x)
end
