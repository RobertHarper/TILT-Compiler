(*
	We have

		Node : HASH_KEY -> NODE
		Graph : NODE -> DIRECTEDGRAPH
		SpeedUpGraph : DIRECTEDGRAPH -> DIRECTEDGRAPH
		LabelGraph : DIRECTEDGRAPH -> LABELLEDGRAPH
		UnlabelGraph : LABELLEDGRAPH -> DIRECTEDGRAPH
		DebugGraph : LABELLEDGRAPH -> LABELLEDGRAPH

	UnlabelGraph exists so that functors that augment
	LABELLEDGRAPH can be applied to DIRECTEDGRAPH; for example,
	UnlabelGraph o DebugGraph o UnlabelGraph : DIRECTEDGRAPH ->
	DIRECTEDGRAPH.
*)

structure GraphHint =
struct
    val hint : int = 128
end

functor Node(structure Key : HASH_KEY
	     val toString : Key.hash_key -> string)
    :> NODE where type node = Key.hash_key =
struct
    structure H = HashTableFn (Key)
    type node = Key.hash_key
    val toString = toString
    open H
    exception Hash
    fun make () : 'a hash_table = mkTable (GraphHint.hint, Hash)
end

functor Graph(Node : NODE) :> DIRECTEDGRAPH where type node = Node.node =
struct
    structure H = Node
    structure A = Array

    structure Node = Node
    type node = H.node

    (* apply f to integers from i...j-1 *)

    fun for (i:int,j:int,f:int -> unit) : unit =
	if i<j then (f i; for (i+1,j,f)) else ()

    fun for_acc (i:int,j:int,f:int * 'a -> 'a,init:'a) : 'a =
	if i<j then for_acc (i+1,j,f,f(i,init)) else init

    fun fill_array (a:'a array,a':'a array) : unit =
	for (0,A.length a,fn i => A.update(a',i,A.sub(a,i)))

    fun copy_array (a:'a array) : 'a array =	(* assumes a nonempty *)
	let val a' = A.array (A.length a, A.sub (a,0))
	in  fill_array(a,a'); a'
	end

    (*
	graph representation:
            hashtable: used to hash vertices into integers,
	    nodes: used to unhash integers to vertices,
	    count: count of vertices,
	    edges: edges are represented as an adjacency structure.
	           For each vertex v, we keep a list of vertices l
		   such that for all v' in l, there is an edge from v
		   to v'.
      Invariants:
		count <= |nodes| = |edges|
		|nodes| > 0
		(n,i) in hashtable iff i <= count and nodes[i] = n
    *)

    type edges = int list array
    datatype graph =
	GRAPH of {hashtable : int H.hash_table,
		  nodes : node A.array ref,
		  count : int ref,
		  edges : edges ref}

    fun hash (g:graph) : node -> int =
	let val GRAPH {hashtable,...} = g
	in  H.lookup hashtable
	end

    fun unhash (g:graph) : int -> node =
	let val GRAPH {nodes=ref nodes,...} = g
	in  fn i => A.sub(nodes,i)
	end

    fun empty (n:node) : graph =
	GRAPH {hashtable=H.make(),
	       nodes=ref (A.array(GraphHint.hint,n)),
	       count=ref 0,
	       edges=ref (A.array(GraphHint.hint,nil))}

    fun insert_node (g:graph) (node:node) : unit =
	let val GRAPH {hashtable,count,nodes as ref n,edges as ref e,...} = g
	in (case H.find hashtable node
	      of SOME _ => ()
	       | NONE =>
		    let val num = !count
			val len = A.length n
			val _ = count := num + 1
			val _ = H.insert hashtable (node,num)
		    in  if num<len then
			    A.update(n,num,node)
			else
			    let val newsize = len*2+1
				val n' = A.array(newsize,node)
				val e' = A.array(newsize,nil)
			    in  fill_array(n,n');
				fill_array(e,e');
				nodes := n';
				edges := e'
			    end
		    end)
	end

    fun insert_edge (g:graph) (n1:node,n2:node) : unit =
	let val GRAPH {edges=ref edges,...} = g
	    val hash = hash g
	    val index_n1 = hash n1
	    val index_n2 = hash n2
	    val adj = A.sub(edges,index_n1)
	in  if List.exists (fn h => h=index_n2) adj then ()
	    else A.update(edges,index_n1,index_n2 :: adj)
	end

    fun delete_edge (g:graph) (n1:node,n2:node) : unit =
	let val GRAPH {edges=ref edges,...} = g
	    val hash = hash g
	    val index_n1 = hash n1
	    val index_n2 = hash n2
            fun remove (h :: t) = if h=index_n2 then t else h :: (remove t)
              | remove [] = []
	in  A.update(edges,index_n1,remove(A.sub(edges,index_n1)))
	end

    fun copy (g:graph) : graph =
	let val GRAPH {hashtable,nodes,count,edges} = g
	in  GRAPH{hashtable=H.copy hashtable,
		  nodes=ref (copy_array(!nodes)),
		  count=ref (!count),
		  edges=ref (copy_array(!edges))}
	end

    (* reverse graph in O(V+E) time *)

    fun rev (g:graph) : graph =
	let val GRAPH {edges=ref e,count=ref count,...} = g
	    val g' as GRAPH {edges=edges',...} = copy g
	    val e' = A.array(count,nil)
            (*
		scan: for all edges of the form (i,j), insert (j,i)
		into the new edge array
	    *)
	    fun scan (i:int) : unit =
		let fun addi (j:int) : unit =
			A.update(e',j,i :: A.sub(e',j))
		in  app addi (A.sub(e,i))
		end
	in  for (0,count,scan);
	    edges' := e';
	    g'
	end

    fun nodecount (g : graph) : int =
	let val GRAPH {count=ref count,...} = g
	in  count
	end

    fun edgecount (g:graph) : int =
	let val GRAPH {edges=ref edges,count=ref count,...} = g
	in  for_acc (0,count,fn (i,n) => n + length (A.sub (edges,i)),0)
	end

    fun vertices (n : int) : int list =
	List.tabulate (n, fn i => i)

    fun nodes (g:graph) : node list = map (unhash g) (vertices (nodecount g))

    fun edges (g:graph) (start:node) : node list =
	let val GRAPH {edges=ref edges,...} = g
	in  map (unhash g) (A.sub(edges,hash g start))
	end

    fun all_edges (g:graph) : (int * int) list =
	let val GRAPH{count=ref count, edges=ref edges, ...} = g
	    fun from (n : int) : (int * int) list =
		let val edges = A.sub(edges,n)
		in  map (fn j => (n,j)) edges
		end
	in  List.concat(for_acc(0,count,fn (i,acc) => from i :: acc,nil))
	end

    (*
	Based on Okasaki's code in graphutil.sml and on

		"Structured depth-first search algorithms in Haskell"
		King and Launchbury
		POPL 1995

	A little code is duplicated here from GraphUtil to avoid
	breaking down our adjacency array and passing it to GraphUtil
	as a list of edges (to be reconstructed).
    *)
    datatype tree = TREE of int * tree list

    fun post (TREE (i,ts), acc:int list) : int list =
	foldr post (i::acc) ts

    fun postorder (ts:tree list) : int list =
	foldr post nil ts

    fun listtree (t:tree) : int list = post (t,nil)

    fun pre (TREE (i,ts), acc:int list) : int list =
	i :: (foldr pre acc ts)

    fun preorder (ts:tree list) : int list =
	foldr pre nil ts

    fun dfs (edges : edges) (nodes : int list) : tree list =
	let val visited = A.array (A.length edges,false)

	    fun visit (i : int) : tree list =
		if A.sub(visited,i) then nil
		else (A.update(visited,i,true);
		      [TREE(i,visitmany (A.sub(edges,i)))])

	    and visitmany (nodes : int list) : tree list =
		List.concat(map visit nodes)

	in  visitmany nodes
	end

    fun dff (n : int) (edges : edges) : tree list =
	dfs edges (vertices n)

    fun tsort (n : int) (edges : edges) : int list =
	List.rev (postorder (dff n edges))

    fun topsort (g:graph) : node list =
	let val GRAPH {edges=ref edges,count=ref n,...} = g
	in  map (unhash g) (tsort n edges)
	end

    fun scc' (g:graph,gtranspose:graph) : node list list =
	let val GRAPH {edges=ref forwedges,count=ref n,...} = g
	    val GRAPH {edges=ref backedges,...} = gtranspose
	    val nodes : int list list =
		map listtree (dfs backedges (tsort n forwedges))
	in  Listops.mapmap (unhash g) nodes
	end

    fun scc (g:graph) : node list list = scc'(g,rev g)

    fun reachable (g:graph) (start:node list) : node list =
	let val GRAPH {edges=ref edges,count=ref n,...} = g
	    val nodes : int list = preorder (dfs edges (map (hash g) start))
	in  map (unhash g) nodes
	end

    (*
	We have to reverse the graph at each application because
	g is not persistent.
    *)
    fun edgesRev (g:graph) (start:node) : node list = edges(rev g) start
end

functor SpeedUpGraph(G : DIRECTEDGRAPH)
    :> DIRECTEDGRAPH where type node = G.node =
struct
    structure Node = G.Node
    type node = G.node
    type graph = G.graph * G.graph
    fun empty n = (G.empty n, G.empty n)
    fun insert_node (g,t) n = (G.insert_node g n; G.insert_node t n)
    fun swap (n1,n2) = (n2,n1)
    fun insert_edge (g,t) e = (G.insert_edge g e; G.insert_edge t (swap e))
    fun delete_edge (g,t) e = (G.delete_edge g e; G.delete_edge t (swap e))
    fun copy (g,t) = (G.copy g,G.copy t)
    fun rev (g,t) = (G.copy t,G.copy g)
    fun lift (f : G.graph -> 'a) ((g,_) : graph) : 'a = f g
    val nodecount = lift G.nodecount
    val edgecount = lift G.edgecount
    val nodes = lift G.nodes
    val edges = lift G.edges
    val topsort = lift G.topsort
    val scc = lift G.scc
    val reachable = lift G.reachable
    fun edgesRev (_,t) = G.edges t
end

functor LabelGraph(G : DIRECTEDGRAPH)
    :> LABELLEDGRAPH where type node = G.node =
struct
    structure Node = G.Node
    type node = G.node
    type 'a graph = G.graph * 'a Node.hash_table
    fun empty n = (G.empty n,Node.make())
    fun insert_node (g,a) (n,x) = (G.insert_node g n; Node.insert a (n,x))
    fun lift (f : G.graph -> 'a) ((g,_) : 'b graph) : 'a = f g
    val insert_edge = fn x => lift G.insert_edge x
    val delete_edge = fn x => lift G.delete_edge x
    fun copy (g,a) = (G.copy g,Node.copy a)
    fun rev (g,a) = (G.rev g,Node.copy a)
    fun attribute ((_,a) : 'a graph) = Node.lookup a
    val nodecount = fn x => lift G.nodecount x
    val edgecount = fn x => lift G.edgecount x
    val nodes = fn x => lift G.nodes x
    val edges = fn x => lift G.edges x
    val topsort = fn x => lift G.topsort x
    val scc = fn x => lift G.scc x
    val reachable = fn x => lift G.reachable x
    val edgesRev = fn x => lift G.edgesRev x
end

functor UnlabelGraph(G : LABELLEDGRAPH)
    :> DIRECTEDGRAPH where type node = G.node =
struct
    open G
    type graph = unit G.graph
    fun insert_node g n = G.insert_node g (n,())
end

functor DebugGraph(structure G : LABELLEDGRAPH)
    :> LABELLEDGRAPH where type node = G.node =
struct
    val GraphDebug = Stats.ff "GraphDebug"

    fun printmsg (msg : unit -> string) : unit = (print (msg()); print "\n")

    fun wrap (msg : unit -> string, f : unit -> 'a) : 'a =
	let val _ = if !GraphDebug then printmsg msg else ()
	    val r = (f () handle e =>
		     (print ("graph saw exception:" ^ exnMessage e ^
			     "\nduring: ");
		      printmsg msg;
		      raise e))
	in  r
	end

    fun wrap' (what:string,f:'a -> 'b) (x : 'a) : 'b =
	wrap(fn() => what, fn() => f x)

    structure Node = G.Node
    val nodeString = Node.toString
    type node = G.node
    type 'a graph = 'a G.graph
    fun empty n =
	wrap(fn() => concat["empty ",nodeString n],
	     fn() => G.empty n)
    fun insert_node g (n,x) =
	wrap(fn() => concat["insert_node ",nodeString n],
	     fn() => G.insert_node g (n,x))
    fun insert_edge g (n1,n2) =
	wrap(fn() => concat["insert_edge ",nodeString n1,",",nodeString n2],
	     fn() => G.insert_edge g (n1,n2))
    fun delete_edge g (n1,n2) =
	wrap(fn() => concat["delete_edge ",nodeString n1,",",nodeString n2],
	     fn() => G.delete_edge g (n1,n2))
    val copy = fn x => wrap'("copy",G.copy) x
    val rev = fn x => wrap'("rev",G.rev) x
    fun attribute g n =
	wrap(fn() => concat["attribute ",nodeString n],
	     fn() => G.attribute g n)
    val nodecount = fn x => wrap'("nodecount",G.nodecount) x
    val edgecount = fn x => wrap'("edgecount",G.edgecount) x
    val nodes = fn x => wrap'("nodes",G.nodes) x
    fun edges g n =
	wrap(fn() => concat["edges ",nodeString n],
	     fn() => G.edges g n)
    val topsort = fn x => wrap'("topsort",G.topsort) x
    val scc = fn x => wrap'("scc",G.scc) x
    fun reachable g start =
	wrap(fn() => concat(Listops.join " "
			    ("reachable" :: (map nodeString start))),
	     fn () => G.reachable g start)
    fun edgesRev g n =
	wrap(fn() => concat["edgesRev ",nodeString n],
	     fn() => G.edgesRev g n)
end
