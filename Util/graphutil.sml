(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

structure GraphUtil : GRAPHUTIL =
  (* see                                                                *)
  (*   "Lazy Depth-First Search and Linear Graph Algorithms in Haskell" *)
  (*   King and Launchbury                                              *)
  (*   submitted to LFP '94                                             *)
struct



  open Array

(* next 2 lines inserted by Perry to work with SML/NJ 109 *)
  fun fold f l b = List.foldr f b l
  val app = List.app

  infix 9 sub

  type Vertex = int

  datatype Tree = Tree of Vertex * Tree list

  fun post (Tree (v,ts),rest) = fold post ts (v::rest)

  fun postorder ts = fold post ts []

  fun listtree t = post (t, [])

  fun buildgraph n edgelist =
    let val forwedges = array (n, []) : Vertex list array
	fun addedge (v,w) = update (forwedges,v,w :: forwedges sub v)
    in
	app addedge edgelist;
        forwedges
    end

  fun builddualgraph n edgelist =
    let val forwedges = array (n, []) : Vertex list array
        val backedges = array (n, []) : Vertex list array
	fun addedge (v,w) =
	      (update (forwedges,v,w :: forwedges sub v);
	       update (backedges,w,v :: backedges sub w))
    in
	app addedge edgelist;
	(forwedges,backedges)
    end

  fun vertices n =
    let fun upto i = if i = n then [] else i :: upto (i+1)
    in
	upto 0
    end

  fun flatten xss = fold op@ xss []

  fun dfs vs edges =
    let val visited = array (length edges, false)

          fun visit v = if visited sub v then []
                        else (update (visited,v,true);
                              [Tree (v,visitmany (edges sub v))])

          and visitmany vs = flatten (map visit vs)

      in visitmany vs end

  fun dff n edges = dfs (vertices n) edges

  fun tsort n edges = rev (postorder (dff n edges))

  fun topsort n edgelist = tsort n (buildgraph n edgelist)
        
  fun scc n edgelist =
    let val (forwedges,backedges) = builddualgraph n edgelist
    in
        map listtree (dfs (tsort n forwedges) backedges)
    end

end
