(*$import Prelude TextIO *)

signature NODE =
sig
  type node
  type 'a hash_table
  val insert : '_a hash_table -> node * '_a -> unit
  val find : '_a hash_table -> node -> '_a option
  val make : int -> '_a hash_table
  val copy : '_a hash_table -> '_a hash_table
end

signature DIRECTEDGRAPH =
sig
   type node
   type graph
   val empty : node -> graph
   val copy : graph -> graph
   val rev : graph -> graph

   exception Hash
   val hash : graph -> (node -> int)
   val unhash : graph -> int -> node

   val nodes : graph -> node list
   val edges : graph -> int -> int list
   val nodecount : graph -> int

   val insert_node : graph * node -> unit
   val insert_edge : graph * (node * node) -> unit  
   val delete_edge : graph * (node * node) -> unit  
   val app : ((node * int) * (node * int) -> unit) -> graph -> unit

   (* list of nodes reachable from start without going through excluded node.
      Does not include start *)

   val dfs_excluding_node : graph -> {exclude:int, start:int} -> int list
   val dfs : graph -> int -> int list
   val sc_components : (node -> unit) -> graph -> int list list
   val reachable : graph -> node list -> (int -> bool)

   (* (findCycle g) is nil if g is acyclic.  Otherwise, it's
    * a non-empty list [n1, ..., nk] of nodes such that the edges
    * (n1,n2), ..., (n_{k-1},nk), (nk,n1) exist in g.
    *)
   val findCycle : graph -> node list
end

signature DAG =
sig
    type node = string
    type 'a graph
    exception UnknownNode of node	(* Presents the offender (see parents') *)
    exception Cycle of node list	(* Presents a cycle a' la' findCycle. *)

    val refresh : 'a graph -> unit                      (* Force the cache to be computed *)
    							(* may raise Cycle *)

    val empty : unit -> 'a graph                        (* Create a new graph *)
    val insert_node : 'a graph * node * int * 'a -> unit
    val insert_edge : 'a graph * node * node -> unit

    val numNodes : 'a graph -> int
    val numEdges : 'a graph -> int
    val has_edge : 'a graph * node * node -> bool
    val nodeAttribute : 'a graph * node -> 'a
    val nodeWeight : 'a graph * node -> int
    val ancestorWeight : 'a graph * node -> int
    val descendentWeight : 'a graph * node -> int
    val nodes : 'a graph -> node list
    val children : 'a graph * node -> node list
    val children' : 'a graph * node -> node list    (* Doesn't use cache -- safe for exn handlers  *)
    val parents : 'a graph * node -> node list
    val ancestors : 'a graph * node -> node list    (* topologically sorted with descendants first *)

    datatype nodeStatus = Black  (* Completed nodes whose outgoing edges will not be shown *)
	                | Gray   (* Working nodes which will be shown in boxes *)
	                | White  (* Nodes that can be worked on if there are no incoming edges *)
    val makeDot : {graph : 'a graph,
		   out : TextIO.outstream,
		   status : node -> nodeStatus}-> unit  
    val removeTransitive : 'a graph -> 'a graph
    val collapse : 'a graph * 
	                {maxWeight : int,
			 maxParents : int,
			 maxChildren : int} -> 'a graph  (* Create a new graph by removing node 
                                                            with weight <= maxWeight
							    and with <= maxParents parents
							    and with <= maxChildren children.
							    When a node is removed, all its
							    parents point to all its children. *)
								

				 
end

