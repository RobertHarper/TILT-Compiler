signature NODE =
sig
    type node
    val toString : node -> string	(* for debugging *)
    type 'a hash_table
    val insert : 'a hash_table -> node * 'a -> unit
    val lookup : 'a hash_table -> node -> 'a
    val find : 'a hash_table -> node -> 'a option
    val copy : 'a hash_table -> 'a hash_table
    val make : unit -> 'a hash_table
end

signature DIRECTEDGRAPH =
sig
    structure Node : NODE
    type node
	sharing type node = Node.node
    type graph

    val empty : node -> graph

    val insert_node : graph -> node -> unit
    val insert_edge : graph -> node * node -> unit
    val delete_edge : graph -> node * node -> unit

    val copy : graph -> graph
    val rev : graph -> graph

    val nodecount : graph -> int
    val edgecount : graph -> int

    val nodes : graph -> node list
    val edges : graph -> node -> node list

    (*
	See graphutil.sig.sml for a description of topsort and scc.
	Reachable returns an unsorted list of all nodes reachable from
	the given nodes.
    *)
    val topsort : graph -> node list
    val scc : graph -> node list list
    val reachable : graph -> node list -> node list

    (*
	Ignoring errors, we have
		edgesRev = edges o rev
	except that edgesRev may be faster.
    *)
    val edgesRev : graph -> node -> node list
end

signature LABELLEDGRAPH =
sig
    structure Node : NODE
    type node
	sharing type node = Node.node
    type 'a graph

    val empty : node -> 'a graph

    val insert_node : 'a graph -> node * 'a -> unit
    val insert_edge : 'a graph -> node * node -> unit
    val delete_edge : 'a graph -> node * node -> unit

    val copy : 'a graph -> 'a graph
    val rev : 'a graph -> 'a graph

    val attribute : 'a graph -> node -> 'a

    val nodecount : 'a graph -> int
    val edgecount : 'a graph -> int

    val nodes : 'a graph -> node list
    val edges : 'a graph -> node -> node list

    val topsort : 'a graph -> node list
    val scc : 'a graph -> node list list
    val reachable : 'a graph -> node list -> node list

    val edgesRev : 'a graph -> node -> node list
end
