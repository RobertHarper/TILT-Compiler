(* Interference graphs and operations on them *)

signature IFGRAPH =
sig
   type node
   type graph
   structure Regset : ORD_SET where type Key.ord_key = node

   val empty : unit -> graph
   val copy : graph -> graph

   val nodes : graph -> Regset.set

   (* return set of nodes, excluding physical registers *)

   val nodes_excluding_physical : graph -> Regset.set
   val edges : graph -> node -> Regset.set
   val degree : graph -> node -> int
   val insert_node : graph -> node -> unit
   val delete_node : graph -> node -> unit
   val insert_edge : graph -> node * node -> unit
   val print_stats : graph-> unit

end

