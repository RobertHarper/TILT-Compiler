(* Interference graphs and operations on them *)

signature IFGRAPH =
sig
   type node
   type graph
   val empty : unit -> graph
   val copy : graph -> graph

   val nodes : graph -> node list

   (* return list of ndoes, excluding physical registers *)

   val nodes_excluding_physical : graph -> node list
   val edges : graph -> node -> node list
   val degree : graph -> node -> int
   val insert_node : graph -> node -> unit
   val delete_node : graph -> node -> unit
   val insert_edge : graph -> node * node -> unit
   val print_stats : graph-> unit
end

