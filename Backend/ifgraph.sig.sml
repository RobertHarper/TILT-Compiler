(*$import Core *)
(* Interference graphs and operations on them *)

signature IFGRAPH =
sig

   type node = Core.register
   type graph

   val empty : unit -> graph
   val copy : graph -> graph

   val nodes : graph -> Core.Regset.set

   (* return set of nodes, excluding physical registers *)

   val nodes_excluding_physical : graph -> Core.Regset.set
   val edges : graph -> node -> Core.Regset.set
   val degree : graph -> node -> int
   val insert_node : graph -> node -> unit
   val insert_edge : graph -> node * node -> unit
   val insert_edges : graph -> node list * Core.Regset.set -> unit (* add pairwise edges *)
   val delete_node : graph -> node -> unit
   val print_stats : graph-> unit


end



