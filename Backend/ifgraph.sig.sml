(*$import MACHINE *)
(* Interference graphs and operations on them *)

signature IFGRAPH =
sig

   structure Machine : MACHINE
   type node = Machine.register
   type graph

   val empty : unit -> graph
   val copy : graph -> graph

   val nodes : graph -> Machine.Regset.set

   (* return set of nodes, excluding physical registers *)

   val nodes_excluding_physical : graph -> Machine.Regset.set
   val edges : graph -> node -> Machine.Regset.set
   val degree : graph -> node -> int
   val insert_node : graph -> node -> unit
   val delete_node : graph -> node -> unit
   val insert_edge : graph -> node * node -> unit
   val insert_edges : graph -> node * Machine.Regset.set -> unit
   val print_stats : graph-> unit

end

