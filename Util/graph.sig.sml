(*$import TopLevel *)

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
   val app : ((node * int) * (node * int) -> unit) -> graph -> unit

   (* list of nodes reachable from start without going through excluded node.
      Does not include start *)

   val dfs_excluding_node : graph -> {exclude:int, start:int} -> int list
   val dfs : graph -> int -> int list
   val sc_components : (node -> unit) -> graph -> int list list
   val reachable : graph -> node list -> (int -> bool)
end
