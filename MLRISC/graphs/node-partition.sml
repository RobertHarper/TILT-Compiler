signature NODE_PARTITION =
sig

   type 'n node_partition 

   val node_partition : ('n,'e,'g) Graph.graph -> 'n node_partition
   val !!    : 'n node_partition -> Graph.node_id -> 'n Graph.node
   val ==    : 'n node_partition -> Graph.node_id * Graph.node_id -> bool
   val union : 'n node_partition -> ('n Graph.node * 'n Graph.node ->
                                        'n Graph.node) ->
                                        Graph.node_id * Graph.node_id -> bool
   val union': 'n node_partition -> Graph.node_id * Graph.node_id -> bool

end

structure NodePartition :> NODE_PARTITION =
struct

   structure U = UnionFindRef
   structure M = HashMap
   structure G = Graph

   type 'n node_partition = (G.node_id,'n G.node U.uref) M.map

   fun node_partition (G.GRAPH G) =
   let val P   = M.create { order = Int.compare, 
                            hash = fn i => i, 
                            exn = G.NotFound 
                          } (#order G () * 2)
       val ins = M.insert P
       val _   = #forall_nodes G (fn n as (i,_) => ins(i,U.uref n))
   in  P
   end

   fun !! P x          = U.!! (M.lookup P x)
   fun == P (x,y)      = U.== (M.lookup P x, M.lookup P y)
   fun union P f (x,y) = U.union f (M.lookup P x, M.lookup P y)
   fun union' P (x,y)  = U.union' (M.lookup P x, M.lookup P y)
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:04  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:02  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:33  pscheng
# *** empty log message ***
#
 *)
