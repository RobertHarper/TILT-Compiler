signature MLRISC_PROFILING =
sig

   val profiling        : bool ref
   val use_profile_info : bool ref
   val edge_counts      : int Array.array Array.array ref
   val edge_table       : (int * int) list Array.array ref
   val module_table     : (string * int * int) list ref
   val module           : string ref
   val cluster_id       : int ref
   val new_cluster      : int * (int * int) list -> unit
   val get_cluster_id   : (unit -> int) ref

end

structure MLRISC_Profiling : MLRISC_PROFILING =
struct

   structure A = Array

   val profiling        = ref false
   val use_profile_info = ref false    
   val edge_counts      = ref (A.array(0,A.array(0,0)))
   val edge_table       = ref (A.array(0,[(0,0)]))
   val module_table     = ref []       : (string * int * int) list ref
   val module           = ref ""
   val cluster_id       = ref 0

   fun get_id() = let val id = !cluster_id in cluster_id := 1 + id; id end

   val get_cluster_id = ref get_id

   fun initialize() = Unsafe.setVar(!edge_counts)

   fun grow n =
   let val m = A.length(!edge_counts) * 2
       val m = if m < n then n else m
       val m = if m < 10 then 10 else m
       val new_edge_counts = A.array(m,A.array(0,0))
       val new_edge_table  = A.array(m,[])
   in  A.copy{src= !edge_counts, dst=new_edge_counts,si=0,di=0,len=NONE};
       edge_counts := new_edge_counts;
       A.copy{src= !edge_table,  dst=new_edge_table, si=0,di=0,len=NONE};
       edge_table := new_edge_table;
       initialize()
   end

   fun new_cluster(cluster_id,edges) =
      let val n = length edges
      in  if A.length(!edge_counts) <= cluster_id then grow cluster_id else ();
          A.update(!edge_table,cluster_id,edges);
          A.update(!edge_counts,cluster_id,A.array(n,0))
      end

end

(*
 * $Log$
# Revision 1.1  99/02/17  22:33:26  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:01  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.2  1998/11/16 21:47:28  george
 *   Version 110.10
 *
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
