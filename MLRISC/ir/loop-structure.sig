(*
 * This module is responsible for locating loop structures.
 * All loops have only one single entry (via the header) but
 * potentially multiple exits, i.e. the header dominates all nodes
 * within the loop.   Other definitions are used for ``loops'' and ``headers''
 * in the literature.  We choose a structural definition that has nicer
 * properties.
 *)

signature LOOP_STRUCTURE =
sig

   structure Dom : DOMINATOR_TREE

   (*
    * DEF: An edge i -> j  is a backedge iff j dom i.
    *      Here, j is the header, and i -> j \in backedges(j) 
    *      A loop is identified by its header h.  
    *)
   datatype ('n,'e,'g) loop = 
      LOOP of { nesting    : int,
                header     : Graph.node_id,
                loop_nodes : Graph.node_id list,
                backedges  : 'e Graph.edge list,
                exits      : 'e Graph.edge list
              }

   type ('n,'e,'g) loop_info

   type ('n,'e,'g) loop_structure = 
        (('n,'e,'g) loop,unit, ('n,'e,'g) loop_info) Graph.graph 

          (* O(n+e) *)
   val loop_structure : ('n,'e,'g) Dom.dominator_tree ->
                        ('n,'e,'g) loop_structure 

   val nesting_level : ('n,'e,'g) loop_structure -> Graph.node_id Array.array
   val header        : ('n,'e,'g) loop_structure -> Graph.node_id Array.array

end    

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:46  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:28  pscheng
# *** empty log message ***
#
 *)
