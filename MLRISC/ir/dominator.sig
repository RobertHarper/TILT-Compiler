signature DOMINATOR_TREE =
sig

    exception Dominator

    datatype 'n dom_node = 
       DOM of { node : 'n, level : int, preorder : int, postorder : int }
    type ('n,'e,'g) dom_info

    type ('n,'e,'g) dominator_tree     =
       ('n dom_node,unit,('n,'e,'g) dom_info) Graph.graph
    type ('n,'e,'g) postdominator_tree = 
       ('n dom_node,unit,('n,'e,'g) dom_info) Graph.graph

    type dominator_methods = 
         { dominates              : Graph.node_id * Graph.node_id -> bool,
           immediately_dominates  : Graph.node_id * Graph.node_id -> bool,
           strictly_dominates     : Graph.node_id * Graph.node_id -> bool,
           postdominates          : Graph.node_id * Graph.node_id -> bool,
           immediately_postdominates : Graph.node_id * Graph.node_id -> bool,
           strictly_postdominates : Graph.node_id * Graph.node_id -> bool,
           control_equivalent     : Graph.node_id * Graph.node_id -> bool,
           idom         : Graph.node_id -> Graph.node_id, (* ~1 if none *)
           idoms        : Graph.node_id -> Graph.node_id list,
           doms         : Graph.node_id -> Graph.node_id list,
           ipdom        : Graph.node_id -> Graph.node_id, (* ~1 if none *)
           ipdoms       : Graph.node_id -> Graph.node_id list,
           pdoms        : Graph.node_id -> Graph.node_id list,
           dom_lca      : Graph.node_id * Graph.node_id -> Graph.node_id,
           pdom_lca     : Graph.node_id * Graph.node_id -> Graph.node_id,
           dom_level    : Graph.node_id -> int,
           pdom_level   : Graph.node_id -> int,
           control_equivalent_partitions : unit -> Graph.node_id list list
         }

       (* Compute the dominator tree from a flowgraph *)
    val dominator_trees : ('n,'e,'g) Graph.graph -> 
                            ('n,'e,'g) dominator_tree * 
                            ('n,'e,'g) postdominator_tree 

    val methods    : ('n,'e,'g) dominator_tree -> dominator_methods
    val cfg        : ('n,'e,'g) dominator_tree -> ('n,'e,'g) Graph.graph
    val max_levels : ('n,'e,'g) dominator_tree -> int

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:41  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:19  pscheng
# *** empty log message ***
#
 *)
