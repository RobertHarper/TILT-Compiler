(*
 * Insert various types of dummy blocks into the CFG
 *)

signature CONTROL_FLOW_GRAPH_RESTRUCTURE = 
sig

   structure Loop : LOOP_STRUCTURE

   val restructure : 
        ('n,'e,'g) Graph.graph * ('n,'e,'g) Loop.loop_structure -> 
             { add_preheader    : ({header  : 'n Graph.node,
                                    entries : 'e Graph.edge list
                                   } -> unit) option,
               add_landing_pad  : ({exit:'e Graph.edge} -> unit) option
             } -> unit

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:38  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:14  pscheng
# *** empty log message ***
#
 *)
