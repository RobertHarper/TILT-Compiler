(*
 * This is Reif and Tarjan's algorithm (SIAM J Computing 1981) 
 * for computing approximate birthpoints for expressions.   
 * For each basic block B,
 *   idef(x) = { defs(v_i) | i = 1 ... n in all paths 
 *                           idom(x) v_1 v_2 ... v_n x where n >= 1 and
 *                                   v_i <> idom(x) for all 1 <= i <= n
 *             }
 *)
signature IDEFS =
sig

   type var = int

   val compute_idefs : 
       {def_use : 'n Graph.node -> var list * var list,
        cfg     : ('n,'e,'g) Graph.graph
       } ->
       { idefuse      : unit -> (RegSet.regset * RegSet.regset) Array.array,
         ipostdefuse  : unit -> (RegSet.regset * RegSet.regset) Array.array
       }

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:14  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:44  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:23  pscheng
# *** empty log message ***
#
 *)
