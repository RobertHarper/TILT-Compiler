signature STATIC_SINGLE_ASSIGNMENT_FORM =
sig

   structure Dom : DOMINATOR_TREE

   type var     = int 
   type phi  = var * var * var list (* orig def/def/uses *)
   type renamer = {defs : var list, uses: var list} ->
                  {defs : var list, uses: var list}
   type copy    = {dst : var list, src: var list} -> unit

         (* 
          * Given a set of definitions for each block,
          * Compute the set of phi nodes.
          *)
   val compute_ssa : 
       ('n,'e,'g) Dom.dominator_tree ->
       { max_var      : var,  
         defs         : 'n Graph.node -> var list,
         is_live      : var * int -> bool,
         rename_var   : var -> var,
         rename_stmt  : {rename:renamer,copy:copy} -> 'n Graph.node -> unit,
         insert_phi   : {block    : 'n Graph.node,
                         in_edges : 'e Graph.edge list,
                         phis     : phi list 
                        } -> unit
       } -> unit
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:14  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:49  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:31  pscheng
# *** empty log message ***
#
 *)
