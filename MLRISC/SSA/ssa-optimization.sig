(*
 * Signature for an SSA optimization phase
 *)
signature SSA_OPTIMIZATION =
sig

   structure SSA : SSA 

   val optimize : SSA.ssa -> SSA.ssa

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:15:05  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:28  pscheng
# *** empty log message ***
#
 *)
