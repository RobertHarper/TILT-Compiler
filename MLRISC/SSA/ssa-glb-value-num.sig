signature SSA_GLOBAL_VALUE_NUMBERING =
sig

   structure SSA : SSA

   val computeValueNumbers : SSA.ssa -> int Array.array
   val top : int

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:48  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:01  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:24  pscheng
# *** empty log message ***
#
 *)
