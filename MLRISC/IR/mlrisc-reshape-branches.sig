(*
 * Restructure the branches according to the branch frequencies 
 * in the program.  Try to eliminate the number of branches within a loop.
 *)
signature RESHAPE_BRANCHES =
sig

   structure IR : MLRISC_IR

   val reshapeBranches : IR.IR -> unit

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:38  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:06  pscheng
# *** empty log message ***
#
 *)
