(*
 * Restructure the branches according to the branch frequencies 
 * in the program.  Try to eliminate the number of branches within a loop.
 *)
signature RESHAPE_BRANCHES =
sig

   structure IR : MLRISC_IR

   val reshapeBranches : IR.IR -> unit

end
