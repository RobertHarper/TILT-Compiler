(*
 * This signature matches an instruction set that provides full predication
 *)

signature PREDICATED_INSTRUCTIONS =
sig
   include INSTRUCTIONS
   
   type predicate  (* basically says implement it however you want to *)

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:15:41  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:11  pscheng
# *** empty log message ***
#
 *)
