(*
 *  This matches a VLIW instruction set with predication
 *)

signature PREDICATED_VLIW_INSTRUCTIONS =
sig

   include VLIW_INSTRUCTIONS
   type predicate

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:58  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:42  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:12  pscheng
# *** empty log message ***
#
 *)
