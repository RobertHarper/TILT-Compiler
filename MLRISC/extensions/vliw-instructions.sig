(*
 * VLIW instructions involve functional unit assignments
 *)

signature VLIW_INSTRUCTIONS =
sig

   include INSTRUCTIONS
   structure FU : FUNITS       (* functional unit assignment *)
   structure X  : CROSSPATHS   (* for clustered architectures *)

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:58  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:43  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:13  pscheng
# *** empty log message ***
#
 *)
