signature REGION = sig
  type region
  val stack : region
  val memory : region
  val toString : region -> string
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:28  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:28  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
