signature CLOSED_SEMI_RING =
sig

  type elem

  val zero : elem
  val one  : elem
  val +    : elem * elem -> elem
  val *    : elem * elem -> elem
  val star : elem -> elem

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:59  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:46  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:17  pscheng
# *** empty log message ***
#
 *)
