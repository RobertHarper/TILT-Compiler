signature TRANSACTION =
sig

   exception Abort

   val transaction : 'a -> (unit -> 'a) -> 'a

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:22  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:15  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:10  pscheng
# *** empty log message ***
#
 *)
