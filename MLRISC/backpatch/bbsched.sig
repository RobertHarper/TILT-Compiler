signature BBSCHED = sig
  structure F : FLOWGRAPH

  val bbsched : F.cluster -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:52  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:23  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:54  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
