signature GRAPH_DISPLAY =
sig

   val suffix    : unit -> string
   val program   : unit -> string
   val visualize : (string -> unit) -> GraphLayout.layout -> unit

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:31  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:55  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:10:08  pscheng
# *** empty log message ***
#
 *)
