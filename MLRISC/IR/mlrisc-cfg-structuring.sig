signature CFG_STRUCTURING =
sig

   structure IR : MLRISC_IR
 
   val reshape : IR.IR ->
                 { add_preheader        : bool,
                   split_critical_edges : bool
                 } -> unit

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:25  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:05:55  pscheng
# *** empty log message ***
#
 *)
