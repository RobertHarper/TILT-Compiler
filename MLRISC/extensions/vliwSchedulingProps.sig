signature VLIW_SCHEDULING_PROPERTIES = sig
   structure I : PREDICATED_VLIW_INSTRUCTIONS
   structure X : CROSSPATHS
      sharing X = I.X  

   type register = I.C.register
   type latency  = int

        (* Return def/use information +
           latency for defs +
           crosspath constraints for uses
         *)
   val defUse  : I.instruction -> 
              ((register * latency) list *           (* defs *)
               (register * int * X.crosspath) list   (* uses *)
              )
   val predicate      : I.instruction -> (register * int * X.crosspath) list
   val branchLatency  : I.instruction -> latency

end


(*
 * $Log$
# Revision 1.1  99/02/17  21:15:44  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:14  pscheng
# *** empty log message ***
#
 *)
