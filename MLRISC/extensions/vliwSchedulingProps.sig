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
