(*$import MACHINE BBLOCK TRACETABLE Rtl *)
(* Translation of RTL to pre-annotated assembly *)

signature TOASM =
sig

   structure Machine : MACHINE
   structure Bblock : BBLOCK
   structure Tracetable : TRACETABLE


   val translateIReg         : Rtl.regi -> Machine.register
   val translateFReg         : Rtl.regf -> Machine.register
   val translateReg          : Rtl.reg -> Machine.register

   (* Returns list of labels for all the basic blocks, in the same
      order as the blocks appeared in the Rtl code, and the
      mapping from labels to basic blocks.

      Also returns set of registers much must be homed to the stack.
      These are registers used in COMPUTEs.*)
   val translateProc   : Rtl.proc -> 
     (Machine.label list) * (Bblock.bblock Core.Labelmap.map)
     * ((Machine.register option * Tracetable.trace) Core.Regmap.map)
     * (Machine.stacklocation Core.Regmap.map)
				    
   val translateRep : Rtl.rep -> Machine.register option * Tracetable.trace 

end
