(* Translation of RTL to pre-annotated assembly *)

signature TOASM =
sig
   structure Bblock : BBLOCK
   structure Tracetable : TRACETABLE

   structure Machineutils : MACHINEUTILS
   structure Rtl : RTL


   sharing Machineutils = Bblock.Machineutils
   sharing Machineutils.Machine = Tracetable.Machine 
   sharing Machineutils.Machine.Rtl = Rtl



   val translateIReg         : Rtl.regi -> Machineutils.Machine.register
   val translateFReg         : Rtl.regf -> Machineutils.Machine.register
   val translateLocalLabel   : Rtl.local_label -> Machineutils.Machine.loclabel

   val untranslateLocalLabel : Machineutils.Machine.loclabel -> Rtl.local_label

   (* Returns list of labels for all the basic blocks, in the same
      order as the blocks appeared in the Rtl code, and the
      mapping from labels to basic blocks.

      Also returns set of registers much must be homed to the stack.
      These are registers used in COMPUTEs.*)
   val translateProc   : Rtl.proc -> 
     (Machineutils.Machine.loclabel list) * (Bblock.bblock Machineutils.Labelmap.map)
     * (Tracetable.trace Machineutils.Regmap.map)
     * (Machineutils.Machine.stacklocation Machineutils.Regmap.map)
				    
   val translateRep : Rtl.rep -> Tracetable.trace
end
