(*$PPRTL: RTL *)

signature PPRTL =
sig
   structure Rtl : RTL
   type StringTree
   val stringtree2string : StringTree -> string
   val printst : StringTree -> unit

   val regi2s : Rtl.regi -> string
   val regf2s : Rtl.regf -> string
   val var2s : Rtl.var -> string
   val local_label2s : Rtl.local_label -> string
   val label2s : Rtl.label -> string
   val ea2s : Rtl.ea -> string
   val sv2s : Rtl.sv -> string
   val align2s : Rtl.align -> string

   val layoutSave : Rtl.save -> StringTree
   val layoutInstr : Rtl.instr -> StringTree
   val layoutData : Rtl.data -> StringTree
   val layoutProc : Rtl.proc -> StringTree
   val layoutModule : Rtl.module -> StringTree

   val printSave : Rtl.save -> unit
   val printInstr : Rtl.instr -> unit
   val printData : Rtl.data -> unit
   val printProc : Rtl.proc -> unit
   val printModule : Rtl.module -> unit
 
   val elideSave : bool ref
end

