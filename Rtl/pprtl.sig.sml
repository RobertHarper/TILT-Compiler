(*$import Formatter RTL Rtltags *)

signature PPRTL =
sig

   val regi2s  : Rtl.regi -> string
   val regf2s  : Rtl.regf -> string
   val reg2s   : Rtl.reg -> string
   val var2s   : Rtl.var -> string
   val label2s : Rtl.label -> string
   val ea2s    : Rtl.ea -> string
   val sv2s    : Rtl.sv -> string
   val align2s : Rtl.align -> string
   val rep2s   : Rtl.rep -> string

   val pp_var' : Rtl.var -> Formatter.format
   val pp_Instr' : Rtl.instr -> Formatter.format
   val pp_Data' : Rtl.data -> Formatter.format
   val pp_Proc' : Rtl.proc -> Formatter.format
   val pp_Module' : Rtl.module -> Formatter.format
   val pp_tag': Rtltags.tag -> Formatter.format

   val pp_var : Rtl.var -> unit
   val pp_Instr : Rtl.instr -> unit
   val pp_Data : Rtl.data -> unit
   val pp_Proc : Rtl.proc -> unit
   val pp_Module : Rtl.module -> unit
   val pp_tag: Rtltags.tag -> unit

   val elideSave : bool ref
end

