signature MACHINEUTILS =
sig

   structure Machine  : MACHINE 
   structure Labelmap : ORD_MAP where type Key.ord_key = Machine.loclabel
   structure Regmap   : ORD_MAP where type Key.ord_key = Machine.register
   structure Regset   : ORD_SET where type Key.ord_key = Machine.register


   val unsaved_regs  : Machine.register list (* Not to be saved across C call *)
   val C_caller_saved_regs : Machine.register list
   val C_int_args          : Machine.register list
   val C_fp_args           : Machine.register list
   val C_int_res           : Machine.register list
   val C_fp_res            : Machine.register list

   (* these registers are dedicated registers such as the heap ptr
      that are always live, and they must be saved / restored
      around C calls because they're in the C caller save set.*)

   val save_across_C : Machine.register list

   val indirect_int_args   : Machine.register list
   val indirect_fp_args    : Machine.register list
   val indirect_int_res    : Machine.register list
   val indirect_fp_res     : Machine.register list
   val indirect_caller_saved_regs   : Machine.register list
   val indirect_callee_saved_regs   : Machine.register list


   val listToSet     : Machine.register list -> Regset.set
   val setToList     : Regset.set -> Machine.register list
   val msRegSet      : Regset.set -> string
   val msRegList     : Machine.register list -> string


  val makeAsmHeader : Machine.procsig -> string
  val programHeader : string list
  val procedureHeader : Machine.Rtl.label -> string list
  val procedureTrailer : string -> string list
  val textStart : string list
  val dataStart : string list
  val CodeLabelDecl : Machine.label -> string

end


