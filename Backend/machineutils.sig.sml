signature MACHINEUTILS =
sig

   val unsaved_regs        : Core.Regset.set     (* Not to be saved across C call *)
   val C_caller_saved_regs : Core.Regset.set
   val C_int_args          : Core.register list
   val C_fp_args           : Core.register list
   val C_int_res           : Core.register list
   val C_fp_res            : Core.register list

   (* these registers are dedicated registers such as the heap ptr
      that are always live, and they must be saved / restored
      around C calls because they're in the C caller save set.*)

   val save_across_C : Core.Regset.set

   val indirect_int_args   : Core.register list
   val indirect_fp_args    : Core.register list
   val indirect_int_res    : Core.register list
   val indirect_fp_res     : Core.register list
   val indirect_caller_saved_regs   : Core.Regset.set
   val indirect_callee_saved_regs   : Core.Regset.set


   val listToSet     : Core.register list -> Core.Regset.set
   val setToList     : Core.Regset.set -> Core.register list
   val msRegSet      : Core.Regset.set -> string
   val msRegList     : Core.register list -> string


  val makeAsmHeader : Core.procsig -> string
  val programHeader : string list
  val procedureHeader : Core.label -> string list
  val procedureTrailer : string -> string list
  val textStart : string list
  val dataStart : string list
  val GCdataStart : string list
end


