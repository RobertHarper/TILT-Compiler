signature TRACKSTORAGE =
sig
  structure Machineutils : MACHINEUTILS

  structure Machine : MACHINE
  sharing Machine = Machineutils.Machine

  type info

  val debug : bool ref

  (* max_on_stack: maximum # of args passed on stack by this
     procedure.

     stack_resident: set of traceable int variables which must
     be resident on the stack.   These are assumed to fill the
     first N integers slots.*)

  val newInfo : {callee_saves: Machine.register list,
		 regs_destroyed   : Machine.register list,
		 max_on_stack : int,
		 max_C_args : int,
		 stack_resident : Machine.stacklocation Machineutils.Regmap.map}
                    -> info (* init with callee-save registers *)
  
  val stackOffset : info -> Machine.register -> Machine.stacklocation
  val noteUsed    : info -> Machine.register -> unit

  datatype summary = SUMMARY of 
                    {registers_used    : Machine.register list,
		     stackframe_size   : int,
		     callee_save_slots : (Machine.register * Machine.stacklocation) list,
		     tailcallImpossible : unit -> bool,
		     fixStackOffset    : Machine.stacklocation -> 
		                         Machine.stacklocation}

  val summarize : info -> summary

end

