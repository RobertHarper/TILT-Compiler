(*$import Core *)

signature TRACKSTORAGE =
sig

  type info

  val debug : bool ref

  (* max_on_stack: maximum # of args passed on stack by this
     procedure.

     stack_resident: set of traceable int variables which must
     be resident on the stack.   These are assumed to fill the
     first N integers slots.*)

  val newInfo : {callee_saves: Core.register list,
		 regs_destroyed   : Core.register list,
		 max_on_stack : int,
		 max_C_args : int,
		 stack_resident : Core.stacklocation Core.Regmap.map}
                    -> info (* init with callee-save registers *)
  
  val stackOffset : info -> Core.register list * Core.register -> Core.stacklocation
  val noteUsed    : info -> Core.register -> unit

  datatype summary = SUMMARY of 
                    {registers_used    : Core.register list,
		     stackframe_size   : int,
		     callee_save_slots : (Core.register * Core.stacklocation) list,
		     tailcallImpossible : unit -> bool,
		     fixStackOffset    : Core.stacklocation -> 
		                         Core.stacklocation}

  val summarize : info -> summary

end

