(*$import BBLOCK TRACETABLE MACHINE MACHINEUTILS Array *)
signature PRINTUTILS =
sig
   structure Bblock : BBLOCK
   structure Tracetable : TRACETABLE
   structure Machine : MACHINE

   val openOutput  : string -> unit
   val openAppend  : string -> unit
   val closeOutput : unit -> unit

   val emitString : string -> unit
   val emitInstr  : string -> Machine.instruction -> unit
   val emitData   : Machine.Rtl.data -> unit

   val print_reg  : Machine.register -> unit
   val print_pos  : Machine.assign -> unit
   val print_lab  : Machine.loclabel -> unit
   val print_asn  : Machine.register * Machine.assign -> unit
   val print_move : Machine.register *
                    (Machine.assign * Machine.assign) -> unit
   val print_list : ('a -> unit) -> 'a list -> unit
   val print_set  : Machine.Regset.set -> unit
   val print_map  : Machine.assign Machine.Regmap.map -> unit
   val print_pair : ('a -> unit ) -> ('b -> unit) -> ('a * 'b) -> unit
   val print_int  : int -> unit
   val print_trace : Tracetable.trace -> unit

   val show_labels : bool ref

   val dumpBlocks : bool -> Machine.loclabel -> 
                            Machine.procsig -> 
			    Bblock.bblock Machine.Labelmap.map -> 
			    Machine.loclabel list -> unit

   val dumpProc : Machine.loclabel * Machine.Rtl.label option *
                 Machine.procsig *
                 Bblock.bblock Machine.Labelmap.map * 
	         Machine.loclabel list *
		 bool -> unit
   val dumpCodeLabel : Machine.label list -> unit
   val dumpData : Machine.Rtl.data Array.array -> unit

   val dumpDatalist : Machine.Rtl.data list -> unit


end
