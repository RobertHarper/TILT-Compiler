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
   val emitInstr  : string * Machine.instruction -> unit
   val emitInstrs  : (string * Machine.instruction) list -> unit  (* May perform peephole optimizations *)
   val emitData   : Machine.data -> unit

   val print_reg  : Machine.register -> unit
   val print_pos  : Machine.assign -> unit
   val print_asn  : Machine.register * Machine.assign -> unit
   val print_move : Machine.register *
                    (Machine.assign * Machine.assign) -> unit
   val print_lab  : Rtl.label -> unit
   val print_list : ('a -> unit) -> 'a list -> unit
   val print_set  : Core.Regset.set -> unit
   val print_map  : Machine.assign Core.Regmap.map -> unit
   val print_pair : ('a -> unit ) -> ('b -> unit) -> ('a * 'b) -> unit
   val print_int  : int -> unit
   val print_trace : Tracetable.trace -> unit

   val show_labels : bool ref

   val dumpBlocks : bool -> Machine.label -> 
                            Core.procsig -> 
			    Bblock.bblock Core.Labelmap.map -> 
			    Machine.label list -> unit

   val dumpProc : Machine.label *
                  Core.procsig *
                 Bblock.bblock Core.Labelmap.map * 
	         Machine.label list *
		 bool -> unit

   val dumpData : Machine.data Array.array -> unit
   val dumpDatalist : Machine.data list -> unit
   val dumpGCDatalist : Machine.data list -> unit

end
