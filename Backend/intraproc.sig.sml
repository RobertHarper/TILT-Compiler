signature PROCALLOC =
sig
   structure Bblock : BBLOCK
   structure Tracetable : TRACETABLE

   structure Machine : MACHINE
   structure Machineutils: MACHINEUTILS

   sharing Machine = Machineutils.Machine = Tracetable.Machine
   sharing Machineutils = Bblock.Machineutils 
  
   val debug : bool ref
   val msgs : bool ref
   val reset_times : unit -> unit
   val print_times : unit -> unit

   val allocateProc : {getSignature  : Machine.loclabel -> Machine.procsig,
		       name          : Machine.loclabel,
		       block_map     : Bblock.bblock Machineutils.Labelmap.map,
		       tracemap      : Tracetable.trace Machineutils.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machineutils.Regmap.map,
		       procsig       : Machine.procsig} 
                      -> (Machine.procsig * 
			  Bblock.bblock Machineutils.Labelmap.map *
			  Machine.loclabel list *
			  Machine.Rtl.data list)


   val allocateProc1 : {getSignature  : Machine.loclabel -> Machine.procsig,
		       name          : Machine.loclabel,
		       block_map     : Bblock.bblock Machineutils.Labelmap.map,
		       tracemap      : Tracetable.trace Machineutils.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machineutils.Regmap.map,
		       procsig       : Machine.procsig} 
                      -> 
                       ({getSignature  : Machine.loclabel -> Machine.procsig,
		       name          : Machine.loclabel,
		       block_map     : Bblock.bblock Machineutils.Labelmap.map,
		       tracemap      : Tracetable.trace Machineutils.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machineutils.Regmap.map,
		       procsig       : Machine.procsig} 
			* (Machine.assign list) option   
			* int * int * Machine.loclabel list)

   val allocateProc2 : 
                     ({getSignature  : Machine.loclabel -> Machine.procsig,
		       name          : Machine.loclabel,
		       block_map     : Bblock.bblock Machineutils.Labelmap.map,
		       tracemap      : Tracetable.trace Machineutils.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machineutils.Regmap.map,
		       procsig       : Machine.procsig} 
			* (Machine.assign list) option 		
			* int * int * Machine.loclabel list)
                      -> (Machine.procsig * 
			  Bblock.bblock Machineutils.Labelmap.map *
			  Machine.loclabel list *
			  Machine.Rtl.data list)
end


