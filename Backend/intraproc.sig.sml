(*$import MACHINE BBLOCK TRACETABLE *)
signature PROCALLOC =
sig
   structure Machine : MACHINE
   structure Bblock : BBLOCK
   structure Tracetable : TRACETABLE

   val debug : bool ref
   val msgs : bool ref

   val allocateProc : {getSignature  : Machine.label -> Machine.procsig,
		       name          : Machine.label,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : (Machine.register option * Tracetable.trace) Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
                      -> (Machine.procsig * 
			  Bblock.bblock Machine.Labelmap.map *
			  Machine.label list *
			  Machine.data list)


   val allocateProc1 : {getSignature  : Machine.label -> Machine.procsig,
		       name          : Machine.label,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : (Machine.register option * Tracetable.trace) Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
                      -> 
                       ({getSignature  : Machine.label -> Machine.procsig,
		       name          : Machine.label,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : (Machine.register option * Tracetable.trace) Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
			* (Machine.assign list) option   
			* int * int * Machine.label list)

   val allocateProc2 : 
                     ({getSignature  : Machine.label -> Machine.procsig,
		       name          : Machine.label,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : (Machine.register option * Tracetable.trace) Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
			* (Machine.assign list) option 		
			* int * int * Machine.label list)
                      -> (Machine.procsig * 
			  Bblock.bblock Machine.Labelmap.map *
			  Machine.label list *
			  Machine.data list)
end


