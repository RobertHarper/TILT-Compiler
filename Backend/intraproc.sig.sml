(*$import MACHINE BBLOCK TRACETABLE *)
signature PROCALLOC =
sig
   structure Machine : MACHINE
   structure Bblock : BBLOCK
   structure Tracetable : TRACETABLE

   val debug : bool ref
   val msgs : bool ref

   val allocateProc : {getSignature  : Machine.loclabel -> Machine.procsig,
		       external_name : Machine.Rtl.label option,
		       name          : Machine.loclabel,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : Tracetable.trace Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
                      -> (Machine.procsig * 
			  Bblock.bblock Machine.Labelmap.map *
			  Machine.loclabel list *
			  Machine.Rtl.data list)


   val allocateProc1 : {getSignature  : Machine.loclabel -> Machine.procsig,
		       external_name : Machine.Rtl.label option,
		       name          : Machine.loclabel,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : Tracetable.trace Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
                      -> 
                       ({getSignature  : Machine.loclabel -> Machine.procsig,
		       external_name : Machine.Rtl.label option,
		       name          : Machine.loclabel,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : Tracetable.trace Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
			* (Machine.assign list) option   
			* int * int * Machine.loclabel list)

   val allocateProc2 : 
                     ({getSignature  : Machine.loclabel -> Machine.procsig,
		       name          : Machine.loclabel,
		       external_name : Machine.Rtl.label option,
		       block_map     : Bblock.bblock Machine.Labelmap.map,
		       tracemap      : Tracetable.trace Machine.Regmap.map,
		       stack_resident : Machine.stacklocation 
		                                 Machine.Regmap.map,
		       procsig       : Machine.procsig} 
			* (Machine.assign list) option 		
			* int * int * Machine.loclabel list)
                      -> (Machine.procsig * 
			  Bblock.bblock Machine.Labelmap.map *
			  Machine.loclabel list *
			  Machine.Rtl.data list)
end


