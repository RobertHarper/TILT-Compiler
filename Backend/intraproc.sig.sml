(*$import Core BBLOCK TRACETABLE *)

signature PROCALLOC =
sig
   structure Bblock : BBLOCK
   structure Tracetable : TRACETABLE

   val debug : bool ref
   val msgs : bool ref

   val allocateProc : {getSignature  : Core.label -> Core.procsig,
		       name          : Core.label,
		       block_map     : Bblock.bblock Core.Labelmap.map,
		       tracemap      : (Core.register option * Tracetable.trace) Core.Regmap.map,
		       stack_resident : Core.stacklocation 
		                                 Core.Regmap.map,
		       procsig       : Core.procsig} 
                      -> (Core.procsig * 
			  Bblock.bblock Core.Labelmap.map *
			  Core.label list *
			  Core.data list)


end


