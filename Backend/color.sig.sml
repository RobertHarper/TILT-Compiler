(*$import IFGRAPH TRACKSTORAGE MACHINEUTILS Core *)

signature COLOR =
sig
   structure Trackstorage : TRACKSTORAGE
   structure Ifgraph : IFGRAPH

   (* color:  assign registers to physical locations.
      Takes 
	  (1) an interference graph,
	  (2) an abstract datatype for computing and tracking
	      storage information (i.e. next stack location, etc.)
          (3) a mapping of "registers that must
	      be stack resident" to their stack locations.
	  (4) a function mapping pseudo registers to
	      biasing information
      Returns a mapping from registers to locations.*)


   val debug : bool ref


   val color : Ifgraph.graph * Trackstorage.info * 
	       Core.stacklocation Core.Regmap.map *
	       (Core.register -> (Core.register * int) list) ->
	        Core.assign Core.Regmap.map
end

	 
