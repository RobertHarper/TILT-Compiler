(*$import IFGRAPH TRACKSTORAGE MACHINEUTILS *)

signature COLOR =
sig
   structure Machine : MACHINE
   structure Trackstorage : TRACKSTORAGE
   structure Ifgraph : IFGRAPH

   (* color:  assign registers to physical locations.
      Takes 
	  (1)an interference graph,
	  (2) an abstract datatype for computing and tracking
	      storage information (i.e. next stack location, etc.)
          (3) a mapping of registers that must
	      be stack resident to their stack locations.  

      Returns a mapping from registers to locations.*)


   val debug : bool ref


   val color : Ifgraph.graph * Trackstorage.info * 
	       Machine.stacklocation Machine.Regmap.map *
	       (Machine.register -> (Machine.register * int) list) ->
	        Machine.assign Machine.Regmap.map
end

	 
