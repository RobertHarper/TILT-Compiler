(*$COLOR: IFGRAPH TRACKSTORAGE MACHINEUTILS *)

signature COLOR =
sig
   structure Trackstorage : TRACKSTORAGE
   structure Ifgraph : IFGRAPH

   structure MU : MACHINEUTILS

   sharing MU = Trackstorage.Machineutils
   sharing MU.Machine = Trackstorage.Machine

   sharing type Ifgraph.node = MU.Machine.register

   (* color:  assign registers to physical locations.
      Takes 
	  (1)an interference graph,
	  (2) an abstract datatype for computing and tracking
	      storage information (i.e. next stack location, etc.)
          (3) a mapping of registers that must
	      be stack resident to their stack locations.  

      Returns a mapping from registers to locations.*)


   val debug : bool ref

   val reset_times : unit -> unit
   val print_times : unit -> unit

   val color : Ifgraph.graph * Trackstorage.info * 
	       MU.Machine.stacklocation MU.Regmap.map *
	       (MU.Machine.register -> (MU.Machine.register * int) list) ->
	                    MU.Machine.assign MU.Regmap.map
end

	 
