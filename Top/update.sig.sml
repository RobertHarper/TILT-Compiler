(*$import Prelude *)

signature UPDATE =
sig

    val showStale : bool ref		(* Chat about why files are out of date. *)
    val showPlan : bool ref		(* Chat about plan for out of date files. *)
	    
    datatype todo = ELABORATE | GENERATE | PREPARE | ASSEMBLE | CLEANUP
    val toString : todo -> string
    val fromString : string -> todo
	
    type status					(* abstract *)
    type unit_paths				(* parameter *)
    type import					(* parameter *)
    type plan = todo list
	
    val plan : unit_paths * unit_paths list -> status * plan (* direct imports *)
    val interfaceUptodate : status -> bool

    type state
    val init : unit_paths * (unit_paths * import) list -> state
    val execute : todo * state -> state
    val flush : unit_paths * plan -> unit	(* Flush files in anticipation of other processor executing plan *)
    val flushAll : unit -> unit

end
