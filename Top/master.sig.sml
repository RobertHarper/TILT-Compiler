(*$import Time Crc *)


signature MASTER = 
sig
    type state
    datatype result = PROCESSING of state        (* All slaves utilized *)
                    | IDLE of state * int * string list * string list  
                                                 (* Number of idle slaves, waiting jobs, and pending jobs *)
	            | COMPLETE
    (* Takes the mapfile, a list of units that are desired,
       and a name of the executable.  Generates the executable
       and compiles the necessary units. *)
    val once : string * string list * string option -> 
	{setup : unit -> state,
	 step : state -> result,
	 complete : unit -> unit}
    val run : string * string list * string option -> unit 
    val purge : string -> unit

    type collapse = {maxWeight : int, maxParents : int, maxChildren : int}
    val makeGraph     : string * collapse option -> string  (* Creates a .dot file *)
    val makeGraphShow : string * collapse option -> string  (* Creates a .ps file and invokes gv on it *)

end

