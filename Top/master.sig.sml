(*$import Time Crc *)


signature MASTER = 
sig
    val showEnable : bool ref		(* Chat when units become enabled for compilation. *)
	
    type state
    datatype result = PROCESSING of Time.time * state  (* All slaves utilized *)
                    | IDLE of Time.time * state * int  (* Num slaves idle and there are waiting jobs *)
	            | COMPLETE of Time.time
	
    (* Compile units in mapfile, generating object files and
     * executable targets if we're running native. *)
    val once : string -> {setup : unit -> state,
			  step : state -> result,
			  complete : unit -> unit}
    val run : string -> unit 
    val purge : string -> unit
    val purgeAll : string -> unit

    type collapse = {maxWeight : int, maxParents : int, maxChildren : int}
    val makeGraph     : string * collapse option -> string  (* Creates a .dot file *)
    val makeGraphShow : string * collapse option -> string  (* Creates a .ps file and invokes gv on it *)

end

