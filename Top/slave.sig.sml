(*$import *)

signature SLAVE = 
sig
    type state
    datatype result = WORK of string | WAIT | READY

    val slave : int option -> {setup : unit -> state,
			       step : state -> state * result,
			       complete : state -> unit}
	
    val run : unit -> unit       (* run slave repeatedly and restart on termination *)
end

