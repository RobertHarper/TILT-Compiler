(*$import Prelude *)

signature SLAVE = 
sig
    datatype result = WORK of string | WAIT | READY
    val setup : unit -> unit
    val step : unit -> result
    val run : unit -> unit       (* run slave repeatedly and restart on termination *)
end

