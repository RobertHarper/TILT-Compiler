signature SLAVE =
sig
    val TimeEachFile : bool ref
    val SlaveDiag : bool ref

    type state
    val slave : unit -> {setup : unit -> state,
			 step : state -> state,
			 complete : state -> unit}

    val run : unit -> 'a       (* run slave repeatedly *)
end

