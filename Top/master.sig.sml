signature MASTER =
sig
    val MasterDiag : bool ref
    val Checkpoint : bool ref
    val CheckpointVerbose : bool ref	(* Checkpoints include a break-down of units by status. *)
    val ShowEnable : bool ref		(* Chat when units become enabled for compilation. *)
    val ShowFinalReport : bool ref
    val ShowUptodate : bool ref

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
    val clean : string -> unit
    val purge : string -> unit
    val purgeAll : string -> unit
end
