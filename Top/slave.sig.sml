signature SLAVE =
sig
	val Standalone : bool ref	(* false if parent process is TILT master *)
	val PauseTime : int ref	(* ms *)
	val SlaveDiag : bool ref
	val PrintEachFile : bool ref

	val slave : unit -> 'a
end
