signature MANAGER =
sig

	(*
		DiagLevel determines how verbose the compiler is; zero (the
		default) is quiet and higher numbers are more verbose.
	*)
	val DiagLevel : int ref	(* Print diagnostics. *)
	val PrintStats : bool ref	(* Print stats after compilation. *)
	val ResetStats : bool ref	(* Reset stats before compilation. *)
	(*
		NumSlaves determines how many slaves are launched; -1,
		the default, runs one slave for each processor.
	*)
	val NumSlaves : int ref

	(*
		This is similar to the command-line interface except
		that (1) it does not exit on errors (2) the compiler's
		state persists between calls to make.
	*)
	val make : string list -> unit

end
