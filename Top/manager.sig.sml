signature MANAGER =
sig

  (*
	DiagLevel determines how verbose the compiler is; zero (the
	default) is quiet and higher numbers are more verbose.
  *)
  val DiagLevel : int ref	(* Print diagnostics. *)	
  val TimeFinal : bool ref	(* Dump statistics at end of compilation. *)
  val ResetStats : bool ref	(* Reset stats before compilation. *)

  (* purgeAll       takes a mapfile and removes all generated files (for current platform and flags)
		    of units named in mapfile
     purge          doesn't remove interfaces
     clean          remove only the .o files
     slave          run a slave
     slaves	    run some slaves on the given machines
     master         run a master on the given mapfile
     make           run a master on the given mapfile
                    also interleave slave work in the same process
  *)

  val clean : string -> unit
  val purge : string -> unit
  val purgeAll : string -> unit
  val slave : unit -> unit
  val slaves : (int * string) list -> unit
  val master : string -> unit
  val make : string -> unit

end

