(*$import Prelude *)

signature MANAGER = 
sig

  val statFinal : bool ref		(* Dump statistics at end of compilation. *)
  val resetStats : bool ref		(* Reset stats before compilation. *)
	    
  (* purgeAll       takes a mapfile and removes all generated files (for current platform and flags)
		    of units named in mapfile
     purge          doesn't remove interfaces
     slave          run a slave
     slaves	    run some slaves on the given machines
     master         run a master on the given mapfile
     make           run a master on the given mapfile
                    also interleave slave work in the same process
  *)

  val purge : string -> unit
  val purgeAll : string -> unit
  val slave : unit -> unit
  val slaves : (int * string) list -> unit
  val master : string -> unit  
  val make : string -> unit

end

