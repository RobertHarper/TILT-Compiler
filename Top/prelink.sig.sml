(*$import Prelude *)

signature PRELINK =
sig

    val doConsistent : bool ref		(* Stop when generated files are inconsistent. *)
	
    type ue
    type package = {unit : string, imports : ue, exports : ue}
	
    (* Combine a sequence of units environments, performing
     * consistency checks and creating new import and export
     * environments.  *)
    val check : package list -> {imports : ue, exports : ue}

    val checkTarget : string * package list -> unit (* unit name *)
	
end
