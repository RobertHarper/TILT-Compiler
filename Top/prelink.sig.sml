(*
	XXX: Interfaces have unit environments too.
*)
signature PRELINK =
sig

    val doConsistent : bool ref		(* Stop when generated files are inconsistent. *)

    type equiv = Crc.crc * Crc.crc -> bool
    type ue = UnitEnvironment.ue
    type package = {unit : string, imports : ue, exports : ue}

    (* Combine a sequence of units environments, performing
     * consistency checks and creating new import and export
     * environments.  *)
    val check : equiv -> package list -> {imports : ue, exports : ue}

    val checkTarget : equiv -> string * package list -> unit (* target *)

end
