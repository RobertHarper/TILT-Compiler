(*$import Prelude UnitEnvironment Time *)

(* XXX: info should be abstract and should support
 * per-target information.  lastWritten is silly..
 *)

signature INFO =
sig

    (* Invariant: lastWritten <= lastChecked *)
    type info = {unit : string,				(* Unit name. *)
		 lastWritten : Time.time,		(* When unit's exports were generated. *)
		 lastChecked : Time.time,		(* When unit was last elaborated (and exports were checked). *)
		 constrained : bool,			(* Was last elaboration constrained?  *)
		 imports : UnitEnvironment.ue,		(* Contexts imported during last elaboration. *)
		 exports : UnitEnvironment.ue}		(* Contexts provided by last elaboration. *)

    val validUnit : string -> bool	(* Check unit name *)
    val read : string -> info
    val write : string * info -> unit
    val equal : info * info -> bool
	
end
