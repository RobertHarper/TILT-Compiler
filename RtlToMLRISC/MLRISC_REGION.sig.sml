(*$import *)


(* =========================================================================
 * MLRISC_REGION.sig.sml
 * ========================================================================= *)

signature MLRISC_REGION = sig

  (* -- types -------------------------------------------------------------- *)

  type region

  (* -- values ------------------------------------------------------------- *)

  (*
   * Interfaces to MLRISC.
   *)
  val stack:  region
  val memory: region

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Interfaces to MLRISC.
   *)
  val toString: region -> string

end

