
(* =========================================================================
 * MLRISC_CONSTANT.sig.sml
 * ========================================================================= *)

signature MLRISC_CONSTANT = sig

  (* -- types -------------------------------------------------------------- *)

  datatype const =
    Int of int
  | DeferInt of unit -> int

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Interfaces to MLRISC.
   *)
  val toString: const -> string
  val valueOf:	const -> int

end

