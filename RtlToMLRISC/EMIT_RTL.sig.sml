(*$import TopLevel *)


(* =========================================================================
 * EMIT_RTL.sig.sml
 * ========================================================================= *)

signature EMIT_RTL = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * A label within a given module.
   *)
  type local_label

  (*
   * A module of code and data.
   *)
  type module

  (* -- exceptions --------------------------------------------------------- *)

  (*
   * Raised when malformed Rtl code is encountered.
   *)
  exception InvalidRtl of string

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Emit a given module.
   * module -> the module to emit
   *)
  val emitModule: module -> unit

  (*
   * Emit a native entry table for a given list of module labels.
   * labels -> the module labels to construct the entry table from
   *)
  val emitEntryTable: local_label list -> unit

end

