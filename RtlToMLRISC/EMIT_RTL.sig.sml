
(* =========================================================================
 * EMIT_RTL.sig.sml
 * ========================================================================= *)

signature EMIT_RTL = sig

  (* -- types -------------------------------------------------------------- *)

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
   * Emit a given named module as an aggregate of native code.
   * name   -> the name to give the module
   * module -> the module to emit
   *)
  val emitModule: string * module -> unit

  (*
   * Emit a native entry table for a list of module names.
   * names -> the list of module names to construct the entry table from
   *)
  val emitEntryTable: string list -> unit

end

