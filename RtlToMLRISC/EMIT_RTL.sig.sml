(*$import TopLevel *)


(* =========================================================================
 * EMIT_RTL.sig.sml
 * ========================================================================= *)

signature EMIT_RTL = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * A label within a given module.
   *)
  type label

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

end

