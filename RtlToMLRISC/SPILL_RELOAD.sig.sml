(* =========================================================================
 * SPILL_RELOAD.sig.sml
 * ========================================================================= *)

signature SPILL_RELOAD = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a register.
   *)
  type id

  (*
   * An integer expression, floating-point expression or statement/directive.
   *)
  type rexp
  type fexp
  type mltree

  (* -- exceptions --------------------------------------------------------- *)

  (*
   * Raised when malformed source code is encountered.
   *)
  exception InvalidSource of string

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a given list of statement/directives with certain registers
   * replaced by memory accesses according to a given set of spill and reload
   * functions.
   * integerSpill,
   * integerReload -> returns an integer expression for a given
   *		      integer register
   * floatSpill,
   * floatReload   -> returns a floating-point expression for a given
   *		      floating-point register
   * mltrees	   -> the list of statement/directives to transform
   * <- mltrees with certain registers spilled
   *)
  val transform: ((id -> rexp) * (id -> rexp)) *
		 ((id -> fexp) * (id -> fexp)) -> mltree list -> mltree list

end

