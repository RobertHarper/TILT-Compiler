
(* =========================================================================
 * FLOAT_CONVENTION.sig.sml
 * ========================================================================= *)

signature FLOAT_CONVENTION = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a register.
   *)
  type id

  (*
   * A floating-point expression.
   *)
  type fexp

  (* -- values ------------------------------------------------------------- *)

  (*
   * Always zero.
   *)
  val zero: id

  (*
   * Used to hold temporary values in standard code sequences.
   *)
  val temporary1: id
  val temporary2: id

  (*
   * Used to pass arguments to, and return results from, a given procedure.
   *)
  val arguments: id list
  val results:	 id list

  (*
   * Can/cannot be used by the register allocator as temporaries.
   *)
  val available: id list
  val dedicated: id list

  (*
   * Must be preserved across procedure calls.
   *)
  val preserve: id list

  (*
   * Defined and used by all procedure calls.
   *)
  val define: id list
  val use:    id list

  (*
   * Always escape from procedures.
   *)
  val escape: id list

  (* -- values ------------------------------------------------------------- *)

  (*
   * Return a floating-point expression for a given register id.
   * id -> the id of the register to return the expression for
   * <- an expression representing register id
   *)
  val expression: id -> fexp

end

