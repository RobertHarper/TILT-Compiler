(*$import TopLevel *)


(* =========================================================================
 * INTEGER_CONVENTION.sig.sml
 * ========================================================================= *)

signature INTEGER_CONVENTION = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a register.
   *)
  type id

  (*
   * An integer expression.
   *)
  type rexp

  (* -- values ------------------------------------------------------------- *)

  (*
   * Always zero.
   *)
  val zero: id

  (*
   * The code address for a procedure call.
   *)
  val callPointer: id

  (*
   * The code address for a procedure return.
   *)
  val returnPointer: id

  (*
   * The base address of the global variables.
   *)
  val globalPointer: id

  (*
   * The address of the top of the stack.
   *)
  val stackPointer: id

  (*
   * The address of the next heap value to be allocated and the end of the
   * heap.
   *)
  val heapPointer: id
  val heapLimit:   id

  (*
   * The address of the current exception and its argument.
   *)
  val exceptionPointer:	 id
  val exceptionArgument: id

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
   * Return an integer expression for a given register id.
   * id -> the id of the register to return the expression for
   * <- an expression representing register id
   *)
  val expression: id -> rexp

end

