(*$import TopLevel *)


(* =========================================================================
 * CALL_CONVENTION.sig.sml
 * ========================================================================= *)

signature CALL_CONVENTION = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a register of a given type.
   *)
  type id

  (*
   * Identifies a register, including its type.
   *)
  type register

  (*
   * Assigns a set of registers to another set of registers.
   *)
  type assignment

  (*
   * A stack frame.
   *)
  type frame

  (*
   * An integer expression, statement, or statement/directive.
   *)
  type rexp
  type mltree

  (* -- values ------------------------------------------------------------- *)

  (*
   * A statement/directive to annotate the escaping set of registers.
   *)
  val escape: mltree

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a register of a specific type for a given id.
   * id -> the id of the register
   * <- the register
   *)
  val integer: id -> register
  val float:   id -> register

  (*
   * Return code to call a given procedure with a given list of argument and
   * result registers.
   * wrapper   -> a procedure to call to generate a wrapper for the call site
   * frame    <-> the stack frame to store memory arguments in
   * procedure -> the procedure to call
   * arguments -> the argument registers
   * results   -> the result registers
   * <- a list of mltree values to implement the call
   *)
  val call: (rexp -> mltree list * mltree list) ->
	      frame -> rexp * register list * register list -> mltree list

  (*
   * Return an assignment for registers to save and restore in a given
   * procedure.
   * registers -> the registers to save and restore
   * <- the integer and floating-point save/restore assignments for all saved
   *	registers in the procedure
   *)
  val save: register list -> assignment

  (*
   * Return the integer/floating-point register assignments of a given save
   * assignment.
   * assignment -> the save assignment to return the register assignments of
   * <- a list of pairs mapping target registers to source registers
   *)
  val integerSave: assignment -> (id * id) list
  val floatSave:   assignment -> (id * id) list

  (*
   * Return code to enter/exit the body of a given procedure using a given
   * stack frame and a given list of arguments/results.
   * frame     <-> the stack frame to save registers to and to load memory
   *		   arguments from
   * arguments/
   * results	-> the argument/result registers used by the body
   * saves	-> the assignment for registers to save/restore
   * body	-> the body of the procedure
   * <- a list of mltree values to implement the entry/exit sequence
   *)
  val enter: frame -> register list * assignment * mltree list -> mltree list
  val exit:  frame -> register list * assignment * mltree list -> mltree list

end

