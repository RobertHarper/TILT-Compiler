(*$import TopLevel *)


(* =========================================================================
 * CALL_CONVENTION_BASIS.sig.sml
 * ========================================================================= *)

signature CALL_CONVENTION_BASIS = sig

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
   * Designates a location on the stack.
   *)
  type offset

  (*
   * Assigns a set of registers to another set of registers or to stack
   * offsets.
   *)
  type assignment
  type stackAssignment

  (*
   * A stack frame.
   *)
  type frame

  (*
   * An integer expression, statement, or statement/directive.
   *)
  type rexp
  type stm
  type mltree

  (* -- code generation functions ------------------------------------------ *)

  (*
   * Generate code add/subtract a given offset to/from the stack pointer.
   * offset -> the offset to add/subtract
   * <- the add/subtract expression
   *)
  val addStack: offset -> rexp
  val subStack: offset -> rexp

  (*
   * Generate code to load/store a given pseudo-register from/to the stack.
   * offset -> the offset at which to load/store it
   * id	    -> the id of the pseudo-register to load/store
   * <- the load/store statement
   *)
  val loadStack:  offset -> id -> stm
  val storeStack: offset -> id -> stm

  (*
   * Generate code to allocate/deallocate a given stack frame.
   * frame -> the frame to allocate/deallocate
   * <- the allocate/deallocate statement
   *)
  val allocateFrame:   frame -> stm
  val deallocateFrame: frame -> stm

  (*
   * Generate code to save/restore the return pointer using the return region
   * of a given stack frame.
   * frame -> the stack frame to store the return pointer in
   * <- the save/restore statement
   *)
  val saveReturn:    frame -> stm
  val restoreReturn: frame -> stm

  (*
   * Generate code to save/restore the return pointer, but only if a given
   * procedure body contains a call statement.
   * frame -> the stack frame to store the return pointer in
   * body  -> the procedure body to check
   * <- the save/restore statement list
   *)
  val saveReturnIfCall:	   frame -> mltree list -> stm list
  val restoreReturnIfCall: frame -> mltree list -> stm list

  (* -- register assignment functions -------------------------------------- *)

  (*
   * Return a register of a specific type for a given id.
   * id -> the id of the register
   * <- the register
   *)
  val integer: id -> register
  val float:   id -> register

  (*
   * Return the union of two register assignments.
   * assignment1, assignment2 -> the assignments
   * <- the union of assignment1 and assignment2
   *)
  val assignUnion: assignment * assignment -> assignment

  (*
   * Return the result of assigning a given list of registers to new
   * pseudo-registers.
   * registers -> the registers to assign
   * <- the assignment
   *)
  val assignNew:  register list -> assignment
  val assignNew': id list * id list -> assignment

  (*
   * Return the result of assigning a given list of registers to given sets of
   * integer and floating-point registers.
   * targets   -> the integer and floating-point registers to assign the
   *		  registers to
   * registers -> the registers to assign
   * <- the registers left unassigned
   * <- the assignment
   * The alternate version of this function will not assign both an integer
   * and floating-point register with the same index.
   *)
  val assignRegister:  id list * id list ->
			 register list -> register list * assignment
  val assignRegister': id list * id list ->
			 register list -> register list * assignment

  (*
   * Return the result of assigning a given list of registers to stack offsets.
   * allocateInteger,
   * allocateFloat    -> the stack offset allocation functions
   * registers	      -> the registers to assign
   * <- the stack assignment
   *)
  val assignStack: (id -> int) * (id -> int) ->
		     register list -> stackAssignment

  (* -- marshaling functions ----------------------------------------------- *)

  (*
   * Move the source registers of a given assignment to the target
   * registers (set), or vice versa (get).
   * assignment -> the assignment to set/get
   * <- the statement list
   *)
  val setAssignment: assignment -> stm list
  val getAssignment: assignment -> stm list

  (*
   * Store/load the registers of a given stack assignment to/from their
   * target offsets.
   * base	-> a function mapping integer offsets to offset constants
   * assignment -> the stack assignment to store/load
   * <- the statement list
   *)
  val storeAssignment: (int -> offset) -> stackAssignment -> stm list
  val loadAssignment:  (int -> offset) -> stackAssignment -> stm list

  (*
   * Return the integer/floating-point register assignments of a given
   * assignment.
   * assignment -> the assignment to return the register assignments of
   * <- a list of pairs mapping target registers to source registers
   *)
  val integerAssignments: assignment -> (id * id) list
  val floatAssignments:	  assignment -> (id * id) list

end

