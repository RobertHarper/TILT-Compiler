
(* =========================================================================
 * MLTREE_EXTRA.sig.sml
 * ========================================================================= *)

signature MLTREE_EXTRA = sig

  (* -- structures --------------------------------------------------------- *)

  structure MLTree: MLTREE

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return an MLRISC value for a given pseudo-register id.
   * id -> the pseudo-register id
   * <- the MLRISC value
   *)
  val ccr: int -> MLTree.mlrisc
  val gpr: int -> MLTree.mlrisc
  val fpr: int -> MLTree.mlrisc

  (*
   * Return an MLRISC statement for moving the value of a given
   * pseudo-register/expression into a given pseudo-register.
   * target -> the id of the pseudo-register to move the value into
   * source -> the pseudo-register/expression to move the value of
   * <- the statement
   *)
  val copy:  int * int -> MLTree.stm
  val fcopy: int * int -> MLTree.stm
  val mv:    int * MLTree.rexp -> MLTree.stm
  val fmv:   int * MLTree.fexp -> MLTree.stm

  val copyList:	 int list * int list -> MLTree.stm list
  val fcopyList: int list * int list -> MLTree.stm list

  (*
   * Return a constant-folded MLRISC expression for a given set of operand
   * expressions.
   * -> the operand expressions
   * <- the constant-folded expression
   *)
  val add: MLTree.rexp * MLTree.rexp -> MLTree.rexp
  val sll: MLTree.rexp * MLTree.rexp -> MLTree.rexp

end

