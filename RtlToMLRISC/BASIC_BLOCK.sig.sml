
(* =========================================================================
 * BASIC_BLOCK.sig.sml
 * ========================================================================= *)

signature BASIC_BLOCK = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * A statement/directive.
   *)
  type mltree

  (*
   * A basic block of statement/directives.
   *)
  type block = mltree list

  (* -- exceptions --------------------------------------------------------- *)

  (*
   * Raised when malformed source code is encountered.
   *)
  exception InvalidSource of string

  (*
   * Raised when the target label of a branch is not defined.
   *)
  exception UndefinedLabel of string

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a list of statement/directives without labels that are not
   * branch targets.
   * mltrees -> the list of statement/directives to strip the labels of
   * <- mltrees stripped of non-target labels
   *)
  val keepTargetLabels: mltree list -> mltree list

  (*
   * Return a given list of statement/directives partitioned into basic blocks.
   * mltrees -> the list of statement/directives to partition
   * <- a list of lists of statement/directives representing the basic blocks
   *	of mltrees
   *)
  val partition: mltree list -> block list

  (*
   * Return a list of the indices of the successors/predecessors of a given
   * list of basic blocks.
   * blocks -> the blocks to return the successors/predecessors of
   * <- a list of lists of indices representing the successors/predecessors
   *	of the corresponding elements of blocks
   *)
  val successors:   block list -> int list list
  val predecessors: block list -> int list list

end

