(* =========================================================================
 * REGISTER_LIVENESS.sig.sml
 * ========================================================================= *)

signature REGISTER_LIVENESS = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * A set of register ids.
   *)
  type idSet

  (*
   * A statement/directive.
   *)
  type mltree

  (*
   * A basic block of statement/directives.
   *)
  type block = mltree list

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return the live sets of a given list of basic blocks according to the
   * fixed point of an iterative liveness analysis.
   * blocks -> the blocks to compute the live sets of
   * <- a list of sets representing the registers live entering and leaving
   *	the corresponding elements of blocks
   *)
  val liveness: block list -> idSet list

end

