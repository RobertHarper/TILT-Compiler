
(* =========================================================================
 * REGISTER_DATA_FLOW.sig.sml
 * ========================================================================= *)

signature REGISTER_DATA_FLOW = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * A set of register ids.
   *)
  type set

  (*
   * An expression.
   *)
  type mlrisc

  (*
   * A statement/directive.
   *)
  type mltree

  (* -- exceptions --------------------------------------------------------- *)

  (*
   * Raised when malformed source code is encountered.
   *)
  exception InvalidSource of string

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return the define/use set of a given expression.
   * exp -> the expression to return the define/use set of
   * <- the define/use set for exp
   *)
  val defineExpression: mlrisc -> set
  val useExpression:	mlrisc -> set

  (*
   * Return the define/use set of a given sequence of statement/directives.
   * mltrees -> the statement/directives to return the define/use set of
   * <- the define/use set for mltrees
   *)
  val define: mltree list -> set
  val use_:   mltree list -> set

end

