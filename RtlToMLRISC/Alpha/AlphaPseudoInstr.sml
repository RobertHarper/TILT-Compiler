
(* =========================================================================
 * AlphaPseudoInstr.sml
 * ========================================================================= *)

functor AlphaPseudoInstr(
	  structure Alpha32Instr: ALPHA32INSTR
	) :> ALPHA32_PSEUDO_INSTR
               where I = Alpha32Instr
          = struct

  (* -- structures --------------------------------------------------------- *)

  structure I = Alpha32Instr

  (* -- types -------------------------------------------------------------- *)

  type reduceOpnd = I.operand -> int

  (* -- exceptions --------------------------------------------------------- *)

  exception Unimplemented

  (* -- functions ---------------------------------------------------------- *)

  fun divl _ = raise Unimplemented

  fun divlu _ = raise Unimplemented

  fun cvti2d _ = raise Unimplemented

end

