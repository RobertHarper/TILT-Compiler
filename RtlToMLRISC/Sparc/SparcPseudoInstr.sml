(*$import TopLevel ALPHA32INSTR ALPHA32_PSEUDO_INSTR *)

***not ported yet ***

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

  local
    (* not sure what these are used for ??? *)
    val temps = foldl I.C.addReg I.C.empty [23, 24, 25, 26, 28]
  in
    fun divl({ra, rb, rc}, _) = 
      [I.PSEUDOARITH{oper = I.DIVL, ra = ra, rb = rb, rc = rc, tmps = temps}]

    fun divlu({ra, rb, rc}, _) = 
      [I.PSEUDOARITH{oper = I.DIVLU, ra = ra, rb = rb, rc = rc, tmps = temps}]
  end

  (*
   * We use a procedure in the runtime system for this.
   *)
  fun cvti2d _ = raise Unimplemented

end

