(*$import SPARC32INSTR SPARC32_PSEUDO_INSTR *)

(* =========================================================================
 * SparcPseudoInstr.sml
 * ========================================================================= *)

functor SparcPseudoInstr(
	  structure SparcInstr: SPARCINSTR
	) :> SPARC_PSEUDO_INSTR
               where I = SparcInstr
          = struct

  (* -- structures --------------------------------------------------------- *)

  structure I = SparcInstr

  (* -- types -------------------------------------------------------------- *)

  type reduceOpnd = I.operand -> int

  (* -- exceptions --------------------------------------------------------- *)

  exception Unimplemented

  (* -- functions ---------------------------------------------------------- *)

   fun umul({r,i,d},reduceOpnd) = [I.ARITH{a=I.UMUL,r=r,i=i,d=d,cc=false}]
   fun smul({r,i,d},reduceOpnd) = [I.ARITH{a=I.SMUL,r=r,i=i,d=d,cc=false}]
   fun udiv({r,i,d},reduceOpnd) = 
      [I.WRY{r=0,i=I.IMMED 0},I.ARITH{a=I.UDIV,r=r,i=i,d=d,cc=false}]
   fun sdiv({r,i,d},reduceOpnd) = 
      [I.WRY{r=0,i=I.IMMED 0},I.ARITH{a=I.SDIV,r=r,i=i,d=d,cc=false}]

   (* We use the runtime for this *)
   fun cvti2d _ = raise Unimplemented

end

