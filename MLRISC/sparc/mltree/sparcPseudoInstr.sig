(*
 * sparcPseudoInstr.sig --- Sparc pseudo instructions 
 *)

signature SPARC_PSEUDO_INSTR = sig
   structure I : SPARCINSTR

   type reduceOpnd = I.operand -> int

   (* 
    * Signed and unsigned multiplications.
    * For signed operations, trap on overflow and division by zero.
    * For unsigned operations, trap on division by zero.
    *)
   val umul : {r:int, i:I.operand, d:int} * reduceOpnd -> I.instruction list
   val smul : {r:int, i:I.operand, d:int} * reduceOpnd -> I.instruction list
   val udiv : {r:int, i:I.operand, d:int} * reduceOpnd -> I.instruction list
   val sdiv : {r:int, i:I.operand, d:int} * reduceOpnd -> I.instruction list
   val cvti2d : {i:I.operand, d:int} * reduceOpnd -> I.instruction list

end
