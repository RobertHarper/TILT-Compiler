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

(* 
 * $Log$
# Revision 1.2  2001/12/13  16:32:28  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:47  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:48  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
