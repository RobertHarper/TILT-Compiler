(* alpha32PseudoInstr.sig --- alpha pseudo instructions *)

signature ALPHA32_PSEUDO_INSTR = sig
   structure I : ALPHA32INSTR
  
   type reduceOpnd = I.operand -> int

   val divl : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
     (* divide longword generating a trap on divide by zero *)

   val divlu : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
     (* divide longword unsigned generating a trap on divide by zero *)

   val cvti2d : {opnd:I.operand, fd:int} * reduceOpnd  -> I.instruction list
     (* convert longword to double *)
end 

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:52  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:20  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:50  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
