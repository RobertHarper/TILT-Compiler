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
