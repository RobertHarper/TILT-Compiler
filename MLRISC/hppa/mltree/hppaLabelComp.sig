signature LABEL_COMP = sig
  structure T : MLTREE
  structure I : INSTRUCTIONS

  type reduce = 
    {stm:T.stm -> unit, rexp:T.rexp -> int, emit:I.instruction -> unit }
    (* functions to emit MLRISC statements or register expressions *)

  datatype lab_opnd = OPND of I.operand | REG of int 

  val ldLabelEA : 
    (I.instruction -> unit) -> LabelExp.labexp -> (int * I.operand)
    (* generate a label operand to use as an effective address *)

  val ldLabelOpnd : 
    (I.instruction -> unit) -> 
       {label:LabelExp.labexp, pref:int option} -> lab_opnd
    (* generate a label operand to be used by immediate instructions *)

  val doJmp : reduce * T.stm  -> unit
    (* compile a jump involving a label *)

  val doCall : reduce * T.stm -> unit
    (* compile a call involving a label *)

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:11  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:27  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:07  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
