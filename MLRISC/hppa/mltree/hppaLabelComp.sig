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
