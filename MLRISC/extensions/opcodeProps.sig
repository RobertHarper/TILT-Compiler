signature MLRISC_OPCODE_PROPERTIES =
sig

   structure I : INSTRUCTIONS

        (* extract the opcode of an instruction *)
   val opcode : I.instruction -> Opcode.opcode

        (* extract the operands of an instruction *)
   val operands : { regmap  : int -> int,         
                    immed   : int -> int,
                    operand : I.operand -> int 
                  } -> 
                    I.instruction -> (int * I.C.cellclass) list *  (* dst *)
                                     (int * I.C.cellclass) list    (* src *)

        (* replace the operands of an instruction *)
   val rewriteOperands : I.instruction * int list * int list -> I.instruction

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:15:40  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:08  pscheng
# *** empty log message ***
#
 *)
