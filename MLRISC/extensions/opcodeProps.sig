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
