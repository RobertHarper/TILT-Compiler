(*
 * Properties for utilizing the SSA form
 *)
signature SSA_PROPERTIES =
sig

   structure I : INSTRUCTIONS
   structure C : CELLS
      sharing I.C = C

   datatype opn = 
     OPN of int                         (* a constant operand *)
   | REG of C.register * C.cellclass    (* can be renamed *)
   | FIX of C.register * C.cellclass    (* cannot be renamed *) 
   | MEM of C.register                  (* store operand *)

   datatype const =
     IMMED of int                       (* integer operand *)
   | LABEL of Label.label               (* label operand *)
   | OPERAND of I.operand               (* other operand *)  

   val immedRange    : {low:int, high:int}
   val loadImmed     : {immed:int,t:C.register} -> I.instruction

       (* physical registers that are volatile *)
   val volatile : C.register list
 
       (* physical registers whose definition cannot be removed *) 
   val mustDef : C.register list

   val hashOpn : I.operand -> int             (* hash function for operands *)

   val eqOpn : I.operand * I.operand -> bool  (* equality for operands *)

   val isNonSafeRead : I.instruction -> bool  (* can't be moved up? *)

   val exp : I.instruction -> SSAExp.exp      (* semantics of an instruction *)

   val operands :     (* operands used by instruction *)
        { regmap  : C.register -> C.register,
          immed   : int -> C.register,
          label   : Label.label -> C.register,
          operand : I.operand -> C.register
        } -> I.instruction ->  opn list * opn list  (* dst/src *)

   val rewriteOperands :   (* replace operands used *)
        { const : int -> const
        } ->
        { instr : I.instruction,
          dst   : C.register list,
          src   : C.register list 
        } -> I.instruction

         (* generate a set of parallel copies *)
   val copies : {class:C.cellclass, dst:C.register, src:C.register} list ->
                   I.instruction list

end
