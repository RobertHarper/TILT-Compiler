(* instruction.sig --- target machine instructions 
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* basically says: 
 * represent instructions any which way you want 
 *)
signature INSTRUCTIONS = sig
    structure C : CELLS
    structure Constant : CONSTANT

    type ea
    type operand    
    type instruction
end

