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


(*
 * $Log$
# Revision 1.1  99/02/17  21:16:35  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:11  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
