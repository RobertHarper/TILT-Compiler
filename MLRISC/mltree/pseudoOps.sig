(* pseudo-ops.sml --- description of assembly pseudo-ops
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature PSEUDO_OPS = sig
  type pseudo_op

  val toString : pseudo_op -> string

  val emitValue : {pOp:pseudo_op, loc:int, emit:Word8.word -> unit} -> unit
    (* emit value of pseudo op give current location counter and output
     * stream. The value emitted should respect the endianness of the
     * target machine.
     *)

  val sizeOf : pseudo_op * int -> int
    (* Size of the pseudo_op in bytes given the current location counter
     * The location counter is provided in case some pseudo ops are 
     * dependent on alignment considerations.
     *)

  val adjustLabels : pseudo_op * int -> unit
    (* adjust the value of labels in the pseudo_op given the current
     * location counter.
     *)
  
end


(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:25  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:27  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:27  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/11/16 21:49:03  george
 *   Version 110.10
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
