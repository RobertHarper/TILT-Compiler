(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)
signature MLTREECOMP = sig
    structure T : MLTREE
    structure I : INSTRUCTIONS

    val mltreeComp : T.mltree -> unit
    val mlriscComp : T.stm -> unit
    val emitInstr : I.instruction -> unit
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
 * Revision 1.2  1998/08/11 14:03:24  george
 *   Exposed emitInstr in MLTREECOMP to allow a client to directly
 *   inject native instructions into the flowgraph.
 *
 *)
