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
