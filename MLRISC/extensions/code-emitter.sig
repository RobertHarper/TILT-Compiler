(*
 * This is an abstract interface for generating assembly, or machine code.
 * It is also used by the front-end to generate code for the back-end.
 * Unlike the signature EMITTER_NEW or FLOWGRAPH_GEN, this one is not
 * tied into any form of flowgraph representation.  
 *) 

signature CODE_EMITTER =
sig

   structure I : INSTRUCTIONS
   structure C : CELLS
   structure P : PSEUDO_OPS
   structure B : BLOCK_NAMES
      sharing I.C = C

   type emitter =
   {  defineLabel : Label.label -> unit,   (* internal labels *)
      entryLabel  : Label.label -> unit,   (* external labels *)
      exitBlock   : C.cellset -> unit,     (* escaping exit *)
      pseudoOp    : P.pseudo_op -> unit,   (* emit pseudo ops *)
      emitInstr   : I.instruction -> unit, (* emit an instruction *) 
      blockName   : B.name -> unit,        (* change the block name *)
      comment     : string -> unit,        (* emit a comment *) 
      init        : int -> unit,           
                           (* initialize (n bytes if for machine code) *)
      finish      : unit -> unit           (* finish and clean up *)
   } 

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:57  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:38  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:06  pscheng
# *** empty log message ***
#
 *)
