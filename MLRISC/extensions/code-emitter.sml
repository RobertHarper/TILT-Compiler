(*
 * This is an abstract interface for generating assembly, or machine code.
 * It is also used by the front-end to generate code for the back-end.
 * Unlike the signature EMITTER_NEW or FLOWGRAPH_GEN, this one is not
 * tied into any form of flowgraph representation.  
 *) 

functor CodeEmitterFn
  (structure I : INSTRUCTIONS
   structure P : PSEUDO_OPS
   structure B : BLOCK_NAMES
  ) : CODE_EMITTER =
struct

   structure I = I
   structure C = I.C
   structure P = P
   structure B = B

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

