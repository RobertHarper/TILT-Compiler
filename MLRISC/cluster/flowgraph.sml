(* flowgraph.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Defines the flowgraph data structure used as the intermediate program
 * representation.
 *
 *)

signature FLOWGRAPH = sig

  structure B : BLOCK_NAMES
  structure C : CELLS
  structure I : INSTRUCTIONS
  structure P : PSEUDO_OPS
	  sharing I.C = C



  datatype block =
      PSEUDO of P.pseudo_op
    | LABEL of Label.label
    | BBLOCK of { blknum  : int,
		  name    : B.name,
		  liveIn  : C.cellset ref,
		  liveOut : C.cellset ref,
		  succ 	  : block list ref,
		  pred 	  : block list ref,
		  insns	  : I.instruction list ref
	        }
    | ENTRY of {blknum : int,
		succ : block list ref}
    | EXIT of {blknum : int,
	       pred : block list ref}
    | ORDERED of block list

  datatype cluster = 
      CLUSTER of {
        blocks: block list, 
	entry : block,
	exit  : block,	  
        regmap: int Intmap.intmap,
        blkCounter : int ref
      }
end


(*  Create the flowgraph data structure specialized towards	
 *  a specific type of cells and instructions.
 *)
functor FlowGraph(structure I : INSTRUCTIONS
		  structure P : PSEUDO_OPS
		  structure B : BLOCK_NAMES) : FLOWGRAPH = 
struct
  structure I = I
  structure C = I.C
  structure P = P
  structure B = B

  datatype block =
      PSEUDO of P.pseudo_op
    | LABEL of Label.label
    | BBLOCK of { blknum  : int,
		  name    : B.name,
		  liveIn  : C.cellset ref,
		  liveOut : C.cellset ref,
		  succ 	  : block list ref,
		  pred 	  : block list ref,
		  insns	  : I.instruction list ref
	        }
    | ENTRY of {blknum : int,
		succ : block list ref}
    | EXIT of {blknum : int,
	       pred : block list ref}
    | ORDERED of block list

  datatype cluster = 
      CLUSTER of {
        blocks: block list, 
	entry : block,
	exit  : block,	  
        regmap: int Intmap.intmap,
        blkCounter : int ref
      }
end




