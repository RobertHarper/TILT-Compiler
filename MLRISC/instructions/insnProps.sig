(* insnProps.sig --- instruction set properties
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)
signature INSN_PROPERTIES = sig
   structure C : CELLS
   structure I : INSTRUCTIONS

   sharing I.C = C 

   exception NegateConditional

   datatype kind = IK_JUMP | IK_NOP | IK_INSTR
   datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
   val instrKind     : I.instruction -> kind
      (* kind of instruction  *)

   val moveInstr     : I.instruction -> bool
      (* is the instruction a move? Assumed to have exactly one
       * source and one destination 
       *)
   val nop 	     : unit -> I.instruction
      (* generate a nop *)

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
   val moveTmpR : I.instruction -> int option
      (* temporary register associated with parallel move
       * instructions if any.
       *)

   val moveDstSrc : I.instruction -> int list * int list
      (* source and destinations associated with a parallel move *)

   val copy : { src : int list, dst : int list } -> I.instruction
      (* create a parallel move *) 

   val fcopy : { src : int list, dst : int list } -> I.instruction
      (* create a floating point parallel move *) 

   val splitCopies:{regmap:int->int, insns:I.instruction list} -> I.instruction list
      (* split all parallel moves in the instruction stream into individual moves *)

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
   val branchTargets : I.instruction -> target list
      (* targets of an instruction. The instruction kind must be IK_JUMP *)

   val jump : Label.label -> I.instruction
      (* create a jump instruction *)

   val setTargets : I.instruction * Label.label list -> I.instruction
      (* set the targets of a branch/jump instruction. 
       * do nothing if the instruction is not a branch/jump instruction
       *)   

   val negateConditional : I.instruction -> I.instruction
      (* negate a conditional branch.
       * raise NegateConditional if the instruction is not a conditional 
       * or if it cannot be negated.
       *)

   (*========================================================================
    *  Definition and use (for register allocation mainly)
    *========================================================================*)
   val defUse : C.cellclass -> I.instruction -> (int list * int list)
      (* def/use lists *)
end
