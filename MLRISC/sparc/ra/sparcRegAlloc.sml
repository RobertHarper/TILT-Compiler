(* sparcRegAlloc.sml --- sparc integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)

functor SparcRegAlloc(structure I : INSTRUCTIONS where C = SparcCells
		      structure P : INSN_PROPERTIES where I = I
		      structure F : FLOWGRAPH where I = I 
		      structure Asm : EMITTER_NEW where I = I and P=F.P
		     ) :
  sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where type I.operand = I.operand
		       and type I.instruction = I.instruction
		       and type B.name = F.B.name) : sig
      datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
      val ra : mode -> F.cluster -> F.cluster
     end
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		     where type I.operand = I.operand
		       and type I.instruction = I.instruction
		       and type B.name = F.B.name) : sig
      datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
      val ra : mode -> F.cluster -> F.cluster
     end
   end=
struct

  structure C=I.C

  (* liveness analysis for general purpose registers *)
  structure RegLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     fun regSet c = #1 (c:SparcCells.cellset)
	     fun cellset((_,f),r) = (r,f))


  functor IntRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=RegLiveness

	   val defUse = P.defUse C.GP
	   val firstPseudoR = 32
	   val maxPseudoR = SparcCells.maxCell
	   val numRegs = SparcCells.numCell SparcCells.GP
	   fun regSet c = #1 (c:SparcCells.cellset)
	end)

  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     fun regSet c = #2 (c:SparcCells.cellset)
	     fun cellset((r,_),f) = (r,f))

  functor FloatRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=FregLiveness

 	   val defUse = P.defUse C.FP
	   val firstPseudoR = 32
	   val maxPseudoR = SparcCells.maxCell 
	   val numRegs = SparcCells.numCell SparcCells.FP
	   fun regSet c = #2 (c:SparcCells.cellset)
	end)
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:47  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:50  pscheng
# *** empty log message ***
#
 * Revision 1.3  1998/10/06 14:06:27  george
 *  fixed up some machine description problems. [leunga]
 *
 * Revision 1.2  1998/09/30 19:38:39  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
