(* hppaRegAlloc.sml --- hppa integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)

functor HppaRegAlloc(structure I : INSTRUCTIONS where C = HppaCells
		     structure P : INSN_PROPERTIES where I = I
		     structure F : FLOWGRAPH where I = I and P = P
		     structure Asm : EMITTER_NEW where I = I and P = P) :
  sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name) : sig
      datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
      val ra : mode -> F.cluster -> F.cluster
     end
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name) : sig
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
	     fun regSet c = #1 (c:HppaCells.cellset)
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
	   val maxPseudoR = HppaCells.maxCell
	   val numRegs = HppaCells.numCell HppaCells.GP
	   fun regSet c = #1 (c:HppaCells.cellset)
	end)

  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     fun regSet c = #2 (c:HppaCells.cellset)
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
	   val maxPseudoR = HppaCells.maxCell 
	   val numRegs = HppaCells.numCell HppaCells.FP
	   fun regSet c = #2 (c:HppaCells.cellset)
	end)
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:11  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:28  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:09  pscheng
# *** empty log message ***
#
 * Revision 1.6  1998/10/06 14:04:36  george
 *   The instruction sequence FCMP, FTEST, FBCC is being replaced
 *   by the composite instruction FBRANCH.  This makes scheduling and
 *   other tasks easier.  Also, added BLR and BL in the instruction set.
 * 							[leunga]
 *
 * Revision 1.5  1998/09/30 19:36:10  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.4  1998/07/25 03:08:16  george
 *   added to support block names in MLRISC
 *
 * Revision 1.3  1998/05/25 15:10:59  george
 *   Fixed RCS keywords
 *
 *)
