(* alpha32RegAlloc.sml --- alpha integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)



functor Alpha32RegAlloc(structure I : INSTRUCTIONS where C = Alpha32Cells
			structure P : INSN_PROPERTIES where I = I
			structure F : FLOWGRAPH where I = I 
			structure Asm : EMITTER_NEW 
			  where I = I and P = F.P) :
   sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name
		     (* should be: where I = I -- bug 1205 *)) : sig
        datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
	val ra : mode -> F.cluster -> F.cluster
      end
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		       where I = I
		       where type B.name = F.B.name
		       (* should be: where I = I *)) : sig
        datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
        val ra : mode -> F.cluster -> F.cluster
      end
   end =
struct
  structure C = I.C
    (* liveness analysis for general purpose registers *)
  structure RegLiveness =
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     fun regSet c = #1 (c:Alpha32Cells.cellset)
	     fun cellset((_,f),r) = (r,f))


  (* integer register allocator *)
  functor IntRa = 
      RegAllocator
	 (structure RaArch = struct

	     structure InsnProps = P
	     structure AsmEmitter = Asm
	     structure I = I
	     structure Liveness=RegLiveness
	     val defUse = P.defUse C.GP
	     val firstPseudoR = 32
	     val maxPseudoR = Alpha32Cells.maxCell
	     val numRegs = Alpha32Cells.numCell Alpha32Cells.GP
	     fun regSet c = #1 (c:Alpha32Cells.cellset)
	  end)



  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     fun regSet c = #2 (c:Alpha32Cells.cellset)
	     fun cellset((r,_),f) = (r,f))

  (* floating register allocator *)
  functor FloatRa = 
    RegAllocator
       (structure RaArch = struct

          structure InsnProps = P
	  structure AsmEmitter = Asm
	  structure Liveness=FregLiveness
	  structure I = I

	  val defUse = P.defUse C.FP
	  val firstPseudoR = 32
	  val maxPseudoR = Alpha32Cells.maxCell 
	  val numRegs = Alpha32Cells.numCell Alpha32Cells.FP
	  fun regSet c = #2 (c:Alpha32Cells.cellset)
	end)
end




(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:52  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:20  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:51  pscheng
# *** empty log message ***
#
 * Revision 1.6  1998/10/06 14:07:31  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.5  1998/09/30 19:34:39  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.4  1998/07/25 03:08:13  george
 *   added to support block names in MLRISC
 *
 * Revision 1.3  1998/05/25 15:10:49  george
 *   Fixed RCS keywords
 *
 *)
