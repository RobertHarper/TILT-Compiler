functor AsmEmit
  (structure F : FLOWGRAPH
   structure E : EMITTER_NEW
      sharing F.I = E.I
      sharing F.P = E.P) = 
struct
  structure PseudoOp = F.P
  fun asmEmit(F.CLUSTER{blocks, regmap, ...}) = let
    fun emit(F.PSEUDO pOp) = E.pseudoOp pOp
      | emit(F.LABEL lab) = E.defineLabel lab
      | emit(F.BBLOCK{insns, ...}) =
         app (fn insn => E.emitInstr(insn, regmap)) (rev (!insns))
      | emit(F.ORDERED blks) = app emit blks
  in app emit blocks
  end
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:56  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:28  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:03  pscheng
# *** empty log message ***
#
 * Revision 1.2  1998/10/06 14:07:42  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
