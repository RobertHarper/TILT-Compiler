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

