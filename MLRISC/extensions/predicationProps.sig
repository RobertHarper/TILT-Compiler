signature PREDICATION_PROPERTIES = 
sig
   structure I : PREDICATED_INSTRUCTIONS

   (*========================================================================
    *  These are the predicate expressions we currently recongize
    *========================================================================*)
   datatype pred_expr =
       CMP of string * pred_opn * pred_opn
     | PREG of I.C.register
     | OR   of pred_expr * pred_expr
     | AND  of pred_expr * pred_expr
     | NOT  of pred_expr
     | FALSE
     | TRUE

   and pred_opn = REG of I.C.register
                | IMMED of int
                | OTHER_IMMED of int

   datatype predicate_instr =
     DEF   of I.C.register * pred_expr
   | OTHER of {defs:I.C.register list,uses:I.C.register list}

   (*========================================================================
    *  The true predicate
    *========================================================================*)
   val truePredicate : I.predicate

   (*========================================================================
    *  Update the predicate of an instruction
    *========================================================================*)
   val updatePredicate : I.instruction * I.predicate -> I.instruction

   (*========================================================================
    *  Lookup the current predicate
    *========================================================================*)
   val lookupPredicate : I.instruction -> I.predicate

   (*========================================================================
    *  Translate a predicate 
    *========================================================================*)
   val info : I.predicate -> {r:I.C.register, neg:bool} option

   (*========================================================================
    *  Generate code to compute the disjunction of a list of predicates
    *========================================================================*)
   val predicatedOr  : I.predicate list
                         -> {instrs : I.instruction list,
                             p      : I.predicate
                            }

   (*========================================================================
    *  Given a branch instruction, the current predicate p,
    *  compute the predicate for the true branch and the false branch
    *  If the branch condition is c, then we generate c/\p and 
    *  ~c/\p as the p_T and p_F predicates.
    *========================================================================*)
   val branchToSetPredicate : {instr       : I.instruction, 
                               p           : I.predicate,
                               trueBranch  : bool, 
                               falseBranch : bool
                              } -> 
                              { instrs : I.instruction list, 
                                p_T    : I.predicate, 
                                p_F    : I.predicate
                              }
   (*========================================================================
    *  Given a branch instruction, the current predicate p,
    *  generate code that branch if both the predicate and the
    *  condition is true.
    *========================================================================*)
   val branchToSideExit     : {instr:I.instruction, p:I.predicate}
                               -> I.instruction list

   (*========================================================================
    *  Given an instruction, classify it as either a predicate 
    *  definition instruction or others
    *========================================================================*)
   val predicateInstr : I.instruction -> predicate_instr

end
