
(* =========================================================================
 * IntegerDataFlow.sml
 * ========================================================================= *)

functor IntegerDataFlow(
	  structure IntSet:	 ORD_SET where type Key.ord_key = int
	  structure MLTreeExtra: MLTREE_EXTRA
	) :> REGISTER_DATA_FLOW
	       where type set	 = IntSet.set
		 and type mlrisc = MLTreeExtra.MLTree.mlrisc
		 and type mltree = MLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type set = IntSet.set

  type mlrisc = MLTree.mlrisc

  type mltree = MLTree.mltree

  (* -- exceptions --------------------------------------------------------- *)

  exception InvalidSource of string

  (* -- set functions ------------------------------------------------------ *)

  (*
   * Remove a given element/list of elements from a given set.
   * set -> the set to remove the element(s) from
   *	 -> the element(s) to remove
   * <- the new set
   *)
  fun remove(set, element) =
	IntSet.difference(set, IntSet.singleton element)
  fun removeList(set, elements) =
	IntSet.difference(set, IntSet.addList(IntSet.empty, elements))

  (* -- functions ---------------------------------------------------------- *)

  local
    (*
     * Add the define set of a given register to a given set.
     *)
    fun defineMlrisc(MLTree.GPR(MLTree.REG id), set) = IntSet.add(set, id)
      | defineMlrisc(_, set)			     = set

    (*
     * Add the define set of a given statement to a given set.
     *)
    fun defineStatement(MLTree.MV(target, _), set) =
	  IntSet.add(set, target)
      | defineStatement(MLTree.COPY(targets, _), set) =
	  IntSet.addList(set, targets)
      | defineStatement(MLTree.CALL(_, defines, _), set) =
	  foldr defineMlrisc set defines
      | defineStatement(_, set) =
	  set

    (*
     * Add the define set of a given statement/directive to a given set.
     *)
    fun defineTree(MLTree.CODE code, set) = foldr defineStatement set code
      | defineTree(_, set)		  = set
  in
    fun defineExpression _ = IntSet.empty

    val define = foldr defineTree IntSet.empty
  end

  local
    (*
     * Add the use set of a given integer expression to a given set.
     *)
    fun useExp(MLTree.REG source, set) = IntSet.add(set, source)
      | useExp(MLTree.LI _, set)       = set
      | useExp(MLTree.LI32 _, set)     = set
      | useExp(MLTree.LABEL _, set)    = set
      | useExp(MLTree.CONST _, set)    = set
      | useExp(MLTree.ADD exps, set)   = useExp2(exps, set)
      | useExp(MLTree.SUB exps, set)   = useExp2'(exps, set)
      | useExp(MLTree.MULU exps, set)  = useExp2(exps, set)
      | useExp(MLTree.DIVU exps, set)  = useExp2'(exps, set)
      | useExp(MLTree.ADDT exps, set)  = useExp2(exps, set)
      | useExp(MLTree.SUBT exps, set)  = useExp2'(exps, set)
      | useExp(MLTree.MULT exps, set)  = useExp2(exps, set)
      | useExp(MLTree.DIVT exps, set)  = useExp2'(exps, set)
      | useExp(MLTree.LOAD8 exp, set)  = useExp(exp, set)
      | useExp(MLTree.LOAD32 exp, set) = useExp(exp, set)
      | useExp(MLTree.ANDB exps, set)  = useExp2(exps, set)
      | useExp(MLTree.ORB exps, set)   = useExp2(exps, set)
      | useExp(MLTree.XORB exps, set)  = useExp2(exps, set)
      | useExp(MLTree.SRA exps, set)   = useExp2'(exps, set)
      | useExp(MLTree.SRL exps, set)   = useExp2'(exps, set)
      | useExp(MLTree.SLL exps, set)   = useExp2'(exps, set)
      | useExp(MLTree.SEQ _, _)	       =
	  raise InvalidSource "unable to analyze sequence expressions"

    and useExp2((exp1, exp2), set) = useExp(exp1, useExp(exp2, set))

    and useExp2'((exp1, exp2, _), set) = useExp(exp1, useExp(exp2, set))

    (*
     * Add the use set of a given floating-point expression to a given set.
     *)
    fun useFExp(MLTree.FREG _, set)	= set
      | useFExp(MLTree.LOADD exp, set)	= useExp(exp, set)
      | useFExp(MLTree.FADDD _, set)	= set
      | useFExp(MLTree.FSUBD _, set)	= set
      | useFExp(MLTree.FMULD _, set)	= set
      | useFExp(MLTree.FDIVD _, set)	= set
      | useFExp(MLTree.FABSD _, set)	= set
      | useFExp(MLTree.FNEGD _, set)	= set
      | useFExp(MLTree.CVTI2D exp, set) = useExp(exp, set)
      | useFExp(MLTree.FSEQ _, _)	=
	  raise InvalidSource "unable to analyze sequence expressions"

    (*
     * Add the use set of a given conditional expression to a given set.
     *)
    fun useCCExp(MLTree.CC _, set) =
	  set
      | useCCExp(MLTree.LOADCC exp, set) =
	  useExp(exp, set)
      | useCCExp(MLTree.CMP(_, exp1, exp2, _), set) =
	  useExp(exp1, useExp(exp2, set))
      | useCCExp(MLTree.FCMP _, set) =
	  set

    (*
     * Remove the define set of a given register from a given set.
     *)
    fun defineMlrisc(MLTree.GPR(MLTree.REG id), set) =
	  remove(set, id)
      | defineMlrisc(MLTree.GPR _, _) =
	  raise InvalidSource "call parameter is not a register"
      | defineMlrisc(_, set) =
	  set

    (*
     * Add the use set of a given register to a given set.
     *)
    fun useMlrisc(MLTree.GPR(MLTree.REG id), set) =
	  IntSet.add(set, id)
      | useMlrisc(MLTree.GPR _, _) =
	  raise InvalidSource "call parameter is not a register"
      | useMlrisc(_, set) =
	  set

    (*
     * Adjust a given use set according to the use and define sets of a given
     * statement.
     *)
    fun useStatement(MLTree.MV(target, exp), set) =
	  useExp(exp, remove(set, target))
      | useStatement(MLTree.FMV(_, fexp), set) =
	  useFExp(fexp, set)
      | useStatement(MLTree.CCMV(_, ccexp), set) =
	  useCCExp(ccexp, set)
      | useStatement(MLTree.COPY(targets, sources), set) =
	  IntSet.addList(removeList(set, targets), sources)
      | useStatement(MLTree.FCOPY _, set) =
	  set
      | useStatement(MLTree.JMP(exp, _), set) =
	  useExp(exp, set)
      | useStatement(MLTree.CALL(exp, defines, uses), set) =
	  useExp(exp, foldr useMlrisc (foldr defineMlrisc set defines) uses)
      | useStatement(MLTree.RET, set) =
	  set
      | useStatement(MLTree.STORE8(exp1, exp2), set) =
	  useExp(exp1, useExp(exp2, set))
      | useStatement(MLTree.STORE32(exp1, exp2), set) =
	  useExp(exp1, useExp(exp2, set))
      | useStatement(MLTree.STORED(exp, fexp), set) =
	  useExp(exp, useFExp(fexp, set))
      | useStatement(MLTree.STORECC(exp, ccexp), set) =
	  useExp(exp, useCCExp(ccexp, set))
      | useStatement(MLTree.BCC(_, ccexp, _), set) =
	  useCCExp(ccexp, set)
      | useStatement(MLTree.FBCC(_, ccexp, _), set) =
	  useCCExp(ccexp, set)

    (*
     * Adjust a given use set according to the use and define sets of a given
     * statement/directive.
     *)
    fun useTree(MLTree.CODE code, set)		 = foldr useStatement set code
      | useTree(MLTree.ESCAPEBLOCK escapes, set) = foldr useMlrisc set escapes
      | useTree(_, set)				 = set
  in
    fun useExpression(MLTree.GPR exp)	= useExp(exp, IntSet.empty)
      | useExpression(MLTree.FPR fexp)	= useFExp(fexp, IntSet.empty)
      | useExpression(MLTree.CCR ccexp) = useCCExp(ccexp, IntSet.empty)

    val use_ = foldr useTree IntSet.empty
  end

end

