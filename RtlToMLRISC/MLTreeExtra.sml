
(* =========================================================================
 * MLTreeExtra.sml
 * ========================================================================= *)

functor MLTreeExtra(
	  structure MLTree: MLTREE
	) :> MLTREE_EXTRA
	       where type MLTree.Constant.const	    = MLTree.Constant.const
		 and type MLTree.PseudoOp.pseudo_op = MLTree.PseudoOp.pseudo_op
		 and type MLTree.Region.region      = MLTree.Region.region
		 and type MLTree.cond		    = MLTree.cond
		 and type MLTree.fcond		    = MLTree.fcond
		 and type MLTree.order		    = MLTree.order
		 and type MLTree.stm		    = MLTree.stm
		 and type MLTree.rexp		    = MLTree.rexp
		 and type MLTree.fexp		    = MLTree.fexp
		 and type MLTree.ccexp		    = MLTree.ccexp
		 and type MLTree.mlrisc		    = MLTree.mlrisc
		 and type MLTree.mltree		    = MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTree = MLTree

  (* -- functions ---------------------------------------------------------- *)

  val ccr = MLTree.CCR o MLTree.CC
  val gpr = MLTree.GPR o MLTree.REG
  val fpr = MLTree.FPR o MLTree.FREG

  fun copy(target, source) = MLTree.COPY([target], [source])

  fun fcopy(target, source) = MLTree.FCOPY([target], [source])

  fun mv(target, MLTree.REG source) = MLTree.COPY([target], [source])
    | mv(target, source)	    = MLTree.MV(target, source)

  fun fmv(target, MLTree.FREG source) = MLTree.FCOPY([target], [source])
    | fmv(target, source)	      = MLTree.FMV(target, source)

  fun copyList([], [])	       = []
    | copyList(target, source) = [MLTree.COPY(target, source)]

  fun fcopyList([], [])		= []
    | fcopyList(target, source) = [MLTree.FCOPY(target, source)]

  fun add(MLTree.LI int1, MLTree.LI int2) = MLTree.LI(int1+int2)
    | add(MLTree.LI 0, exp2)		  = exp2
    | add(exp1, MLTree.LI 0)		  = exp1
    | add(exp1, exp2)			  = MLTree.ADD(exp1, exp2)

  local
    fun <<(int1, 0)    = int1
      | <<(int1, int2) = <<(int1*2, int2-1)
  in
    fun sll(MLTree.LI int1, MLTree.LI int2) = MLTree.LI(<<(int1, int2))
      | sll(MLTree.LI 0, _)		    = MLTree.LI 0
      | sll(exp1, MLTree.LI 0)		    = exp1
      | sll(exp1, exp2)			    = MLTree.SLL(exp1, exp2, MLTree.LR)
  end

end

