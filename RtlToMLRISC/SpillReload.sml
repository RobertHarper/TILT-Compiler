(*$import MLTREE_EXTRA SPILL_RELOAD *)

(* =========================================================================
 * SpillReload.sml
 * ========================================================================= *)

functor SpillReload(
	  structure MLTreeExtra:  MLTREE_EXTRA
	) :> SPILL_RELOAD
	       where type id	 = int
		 and type rexp	 = MLTreeExtra.MLTree.rexp
		 and type fexp	 = MLTreeExtra.MLTree.fexp
		 and type mltree = MLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type rexp   = MLTree.rexp
  type fexp   = MLTree.fexp
  type mltree = MLTree.mltree

  (* -- exceptions --------------------------------------------------------- *)

  exception InvalidSource of string

  (* -- assignment functions ----------------------------------------------- *)

  (*
   * Normalize a given move.
   *)
  fun integerAssign(MLTree.REG target, source) =
	MLTree.MV(target, source)
    | integerAssign(MLTree.LOAD32(target, region), source) =
	MLTree.STORE32(target, source, region)
    | integerAssign _ =
	raise InvalidSource "unable to normalize expression"

  fun floatAssign(MLTree.FREG target, source) =
	MLTree.FMV(target, source)
    | floatAssign(MLTree.LOADD(target, region), source) =
	MLTree.STORED(target, source, region)
    | floatAssign _ =
	raise InvalidSource "unable to normalize expression"

  (*
   * Return registers that can be copied from a given assignment.
   *)
  fun integerCopied(MLTree.REG target, MLTree.REG source, (targets, sources)) =
	(target::targets, source::sources)
    | integerCopied(_, _, results) =
	results

  fun floatCopied(MLTree.FREG target, MLTree.FREG source, (targets, sources)) =
	(target::targets, source::sources)
    | floatCopied(_, _, results) =
	results

  (*
   * Return the necessary move and store statements from a given assignment.
   *)
  fun integerMoved(MLTree.REG _, MLTree.REG _, result) =
	result
    | integerMoved(MLTree.REG target, source, result) =
	MLTree.MV(target, source)::result
    | integerMoved(MLTree.LOAD32(target, region), source, result) =
	MLTree.STORE32(target, source, region)::result
    | integerMoved _ =
	raise InvalidSource "unable to normalize expression"

  fun floatMoved(MLTree.FREG _, MLTree.FREG _, result) =
	result
    | floatMoved(MLTree.FREG target, source, result) =
	MLTree.FMV(target, source)::result
    | floatMoved(MLTree.LOADD(target, region), source, result) =
	MLTree.STORED(target, source, region)::result
    | floatMoved _ =
	raise InvalidSource "unable to normalize expression"

  (* -- functions ---------------------------------------------------------- *)

  fun transform((integerSpill, integerReload), (floatSpill, floatReload)) =
	let
	  (*
	   * Spill/reload the registers in a given integer expression.
	   *)
	  fun transformExp(MLTree.REG source) =
		integerReload source
	    | transformExp(exp as MLTree.LI _) =
		exp
	    | transformExp(exp as MLTree.LI32 _) =
		exp
	    | transformExp(exp as MLTree.LABEL _) =
		exp
	    | transformExp(exp as MLTree.CONST _) =
		exp
	    | transformExp(MLTree.ADD(exp1, exp2)) =
		MLTree.ADD(transformExp exp1, transformExp exp2)
	    | transformExp(MLTree.SUB(exp1, exp2, order)) =
		MLTree.SUB(transformExp exp1, transformExp exp2, order)
	    | transformExp(MLTree.MULU(exp1, exp2)) =
		MLTree.MULU(transformExp exp1, transformExp exp2)
	    | transformExp(MLTree.DIVU(exp1, exp2, order)) =
		MLTree.DIVU(transformExp exp1, transformExp exp2, order)
	    | transformExp(MLTree.ADDT(exp1, exp2)) =
		MLTree.ADDT(transformExp exp1, transformExp exp2)
	    | transformExp(MLTree.SUBT(exp1, exp2, order)) =
		MLTree.SUBT(transformExp exp1, transformExp exp2, order)
	    | transformExp(MLTree.MULT(exp1, exp2)) =
		MLTree.MULT(transformExp exp1, transformExp exp2)
	    | transformExp(MLTree.DIVT(exp1, exp2, order)) =
		MLTree.DIVT(transformExp exp1, transformExp exp2, order)
	    | transformExp(MLTree.LOAD8(exp, region)) =
		MLTree.LOAD8(transformExp exp, region)
	    | transformExp(MLTree.LOAD32(exp, region)) =
		MLTree.LOAD32(transformExp exp, region)
	    | transformExp(MLTree.ANDB(exp1, exp2)) =
		MLTree.ANDB(transformExp exp1, transformExp exp2)
	    | transformExp(MLTree.ORB(exp1, exp2)) =
		MLTree.ORB(transformExp exp1, transformExp exp2)
	    | transformExp(MLTree.XORB(exp1, exp2)) =
		MLTree.XORB(transformExp exp1, transformExp exp2)
	    | transformExp(MLTree.SRA(exp1, exp2, order)) =
		MLTree.SRA(transformExp exp1, transformExp exp2, order)
	    | transformExp(MLTree.SRL(exp1, exp2, order)) =
		MLTree.SRL(transformExp exp1, transformExp exp2, order)
	    | transformExp(MLTree.SLL(exp1, exp2, order)) =
		MLTree.SLL(transformExp exp1, transformExp exp2, order)
	    | transformExp(MLTree.SEQ _) =
		raise InvalidSource "unable to transform sequence expressions"

	  (*
	   * Spill/reload the registers in a given floating-point expression.
	   *)
	  fun transformFExp(MLTree.FREG source) =
		floatReload source
	    | transformFExp(MLTree.LOADD(exp, region)) =
		MLTree.LOADD(transformExp exp, region)
	    | transformFExp(MLTree.FADDD(fexp1, fexp2)) =
		MLTree.FADDD(transformFExp fexp1, transformFExp fexp2)
	    | transformFExp(MLTree.FSUBD(fexp1, fexp2, order)) =
		MLTree.FSUBD(transformFExp fexp1, transformFExp fexp2, order)
	    | transformFExp(MLTree.FMULD(fexp1, fexp2)) =
		MLTree.FMULD(transformFExp fexp1, transformFExp fexp2)
	    | transformFExp(MLTree.FDIVD(fexp1, fexp2, order)) =
		MLTree.FDIVD(transformFExp fexp1, transformFExp fexp2, order)
	    | transformFExp(MLTree.FABSD fexp) =
		MLTree.FABSD(transformFExp fexp)
	    | transformFExp(MLTree.FNEGD fexp) =
		MLTree.FNEGD(transformFExp fexp)
	    | transformFExp(MLTree.CVTI2D exp) =
		MLTree.CVTI2D(transformExp exp)
	    | transformFExp(MLTree.FSEQ _) =
		raise InvalidSource "unable to transform sequence expressions"

	  (*
	   * Spill/reload the registers in a given conditional expression.
	   *)
	  fun transformCCExp(ccexp as MLTree.CC _) =
		ccexp
	    | transformCCExp(MLTree.LOADCC(exp, region)) =
		MLTree.LOADCC(transformExp exp, region)
	    | transformCCExp(MLTree.CMP(cond, exp1, exp2, order)) =
		MLTree.CMP(cond, transformExp exp1, transformExp exp2,
			   order)
	    | transformCCExp(MLTree.FCMP(fcond, fexp1, fexp2, order)) =
		MLTree.FCMP(fcond, transformFExp fexp1, transformFExp fexp2,
			    order)

	  (*
	   * Spill/reload the registers in a given statement.
	   *)
	  fun transformStatement(MLTree.MV(target, exp)) =
		[integerAssign(integerSpill target, transformExp exp)]
	    | transformStatement(MLTree.FMV(target, fexp)) =
		[floatAssign(floatSpill target, transformFExp fexp)]
	    | transformStatement(MLTree.CCMV(target, ccexp)) =
		[MLTree.CCMV(target, transformCCExp ccexp)]
	    | transformStatement(MLTree.COPY(targets, sources)) =
		let
		  val assign = (map integerSpill targets,
				map integerReload sources)
		  val copies = ListPair.foldr integerCopied ([], []) assign
		  val moves  = ListPair.foldr integerMoved [] assign
		in
		  case copies of
		    ([], []) => moves
		  | _	     => MLTree.COPY copies::moves
		end
	    | transformStatement(MLTree.FCOPY(targets, sources)) =
		let
		  val assign = (map floatSpill targets,
				map floatReload sources)
		  val copies = ListPair.foldr floatCopied ([], []) assign
		  val moves  = ListPair.foldr floatMoved [] assign
		in
		  case copies of
		    ([], []) => moves
		  | _	     => MLTree.FCOPY copies::moves
		end
	    | transformStatement(MLTree.JMP(exp, predict)) =
		[MLTree.JMP(transformExp exp, predict)]
	    | transformStatement(MLTree.CALL(exp, defines, uses)) =
		[MLTree.CALL(transformExp exp, defines, uses)]
	    | transformStatement(MLTree.RET) =
		[MLTree.RET]
	    | transformStatement(MLTree.STORE8(exp1, exp2, region)) =
		[MLTree.STORE8(transformExp exp1, transformExp exp2,
			       region)]
	    | transformStatement(MLTree.STORE32(exp1, exp2, region)) =
		[MLTree.STORE32(transformExp exp1, transformExp exp2,
				region)]
	    | transformStatement(MLTree.STORED(exp, fexp, region)) =
		[MLTree.STORED(transformExp exp, transformFExp fexp,
			       region)]
	    | transformStatement(MLTree.STORECC(exp, ccexp, region)) =
		[MLTree.STORECC(transformExp exp, transformCCExp ccexp,
				region)]
	    | transformStatement(MLTree.BCC(cond, ccexp, label)) =
		[MLTree.BCC(cond, transformCCExp ccexp, label)]
	    | transformStatement(MLTree.FBCC(fcond, ccexp, label)) =
		[MLTree.FBCC(fcond, transformCCExp ccexp, label)]

	  (*
	   * Spill/reload the registers in a given statement/directive.
	   *)
	  local
	    fun transformStatement'(statement, list) =
		  transformStatement statement@list
	  in
	    fun transformTree(MLTree.CODE code) =
		  MLTree.CODE(foldr transformStatement' [] code)
	      | transformTree(MLTree.ORDERED _) =
		  raise InvalidSource "unable to transform ordered directives"
	      | transformTree tree =
		  tree
	  end
	in
	  map transformTree
	end

end

