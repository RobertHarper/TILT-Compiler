
(* =========================================================================
 * AlphaIntegerAllocation.sml
 * ========================================================================= *)

functor AlphaIntegerAllocation(
	  structure AlphaInstructions: ALPHA32INSTR
	  structure Cells:	       CELLS
	  structure FlowGraph:	       FLOWGRAPH
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int
	  structure RegisterSpillMap:  REGISTER_SPILL_MAP

	  structure AlphaRewrite: sig

	    structure I: ALPHA32INSTR

	    val rewriteUse: I.instruction * int * int -> I.instruction
	    val rewriteDef: I.instruction * int * int -> I.instruction

	  end

	  functor RegisterAllocation(
	    structure RaUser: RA_USER_PARAMS
				where type I.operand =
					     AlphaInstructions.operand
				  and type I.instruction =
					     AlphaInstructions.instruction
	  ): sig
	    val ra: FlowGraph.cluster -> FlowGraph.cluster
	  end

	  sharing type AlphaInstructions.instruction =
		       AlphaRewrite.I.instruction
	      and type AlphaInstructions.Constant.const =
		       RegisterSpillMap.offset
	      and type IntegerConvention.id =
		       RegisterSpillMap.id
	) :> REGISTER_ALLOCATION
	       where type spillMap = RegisterSpillMap.map
		 and type cluster  = FlowGraph.cluster
	  = struct

  (* -- types -------------------------------------------------------------- *)

  type spillMap = RegisterSpillMap.map

  type cluster = FlowGraph.cluster

  (* -- values ------------------------------------------------------------- *)

  val spillMap = RegisterSpillMap.map()

  (* -- get register structure --------------------------------------------- *)

  structure GetRegister =
    GetReg(val available = IntegerConvention.available
	   val nRegs	 = 32)

  (* -- user parameters structure ------------------------------------------ *)

  structure UserParameters
	      :> RA_USER_PARAMS
		   where type I.operand	    = AlphaInstructions.operand
		     and type I.instruction = AlphaInstructions.instruction
	      = struct

    (* -- structures ------------------------------------------------------- *)

    structure I = AlphaInstructions

    (* -- register allocation values --------------------------------------- *)

    val nFreeRegs = length IntegerConvention.available
    val dedicated = IntegerConvention.dedicated
    val getreg	  = GetRegister.getreg

    fun copyInstr(dest, src) = AlphaInstructions.COPY(dest, src, ref NONE)

    (* -- spill functions -------------------------------------------------- *)

    local
      fun template(id, offset) =
	    AlphaInstructions.STORE{
	      stOp = AlphaInstructions.STL,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = AlphaInstructions.CONSTop offset
	    }::nil

      val lookup = RegisterSpillMap.lookupSpill spillMap
    in
      fun spill{instr = instruction, reg = target} =
	    case instruction of
	      AlphaInstructions.COPY([_], [source], _) =>
		{code  = template(source, lookup target),
		 proh  = [],
		 instr = NONE}
	    | _ =>
		let
		  val target' = Cells.newReg()
		in
		  {code	 = template(target', lookup target),
		   proh	 = [target'],
		   instr = SOME(AlphaRewrite.rewriteDef
				  (instruction, target, target'))}
		end
    end

    local
      fun template(id, offset, tail) =
	    AlphaInstructions.LOAD{
	      ldOp = AlphaInstructions.LDL,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = AlphaInstructions.CONSTop offset
	    }::tail

      val lookup = RegisterSpillMap.lookupReload spillMap
    in
      fun reload{instr = instruction, reg = source} =
	    case instruction of
	      AlphaInstructions.COPY([target], [_], _) =>
		{code = template(target, lookup source, []),
		 proh = []}
	    | _ =>
		let
		  val source' = Cells.newReg()
		in
		  {code = template(source', lookup source,
				   [AlphaRewrite.rewriteUse
				      (instruction, source, source')]),
		   proh = [source']}
		end
    end

  end

  (* -- structures --------------------------------------------------------- *)

  structure IntegerAllocation =
    RegisterAllocation(structure RaUser = UserParameters)

  (* -- functions ---------------------------------------------------------- *)

  fun allocateCluster cluster = (IntegerAllocation.ra cluster before
				 GetRegister.reset())

end

