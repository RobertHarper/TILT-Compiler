
(* =========================================================================
 * AlphaIntegerAllocation.sml
 * ========================================================================= *)

functor AlphaIntegerAllocation(
	  structure AlphaInstructions: ALPHA32INSTR
	  structure Cells:	       CELLS
	  structure FlowGraph:	       FLOWGRAPH
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int

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
	) :> REGISTER_ALLOCATION
	       where type id      = int
		 and type offset  = AlphaInstructions.Constant.const
		 and type cluster = FlowGraph.cluster
	  = struct

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type offset = AlphaInstructions.Constant.const

  type cluster = FlowGraph.cluster

  (* -- exceptions --------------------------------------------------------- *)

  exception NoLookup

  (* -- values ------------------------------------------------------------- *)

  fun noLookup _ = raise NoLookup

  val lookupSpill  = ref(noLookup: id -> offset)
  val lookupReload = ref(noLookup: id -> offset)

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
    in
      fun spill{instr = instruction, reg = target} =
	    case instruction of
	      AlphaInstructions.COPY([_], [source], _) =>
		{code  = template(source, !lookupSpill target),
		 proh  = [],
		 instr = NONE}
	    | _ =>
		let
		  val target' = Cells.newReg()
		in
		  {code	 = template(target', !lookupSpill target),
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
    in
      fun reload{instr = instruction, reg = source} =
	    case instruction of
	      AlphaInstructions.COPY([target], [_], _) =>
		{code = template(target, !lookupReload source, []),
		 proh = []}
	    | _ =>
		let
		  val source' = Cells.newReg()
		in
		  {code = template(source', !lookupReload source,
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

  fun setLookup(lookupSpill', lookupReload') = (lookupSpill  := lookupSpill';
						lookupReload := lookupReload')

  fun allocateCluster cluster = (IntegerAllocation.ra cluster before
				 GetRegister.reset())

end

