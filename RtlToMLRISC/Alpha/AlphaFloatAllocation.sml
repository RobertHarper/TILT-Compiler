
(* =========================================================================
 * AlphaFloatAllocation.sml
 * ========================================================================= *)

functor AlphaFloatAllocation(
	  structure AlphaInstructions: ALPHA32INSTR
	  structure Cells:	       CELLS
	  structure FloatConvention:   FLOAT_CONVENTION
	  structure FlowGraph:	       FLOWGRAPH
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int
	  structure MLRISCRegion:      MLRISC_REGION

	  structure AlphaRewrite: sig

	    structure I: ALPHA32INSTR

	    val frewriteUse: I.instruction * int * int -> I.instruction
	    val frewriteDef: I.instruction * int * int -> I.instruction

	  end

	  functor RegisterAllocation(
	    structure RaUser: RA_USER_PARAMS
				where type I.operand =
					     AlphaInstructions.operand
				  and type I.instruction =
					     AlphaInstructions.instruction
	  ): sig
	    datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
	    val ra: mode -> FlowGraph.cluster -> FlowGraph.cluster
	  end

	  sharing type AlphaInstructions.instruction =
		       AlphaRewrite.I.instruction
	      and type IntegerConvention.id =
		       FloatConvention.id
	      and type MLRISCRegion.region =
		       AlphaInstructions.Region.region
	) :> REGISTER_ALLOCATION
	       where type id	  = int
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

  val stack = MLRISCRegion.stack

  fun noLookup _ = raise NoLookup

  val lookupSpill  = ref(noLookup: id -> offset)
  val lookupReload = ref(noLookup: id -> offset)

  (* -- get register structure --------------------------------------------- *)

  structure GetRegister =
    GetReg(val available = FloatConvention.available
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

    val nFreeRegs = length FloatConvention.available
    val dedicated = FloatConvention.dedicated
    val getreg	  = GetRegister.getreg

    fun copyInstr(dest, src) = AlphaInstructions.FCOPY(dest, src, ref NONE)

    (* -- spill functions -------------------------------------------------- *)

    local
      fun template(id, offset) =
	    AlphaInstructions.FSTORE{
	      stOp = AlphaInstructions.STT,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = AlphaInstructions.CONSTop offset,
	      mem  = stack
	    }::nil
    in
      fun spill{instr = instruction, reg = target} =
	    case instruction of
	      AlphaInstructions.FCOPY([_], [source], _) => 
		{code  = template(source, !lookupSpill target),
		 proh  = [],
		 instr = NONE}
	    | _ =>
		let
		  val target' = Cells.newFreg()
		in
		  {code	 = template(target', !lookupSpill target),
		   proh	 = [target'],
		   instr = SOME(AlphaRewrite.frewriteDef
				  (instruction, target, target'))}
		end
    end

    local
      fun template(id, offset, tail) =
	    AlphaInstructions.FLOAD{
	      ldOp = AlphaInstructions.LDT,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = AlphaInstructions.CONSTop offset,
	      mem  = stack
	    }::tail
    in
      fun reload{instr = instruction, reg = source} =
	    case instruction of
	      AlphaInstructions.FCOPY([target], [_], _) => 
		{code = template(target, !lookupReload source, []),
		 proh = []}
	    | _ =>
		let
		  val source' = Cells.newFreg()
		in
		  {code = template(source', !lookupReload source,
				   [AlphaRewrite.frewriteUse
				      (instruction, source, source')]),
		   proh = [source']}
		end
    end

  end

  (* -- structures --------------------------------------------------------- *)

  structure FloatAllocation =
    RegisterAllocation(structure RaUser = UserParameters)

  (* -- functions ---------------------------------------------------------- *)

  fun setLookup(lookupSpill', lookupReload') = (lookupSpill  := lookupSpill';
						lookupReload := lookupReload')

  local
    val allocate = FloatAllocation.ra FloatAllocation.REGISTER_ALLOCATION
  in
    fun allocateCluster cluster = allocate cluster before GetRegister.reset()
  end

end

