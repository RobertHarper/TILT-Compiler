(*$import TopLevel SPARC32Instr CELLS FlowGraph FLOAT_CONVENTION INTEGER_CONVENTION MLRISC_REGION SPARC32INSTR RA REGISTER_ALLOCATION *)

(* =========================================================================
 * SparcFloatAllocation.sml
 * ========================================================================= *)

functor SparcFloatAllocation(
	  structure SparcInstructions: SPARC32INSTR
	  structure Cells:	       CELLS
	  structure FloatConvention:   FLOAT_CONVENTION
	  structure FlowGraph:	       FLOWGRAPH
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int
	  structure MLRISCRegion:      MLRISC_REGION

	  structure SparcRewrite: sig

	    structure I: SPARC32INSTR

	    val frewriteUse: I.instruction * int * int -> I.instruction
	    val frewriteDef: I.instruction * int * int -> I.instruction

	  end

	  functor RegisterAllocation(
	    structure RaUser: RA_USER_PARAMS
				where type I.operand =
					     SparcInstructions.operand
				  and type I.instruction =
					     SparcInstructions.instruction
	  ): sig
	    datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
	    val ra: mode -> FlowGraph.cluster -> FlowGraph.cluster
	  end

	  sharing type SparcInstructions.instruction =
		       SparcRewrite.I.instruction
	      and type IntegerConvention.id =
		       FloatConvention.id
	      and type MLRISCRegion.region =
		       SparcInstructions.Region.region
	) :> REGISTER_ALLOCATION
	       where type id	  = int
		 and type offset  = SparcInstructions.Constant.const
		 and type cluster = FlowGraph.cluster
	  = struct

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type offset = SparcInstructions.Constant.const

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
		   where type I.operand	    = SparcInstructions.operand
		     and type I.instruction = SparcInstructions.instruction
	      = struct

    (* -- structures ------------------------------------------------------- *)

    structure I = SparcInstructions

    (* -- register allocation values --------------------------------------- *)

    val nFreeRegs = length FloatConvention.available
    val dedicated = FloatConvention.dedicated
    val getreg	  = GetRegister.getreg

    fun copyInstr(dest, src) = SparcInstructions.FCOPY(dest, src, ref NONE)

    (* -- spill functions -------------------------------------------------- *)

    local
      fun template(id, offset) =
	    SparcInstructions.FSTORE{
	      stOp = SparcInstructions.STT,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = SparcInstructions.CONSTop offset,
	      mem  = stack
	    }::nil
    in
      fun spill{instr = instruction, reg = target} =
	    case instruction of
	      SparcInstructions.FCOPY([_], [source], _) => 
		{code  = template(source, !lookupSpill target),
		 proh  = [],
		 instr = NONE}
	    | _ =>
		let
		  val target' = Cells.newFreg()
		in
		  {code	 = template(target', !lookupSpill target),
		   proh	 = [target'],
		   instr = SOME(SparcRewrite.frewriteDef
				  (instruction, target, target'))}
		end
    end

    local
      fun template(id, offset, tail) =
	    SparcInstructions.FLOAD{
	      ldOp = SparcInstructions.LDT,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = SparcInstructions.CONSTop offset,
	      mem  = stack
	    }::tail
    in
      fun reload{instr = instruction, reg = source} =
	    case instruction of
	      SparcInstructions.FCOPY([target], [_], _) => 
		{code = template(target, !lookupReload source, []),
		 proh = []}
	    | _ =>
		let
		  val source' = Cells.newFreg()
		in
		  {code = template(source', !lookupReload source,
				   [SparcRewrite.frewriteUse
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

