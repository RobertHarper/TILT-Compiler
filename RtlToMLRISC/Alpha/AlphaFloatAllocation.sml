(*$import ALPHA32Instr CELLS FlowGraph FLOAT_CONVENTION INTEGER_CONVENTION MLRISC_REGION ALPHA32INSTR RA REGISTER_ALLOCATION *)

(* =========================================================================
 * AlphaFloatAllocation.sml
 * ========================================================================= *)

functor AlphaFloatAllocation(
	  structure AlphaInstructions: ALPHA32INSTR
	  structure Cells:	       CELLS
	  structure FloatConvention:   FLOAT_CONVENTION where type id = int
	  structure FlowGraph:	       FLOWGRAPH 
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int
	  structure MLRISCRegion:      MLRISC_REGION

	  structure AlphaRewrite: sig

	    structure I: ALPHA32INSTR

	    val frewriteUse: (int -> int) * I.instruction * int * int -> I.instruction
	    val frewriteDef: (int -> int) * I.instruction * int * int -> I.instruction

	  end

	  functor RegisterAllocation(
	    structure RaUser: RA_USER_PARAMS
				where type I.operand =
					     AlphaInstructions.operand
				  and type I.instruction =
					     AlphaInstructions.instruction
				  and type B.name = 
				             AlphaMLRISCBlockname.name

	  ): sig
	    datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
	    val ra: mode -> FlowGraph.cluster -> FlowGraph.cluster
	  end

	  sharing type AlphaInstructions.instruction =
		       AlphaRewrite.I.instruction
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
		     and type B.name        = AlphaMLRISCBlockname.name
	      = struct

    (* -- structures ------------------------------------------------------- *)

    structure I = AlphaInstructions
    structure B = AlphaMLRISCBlockname

    (* -- register allocation values --------------------------------------- *)

    val nFreeRegs = length FloatConvention.available
    val dedicated = FloatConvention.dedicated
    val getreg	  = GetRegister.getreg

    exception Unimplemented
    fun copyInstr((dest, src), i) = (AlphaInstructions.FCOPY{dst = dest, src = src, 
							     impl = ref NONE, tmp = NONE})

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
      fun spill{regmap = _, id = _, 
		instr = instruction, reg = target} =
	    case instruction of
	      AlphaInstructions.FCOPY{dst = [_], src = [source], ...} => 
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
				  (fn (x:int) => x, instruction, target, target'))}
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
      fun reload{regmap = _, id = _, 
		 instr = instruction, reg = source} =
	    case instruction of
	      AlphaInstructions.FCOPY{dst = [target], src = [_], ...} => 
		{code = template(target, !lookupReload source, []),
		 proh = []}
	    | _ =>
		let
		  val source' = Cells.newFreg()
		in
		  {code = template(source', !lookupReload source,
				   [AlphaRewrite.frewriteUse
				      (fn (x:int) => x, instruction, source, source')]),
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

