(* =========================================================================
 * AlphaIntegerAllocation.sml
 * ========================================================================= *)

functor AlphaIntegerAllocation(
	  structure AlphaInstructions: ALPHA32INSTR
	  structure Cells:	       CELLS
	  structure FlowGraph:	       FLOWGRAPH
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int
	  structure MLRISCRegion:      MLRISC_REGION

	  structure AlphaRewrite: sig

	    structure I: ALPHA32INSTR

	    val rewriteUse: (int -> int) * I.instruction * int * int -> I.instruction
	    val rewriteDef: (int -> int) * I.instruction * int * int -> I.instruction

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
	) 
 :> REGISTER_ALLOCATION
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
    GetReg(val available = IntegerConvention.available
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

    val nFreeRegs = length IntegerConvention.available
    val dedicated = IntegerConvention.dedicated
    val getreg	  = GetRegister.getreg

    exception Unimplemented
    fun copyInstr ((dest, src), i) = 
	(AlphaInstructions.COPY{dst = dest, src = src, 
			       impl = ref NONE, tmp = NONE})

    (* -- spill functions -------------------------------------------------- *)

    local
      fun template(id, offset) =
	    AlphaInstructions.STORE{
	      stOp = AlphaInstructions.STL,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = AlphaInstructions.CONSTop offset,
	      mem  = stack
	    }::nil
    in
      fun spill{regmap = _, id = _, 
		instr = instruction, reg = target} =
	    case instruction of
	      AlphaInstructions.COPY{dst = [_], src = [source], ...} =>
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
				  (fn (x:int) => x, instruction, target, target'))}
		end
    end

    local
      fun template(id, offset, tail) =
	    AlphaInstructions.LOAD{
	      ldOp = AlphaInstructions.LDL,
	      r	   = id,
	      b	   = IntegerConvention.stackPointer,
	      d	   = AlphaInstructions.CONSTop offset,
	      mem  = stack
	    }::tail
    in
      fun reload{regmap = _, id = _, 
		 instr = instruction, reg = source} =
	    case instruction of
	      AlphaInstructions.COPY{dst = [target], src = [_], ...} =>
		{code = template(target, !lookupReload source, []),
		 proh = []}
	    | _ =>
		let
		  val source' = Cells.newReg()
		in
		  {code = template(source', !lookupReload source,
				   [AlphaRewrite.rewriteUse
				      (fn (x:int) => x, instruction, source, source')]),
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

  local
    val allocate = IntegerAllocation.ra IntegerAllocation.REGISTER_ALLOCATION
  in
    fun allocateCluster cluster = 
	let val _ = print "Perry: AlphaIntegerAllocation.sml: allocateCluster start\n"
	    val res = allocate cluster 
	    val _ = print "Perry: AlphaIntegerAllocation.sml: allocateCluster 1\n"
	    val _ = GetRegister.reset()
	    val _ = print "Perry: AlphaIntegerAllocation.sml: allocateCluster end\n"
	in  res
	end
  end


end

