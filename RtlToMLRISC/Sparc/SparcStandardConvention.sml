(*$import TopLevel FLOAT_CONVENTION INTEGER_CONVENTION SparcCallconventionBasis SparcMLTreeExtra SparcStandardFrame *)

(* =========================================================================
 * SparcStandardConvention.sml
 * ========================================================================= *)

functor SparcStandardConvention(
	  structure FloatConvention:   FLOAT_CONVENTION where type id = int
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int
	) :> CALL_CONVENTION
	       where type id	     = SparcCallConventionBasis.id
		 and type register   = SparcCallConventionBasis.register
		 and type assignment = SparcCallConventionBasis.assignment
		 and type frame	     = SparcCallConventionBasis.frame
		 and type rexp	     = SparcMLTreeExtra.MLTree.rexp
		 and type mltree     = SparcMLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure Basis	= SparcCallConventionBasis
  structure MLTreeExtra = SparcMLTreeExtra
  structure StackFrame	= SparcStandardFrame

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = Basis.id

  type register = Basis.register

  type assignment = Basis.assignment

  type frame = StackFrame.frame

  type rexp   = MLTree.rexp
  type mltree = MLTree.mltree

  (* -- values ------------------------------------------------------------- *)

  val returnPointer = IntegerConvention.returnPointer

  val arguments = (IntegerConvention.arguments, FloatConvention.arguments)
  val results	= (IntegerConvention.results, FloatConvention.results)
  val preserve	= (IntegerConvention.preserve, FloatConvention.preserve)

  val escape = MLTree.ESCAPEBLOCK(
		 (map MLTreeExtra.gpr IntegerConvention.escape)@
		 (map MLTreeExtra.fpr FloatConvention.escape))

  (* -- marshaling functions ----------------------------------------------- *)

  (*
   * Marshal/unmarshal a given list of registers using a given stack frame.
   * frame    <-> the stack frame to hold memory arguments/results
   * registers -> the registers to marshal/unmarshal
   * <- the mltree statements
   *)
  local
    fun marshal' (move, stack) convention base (frame: frame) registers =
	let
	  val size = ref 0

	  fun allocate _ =
		let
		  val offset = !size
		in
		  size := offset+8; offset
		end

	  val (registers', assignment) =
		Basis.assignRegister' convention registers
	  val assignment' =
		Basis.assignStack (allocate, allocate) registers'
	in
	  move assignment@stack (base frame (!size)) assignment'
	end

    val marshal	  = marshal'(Basis.setAssignment, Basis.storeAssignment)
    val unmarshal = marshal'(Basis.getAssignment, Basis.loadAssignment)

    fun top frame _ = StackFrame.offsetTop frame
    val bottom	    = StackFrame.allocateArgument
  in
    val unmarshalArguments = unmarshal arguments top
    val marshalResults	   = marshal results top
    val marshalArguments   = marshal arguments bottom
    val unmarshalResults   = unmarshal results bottom
  end

  (*
   * Return the registers used by the argument list for a given call.
   * registers -> the argument registers to return the used registers of
   * <- the registers used by registers
   *)
  fun useArguments registers =
	let
	  val (_, assignment) = Basis.assignRegister' arguments registers
	in
	  map (MLTreeExtra.gpr o #1) (Basis.integerAssignments assignment)@
	  map (MLTreeExtra.fpr o #1) (Basis.floatAssignments assignment)
	end

  (* -- functions ---------------------------------------------------------- *)

  val integer = Basis.integer
  val float   = Basis.float

  local
    val define = (map MLTreeExtra.gpr IntegerConvention.define)@
		 (map MLTreeExtra.fpr FloatConvention.define)
  in
    fun call wrapper frame (procedure, arguments, results) =
	  let
	    nonfix before
	    val (before, after) = wrapper procedure
	    val use		 = useArguments arguments
	  in
	    [MLTree.CODE(marshalArguments frame arguments)]@
	    before@
	    [MLTree.CODE[MLTree.CALL(procedure, define, use)]]@
	    after@
	    [MLTree.CODE(unmarshalResults frame results)]
	  end
  end

  fun save registers =
	Basis.assignUnion(Basis.assignNew registers, Basis.assignNew' preserve)

  val integerSave = Basis.integerAssignments
  val floatSave	  = Basis.floatAssignments

  fun enter frame (arguments, saves, body) =
	[MLTree.CODE[Basis.allocateFrame frame],
	 MLTree.CODE(Basis.saveReturnIfCall frame body),
	 MLTree.CODE(Basis.setAssignment saves),
	 MLTree.CODE(unmarshalArguments frame arguments)]

  fun exit frame (results, saves, body) =
	[MLTree.CODE(marshalResults frame results),
	 MLTree.CODE(Basis.getAssignment saves),
	 MLTree.CODE(Basis.restoreReturnIfCall frame body),
	 MLTree.CODE[Basis.deallocateFrame frame],
	 escape,
	 MLTree.CODE[MLTree.JMP(MLTree.ADD(MLTree.REG IntegerConvention.returnPointer,
					   MLTree.LI 8), [])],
	 MLTree.CODE[MLTree.RET]]
	    (* not RET since RET uses register windows *)

end

