
(* =========================================================================
 * AlphaStandardConvention.sml
 * ========================================================================= *)

functor AlphaStandardConvention(
	  structure Basis:	       CALL_CONVENTION_BASIS
					 where type id = int
	  structure FloatConvention:   FLOAT_CONVENTION
					 where type id = int
	  structure IntegerConvention: INTEGER_CONVENTION
					 where type id = int
	  structure MLTreeExtra:       MLTREE_EXTRA
	  structure StackFrame:	       STACK_FRAME

	  sharing type MLTreeExtra.MLTree.mltree = Basis.mltree
	      and type MLTreeExtra.MLTree.stm	 = Basis.stm
	      and type StackFrame.offset	 = Basis.offset
	      and type StackFrame.frame		 = Basis.frame
	) :> CALL_CONVENTION
	       where type id	     = Basis.id
		 and type register   = Basis.register
		 and type assignment = Basis.assignment
		 and type frame	     = StackFrame.frame
		 and type rexp	     = MLTreeExtra.MLTree.rexp
		 and type mltree     = MLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

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
  val callPointer   = IntegerConvention.callPointer
  val globalPointer = IntegerConvention.globalPointer

  val arguments = (IntegerConvention.arguments, FloatConvention.arguments)
  val results	= (IntegerConvention.results, FloatConvention.results)
  val preserve	= (IntegerConvention.preserve, FloatConvention.preserve)

  val define = (map MLTreeExtra.gpr IntegerConvention.define)@
	       (map MLTreeExtra.fpr FloatConvention.define)
  val use    = (map MLTreeExtra.gpr IntegerConvention.use)@
	       (map MLTreeExtra.fpr FloatConvention.use)
  val escape = (map MLTreeExtra.gpr IntegerConvention.escape)@
	       (map MLTreeExtra.fpr FloatConvention.escape)

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

  fun call wrapper frame (procedure, arguments, results) =
	let
	  val (before_, after) = wrapper procedure
	  val use	       = useArguments arguments
	in
	  [MLTree.CODE(marshalArguments frame arguments)]@
	  [MLTree.CODE[MLTree.MV(callPointer, procedure)]]@
	  before_@
	  [MLTree.CODE[MLTree.CALL(MLTree.REG callPointer, define, use)]]@
	  after@
	  [MLTree.CODE[MLTree.MV(globalPointer, MLTree.REG returnPointer)]]@
	  [MLTree.CODE(unmarshalResults frame results)]
	end

  fun save registers =
	Basis.assignUnion(Basis.assignNew registers, Basis.assignNew' preserve)

  val integerSave = Basis.integerAssignments
  val floatSave	  = Basis.floatAssignments

  fun enter frame (arguments, saves, body) =
	[MLTree.CODE[MLTree.MV(globalPointer, MLTree.REG callPointer)],
	 MLTree.CODE[Basis.allocateFrame frame],
	 MLTree.CODE(Basis.saveReturnIfCall frame body),
	 MLTree.CODE(Basis.setAssignment saves),
	 MLTree.CODE(unmarshalArguments frame arguments)]

  fun exit frame (results, saves, body) =
	[MLTree.CODE(marshalResults frame results),
	 MLTree.CODE(Basis.getAssignment saves),
	 MLTree.CODE(Basis.restoreReturnIfCall frame body),
	 MLTree.CODE[Basis.deallocateFrame frame],
	 MLTree.ESCAPEBLOCK escape,
	 MLTree.CODE[MLTree.RET]]

end

