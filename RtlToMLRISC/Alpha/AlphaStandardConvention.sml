(*$import FLOAT_CONVENTION INTEGER_CONVENTION AlphaCallconventionBasis AlphaMLTreeExtra AlphaStandardFrame *)


(* =========================================================================
 * AlphaStandardConvention.sml
 * ========================================================================= *)

functor AlphaStandardConvention(
	  structure FloatConvention:   FLOAT_CONVENTION where type id = int
	  structure IntegerConvention: 
		    sig
		      include INTEGER_CONVENTION 
		      val callPointer: id (* address for a procedure call, needed to fix globalPointer *)
		      val globalPointer: id (* base address of current global table *)
		    end where type id = int
	) :> CALL_CONVENTION
	       where type id	     = AlphaCallConventionBasis.id
		 and type register   = AlphaCallConventionBasis.register
		 and type assignment = AlphaCallConventionBasis.assignment
		 and type frame	     = AlphaCallConventionBasis.frame
		 and type rexp	     = AlphaMLTreeExtra.MLTree.rexp
		 and type mltree     = AlphaMLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure Basis	= AlphaCallConventionBasis
  structure MLTreeExtra = AlphaMLTreeExtra
  structure StackFrame	= AlphaStandardFrame

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
    fun call wrapper frame isExternal (procedure, arguments, results) =
	  let
	    val (before_, after) = wrapper procedure
	    val use		 = useArguments arguments
	  in
	    [MLTree.CODE(marshalArguments frame arguments)]@
	    [MLTree.CODE[MLTreeExtra.mv(callPointer, procedure)]]@
	    before_@
	    [MLTree.CODE[MLTree.CALL(MLTree.REG callPointer, define, use)]]@
	    after@
	    [MLTree.CODE[MLTree.MV(globalPointer, MLTree.REG returnPointer)]]@
	    [MLTree.CODE(unmarshalResults frame results)]
	  end
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
	 escape,
	 MLTree.CODE[MLTree.RET]]

end

