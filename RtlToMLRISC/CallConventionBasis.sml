(*$import CELLS INTEGER_CONVENTION MLRISC_REGION MLTREE_EXTRA STACK_FRAME CALL_CONVENTION_BASIS *)


(* =========================================================================
 * CallConventionBasis.sml
 * ========================================================================= *)

functor CallConventionBasis(
	  structure Cells:	       CELLS
	  structure IntegerConvention: INTEGER_CONVENTION where type id = int
	  structure MLRISCRegion:      MLRISC_REGION
	  structure MLTreeExtra:       MLTREE_EXTRA
	  structure StackFrame:	       STACK_FRAME

	  sharing type MLRISCRegion.region =
		       MLTreeExtra.MLTree.Region.region
	      and type MLTreeExtra.MLTree.Constant.const =
		       StackFrame.offset
	) :> CALL_CONVENTION_BASIS
	       where type id	 = int
		 and type offset = MLTreeExtra.MLTree.Constant.const
		 and type frame	 = StackFrame.frame
		 and type rexp	 = MLTreeExtra.MLTree.rexp
		 and type stm	 = MLTreeExtra.MLTree.stm
		 and type mltree = MLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = int

  datatype register =
    Integer of id
  | Float of id

  type offset = MLTree.Constant.const

  (*
   * An assignment is a mapping from integer registers to integer registers and
   * a mapping from floating-point registers to floating-point registers.
   * Target registers are the left members of each pair.
   *)
  type assignment = (id * id) list * (id * id) list

  (*
   * An assignment is a mapping from integer and floating-point registers to
   * stack offsets.
   * Target offsets are the left members of each pair.
   *)
  type stackAssignment = (id * int) list * (id * int) list

  type frame = StackFrame.frame

  type rexp   = MLTree.rexp
  type stm    = MLTree.stm
  type mltree = MLTree.mltree

  (* -- values ------------------------------------------------------------- *)

  val returnPointer = IntegerConvention.returnPointer
  val stackPointer  = IntegerConvention.stackPointer

  val stack = MLRISCRegion.stack

  (* -- code generation functions ------------------------------------------ *)

  fun addStack offset =
	MLTree.ADD(MLTree.REG stackPointer, MLTree.CONST offset)
  fun subStack offset =
	MLTree.SUB(MLTree.REG stackPointer, MLTree.CONST offset, MLTree.LR)

  fun loadStack offset id  =
        MLTree.MV(id, MLTree.LOAD32(addStack offset, stack))
  fun storeStack offset id =
        MLTree.STORE32(addStack offset, MLTree.REG id, stack)

  fun allocateFrame frame =
	MLTree.MV(stackPointer, subStack(StackFrame.size frame))
  fun deallocateFrame frame =
	MLTree.MV(stackPointer, addStack(StackFrame.size frame))

  local
    fun saveRestore access frame =
	  access (StackFrame.allocateReturn frame) returnPointer
  in
    val saveReturn    = saveRestore storeStack
    val restoreReturn = saveRestore loadStack
  end

  local
    (*
     * Return true if a given list of mltree values contains a call statement.
     * mltrees -> the mltree values to check
     * <- true if mltrees contains a call statement
     *)
    local
      fun isCall(MLTree.CALL _) = true
	| isCall _		= false

      fun hasCall'(MLTree.CODE code) = List.exists isCall code
	| hasCall' _		     = false
    in
      val hasCall = List.exists hasCall'
    end

    fun ifCall saveRestore frame body =
	  if hasCall body then [saveRestore frame] else []
  in
    val saveReturnIfCall    = ifCall saveReturn
    val restoreReturnIfCall = ifCall restoreReturn
  end

  (* -- register assignment functions -------------------------------------- *)

  val integer = Integer
  val float   = Float

  fun assignUnion((integers1, floats1), (integers2, floats2)) =
	(integers1@integers2, floats1@floats2)

  fun assignNew nil =
	(nil, nil)
    | assignNew(register::tail) =
	let
	  val (integers, floats) = assignNew tail
	in
	  case register of
	    (Integer id) => ((Cells.newReg(), id)::integers, floats)
	  | (Float id)	 => (integers, (Cells.newFreg(), id)::floats)
	end

  local
    fun assign new = map(fn id => (new(), id: int))
  in
    fun assignNew'(integers, floats) =
	  (assign Cells.newReg integers, assign Cells.newFreg floats)
  end

  fun assignRegister _ nil =
	(nil, (nil, nil))
    | assignRegister (targets as (nil, _)) (Integer id::tail) =
	let
	  val (registers', assignment) = assignRegister targets tail
	in
	  (Integer id::registers', assignment)
	end
    | assignRegister (integer::integerTail, floatList) (Integer id::tail) =
	let
	  val (registers', (integers, floats)) =
		assignRegister (integerTail, floatList) tail
	in
	  (registers', ((integer, id)::integers, floats))
	end
    | assignRegister (targets as (_, nil)) (Float id::tail) =
	let
	  val (registers', assignment) = assignRegister targets tail
	in
	  (Float id::registers', assignment)
	end
    | assignRegister (integerList, float::floatTail) (Float id::tail) =
	let
	  val (registers', (integers, floats)) =
		assignRegister (integerList, floatTail) tail
	in
	  (registers', (integers, (float, id)::floats))
	end

  fun assignRegister' _ nil =
	(nil, (nil, nil))
    | assignRegister' (nil, _) registers =
	(registers, (nil, nil))
    | assignRegister' (_, nil) registers =
	(registers, (nil, nil))
    | assignRegister' (integer::integerTail, float::floatTail)
		      (register::tail) =
	let
	  val (registers', (integers, floats)) =
		assignRegister' (integerTail, floatTail) tail
	in
	  case register of
	    (Integer id) => (registers', ((integer, id)::integers, floats))
	  | (Float id)	 => (registers', (integers, (float, id)::floats))
	end

  fun assignStack _ nil =
	(nil, nil)
    | assignStack (allocate as (allocateInteger, _)) (Integer id::tail) =
	let
	  val integer		 = allocateInteger id
	  val (integers, floats) = assignStack allocate tail
	in
	  ((integer, id)::integers, floats)
	end
    | assignStack (allocate as (_, allocateFloat)) (Float id::tail) =
	let
	  val float		 = allocateFloat id
	  val (integers, floats) = assignStack allocate tail
	in
	  (integers, (float, id)::floats)
	end

  (* -- marshaling functions ----------------------------------------------- *)

  local
    val left  = #1: (id * id) -> id
    val right = #2: (id * id) -> id

    fun set assign = (map left assign, map right assign)
    fun get assign = (map right assign, map left assign)
  in
    fun setAssignment(integers, floats) =
	  MLTreeExtra.copyList(set integers)@MLTreeExtra.fcopyList(set floats)
    fun getAssignment(integers, floats) =
	  MLTreeExtra.copyList(get integers)@MLTreeExtra.fcopyList(get floats)
  end

  local
    (*
     * Load/store a given list of registers on the stack using a given memory
     * access function, a given base function, and a given partial stack
     * assignment.
     * access	  -> the memory access function
     * base	  -> the mapping from integer offsets to offset constants
     * assignment -> the partial stack assignment
     * <- the mltree statements
     *)
    fun place _ _ nil =
	  []
      | place access base ((offset: int, register)::tail) =
	  let
	    val statement = access(register,
				   MLTree.ADD(MLTree.REG stackPointer,
					      MLTree.CONST(base offset)))
	    val result	  = place access base tail
	  in
	    statement::result
	  end

    val loadInteger =
          place (fn(target, address) =>
		   MLTree.MV(target, MLTree.LOAD32(address, stack)))
    val loadFloat =
          place (fn(target, address) =>
		   MLTree.FMV(target, MLTree.LOADD(address, stack)))
    val storeInteger =
          place (fn(source, address) =>
		   MLTree.STORE32(address, MLTree.REG source, stack))
    val storeFloat =
          place (fn(source, address) =>
		   MLTree.STORED(address, MLTree.FREG source, stack))
  in
    fun storeAssignment base (integers, floats) =
	  storeInteger base integers@storeFloat base floats
    fun loadAssignment base (integers, floats) =
	  loadInteger base integers@loadFloat base floats
  end

  fun integerAssignments(integers, _) = integers
  fun floatAssignments(floats, _)     = floats

end

