
(* =========================================================================
 * EmitRtlMLRISC.sml
 * ========================================================================= *)

functor EmitRtlMLRISC(
	  structure BasicBlock:		 BASIC_BLOCK
	  structure CallConventionBasis: CALL_CONVENTION_BASIS
	  structure Cells:		 CELLS
	  structure ExternalConvention:	 CALL_CONVENTION
	  structure FloatAllocation:	 REGISTER_ALLOCATION
	  structure FloatConvention:	 FLOAT_CONVENTION
	  structure IntegerAllocation:	 REGISTER_ALLOCATION
	  structure IntegerConvention:	 INTEGER_CONVENTION
					   where type id = int
	  structure IntegerLiveness:	 REGISTER_LIVENESS
	  structure IntSet:		 ORD_SET
					   where type Key.ord_key = int
	  structure MLRISCConstant:	 MLRISC_CONSTANT
	  structure MLRISCPseudo:	 MLRISC_PSEUDO
	  structure MLTreeComp:		 MLTREECOMP
	  structure MLTreeExtra:	 MLTREE_EXTRA
	  structure RegisterMap:	 REGISTER_MAP
	  structure RegisterSpillMap:	 REGISTER_SPILL_MAP
	  structure RegisterTraceMap:	 REGISTER_TRACE_MAP
					   where type var = Name.var
	  structure Rtl:		 RTL
	  structure StackFrame:		 STACK_FRAME
	  structure TraceTable:		 TRACETABLE

	  sharing type CallConventionBasis.assignment =
		       ExternalConvention.assignment
	      and type IntegerConvention.id =
		       CallConventionBasis.id =
		       ExternalConvention.id =
		       FloatAllocation.id =
		       FloatConvention.id =
		       IntegerAllocation.id =
		       IntegerLiveness.id =
		       MLRISCPseudo.id =
		       RegisterMap.id =
		       RegisterSpillMap.id =
		       RegisterTraceMap.id
	      and type MLRISCConstant.const =
		       CallConventionBasis.offset =
		       FloatAllocation.offset =
		       IntegerAllocation.offset =
		       RegisterSpillMap.offset =
		       StackFrame.offset
	      and type MLRISCPseudo.pseudo_op =
		       MLTreeExtra.MLTree.PseudoOp.pseudo_op
	      and type MLTreeExtra.MLTree.mltree =
		       BasicBlock.mltree =
		       ExternalConvention.mltree =
		       IntegerLiveness.mltree =
		       MLTreeComp.T.mltree
	      and type MLTreeExtra.MLTree.rexp =
		       ExternalConvention.rexp
	      and type MLTreeExtra.MLTree.stm =
		       CallConventionBasis.stm
	      and type Rtl.data =
		       TraceTable.Machine.Rtl.data
	      and type Rtl.label =
		       TraceTable.Machine.Rtl.label
	      and type Rtl.local_label =
		       TraceTable.Machine.Rtl.local_label
	      and type Rtl.rep =
		       RegisterTraceMap.rep
	      and type StackFrame.frame =
		       ExternalConvention.frame
	      and type TraceTable.Machine.stacklocation =
		       RegisterTraceMap.stacklocation
	      and type TraceTable.trace =
		       RegisterTraceMap.trace
	) :> EMIT_RTL
	       where type local_label = Rtl.local_label
		 and type module      = Rtl.module
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure Machine = TraceTable.Machine
  structure MLTree  = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type local_label = Rtl.local_label

  type module = Rtl.module

  (* -- exceptions --------------------------------------------------------- *)

  exception InvalidRtl of string

  (* -- utility functions -------------------------------------------------- *)

  (*
   * Return a list version of a given element function by splicing its result.
   * f -> the element function to splice
   * <- the list version of the element function
   *)
  fun splice f (element, list) = f element@list

  (*
   * Wrap a given function with a given unwind function.
   * operator -> the function to wrap
   * unwind   -> the unwind function to wrap operator with
   * <- the wrapped function
   *)
  fun protect (operator, unwind) operand =
	let
	  val result = operator operand
			 handle except => (unwind(); raise except)
	in
	  unwind(); result
	end

  infix protect

  (* -- surrogate functions ------------------------------------------------ *)

  (*
   * Return an MLRISC statement for moving the value of a given expression
   * into a given pseudo-register.
   * target -> the id of the pseudo-register to move the value into
   * source -> the expression to move the value of
   * <- the statement
   * replace with MLTreeExtra.(f)mv when MLRISC is fixed to recognize
   * dedicated spills in call graph ???
   *)
  local
    fun member list id = List.exists (fn id' => id'=id) list

    val dedicatedInteger = member IntegerConvention.dedicated
    val dedicatedFloat	 = member FloatConvention.dedicated
  in
    fun mv(target, MLTree.REG source) =
	  if dedicatedInteger target orelse dedicatedInteger source then
	    MLTree.MV(target, MLTree.REG source)
	  else
	    MLTree.COPY([target], [source])
      | mv(target, source) =
	  MLTree.MV(target, source)

    fun fmv(target, MLTree.FREG source) =
	  if dedicatedFloat target orelse dedicatedFloat source then
	    MLTree.FMV(target, MLTree.FREG source)
	  else
	    MLTree.FCOPY([target], [source])
      | fmv(target, source) =
	  MLTree.FMV(target, source)
  end

  (*
   * Return a new MLRISC label.
   * <- the new label
   *)
  fun newLabel()    = Label.newLabel ""
  val externalLabel = Label.newLabel o MLRISCPseudo.fixLabel

  (*
   * Return an MLRISC integer expression for a given label.
   * label -> the label
   * <- the integer expression
   *)
  val labelExp	  = MLTree.LABEL o LabelExp.LABEL
  val externalExp = labelExp o externalLabel

  (*
   * Emit a list of mltree values.
   * mltrees -> the mltree values to emit
   *)
  val emitMLTree = app MLTreeComp.mltreeComp  

  (* -- global state structures -------------------------------------------- *)

  structure Module = struct

    local
      (*
       * The call information of the current module.
       *)
      val infosRef = ref([]: TraceTable.callinfo list)
    in
      (*
       * Start a new module.
       *)
      fun open_() = ()

      (*
       * Terminate the current module.
       *)
      fun close() = infosRef := []

      (*
       * Accumulate call information for the current module.
       * info -> the additional call information
       *)
      fun addCallInfo info =
	    (* ??? let
	      val {calllabel = TraceTable.CALLLABEL label,
		   framesize,
		   retaddpos,
		   regtrace,
		   stacktrace} = info

	      val label' = LocalLabel.translate label
	    in
	      print(Label.nameOf label'^"("^Int.toString framesize^"): ");
	      app (fn(Machine.R reg, trace) =>
		     print("$"^Int.toString reg^"=>"^
			   TraceTable.trace2string trace^" ")) regtrace;
	      app (fn(offset, trace) =>
		     print(Int.toString offset^"(sp)=>"^
			   TraceTable.trace2string trace^" ")) stacktrace;
	      print "\n";
	    end *)
	    infosRef := TraceTable.CALLINFO info:: !infosRef

      (*
       * Return the accumulated call information of the current module.
       * <- the call information
       *)
      fun infos() = !infosRef
    end

  end

  structure Cluster = struct

    local
      (*
       * Used to create an empty intmap.
       *)
      exception Impossible

      (*
       * An empty intmap.
       *)
      val emptyMap = Intmap.new(0, Impossible): int Intmap.intmap

      (*
       * The register map of the current cluster.
       *)
      val mapRef = ref emptyMap
    in
      (*
       * Start a new cluster.
       *)
      fun open_() = mapRef := Cells.resetRegs()

      (*
       * Terminate the current cluster.
       *)
      fun close() = mapRef := emptyMap

      (*
       * Return the register map of the current cluster.
       * <- the register map
       *)
      fun map() = !mapRef
    end

  end

  structure Procedure = struct

    local
      (*
       * The stack frame of the current procedure.
       *)
      val frameRef = ref StackFrame.empty

      (*
       * The first integer/floating-point pseudo-register to be allocated to
       * the current procedure.
       *)
      val integerMin = ref 0
      val floatMin   = ref 0

      (*
       * An empty save assignment.
       *)
      val emptySaves = CallConventionBasis.assignNew []

      (*
       * The assignment for the registers saved by the current procedure.
       *)
      val savesRef = ref emptySaves
    in
      (*
       * Start a new procedure.
       *)
      fun open_() = (frameRef	:= StackFrame.frame();
		     integerMin := Cells.maxReg();
		     floatMin	:= Cells.maxFreg())

      (*
       * Terminate the current procedure.
       *)
      fun close() = (frameRef	:= StackFrame.empty;
		     integerMin := 0;
		     floatMin	:= 0;
		     savesRef	:= emptySaves)

      (*
       * Return the stack frame of the current procedure.
       * <- the stack frame
       *)
      fun frame() = !frameRef

      (*
       * Return the set of integer/floating-point registers allocated to
       * the current procedure.
       *)
      fun integers() =
	    let
	      val min = !integerMin
	      val max = Cells.maxReg()
	    in
	      fn id => id>=min andalso id<max
	    end
      fun floats() =
	    let
	      val min = !floatMin
	      val max = Cells.maxFreg()
	    in
	      fn id => id>=min andalso id<max
	    end

      (*
       * Change/return save assignment of the current procedure.
       * saves -> the new save assignment
       *)
      fun setSaves saves = savesRef := saves
      fun saves()	 = !savesRef
    end

  end

  (* -- operand translation structures ------------------------------------- *)

  structure Immediate = struct

    (*
     * Return an MLRISC integer expression for a given Rtl immediate value.
     * immediate -> the Rtl immediate value
     * <- the integer expression
     *)
    val translate = MLTree.LI

    (*
     * Return an MLRISC integer expression for a given 32-bit unsigned Rtl
     * immediate value.
     * immediate -> the 32-bit unsigned Rtl immediate value 
     * <- the integer expression
     *)
    val translate32 = MLTree.LI32

  end

  structure Offset = struct

    (*
     * Return an MLRISC integer expression for a given Rtl offset.
     * offset -> the Rtl offset
     * <- the integer expression
     *)
    val translate = MLTree.LI

  end

  structure Register = struct

    local
      (*
       * The mapping from Rtl integer registers to MLRISC pseudo-registers.
       *)
      val traceMap = RegisterTraceMap.map()

      (*
       * The mapping from MLRISC pseudo-registers to spill offsets.
       *)
      val spillMap = RegisterSpillMap.map()

      (*
       * The set of MLRISC pseudo-registers that have been spilled by MLRISC.
       *)
      val spillStateMap = RegisterMap.map(fn _ => ())

      (*
       * Register spill and reload lookup functions with the register
       * allocator.
       *)
      local
	fun retain lookup id =
	      (RegisterMap.insert spillStateMap (id, ()); lookup id)
      in
	val _ = IntegerAllocation.setLookup(
		  retain(RegisterSpillMap.lookupSpill spillMap),
		  retain(RegisterSpillMap.lookupReload spillMap))
      end

      (*
       * Return an MLRISC register number for a given Rtl register id.
       * -> the Rtl register id
       * <- the MLRISC register number
       *)
      fun lookupSpecial Rtl.HEAPPTR   = IntegerConvention.heapPointer
	| lookupSpecial Rtl.HEAPLIMIT = IntegerConvention.heapLimit
	| lookupSpecial Rtl.EXNPTR    = IntegerConvention.exceptionPointer
	| lookupSpecial Rtl.EXNARG    = IntegerConvention.exceptionArgument
	| lookupSpecial Rtl.STACKPTR  = IntegerConvention.stackPointer

      fun lookupGeneral register =
	    RegisterTraceMap.lookup lookupGeneral traceMap register

      local
	val spill	   = Machine.ACTUAL4 o MLRISCConstant.valueOf o
			     RegisterSpillMap.lookupReload spillMap
	val trace	   = RegisterTraceMap.trace spill traceMap
	val testReload	   = RegisterSpillMap.testReload spillMap
	val testSpillState = RegisterMap.test spillStateMap
      in
	(*
	 * Return the representations of the live loaded integer
	 * pseudo-registers.
	 * physical -> the physical register map function to use
	 * live	    -> the live pseudo-registers
	 * <- the representations of the live loaded registers
	 *)
	fun traceLoaded physical =
	      let
		fun traceLoaded1(id, loaded) = 
		      case testSpillState id of
			SOME _ => loaded
		      | NONE   => (Machine.R(physical id), trace id)::loaded
	      in
		foldr traceLoaded1 []
	      end

	(*
	 * Return the representations of the live spilled integer
	 * pseudo-registers.
	 * live -> the live pseudo-registers
	 * <- the representations of the live spilled registers
	 *)
	local
	  fun traceSpilled1(id, spilled) = 
		case testReload id of
		  SOME offset =>
		    (MLRISCConstant.valueOf offset, trace id)::spilled
		| NONE =>
		    spilled
	in
	  val traceSpilled = foldr traceSpilled1 []
	end
      end
    in
      (*
       * Return an MLRISC register number for a given Rtl register.
       * register -> the Rtl register
       * <- the register number
       *)
      fun translate(Rtl.SREGI register) = lookupSpecial register
	| translate(Rtl.REGI register)	= lookupGeneral register

      (*
       * Return an MLRISC integer expression for a given Rtl register.
       * register -> the Rtl register
       * <- the integer expression
       *)
      val translateExp = MLTree.REG o translate

      (*
       * Assign a given pseudo-register the same representation as another
       * pseudo-register.
       * target -> the pseudo-register to assign
       * source -> the pseudo-register to assign it to
       *)
      val assign = RegisterTraceMap.assign traceMap

      (*
       * Defer spilling a given set of integer pseudo-registers to a given
       * stack frame.
       * frame	  <-> the stack frame to spill the pseudo-registers to, if
       *	      necessary
       * predicate -> a predicate describing the pseudo-register set
       *)
      fun deferSpill frame predicate =
	    let
	      fun deferral _ =
		    let
		      val offset = StackFrame.allocateInteger frame
		    in
		      (offset, offset)
		    end
	    in
	      RegisterSpillMap.defer spillMap (predicate, deferral)
	    end

      (*
       * Record type information for a given call site.
       * label -> the label of the call site
       * live  -> the live integer registers at the time of the call
       *)
      fun callSite label =
	    let
	      val physical     = Intmap.map(Cluster.map())
	      val frame	       = Procedure.frame()
	      val frameSize    = StackFrame.size frame
	      val returnOffset = StackFrame.allocateReturn frame
	    in
	      fn live =>
		(* ??? print(Label.nameOf(LocalLabel.translate label)^": ");
		 app (fn id => print(Int.toString id^" ")) live;
		 print "\n"; *)
		Module.addCallInfo{
		  calllabel  = TraceTable.CALLLABEL label,
		  framesize  = MLRISCConstant.valueOf frameSize,
		  retaddpos  = MLRISCConstant.valueOf returnOffset,
		  regtrace   = traceLoaded physical live,
		  stacktrace = traceSpilled live
		}
	    end

      (*
       * Append spill statements after moves to polymorphic value descriptor
       * pseudo-registers in a given list of mltree values.
       * ids	 -> the pseudo-registers that are live across call sites
       * mltrees -> the mltree values to transform
       * <- mltrees with appended spills
       *)
      local
	val lookupSpill = RegisterSpillMap.lookupSpill spillMap
	val polySpills	= RegisterTraceMap.polySpills traceMap

	fun spill id = CallConventionBasis.storeStack (lookupSpill id) id

	fun spillPolyTree polys (MLTree.CODE code) =
	      let
		fun spillPoly id = if polys id then [spill id] else []

		fun spillPolyStatement(MLTree.MV(id, _)) =
		      spillPoly id
		  | spillPolyStatement(MLTree.COPY(idList, _)) =
		      foldr (splice spillPoly) [] idList
		  | spillPolyStatement _ =
		      []

		fun spillPolyStatement' statement =
		      statement::spillPolyStatement statement
	      in
		MLTree.CODE(foldr (splice spillPolyStatement') [] code)
	      end
	  | spillPolyTree _ mltree =
	      mltree
      in
	fun spillPolys ids = map (spillPolyTree (polySpills ids))
      end

      (*
       * Reset the source-based mappings of the integer register translation.
       *)
      fun resetSource() = RegisterTraceMap.resetSource traceMap

      (*
       * Reset the target-based mappings of the integer register translation.
       *)
      fun resetTarget() = (RegisterTraceMap.resetTarget traceMap;
			   RegisterSpillMap.reset spillMap;
			   RegisterMap.reset spillStateMap)
    end

  end

  structure FloatRegister = struct

    local
      (*
       * The mapping from Rtl floating-point registers to MLRISC
       * pseudo-registers.
       *)
      val map = RegisterMap.map(fn _ => Cells.newFreg())

      (*
       * The mapping from MLRISC pseudo-registers to spill offsets.
       *)
      val spillMap = RegisterSpillMap.map()

      (*
       * Register spill and reload lookup functions with the register
       * allocator.
       *)
      val _ = FloatAllocation.setLookup(RegisterSpillMap.lookupSpill spillMap,
					RegisterSpillMap.lookupReload spillMap)
    in
      (*
       * Return an MLRISC floating-point register number for a given Rtl
       * floating-point register.
       * -> the Rtl register
       * <- the register number
       *)
      fun translate(Rtl.REGF(var, Rtl.NOTRACE_REAL)) =
	    RegisterMap.lookup map (Name.var2int var)
	| translate _ =
	    raise InvalidRtl "invalid floating-point register"

      (*
       * Return an MLRISC floating-point expression for a given Rtl
       * floating-point register.
       * register -> the Rtl register
       * <- the floating-point expression
       *)
      val translateExp = MLTree.FREG o translate

      (*
       * Defer spilling a given set of floating-point pseudo-registers to a
       * given stack frame.
       * frame	  <-> the stack frame to spill the pseudo-registers to, if
       *	      necessary
       * predicate -> a predicate describing the pseudo-register set
       *)
      fun deferSpill frame predicate =
	    let
	      fun deferral _ =
		    let
		      val offset = StackFrame.allocateFloat frame
		    in
		      (offset, offset)
		    end
	    in
	      RegisterSpillMap.defer spillMap (predicate, deferral)
	    end

      (*
       * Reset the source-based mappings of the floating-point register
       * translation.
       *)
      fun resetSource() = RegisterMap.reset map

      (*
       * Reset the target-based mappings of the floating-point register
       * translation.
       *)
      fun resetTarget() = RegisterSpillMap.reset spillMap
    end

  end

  structure RegisterSet = struct

    (*
     * Return a pair of lists of registers for a given Rtl register set.
     * floating-point registers.
     * -> the Rtl register set
     * <- the register lists
     *)
    fun translate(integers, floats) =
	  (map Register.translate integers, map FloatRegister.translate floats)

    (*
     * Return a list of calling convention registers set for a given Rtl
     * register set, using the convention that integer registers precede
     * floating-point registers.
     * -> the Rtl register set
     * <- the register list
     *)
    fun translateCall(integers, floats) =
	  map (ExternalConvention.integer o Register.translate) integers@
	  map (ExternalConvention.float o FloatRegister.translate) floats

  end

  structure SmallValue = struct

    (*
     * Return an MLRISC integer expression for a given Rtl small value.
     * -> the Rtl small value
     * <- the integer expression
     *)
    fun translate(Rtl.REG register)  = Register.translateExp register
      | translate(Rtl.IMM immediate) = Immediate.translate immediate

  end

  structure EffectiveAddress = struct

    (*
     * Return an MLRISC integer expression for a given Rtl effective address.
     * -> the Rtl effective address
     * <- the integer expression
     *)
    fun translate(Rtl.EA(register, offset)) =
	  MLTreeExtra.add(Register.translateExp register,
			  Offset.translate offset)

  end

  structure LocalLabel = struct

    local
      (*
       * The register map from Rtl local label numbers to MLRISC labels.
       *)
      val map	 = RegisterMap.map(fn _ => newLabel())
      val lookup = RegisterMap.lookup map
    in
      (*
       * Return an MLRISC label for a given Rtl local label.
       * -> the Rtl local label
       * <- the label
       *)
      fun translate(Rtl.LOCAL_DATA label) = lookup(Name.var2int label)
	| translate(Rtl.LOCAL_CODE label) = lookup(Name.var2int label)

      (*
       * Return the string of a given Rtl local label.
       * -> the Rtl local label
       * <- the string of the label
       *)
      local
	val toString = MLRISCPseudo.fixLabel o Name.var2string
      in
	fun string(Rtl.LOCAL_DATA label) = "LD"^toString label
	  | string(Rtl.LOCAL_CODE label) = "LC"^toString label
      end

      (*
       * Reset the internal state of the local label translation.
       *)
      fun reset() = RegisterMap.reset map
    end

  end

  structure Label' = struct

    (*
     * Return an MLRISC label for a given Rtl label.
     * -> the Rtl label
     * <- the label
     *)
    fun translate(Rtl.ML_EXTERN_LABEL name) = externalLabel name
      | translate(Rtl.C_EXTERN_LABEL name)  = externalLabel name
      | translate(Rtl.LOCAL_LABEL label)    = LocalLabel.translate label

  end

  structure RegisterOrLabel = struct

    (*
     * Return an MLRISC integer expression for a given Rtl register or label.
     * -> the Rtl register or label
     * <- the integer expression
     *)
    fun translate(Rtl.REG' register) = Register.translateExp register
      | translate(Rtl.LABEL' label)  = labelExp(Label'.translate label)

  end

  structure Comparison = struct

    (*
     * Complement the sense of a given comparison operand.
     * -> the comparison operand
     * <- the complement of the comparison operand
     *)
    fun complement Rtl.EQ  = Rtl.NE
      | complement Rtl.LE  = Rtl.GT
      | complement Rtl.LT  = Rtl.GE
      | complement Rtl.GE  = Rtl.LT
      | complement Rtl.GT  = Rtl.LE
      | complement Rtl.NE  = Rtl.EQ
      | complement Rtl.LBC = Rtl.LBS
      | complement Rtl.LBS = Rtl.LBC

    (*
     * Return an MLRISC condition and conditional expression for a given
     * comparison operand and pair of expressions.
     *		   -> the comparison operand to compare with
     * left, right -> the expressions to compare
     * <- the MLRISC condition
     * <- the MLRISC conditional expression
     *)
    fun translate(Rtl.EQ, left, right) =
	  (MLTree.EQ, MLTree.CMP(MLTree.EQ, left, right, MLTree.LR))
      | translate(Rtl.LE, left, right) =
	  (MLTree.LE, MLTree.CMP(MLTree.LE, left, right, MLTree.LR))
      | translate(Rtl.LT, left, right) =
	  (MLTree.LT, MLTree.CMP(MLTree.LT, left, right, MLTree.LR))
      | translate(Rtl.GE, left, right) =
	  (MLTree.GE, MLTree.CMP(MLTree.GE, left, right, MLTree.LR))
      | translate(Rtl.GT, left, right) =
	  (MLTree.GT, MLTree.CMP(MLTree.GT, left, right, MLTree.LR))
      | translate(Rtl.NE, left, right) =
	  (MLTree.NEQ, MLTree.CMP(MLTree.NEQ, left, right, MLTree.LR))
      | translate(Rtl.LBC, left, right) =
	  (MLTree.EQ, MLTree.CMP(MLTree.EQ, MLTree.ANDB(left, MLTree.LI 1),
				 MLTree.LI 0, MLTree.LR))
      | translate(Rtl.LBS, left, right) =
	  (MLTree.NEQ, MLTree.CMP(MLTree.NEQ, MLTree.ANDB(left, MLTree.LI 1),
				  MLTree.LI 0, MLTree.LR))

    (*
     * Return an unsigned MLRISC condition and conditional expression for a
     * given comparison operand and pair of expressions.
     *		   -> the comparison operand to compare with
     * left, right -> the expressions to compare
     * <- the MLRISC condition
     * <- the MLRISC conditional expression
     *)
    fun translateUnsigned(Rtl.LE, left, right) =
	  (MLTree.LEU, MLTree.CMP(MLTree.LEU, left, right, MLTree.LR))
      | translateUnsigned(Rtl.LT, left, right) =
	  (MLTree.LTU, MLTree.CMP(MLTree.LTU, left, right, MLTree.LR))
      | translateUnsigned(Rtl.GE, left, right) =
	  (MLTree.GEU, MLTree.CMP(MLTree.GEU, left, right, MLTree.LR))
      | translateUnsigned(Rtl.GT, left, right) =
	  (MLTree.GTU, MLTree.CMP(MLTree.GTU, left, right, MLTree.LR))
      | translateUnsigned(operand, left, right) =
	  translate(operand, left, right)

    (*
     * Return an MLRISC condition and conditional expression for a
     * given comparison operand and pair of floating-point expressions.
     *		   -> the comparison operand to compare with
     * left, right -> the floating-point expressions to compare
     * <- the MLRISC condition
     * <- the MLRISC conditional expression
     *)
    fun translateFloat(Rtl.EQ, left, right) =
	  (MLTree.==, MLTree.FCMP(MLTree.==, left, right, MLTree.LR))
      | translateFloat(Rtl.LE, left, right) =
	  (MLTree.<=, MLTree.FCMP(MLTree.<=, left, right, MLTree.LR))
      | translateFloat(Rtl.LT, left, right) =
	  (MLTree.<, MLTree.FCMP(MLTree.<, left, right, MLTree.LR))
      | translateFloat(Rtl.GE, left, right) =
	  (MLTree.>=, MLTree.FCMP(MLTree.>=, left, right, MLTree.LR))
      | translateFloat(Rtl.GT, left, right) =
	  (MLTree.>, MLTree.FCMP(MLTree.>, left, right, MLTree.LR))
      | translateFloat(Rtl.NE, left, right) =
	  (MLTree.<>, MLTree.FCMP(MLTree.<>, left, right, MLTree.LR))
      | translateFloat(_, _, _) =
	  raise InvalidRtl "invalid floating-point comparison"

    (*
     * Return an MLRISC condition and conditional expression for a given
     * comparison operand and expression.
     * operand	  -> the comparison operand to compare with
     * expression -> the expression to compare to zero
     * <- the MLRISC condition
     * <- the MLRISC conditional expression
     *)
    fun translateZero(operand, expression) =
	  translate(operand, expression, MLTree.LI 0)

    (*
     * Return an MLRISC condition and conditional expression for a given
     * comparison operand and floating-point expression.
     * operand	  -> the comparison operand to compare with
     * expression -> the floating-point expression to compare to zero
     * <- the MLRISC condition
     * <- the MLRISC conditional expression
     *)
    fun translateZeroFloat(operand, expression) =
	  translateFloat(operand, expression, MLTree.CVTI2D(MLTree.LI 0))

  end

  structure Alignment = struct

    (*
     * Return an alignment size and offset for a given alignment operand.
     * -> the alignment operand
     * <- the alignment size
     * <- the alignment offset
     *)
    fun translate Rtl.LONG    = (4, 0)
      | translate Rtl.QUAD    = (8, 0)
      | translate Rtl.ODDLONG = (8, 4)
      | translate Rtl.OCTA    = (16, 0)
      | translate Rtl.ODDOCTA = (16, 12)

  end

  (* -- procedure call functions ------------------------------------------- *)

  local
    (*
     * Return MLRISC wrapper statements for a given call.
     * live -> the integer registers live across the call
     *	    -> the procedure being called
     * <- the wrapper statements
     *)
    fun mlWrapper live _ =
	  let
	    val label = Machine.freshCodeLabel()
	    val site  = (ref live, Register.callSite label)
	  in
	    ([MLTree.PSEUDO_OP(MLRISCPseudo.CallSite site)],
	     [MLTree.DEFINELABEL(LocalLabel.translate label)])
	  end

    fun noWrapper _ =
	  ([], [])
  in
    (*
     * Generate code to call a given procedure.
     * live	-> the integer registers live across the call
     * operands -> the procedure, arguments, and results
     * <- the mltree values
     *)
    fun call live operands =
	  ExternalConvention.call (mlWrapper live) (Procedure.frame()) operands
    fun callC operands =
	  ExternalConvention.call noWrapper (Procedure.frame()) operands
  end

  (* -- translation functions ---------------------------------------------- *)

  local
    (*
     * Abbreviations for commonly used translation functions.
     *)
    val srcReg	     = Register.translateExp
    val destReg	     = Register.translate
    val srcFloatReg  = FloatRegister.translateExp
    val destFloatReg = FloatRegister.translate
    val value	     = SmallValue.translate
    val ea	     = EffectiveAddress.translate
    val localLabel   = LocalLabel.translate

    (*
     * Return a list of mltree values for a specific Rtl instruction whose
     * operands have already been translated to their MLRISC equivalents.
     * -> the translated operands
     * <- the equivalent mltree values
     *)

    fun LI(immediate, dest) =
	  [MLTree.CODE[MLTree.MV(dest, immediate)]]

    fun LADDR(label, offset, dest) =
	  [MLTree.CODE[
	     MLTree.MV(dest, MLTreeExtra.add(labelExp label, offset))
	   ]]

    fun LEA(address, dest) =
	  [MLTree.CODE[mv(dest, address)]]

    fun MV(src, dest) =
	  [MLTree.CODE[mv(dest, src)]]

    fun CMV(compare, test, src, dest) =
	  let
	    val (testCondition, testExp) =
		  Comparison.translateZero(Comparison.complement compare, test)
	    val skipLabel =
		  newLabel()
	  in
	    [MLTree.CODE[
	       MLTree.BCC(testCondition, testExp, skipLabel),
	       mv(dest, src)
	     ],
	     MLTree.DEFINELABEL skipLabel]
	  end

    fun FMV(src, dest) =
	  [MLTree.CODE[fmv(dest, src)]]

    local
      fun code operator (left, right, dest) =
	    [MLTree.CODE[MLTree.MV(dest, operator(left, right))]]
    in
      val ADD  = code MLTree.ADD
      val MUL  = code MLTree.MULU
      val ADDT = code MLTree.ADDT
      val MULT = code MLTree.MULT
    end

    local
      fun code operator (left, right, dest) =
	    [MLTree.CODE[MLTree.MV(dest, operator(left, right, MLTree.LR))]]
    in
      (*
       * strictly speaking, this divide shouldn't trap, but our current usage
       * of Rtl doesn't cause any traps
       *)
      val SUB  = code MLTree.SUB
      val DIV  = code MLTree.DIVT
      val SUBT = code MLTree.SUBT
      val DIVT = code MLTree.DIVT
    end

    local
      fun code shift (left, right, dest) =
	    [MLTree.CODE[
	       MLTree.MV(dest, MLTree.ADD(MLTree.SLL(left, MLTree.LI shift,
						     MLTree.LR),
					  right))
	     ]]
    in
      val S4ADD = code 2
      val S8ADD = code 3
    end

    local
      fun code shift (left, right, dest) =
	    [MLTree.CODE[
	       MLTree.MV(dest, MLTree.SUB(MLTree.SLL(left, MLTree.LI shift,
						     MLTree.LR),
					  right, MLTree.LR))
	     ]]
    in
      val S4SUB = code 2
      val S8SUB = code 3
    end

    fun MODT(left, right, dest) =
	  let
	    val quotient  = MLTree.DIVT(left, right, MLTree.LR)
	    val remainder = MLTree.SUBT(left, MLTree.MULT(quotient, right),
					MLTree.LR)
	  in
	    [MLTree.CODE[MLTree.MV(dest, remainder)]]
	  end

    (*
     * strictly speaking, this modulo shouldn't trap, but our current usage
     * of Rtl doesn't cause any traps
     *)
    val MOD = MODT

    local
      fun code((testCondition, testExp), dest) =
	    let
	      val trueLabel = newLabel()
	      val doneLabel = newLabel()
	    in
	      (*
	       * if we had condition code registers in Rtl, we could generate
	       * better code for this
	       *)
	      [MLTree.CODE[
		 MLTree.BCC(testCondition, testExp, trueLabel),
		 MLTree.MV(dest, MLTree.LI 0),
		 MLTree.JMP(labelExp doneLabel, [doneLabel])
	       ],
	       MLTree.DEFINELABEL trueLabel,
	       MLTree.CODE[MLTree.MV(dest, MLTree.LI 1)],
	       MLTree.DEFINELABEL doneLabel]
	    end
    in
      fun CMPSI(compare, left, right, dest) =
	    code(Comparison.translate(compare, left, right), dest)
      fun CMPUI(compare, left, right, dest) =
	    code(Comparison.translateUnsigned(compare, left, right), dest)
    end

    fun NOTB(src, dest) =
	  [MLTree.CODE[MLTree.MV(dest, MLTree.XORB(src, MLTree.LI ~1))]]

    local
      fun code operator (left, right, dest) =
	    [MLTree.CODE[MLTree.MV(dest, operator(left, right))]]
    in
      val ANDB = code MLTree.ANDB
      val ORB  = code MLTree.ORB
      val XORB = code MLTree.XORB
    end

    local
      fun code operator (left, right, dest) =
	    [MLTree.CODE[MLTree.MV(dest, operator(left, right, MLTree.LR))]]
    in
      val SRA = code MLTree.SRA
      val SRL = code MLTree.SRL
      val SLL = code MLTree.SLL
    end

    fun CVT_REAL2INT(src, dest) =
	  callC(externalExp "cvt_real2int",
		[ExternalConvention.float src],
		[ExternalConvention.integer dest])

    fun CVT_INT2REAL(src, dest) =
	  [MLTree.CODE[MLTree.FMV(dest, MLTree.CVTI2D src)]]

    local
      fun code operator (left, right, dest) =
	    [MLTree.CODE[MLTree.FMV(dest, operator(left, right))]]
    in
      val FADDD = code MLTree.FADDD
      val FMULD = code MLTree.FMULD
    end

    local
      fun code operator (left, right, dest) =
	    [MLTree.CODE[MLTree.FMV(dest, operator(left, right, MLTree.LR))]]
    in
      val FSUBD = code MLTree.FSUBD
      val FDIVD = code MLTree.FDIVD
    end

    local
      fun code operator (src, dest) =
	    [MLTree.CODE[MLTree.FMV(dest, operator src)]]
    in
      val FABSD = code MLTree.FABSD
      val FNEGD = code MLTree.FNEGD
    end

    fun CMPF(compare, left, right, dest) =
	  let
	    val (testCondition, testExp) =
		  Comparison.translateFloat(compare, left, right)
	    val trueLabel =
		  newLabel()
	    val doneLabel =
		  newLabel()
	  in
	    (*
	     * if we had condition code registers in Rtl, we could generate
	     * better code for this
	     *)
	    [MLTree.CODE[
	       MLTree.FBCC(testCondition, testExp, trueLabel),
	       MLTree.MV(dest, MLTree.LI 0),
	       MLTree.JMP(labelExp doneLabel, [doneLabel])
	     ],
	     MLTree.DEFINELABEL trueLabel,
	     MLTree.CODE[MLTree.MV(dest, MLTree.LI 1)],
	     MLTree.DEFINELABEL doneLabel]
	  end

    local
      fun code procedure (src, dest) =
	    callC(externalExp procedure,
		  [ExternalConvention.float src],
		  [ExternalConvention.float dest])
    in
      val SQRT	 = code "sqrt"
      val SIN	 = code "sin"
      val COS	 = code "cos"
      val ARCTAN = code "atan"
      val EXP	 = code "exp"
      val LN	 = code "ln"
    end

    fun BR label =
	  [MLTree.CODE[MLTree.JMP(labelExp label, [label])]]

    fun JMP(src, labels) =
	  [MLTree.CODE[MLTree.JMP(src, labels)]]

    local
      fun code((condition, exp), label) =
	    [MLTree.CODE[MLTree.BCC(condition, exp, label)]]
    in
      fun BCNDI(compare, test, label, _) =
	    code(Comparison.translateZero(compare, test), label)

      fun BCNDI2(compare, left, right, label, _) =
	    code(Comparison.translate(compare, left, right), label)
    end

    local
      fun code((condition, exp), label) =
	    [MLTree.CODE[MLTree.FBCC(condition, exp, label)]]
    in
      fun BCNDF(compare, test, label, _) =
	    code(Comparison.translateZeroFloat(compare, test), label)

      fun BCNDF2(compare, left, right, label, _) =
	    code(Comparison.translateFloat(compare, left, right), label)
    end

    fun CALL(procedure, arguments, results, (integers, _)) =
	  call integers (procedure, arguments, results)

    fun externalCALL operand =
	  let
	    val alloc_pointer = externalExp "cur_alloc_pointer"
	    val alloc_limit   = externalExp "cur_alloc_limit"
	    val heapPointer   = IntegerConvention.heapPointer
	    val heapLimit     = IntegerConvention.heapLimit
	  in
	    [MLTree.CODE[
	       MLTree.STORE32(alloc_pointer, MLTree.REG heapPointer),
	       MLTree.STORE32(alloc_limit, MLTree.REG heapLimit)
	     ]]@
	    CALL operand@
	    [MLTree.CODE[
	       MLTree.MV(heapPointer, MLTree.LOAD32 alloc_pointer),
	       MLTree.MV(heapLimit, MLTree.LOAD32 alloc_limit)
	     ]]
	  end

    fun RETURN _ =
	  raise InvalidRtl "should have been translated to a branch"

    fun SAVE_CS _ =
	  [MLTree.CODE(CallConventionBasis.setAssignment(Procedure.saves()))]

    val RESTORE_CS =
	  [MLTree.CODE(CallConventionBasis.getAssignment(Procedure.saves()))]

    val END_SAVE = []

    fun LOAD32I(address, dest) =
	  [MLTree.CODE[MLTree.MV(dest, MLTree.LOAD32 address)]]

    fun STORE32I(address, src) =
	  [MLTree.CODE[MLTree.STORE32(address, src)]]

    fun LOADQF(address, dest) =
	  [MLTree.CODE[MLTree.FMV(dest, MLTree.LOADD address)]]

    fun STOREQF(address, src) =
	  [MLTree.CODE[MLTree.STORED(address, src)]]

    fun NEEDMUTATE dest =
	  let
	    val cursor = externalExp "writelist_cursor"
	  in
	    [MLTree.CODE[
	       MLTree.MV(IntegerConvention.heapLimit,
			 MLTree.SUB(MLTree.REG IntegerConvention.heapLimit,
				    MLTree.LI 8, MLTree.LR)),
	       MLTree.MV(dest, MLTree.LOAD32 cursor),
	       MLTree.STORE32(cursor, MLTree.ADD(MLTree.REG dest, MLTree.LI 4))
	     ]]
	      (* alpha-specific sizes? ??? *)
	  end

    fun NEEDGC src =
	  let
	    val skipLabel =
		  newLabel()
	    val size =
		  MLTreeExtra.sll(src, MLTree.LI 2)
	    (*
	     * strictly speaking, this comparison should be unsigned, but this
	     * generates extraneous "zap" instructions on the Alpha
	     * architecture
	     *)
	    val compare =
		  MLTree.CMP(
		    MLTree.LE,
		    MLTree.ADD(MLTree.REG IntegerConvention.heapPointer, size),
		    MLTree.REG IntegerConvention.heapLimit,
		    MLTree.LR)
	    val gc_raw =
		  externalExp "gc_raw"
	  in
	    [MLTree.CODE[
	       MLTree.BCC(MLTree.LEU, compare, skipLabel),
	       MLTree.MV(IntegerConvention.heapLimit, size)
	     ]]@
	    call [] (gc_raw, [], [])@
	    [MLTree.DEFINELABEL skipLabel]
	  end

    local
      fun code(size, moveValue, dest, raw) =
	    let
	      val alloc_raw = externalExp raw
	    in
	      [MLTree.CODE[
		 MLTree.MV(IntegerConvention.temporary1, size),
		 moveValue
	       ]]@
	      call [] (alloc_raw, [], [])@
	      [MLTree.CODE[
		 MLTree.MV(dest, MLTree.REG IntegerConvention.temporary1)
	       ]]
	    end
    in
      fun INT_ALLOC(size, value, dest) =
	    code(size, mv(IntegerConvention.temporary2, value),
		 dest, "int_alloc_raw")
      fun FLOAT_ALLOC(size, value, dest) =
	    code(size, MLTree.FMV(FloatConvention.temporary1, value),
		 dest, "float_alloc_raw")
      fun PTR_ALLOC(size, value, dest) =
	    code(size, mv(IntegerConvention.temporary2, value),
		 dest, "ptr_alloc_raw")
    end

    val SOFT_VBARRIER = []
    val SOFT_ZBARRIER = []
    val HARD_VBARRIER = []
    val HARD_ZBARRIER = []

    val HALT = []

    val HANDLER_ENTRY = [] (* should set global pointer on alpha !!! *)

    fun ILABEL label =
	  [MLTree.DEFINELABEL label]

    fun IALIGN alignment =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.Align alignment)]

    fun ICOMMENT message =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.Comment message)]
  in
    (*
     * Return a list of mltree values for a given Rtl instruction.
     * -> the Rtl instruction
     * <- the equivalent mltree values
     *)
    fun translateInstruction(Rtl.LI(immediate, dest)) =
	  LI(Immediate.translate32 immediate, destReg dest)
      | translateInstruction(Rtl.LADDR(label, offset, dest)) =
	  LADDR(Label'.translate label, Offset.translate offset, destReg dest)
      | translateInstruction(Rtl.LEA(address, dest)) =
	  LEA(ea address, destReg dest)
      | translateInstruction(Rtl.MV(src, dest)) =
	  MV(srcReg src, destReg dest)
      | translateInstruction(Rtl.CMV(compare, test, src, dest)) =
	  CMV(compare, srcReg test, value src, destReg dest)
      | translateInstruction(Rtl.FMV(src, dest)) =
	  FMV(srcFloatReg src, destFloatReg dest)

      | translateInstruction(Rtl.ADD(left, right, dest)) =
	  ADD(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.SUB(left, right, dest)) =
	  SUB(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.MUL(left, right, dest)) =
	  MUL(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.DIV(left, right, dest)) =
	  DIV(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.MOD(left, right, dest)) =
	  MOD(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.S4ADD(left, right, dest)) =
	  S4ADD(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.S8ADD(left, right, dest)) =
	  S8ADD(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.S4SUB(left, right, dest)) =
	  S4SUB(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.S8SUB(left, right, dest)) =
	  S8SUB(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.ADDT(left, right, dest)) =
	  ADDT(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.SUBT(left, right, dest)) =
	  SUBT(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.MULT(left, right, dest)) =
	  MULT(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.DIVT(left, right, dest)) =
	  DIVT(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.MODT(left, right, dest)) =
	  MODT(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.CMPSI(compare, left, right, dest)) =
	  CMPSI(compare, srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.CMPUI(compare, left, right, dest)) =
	  CMPUI(compare, srcReg left, value right, destReg dest)
 
      | translateInstruction(Rtl.NOTB(src, dest)) =
	  NOTB(srcReg src, destReg dest)
      | translateInstruction(Rtl.ANDB(left, right, dest)) =
	  ANDB(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.ORB(left, right, dest)) =
	  ORB(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.XORB(left, right, dest)) =
	  XORB(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.SRA(left, right, dest)) =
	  SRA(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.SRL(left, right, dest)) =
	  SRL(srcReg left, value right, destReg dest)
      | translateInstruction(Rtl.SLL(left, right, dest)) =
	  SLL(srcReg left, value right, destReg dest)

      | translateInstruction(Rtl.CVT_REAL2INT(src, dest)) =
	  CVT_REAL2INT(destFloatReg src, destReg dest)
      | translateInstruction(Rtl.CVT_INT2REAL(src, dest)) =
	  CVT_INT2REAL(srcReg src, destFloatReg dest)

      | translateInstruction(Rtl.FADDD(left, right, dest)) =
	  FADDD(srcFloatReg left, srcFloatReg right, destFloatReg dest)
      | translateInstruction(Rtl.FSUBD(left, right, dest)) =
	  FSUBD(srcFloatReg left, srcFloatReg right, destFloatReg dest)
      | translateInstruction(Rtl.FMULD(left, right, dest)) =
	  FMULD(srcFloatReg left, srcFloatReg right, destFloatReg dest)
      | translateInstruction(Rtl.FDIVD(left, right, dest)) =
	  FDIVD(srcFloatReg left, srcFloatReg right, destFloatReg dest)
      | translateInstruction(Rtl.FABSD(src, dest)) =
	  FABSD(srcFloatReg src, destFloatReg dest)
      | translateInstruction(Rtl.FNEGD(src, dest)) =
	  FNEGD(srcFloatReg src, destFloatReg dest)
      | translateInstruction(Rtl.CMPF(compare, left, right, dest)) =
	  CMPF(compare, srcFloatReg left, srcFloatReg right, destReg dest)

      | translateInstruction(Rtl.SQRT(src, dest)) =
	  SQRT(destFloatReg src, destFloatReg dest)
      | translateInstruction(Rtl.SIN(src, dest)) =
	  SIN(destFloatReg src, destFloatReg dest)
      | translateInstruction(Rtl.COS(src, dest)) =
	  COS(destFloatReg src, destFloatReg dest)
      | translateInstruction(Rtl.ARCTAN(src, dest)) =
	  ARCTAN(destFloatReg src, destFloatReg dest)
      | translateInstruction(Rtl.EXP(src, dest)) =
	  EXP(destFloatReg src, destFloatReg dest)
      | translateInstruction(Rtl.LN(src, dest)) =
	  LN(destFloatReg src, destFloatReg dest)

      | translateInstruction(Rtl.BR label) =
	  BR(localLabel label)
      | translateInstruction(Rtl.BCNDI(compare, test, label, predict)) =
	  BCNDI(compare, srcReg test, localLabel label, predict)
      | translateInstruction(Rtl.BCNDF(compare, test, label, predict)) =
	  BCNDF(compare, srcFloatReg test, localLabel label, predict)
      | translateInstruction(
	  Rtl.BCNDI2(compare, left, right, label, predict)) =
	  BCNDI2(compare, srcReg left, value right, localLabel label, predict)
      | translateInstruction(
	  Rtl.BCNDF2(compare, left, right, label, predict)) =
	  BCNDF2(compare, srcFloatReg left, srcFloatReg right,
		 localLabel label, predict)
      | translateInstruction(Rtl.JMP(src, labels)) =
	  JMP(srcReg src, map LocalLabel.translate labels)

      | translateInstruction(Rtl.SAVE_CS label) =
	  SAVE_CS(localLabel label)
      | translateInstruction(Rtl.END_SAVE) =
	  END_SAVE
      | translateInstruction(Rtl.RESTORE_CS) =
	  RESTORE_CS

      | translateInstruction(Rtl.LOAD32I(address, dest)) =
	  LOAD32I(ea address, destReg dest)
      | translateInstruction(Rtl.STORE32I(address, src)) =
	  STORE32I(ea address, srcReg src)
      | translateInstruction(Rtl.LOADQF(address, dest)) =
	  LOADQF(ea address, destFloatReg dest)
      | translateInstruction(Rtl.STOREQF(address, src)) =
	  STOREQF(ea address, srcFloatReg src)

      | translateInstruction(Rtl.CALL{func	  = procedure,
				      args	  = arguments,
				      results	  = results,
				      extern_call = externalFlag,
				      tailcall	  = tailFlag, (* ??? *)
				      save	  = Rtl.SAVE save,
				      ...}) =
	  (if externalFlag then externalCALL else CALL)
	     (RegisterOrLabel.translate procedure,
	      RegisterSet.translateCall arguments,
	      RegisterSet.translateCall results,
	      RegisterSet.translate save)
      | translateInstruction(Rtl.RETURN src) =
	  RETURN(srcReg src)

      | translateInstruction(Rtl.NEEDMUTATE dest) =
	  NEEDMUTATE(destReg dest)
      | translateInstruction(Rtl.NEEDGC src) =
	  NEEDGC(value src)
      | translateInstruction(Rtl.FLOAT_ALLOC(length, initial, dest, _)) =
	  FLOAT_ALLOC(srcReg length, srcFloatReg initial, destReg dest)
      | translateInstruction(Rtl.INT_ALLOC(length, initial, dest, _)) =
	  INT_ALLOC(srcReg length, srcReg initial, destReg dest)
      | translateInstruction(Rtl.PTR_ALLOC(length, initial, dest, _)) =
	  PTR_ALLOC(srcReg length, srcReg initial, destReg dest)

      | translateInstruction(Rtl.SOFT_VBARRIER trap) =
	  SOFT_VBARRIER
      | translateInstruction(Rtl.SOFT_ZBARRIER trap) =
	  SOFT_ZBARRIER
      | translateInstruction(Rtl.HARD_VBARRIER trap) =
	  HARD_VBARRIER
      | translateInstruction(Rtl.HARD_ZBARRIER trap) =
	  HARD_ZBARRIER
      | translateInstruction(Rtl.HANDLER_ENTRY) =
	  HANDLER_ENTRY
      | translateInstruction(Rtl.ILABEL label) =
	  ILABEL(localLabel label)
      | translateInstruction(Rtl.IALIGN align) =
	  IALIGN(Alignment.translate align)
      | translateInstruction(Rtl.HALT) =
	  HALT
      | translateInstruction(Rtl.ICOMMENT message) =
	  ICOMMENT message
  end

  local
    (*
     * Return a list of mltree values for a specific Rtl data value whose
     * operands have already been translated to their MLRISC equivalents.
     * -> the translated operands
     * <- the equivalent mltree values
     *)
    fun COMMENT message =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.Comment message)]

    fun STRING value =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.String value)]

    fun INT32 value =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.Integer value)]

    fun INT_FLOATSIZE value =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.IntegerFloatSize value)]

    fun FLOAT value =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.Float value)]

    fun DATA label =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.Label label)]

    fun ARRAYI(size, value) =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.IntegerArray(size, value))]

    fun ARRAYF(size, value) =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.FloatArray(size, value))]

    fun ARRAYP(size, Rtl.PTR label) =
	  [MLTree.PSEUDO_OP(
	     MLRISCPseudo.LabelArray(size, Label'.translate label))]
      | ARRAYP(size, Rtl.TAG immediate) =
	  [MLTree.PSEUDO_OP(
	     MLRISCPseudo.IntegerArray(size, immediate))]

    fun ALIGN alignment =
	  [MLTree.PSEUDO_OP(MLRISCPseudo.Align alignment)]

    fun DLABEL label =
	  [MLTree.DEFINELABEL label]

    fun externalDLABEL label = [
	  MLTree.PSEUDO_OP(MLRISCPseudo.Export label),
	  MLTree.DEFINELABEL label
	]
  in
    (*
     * Return a list of mltree values for a given Rtl data value.
     * -> the Rtl data value
     * <- the equivalent mltree values
     *)
    fun translateData(Rtl.COMMENT message) =
	  COMMENT message
      | translateData(Rtl.STRING string) =
	  STRING string
      | translateData(Rtl.INT32 word32) =
	  INT32 word32
      | translateData(Rtl.INT_FLOATSIZE word32) =
	  INT_FLOATSIZE word32
      | translateData(Rtl.FLOAT string) =
	  FLOAT string
      | translateData(Rtl.DATA label) =
	  DATA(Label'.translate label)
      | translateData(Rtl.ARRAYI(size, value)) =
	  ARRAYI(size, value)
      | translateData(Rtl.ARRAYF(size, value)) =
	  ARRAYF(size, value)
      | translateData(Rtl.ARRAYP(size, value)) =
	  ARRAYP(size, value)
      | translateData(Rtl.ALIGN align) =
	  ALIGN(Alignment.translate align)
      | translateData(Rtl.DLABEL label) =
	  let
	    val label' = Label'.translate label
	  in
	    case label of
	      Rtl.ML_EXTERN_LABEL _ => externalDLABEL label'
	    | Rtl.C_EXTERN_LABEL _  => externalDLABEL label'
	    | Rtl.LOCAL_LABEL _	    => DLABEL label'
	  end
  end

  (*
   * Return a list of mltree values for a given Rtl procedure.
   * -> the Rtl procedure
   * <- the mltree values representing the procedure
   *)
  local
    (*
     * Transform an array of Rtl instructions into a list of Rtl instructions
     * where return instructions have been replaced by branches to a given
     * label.  Discard the last instruction, if a return.
     * return	    -> the return label to use
     * instructions -> the instructions to transform
     * <- the transformed instructions
     *)
    fun transformReturn return instructions =
	  let
	    val last = Array.length instructions-1

	    val count = case Array.sub(instructions, last) of
			  Rtl.RETURN _ => SOME last
			| _	       => NONE

	    fun transform(_, (Rtl.RETURN _), list) = Rtl.BR return::list
	      | transform(_, instruction, list)	   = instruction::list
	  in
	    Array.foldri transform [] (instructions, 0, count)
	  end

    (*
     * Refine the liveness information in the call site pseudo-operations of
     * a given procedure.
     * procedure <-> the procedure to refine the call site liveness
     *		     information of
     *)
    local
      (*
       * filtering physical registers seems arbitrary--what are the properties
       * of live registers that should not be traced across call sites? ???
       *)
      val keepPseudo = List.filter (fn id => id>=Cells.firstPseudoReg)

      fun updateLive live (liveRef, _) =
	    liveRef := keepPseudo live

      fun updateTree update (MLTree.PSEUDO_OP(MLRISCPseudo.CallSite site)) =
	    update site
	| updateTree _ _ =
	    ()

      fun updateBlock update (block, value) =
	    app (updateTree (update value)) block
    in
      fun updateCallSites procedure =
	    let
	      val blocks   = BasicBlock.partition(
			       BasicBlock.keepTargetLabels procedure)
	      val liveness = IntegerLiveness.liveness blocks
	    in
	      ListPair.app (updateBlock updateLive) (blocks, liveness)
	    end
    end

    (*
     * Return the pseudo-registers that are live across the call sites of
     * a given procedure.
     * procedure -> the procedure to return the call site live
     *		    pseudo-registers of
     * <- a list of the pseudo-registers that are live across any call site
     *	  of procedure
     *)
    local
      fun callSiteLiveTree(MLTree.PSEUDO_OP(
			     MLRISCPseudo.CallSite(ref live, _)), set) =
	    IntSet.addList(set, live)
	| callSiteLiveTree(_, set) =
	    set
    in
      val callSiteLive = IntSet.listItems o foldr callSiteLiveTree IntSet.empty
    end

    fun assignCallee(target, source) =
	  Register.assign(target, TraceTable.TRACE_CALLEE(Machine.R source))

    fun translate(Rtl.PROC{name	   = label,
			   args	   = arguments,
			   results = results,
			   code	   = instructions,
			   save	   = Rtl.SAVE saves,
			   known   = knownFlag, (* ??? *)
			   ...}) =
	  let
	    (*
	     * Start translating a new procedure.
	     * procedure must be opened before any rtl is translated
	     *)
	    val _ = Procedure.open_()

	    (*
	     * replace return instructions with branches to return code.
	     *)
	    val return	      = Rtl.fresh_code_label()
	    val instructions' = transformReturn return instructions

	    (*
	     * translate labels and registers
	     *)
	    val label'	   = LocalLabel.translate label
	    val arguments' = RegisterSet.translateCall arguments
	    val results'   = RegisterSet.translateCall results
	    val saves'	   = RegisterSet.translateCall saves

	    (*
	     * assign callee-save registers to pseudo-registers
	     *)
	    val saves'' = ExternalConvention.save saves'
	    val live	   = ExternalConvention.integerSave saves''

	    val _ = Procedure.setSaves saves''
	    val _ = app assignCallee live

	    (*
	     * translate the body of the procedure
	     *)
	    val body = foldr (splice translateInstruction) [] instructions'

	    (*
	     * generate code to enter and exit the procedure
	     *)
	    val frame = Procedure.frame()

	    val enter =
		  [MLTree.PSEUDO_OP(MLRISCPseudo.ProcedureHeader label')]@
		  ExternalConvention.enter frame (arguments', saves'', body)

	    val exit =
		  [MLTree.DEFINELABEL(LocalLabel.translate return)]@
		  ExternalConvention.exit frame (results', saves'', body)@
		  [MLTree.PSEUDO_OP(MLRISCPseudo.ProcedureTrailer label')]

	    (*
	     * define a deferred spill location in the current stack frame
	     * for each register that might be live in the procedure
	     * this must happen after all pseudo-registers are allocated
	     *)
	    val _ = Register.deferSpill frame (Procedure.integers())
	    val _ = FloatRegister.deferSpill frame (Procedure.floats())

	    (*
	     * update the liveness information at the call sites in the
	     * procedure
	     *)
	    val procedure = enter@body@exit
	    val _	  = updateCallSites procedure
	  in
	    (*
	     * spill polymorphic value descriptors that are used by call
	     * sites
	     *)
	    Register.spillPolys (callSiteLive procedure) procedure
	  end

    fun reset() = (Register.resetSource();
		   FloatRegister.resetSource();
		   Procedure.close())
  in
    val translateProcedure = translate protect reset
  end

  (* -- emitter functions -------------------------------------------------- *)

  val emitData = app (emitMLTree o translateData)

  local
    fun emitCluster translateProcedure procedures =
	  let
	    val _ = Cluster.open_()

	    val emitProcedures = app (emitMLTree o translateProcedure)

	    val header = [
		  MLTree.BEGINCLUSTER,
		  MLTree.PSEUDO_OP MLRISCPseudo.ClusterHeader
		]

	    val trailer = [
		  MLTree.PSEUDO_OP MLRISCPseudo.ClusterTrailer,
		  MLTree.ENDCLUSTER(Cluster.map())
		]
	  in
	    emitMLTree header;
	    emitProcedures procedures;
	    emitMLTree trailer
	  end

    fun resetCluster() = (Register.resetTarget();
			  FloatRegister.resetTarget();
			  Cluster.close())

    fun emitBody(prefix, main, procedures, data) =
	  let
	    fun define name =
		  let
		    val label = externalLabel(prefix^name)
		  in
		    [MLTree.PSEUDO_OP(MLRISCPseudo.Export label),
		     MLTree.DEFINELABEL label]
		  end

	    fun translateProcedure'(procedure as Rtl.PROC{name = label, ...}) =
		  (if Rtl.eq_locallabel(label, main) then
		     define "_client_entry"
		   else
		     [])@
		  translateProcedure procedure

	    val emitCluster' = emitCluster translateProcedure'

	    val header = [
		  MLTree.BEGINCLUSTER,
		  MLTree.PSEUDO_OP MLRISCPseudo.ModuleHeader
		]

	    val trailer = [
		  MLTree.PSEUDO_OP MLRISCPseudo.ModuleTrailer,
		  MLTree.ENDCLUSTER(Cluster.map())
		]

	    val textHeader =
		  [MLTree.PSEUDO_OP MLRISCPseudo.TextHeader]@
		  define "_CODE_BEGIN_VAL"

	    val textTrailer =
		  define "_CODE_END_VAL"@
		  [MLTree.PSEUDO_OP MLRISCPseudo.TextTrailer]

	    val dataHeader =
		  [MLTree.PSEUDO_OP MLRISCPseudo.DataHeader]@
		  define "_SML_GLOBALS_BEGIN_VAL"

	    val dataTrailer =
		  define "_SML_GLOBALS_END_VAL"@
		  [MLTree.PSEUDO_OP MLRISCPseudo.DataTrailer]

	    val clusters = map (fn procedure => [procedure]) procedures
	  in
	    emitMLTree header;
	    emitMLTree textHeader;
	    emitMLTree [MLTree.ENDCLUSTER(Cluster.map())];
	    app (emitCluster' protect resetCluster) clusters;
	    emitMLTree [MLTree.BEGINCLUSTER];
	    emitMLTree textTrailer;
	    emitMLTree dataHeader;
	    Array.app (emitMLTree o translateData) data;
	    emitMLTree dataTrailer;
	    emitMLTree trailer
	  end

    fun resetBody() = ()

    fun emitTables(prefix, infos, objects, variables) =
	  let
	    val header = [
		  MLTree.BEGINCLUSTER,
		  MLTree.PSEUDO_OP MLRISCPseudo.TableHeader
		]

	    val trailer = [
		  MLTree.PSEUDO_OP MLRISCPseudo.TableTrailer,
		  MLTree.ENDCLUSTER(Cluster.map())
		]

	    fun translateVariable(label, represent) =
		  (label, RegisterTraceMap.traceGlobalRepresent represent)

	    val objects' = objects

	    val variables' = map translateVariable variables

	    val calls = TraceTable.MakeTableHeader prefix@
			TraceTable.MakeTable infos@
			TraceTable.MakeTableTrailer prefix

	    val mutables = TraceTable.MakeMutableTable(prefix, objects')

	    val globals = TraceTable.MakeGlobalTable(prefix, variables')
	  in
	    emitMLTree header;
	    emitData calls;
	    emitData mutables;
	    emitData globals;
	    emitMLTree trailer
	  end

    fun resetTables() = (LocalLabel.reset();
			 Label.reset())

    fun emit(Rtl.MODULE{main		  = main,
			procs		  = procedures,
			data		  = data,
			mutable_objects	  = objects,
			mutable_variables = variables}) =
	  let
	    val name = LocalLabel.string main

	    fun emitBody' operand = (emitBody operand; Module.infos())

	    val infos =
		  (emitBody' protect resetBody)(name, main, procedures, data)
	  in
	    (emitTables protect resetTables)(name, infos, objects, variables)
	  end
  in
    fun emitModule operand = (Module.open_();
			      (emit protect Module.close) operand)
  end

  fun emitEntryTable labels =
	let
	  val names = map LocalLabel.string labels

	  fun table name =
		let
		  fun data prefix =
			Rtl.DATA(Rtl.ML_EXTERN_LABEL(prefix^"_"^name))
		in
		  Rtl.DLABEL(Rtl.ML_EXTERN_LABEL name)::map data names
		end

	  val header = [
		MLTree.BEGINCLUSTER,
		MLTree.PSEUDO_OP MLRISCPseudo.TableHeader
	      ]

	  val trailer = [
		MLTree.PSEUDO_OP MLRISCPseudo.TableTrailer,
		MLTree.ENDCLUSTER(Cluster.map())
	      ]

	  val tables = [
		table "GCTABLE_BEGIN_VAL",
		table "GCTABLE_END_VAL",
		table "SML_GLOBALS_BEGIN_VAL",
		table "SML_GLOBALS_END_VAL",
		table "GLOBAL_TABLE_BEGIN_VAL",
		table "GLOBAL_TABLE_END_VAL",
		table "MUTABLE_TABLE_BEGIN_VAL",
		table "MUTABLE_TABLE_END_VAL",
		table "CODE_BEGIN_VAL",
		table "CODE_END_VAL"
	      ]

	  val module_count = [
		Rtl.DLABEL(Rtl.ML_EXTERN_LABEL "module_count"),
		Rtl.INT32(Word32.fromInt(length names))
	      ]

	  val client_entry = table "client_entry"
	in
	  emitMLTree header;
	  app emitData tables;
	  emitData module_count;
	  emitData client_entry;
	  emitMLTree trailer
	end

end

