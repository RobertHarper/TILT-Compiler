
(* =========================================================================
 * AlphaLink.sml
 * ========================================================================= *)

structure AlphaLink = struct

  (* -- TIL2 structures ---------------------------------------------------- *)

  structure Decalpha = Decalpha(val exclude_intregs = []
				structure Rtl	    = Linkrtl.Rtl)

  structure Regset = Regset(structure Machine = Decalpha)

  structure Regmap = Regmap(structure Machine = Decalpha)

  structure Labelmap = Labelmap(structure Machine = Decalpha)

  structure Decalphautils = Decalphautils(structure Decalpha = Decalpha
					  structure Labelmap = Labelmap
					  structure Regmap   = Regmap
					  structure Regset   = Regset)

  structure TraceTable = Tracetable(val little_endian = true
				    structure MU      = Decalphautils)

  (* -- MLRISC structures -------------------------------------------------- *)

  structure Constant = AlphaMLRISCConstant
  structure Float    = AlphaFloatConvention
  structure Integer  = AlphaIntegerConvention
  structure Pseudo   = AlphaMLRISCPseudo

  structure RegisterSpillMap =
    RegisterSpillMap(type offset	   = Constant.const
		     structure RegisterMap = RegisterMap)

  structure StackFrame =
    AlphaStackFrame(structure MLRISCConstant = Constant)

  structure Instr = Alpha32Instr(Constant)

  structure Rewrite = Alpha32Rewrite(Instr)

  structure FlowGraph = FlowGraph(structure I = Instr
				  structure P = Pseudo)

  structure AsmEmitter = Alpha32AsmEmitter(structure Instr     = Instr
					   structure FlowGraph = FlowGraph)

  structure Props = Alpha32Props(val exnptrR		= [] (* ??? *)
				 structure Alpha32Instr = Instr)

  structure RegAlloc = Alpha32RegAlloc(structure P   = Props
				       structure I   = Instr
				       structure F   = FlowGraph
				       structure Asm = AsmEmitter)

  structure IntegerAllocation =
    AlphaIntegerAllocation(structure AlphaInstructions = Instr
			   structure AlphaRewrite      = Rewrite
			   structure Cells	       = Alpha32Cells
			   structure FlowGraph	       = FlowGraph
			   structure IntegerConvention = Integer
			   structure RegisterMap       = RegisterMap
			   structure RegisterSpillMap  = RegisterSpillMap
			   functor RegisterAllocation  = RegAlloc.IntRa)

  structure FloatAllocation =
    AlphaFloatAllocation(structure AlphaInstructions = Instr
			 structure AlphaRewrite	     = Rewrite
			 structure Cells	     = Alpha32Cells
			 structure FloatConvention   = Float
			 structure FlowGraph	     = FlowGraph
			 structure IntegerConvention = Integer
			 structure RegisterSpillMap  = RegisterSpillMap
			 functor RegisterAllocation  = RegAlloc.FloatRa)

  structure MLTree = MLTreeF(structure Const = Constant
			     structure P     = Pseudo)

  structure AsmEmit = AsmEmit(structure F = FlowGraph
			      structure E = AsmEmitter)

  val codegen = AsmEmit.asmEmit o
		FloatAllocation.allocateCluster o
		IntegerAllocation.allocateCluster

  structure FlowGraphGen = FlowGraphGen(val codegen	    = codegen
					structure Flowgraph = FlowGraph
					structure InsnProps = Props
					structure MLTree    = MLTree)

  structure MLTreeComp = Alpha32(structure Flowgen	 = FlowGraphGen
				 structure Alpha32MLTree = MLTree
				 structure Alpha32Instr	 = Instr)

  (* -- emitter structures ------------------------------------------------- *)

  structure MLTreeExtra = MLTreeExtra(structure MLTree = MLTree)

  structure CallConventionBasis =
    CallConventionBasis(structure Cells		    = Alpha32Cells
			structure IntegerConvention = Integer
			structure MLTreeExtra	    = MLTreeExtra
			structure StackFrame	    = StackFrame)

  structure ExternalConvention =
    AlphaStandardConvention(structure Basis		= CallConventionBasis
			    structure FloatConvention	= Float
			    structure IntegerConvention = Integer
			    structure MLTreeExtra	= MLTreeExtra
			    structure StackFrame	= StackFrame)

  structure RegisterTraceMap =
    RtlRegisterTraceMap(structure Cells		 = Alpha32Cells
			structure IntSet	 = IntBinarySet
			structure MLRISCConstant = Constant
			structure RegisterMap	 = RegisterMap
			structure Rtl		 = Linkrtl.Rtl
			structure TraceTable	 = TraceTable)

  structure BasicBlock =
    BasicBlock(structure IntMap	      = IntBinaryMap
	       structure IntSet	      = IntBinarySet
	       structure MLRISCPseudo = Pseudo
	       structure MLTreeExtra  = MLTreeExtra)

  structure IntegerDataFlow =
    IntegerDataFlow(structure IntSet	  = IntBinarySet
		    structure MLTreeExtra = MLTreeExtra)

  structure IntegerLiveness =
    IntegerLiveness(structure BasicBlock      = BasicBlock
		    structure IntegerDataFlow = IntegerDataFlow
		    structure IntSet	      = IntBinarySet
		    structure MLTreeExtra     = MLTreeExtra)

  structure EmitRtlMLRISC =
    EmitRtlMLRISC(structure BasicBlock		= BasicBlock
		  structure CallConventionBasis = CallConventionBasis
		  structure Cells		= Alpha32Cells
		  structure ExternalConvention	= ExternalConvention
		  structure FloatAllocation	= FloatAllocation
		  structure FloatConvention	= Float
		  structure IntegerAllocation	= IntegerAllocation
		  structure IntegerConvention	= Integer
		  structure IntegerLiveness	= IntegerLiveness
		  structure IntSet		= IntBinarySet
		  structure MLRISCConstant	= Constant
		  structure MLRISCPseudo	= Pseudo
		  structure MLTreeComp		= MLTreeComp
		  structure MLTreeExtra		= MLTreeExtra
		  structure RegisterMap		= RegisterMap
		  structure RegisterSpillMap	= RegisterSpillMap
		  structure RegisterTraceMap	= RegisterTraceMap
		  structure Rtl			= Linkrtl.Rtl
		  structure StackFrame		= StackFrame
		  structure TraceTable		= TraceTable)

  (* -- translation functions ---------------------------------------------- *)

  local
    val suffix = ".alpha.s"

    fun outputAsm emit (module, asm) =
	  let
	    val stream = TextIO.openOut asm
	  in
	    AsmStream.asmOutStream := stream;
	    emit module;
	    TextIO.closeOut stream
	  end

    val emitModule = EmitRtlMLRISC.emitModule

    fun emitProgram(name, module) =
	  (EmitRtlMLRISC.emitModule(name, module);
	   EmitRtlMLRISC.emitEntryTable["prelude", name])

    val translate  = outputAsm emitModule
    val translate' = outputAsm emitProgram

    fun translateMain(module, asm)  = translate(("main", module), asm)
    fun translateMain'(module, asm) = translate'(("main", module), asm)
  in
    fun compile_prelude sml =
	  let
	    val module = Linkrtl.compile_prelude(false, sml)
	  in
	    translate(("prelude", module), sml^suffix)
	  end

    fun compile sml  = translateMain(Linkrtl.compile sml, sml^suffix)
    fun compile' sml = translateMain'(Linkrtl.compile sml, sml^suffix)
    fun test sml     = translateMain(Linkrtl.test sml, sml^suffix)
    fun test' sml    = translateMain'(Linkrtl.test sml, sml^suffix)
    fun print sml    = Linkrtl.Pprtl.pp_Module(Linkrtl.compile sml)
  end

  fun std_prelude() = compile_prelude "Preludes/Prelude.sml"

  fun no_prelude() = compile_prelude "empty.sml"

end

