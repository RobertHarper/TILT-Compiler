(*$import TopLevel Alpha32Instr AlphaMLRISCConstant AlphaMLRISCRegion AlphaMLRISCPseudo FlorGraph Alpha32Props AlphaPseudoInstr Alpha32Rewrite Alpha32AsmEmitter AsmEmit AlphaIntegerAllocation AlphaFloatAllocation Alpha32 *)

(* =========================================================================
 * AlphaMLTreeComp.sml
 * ========================================================================= *)

local

  (* -- structures --------------------------------------------------------- *)

  structure Alpha32Instr =
    Alpha32Instr(structure Const  = AlphaMLRISCConstant
		 structure Region = AlphaMLRISCRegion)

  (* -- structures --------------------------------------------------------- *)

  structure AlphaFlowGraph =
    FlowGraph(structure I = Alpha32Instr
	      structure P = AlphaMLRISCPseudo)

  structure Alpha32Props =
    Alpha32Props(val exnptrR		= [] (* ??? *)
		 structure Alpha32Instr = Alpha32Instr)

  structure AlphaPseudoInstr =
    AlphaPseudoInstr(structure Alpha32Instr = Alpha32Instr)

  structure Alpha32Rewrite =
    Alpha32Rewrite(Alpha32Instr)

  (* -- structures --------------------------------------------------------- *)

  structure Alpha32AsmEmitter =
    Alpha32AsmEmitter(structure Instr	  = Alpha32Instr
		      structure FlowGraph = AlphaFlowGraph)

  (* -- structures --------------------------------------------------------- *)

  structure AlphaAsmEmit =
    AsmEmit(structure F = AlphaFlowGraph
	    structure E = Alpha32AsmEmitter)

  structure Alpha32RegAlloc =
    Alpha32RegAlloc(structure P	  = Alpha32Props
		    structure I	  = Alpha32Instr
		    structure F	  = AlphaFlowGraph
		    structure Asm = Alpha32AsmEmitter)

  (* -- structures --------------------------------------------------------- *)

  structure AlphaIntegerAllocation =
    AlphaIntegerAllocation(structure AlphaInstructions = Alpha32Instr
			   structure AlphaRewrite      = Alpha32Rewrite
			   structure Cells	       = Alpha32Cells
			   structure FlowGraph	       = AlphaFlowGraph
			   structure IntegerConvention = AlphaIntegerConvention
			   structure MLRISCRegion      = AlphaMLRISCRegion
			   structure RegisterSpillMap  = AlphaRegisterSpillMap
			   functor RegisterAllocation  = Alpha32RegAlloc.IntRa)

  structure AlphaFloatAllocation =
    AlphaFloatAllocation(structure AlphaInstructions = Alpha32Instr
			 structure AlphaRewrite	     = Alpha32Rewrite
			 structure Cells	     = Alpha32Cells
			 structure FloatConvention   = AlphaFloatConvention
			 structure FlowGraph	     = AlphaFlowGraph
			 structure IntegerConvention = AlphaIntegerConvention
			 structure MLRISCRegion	     = AlphaMLRISCRegion
			 structure RegisterSpillMap  = AlphaRegisterSpillMap
			 functor RegisterAllocation  = Alpha32RegAlloc.FloatRa)

  (* -- values ------------------------------------------------------------- *)

  val alpha_codegen = AlphaAsmEmit.asmEmit o
		      AlphaFloatAllocation.allocateCluster o
		      AlphaIntegerAllocation.allocateCluster

  (* -- structures --------------------------------------------------------- *)

  structure AlphaFlowGraphGen =
    FlowGraphGen(val codegen	     = alpha_codegen
		 structure Flowgraph = AlphaFlowGraph
		 structure InsnProps = Alpha32Props
		 structure MLTree    = AlphaMLTree)

in

  (* -- structures --------------------------------------------------------- *)

  structure AlphaMLTreeComp =
    Alpha32(structure Flowgen	    = AlphaFlowGraphGen
	    structure Alpha32MLTree = AlphaMLTree
	    structure Alpha32Instr  = Alpha32Instr
	    structure PseudoInstrs  = AlphaPseudoInstr)

  structure AlphaIntegerAllocation = AlphaIntegerAllocation
  structure AlphaFloatAllocation   = AlphaFloatAllocation

end

