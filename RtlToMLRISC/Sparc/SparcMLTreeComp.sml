(*$import TopLevel Sparc32Instr SparcMLRISCConstant SparcMLRISCRegion SparcMLRISCPseudo FlorGraph Sparc32Props SparcPseudoInstr Sparc32Rewrite Sparc32AsmEmitter AsmEmit SparcIntegerAllocation SparcFloatAllocation Sparc32 *)

(* =========================================================================
 * SparcMLTreeComp.sml
 * ========================================================================= *)

local

  (* -- structures --------------------------------------------------------- *)

  structure Sparc32Instr =
    Sparc32Instr(structure Const  = SparcMLRISCConstant
		 structure Region = SparcMLRISCRegion)

  (* -- structures --------------------------------------------------------- *)

  structure SparcFlowGraph =
    FlowGraph(structure I = Sparc32Instr
	      structure P = SparcMLRISCPseudo)

  structure Sparc32Props =
    Sparc32Props(val exnptrR		= [] (* ??? *)
		 structure Sparc32Instr = Sparc32Instr)

  structure SparcPseudoInstr =
    SparcPseudoInstr(structure Sparc32Instr = Sparc32Instr)

  structure Sparc32Rewrite =
    Sparc32Rewrite(Sparc32Instr)

  (* -- structures --------------------------------------------------------- *)

  structure Sparc32AsmEmitter =
    Sparc32AsmEmitter(structure Instr	  = Sparc32Instr
		      structure FlowGraph = SparcFlowGraph)

  (* -- structures --------------------------------------------------------- *)

  structure SparcAsmEmit =
    AsmEmit(structure F = SparcFlowGraph
	    structure E = Sparc32AsmEmitter)

  structure Sparc32RegAlloc =
    Sparc32RegAlloc(structure P	  = Sparc32Props
		    structure I	  = Sparc32Instr
		    structure F	  = SparcFlowGraph
		    structure Asm = Sparc32AsmEmitter)

  (* -- structures --------------------------------------------------------- *)

  structure SparcIntegerAllocation =
    SparcIntegerAllocation(structure SparcInstructions = Sparc32Instr
			   structure SparcRewrite      = Sparc32Rewrite
			   structure Cells	       = Sparc32Cells
			   structure FlowGraph	       = SparcFlowGraph
			   structure IntegerConvention = SparcIntegerConvention
			   structure MLRISCRegion      = SparcMLRISCRegion
			   structure RegisterSpillMap  = SparcRegisterSpillMap
			   functor RegisterAllocation  = Sparc32RegAlloc.IntRa)

  structure SparcFloatAllocation =
    SparcFloatAllocation(structure SparcInstructions = Sparc32Instr
			 structure SparcRewrite	     = Sparc32Rewrite
			 structure Cells	     = Sparc32Cells
			 structure FloatConvention   = SparcFloatConvention
			 structure FlowGraph	     = SparcFlowGraph
			 structure IntegerConvention = SparcIntegerConvention
			 structure MLRISCRegion	     = SparcMLRISCRegion
			 structure RegisterSpillMap  = SparcRegisterSpillMap
			 functor RegisterAllocation  = Sparc32RegAlloc.FloatRa)

  (* -- values ------------------------------------------------------------- *)

  val alpha_codegen = SparcAsmEmit.asmEmit o
		      SparcFloatAllocation.allocateCluster o
		      SparcIntegerAllocation.allocateCluster

  (* -- structures --------------------------------------------------------- *)

  structure SparcFlowGraphGen =
    FlowGraphGen(val codegen	     = alpha_codegen
		 structure Flowgraph = SparcFlowGraph
		 structure InsnProps = Sparc32Props
		 structure MLTree    = SparcMLTree)

in

  (* -- structures --------------------------------------------------------- *)

  structure SparcMLTreeComp =
    Sparc32(structure Flowgen	    = SparcFlowGraphGen
	    structure Sparc32MLTree = SparcMLTree
	    structure Sparc32Instr  = Sparc32Instr
	    structure PseudoInstrs  = SparcPseudoInstr)

  structure SparcIntegerAllocation = SparcIntegerAllocation
  structure SparcFloatAllocation   = SparcFloatAllocation

end

