(*$import TopLevel SparcInstr SparcMLRISCConstant SparcMLRISCRegion SparcMLRISCPseudo FlorGraph SparcProps SparcPseudoInstr SparcRewrite SparcAsmEmitter AsmEmit SparcIntegerAllocation SparcFloatAllocation Sparc *)

(* =========================================================================
 * SparcMLTreeComp.sml
 * ========================================================================= *)

local

  (* -- structures --------------------------------------------------------- *)

  structure SparcInstr =
    SparcInstr(structure Const  = SparcMLRISCConstant
		 structure Region = SparcMLRISCRegion)

  (* -- structures --------------------------------------------------------- *)

  structure SparcFlowGraph =
    FlowGraph(structure I = SparcInstr
	      structure P = SparcMLRISCPseudo
	      structure B = SparcMLRISCBlockname)

  structure SparcShuffle = 
      SparcShuffle(SparcInstr)

  structure SparcProps =
    SparcProps(val exnptrR		= [] (* ??? *)
	       structure SparcInstr = SparcInstr
	       structure Shuffle = SparcShuffle)

  structure SparcPseudoInstr =
    SparcPseudoInstr(structure SparcInstr = SparcInstr
		     structure Shuffle = SparcShuffle)

  structure SparcRewrite =
    SparcRewrite(SparcInstr)

  (* -- structures --------------------------------------------------------- *)

  structure SparcAsmEmitter =
    SparcAsmEmitter(structure Instr	  = SparcInstr
		    structure FlowGraph = SparcFlowGraph
		    structure Shuffle = SparcShuffle)

  (* -- structures --------------------------------------------------------- *)

  structure SparcAsmEmit =
    AsmEmit(structure F = SparcFlowGraph
	    structure E = SparcAsmEmitter)

  structure SparcRegAlloc =
    SparcRegAlloc(structure P	  = SparcProps
		    structure I	  = SparcInstr
		    structure F	  = SparcFlowGraph
		    structure Asm = SparcAsmEmitter)

  (* -- structures --------------------------------------------------------- *)

  structure SparcIntegerAllocation =
    SparcIntegerAllocation(structure SparcInstructions = SparcInstr
			   structure SparcRewrite      = SparcRewrite
			   structure Cells	       = SparcCells
			   structure FlowGraph	       = SparcFlowGraph
			   structure IntegerConvention = SparcIntegerConvention
			   structure MLRISCRegion      = SparcMLRISCRegion
			   structure RegisterSpillMap  = SparcRegisterSpillMap
			   functor RegisterAllocation  = SparcRegAlloc.IntRa)

  structure SparcFloatAllocation =
    SparcFloatAllocation(structure SparcInstructions = SparcInstr
			 structure SparcRewrite	     = SparcRewrite
			 structure Cells	     = SparcCells
			 structure FloatConvention   = SparcFloatConvention
			 structure FlowGraph	     = SparcFlowGraph
			 structure IntegerConvention = SparcIntegerConvention
			 structure MLRISCRegion	     = SparcMLRISCRegion
			 structure RegisterSpillMap  = SparcRegisterSpillMap
			 functor RegisterAllocation  = SparcRegAlloc.FloatRa)

  (* -- values ------------------------------------------------------------- *)

  val sparc_codegen = SparcAsmEmit.asmEmit o
		      SparcFloatAllocation.allocateCluster o
		      SparcIntegerAllocation.allocateCluster

  (* -- structures --------------------------------------------------------- *)

  structure SparcFlowGraphGen =
    FlowGraphGen(val codegen	     = sparc_codegen
		 structure Flowgraph = SparcFlowGraph
		 structure InsnProps = SparcProps
		 structure MLTree    = SparcMLTree)

in

  (* -- structures --------------------------------------------------------- *)

  val _ = print "\n***Warning: overflowtrap in application of functor Sparc in file Sparc/SparcMLTreeComp.sml is empty***\n\n"
  structure SparcMLTreeComp =
    Sparc(structure Flowgen	    = SparcFlowGraphGen
	  structure SparcMLTree = SparcMLTree
	  structure SparcInstr  = SparcInstr
	  structure PseudoInstrs  = SparcPseudoInstr
	  val overflowtrap = [])

  structure SparcIntegerAllocation = SparcIntegerAllocation
  structure SparcFloatAllocation   = SparcFloatAllocation

end

