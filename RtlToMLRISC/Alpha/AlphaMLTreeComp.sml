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
	      structure P = AlphaMLRISCPseudo
	      structure B = AlphaMLRISCBlockname)

  structure Alpha32Shuffle = 
      Alpha32Shuffle(Alpha32Instr)

  structure Alpha32Props =
    Alpha32Props(val exnptrR		= [] (* ??? *)
		 structure Alpha32Instr = Alpha32Instr
		 structure Shuffle = Alpha32Shuffle)

  structure AlphaPseudoInstr =
    AlphaPseudoInstr(structure Alpha32Instr = Alpha32Instr
		     structure Shuffle = Alpha32Shuffle)

  structure Alpha32Rewrite =
    Alpha32Rewrite(Alpha32Instr)

  (* -- structures --------------------------------------------------------- *)

  structure Alpha32AsmEmitter =
    Alpha32AsmEmitter(structure Instr	  = Alpha32Instr
		      structure FlowGraph = AlphaFlowGraph
		      structure Shuffle = Alpha32Shuffle
		      structure PseudoOps = AlphaMLRISCPseudo)

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

  (* -- values and structures ---------------------------------------------- *)
(*
  val alpha_codegen = AlphaAsmEmit.asmEmit o
		      AlphaFloatAllocation.allocateCluster o
		      AlphaIntegerAllocation.allocateCluster
*)
  fun alpha_codegen arg = 
      let val _ = print "Perry: Alpha/AlphMLTreeComp.sml alpha_codegen start\n"
	  val tmp = AlphaIntegerAllocation.allocateCluster arg
	  val _ = print "Perry: Alpha/AlphMLTreeComp.sml alpha_codegen 1\n"
	  val tmp = AlphaFloatAllocation.allocateCluster tmp
	  val _ = print "Perry: Alpha/AlphMLTreeComp.sml alpha_codegen 2\n"
	  val res = AlphaAsmEmit.asmEmit tmp
	  val _ = print "Perry: Alpha/AlphMLTreeComp.sml alpha_codegen end\n"
      in  res
      end
      

  structure AlphaFlowGraphGen =
    FlowGraphGen(val optimize        = ref (NONE : (AlphaFlowGraph.cluster -> AlphaFlowGraph.cluster) option)
		 val output          = alpha_codegen
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

