(*$import TopLevel BasicBlock SparcMLRISCPseudo SparcMLTreeExtra IntegerDataFlow LittleEndianTraceTable SpillReload SparcFloatConvention SparcIntegerConvention IntegerLiveness EmitRtlMLRISC SparcCells SparcCallConventionBasis SparcFloatAllocation SparcIntegerAllocation SparcMLRISCConstant SparcMLRISCPseudo SparcMLRISCRegion SparcMLTreeComp SparcRegisterSpillMap SparcStandardFrame *)

(* =========================================================================
 * SparcEmitRtlMLRISC.sml
 * ========================================================================= *)

local

  (* -- structures --------------------------------------------------------- *)

  structure SparcTraceTable =
    Tracetable(
      val little_endian = false
    )


  (* -- structures --------------------------------------------------------- *)

  structure SparcBasicBlock =
    BasicBlock(
      structure MLRISCPseudo = SparcMLRISCPseudo
      structure MLTreeExtra  = SparcMLTreeExtra
    )

  structure SparcIntegerDataFlow =
    IntegerDataFlow(
      structure MLTreeExtra = SparcMLTreeExtra
    )

  structure SparcRtlRegisterTraceMap =
    RtlRegisterTraceMap(
      structure Cells	   = SparcCells
      structure TraceTable = SparcTraceTable
    )

  structure SparcSpillReload =
    SpillReload(
      structure MLTreeExtra = SparcMLTreeExtra
    )

  (* -- structures --------------------------------------------------------- *)

  structure SparcExternalConvention =
    SparcStandardConvention(
      structure FloatConvention	  = SparcFloatConvention
      structure IntegerConvention = SparcIntegerConvention
    )

  structure SparcIntegerLiveness =
    IntegerLiveness(
      structure BasicBlock	= SparcBasicBlock
      structure IntegerDataFlow = SparcIntegerDataFlow
      structure MLTreeExtra	= SparcMLTreeExtra
    )

in

  (* -- structures --------------------------------------------------------- *)

  structure SparcEmitRtlMLRISC =
    EmitRtlMLRISC(
      structure BasicBlock	    = SparcBasicBlock
      structure CallConventionBasis = SparcCallConventionBasis
      structure Cells		    = SparcCells
      structure ExternalConvention  = SparcExternalConvention
      structure FloatAllocation	    = SparcFloatAllocation
      structure FloatConvention	    = SparcFloatConvention
      structure IntegerAllocation   = SparcIntegerAllocation
      structure IntegerConvention   = SparcIntegerConvention
      structure IntegerLiveness	    = SparcIntegerLiveness
      structure MLRISCConstant	    = SparcMLRISCConstant
      structure MLRISCPseudo	    = SparcMLRISCPseudo
      structure MLRISCRegion	    = SparcMLRISCRegion
      structure MLTreeComp	    = SparcMLTreeComp
      structure MLTreeExtra	    = SparcMLTreeExtra
      structure RegisterSpillMap    = SparcRegisterSpillMap
      structure RegisterTraceMap    = SparcRtlRegisterTraceMap
      structure SpillReload	    = SparcSpillReload
      structure StackFrame	    = SparcStandardFrame
      structure TraceTable	    = SparcTraceTable
    )

end

