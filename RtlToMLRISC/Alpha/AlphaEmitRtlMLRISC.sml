(*$import TopLevel BasicBlock AlphaMLRISCPseudo AlphaMLTreeExtra IntegerDataFlow LittleEndianTraceTable SpillReload AlphaFloatConvention AlphaIntegerConvention IntegerLiveness EmitRtlMLRISC Alpha32Cells AlphaCallConventionBasis AlphaFloatAllocation AlphaIntegerAllocation AlphaMLRISCConstant AlphaMLRISCPseudo AlphaMLRISCRegion AlphaMLTreeComp AlphaRegisterSpillMap AlphaStandardFrame *)

(* =========================================================================
 * AlphaEmitRtlMLRISC.sml
 * ========================================================================= *)

local

  (* -- structures --------------------------------------------------------- *)

  structure AlphaBasicBlock =
    BasicBlock(
      structure MLRISCPseudo = AlphaMLRISCPseudo
      structure MLTreeExtra  = AlphaMLTreeExtra
    )

  structure AlphaIntegerDataFlow =
    IntegerDataFlow(
      structure MLTreeExtra = AlphaMLTreeExtra
    )

  structure AlphaRtlRegisterTraceMap =
    RtlRegisterTraceMap(
      structure Cells	   = Alpha32Cells
      structure TraceTable = LittleEndianTraceTable
    )

  structure AlphaSpillReload =
    SpillReload(
      structure MLTreeExtra = AlphaMLTreeExtra
    )

  (* -- structures --------------------------------------------------------- *)

  structure AlphaExternalConvention =
    AlphaStandardConvention(
      structure FloatConvention	  = AlphaFloatConvention
      structure IntegerConvention = AlphaIntegerConvention
    )

  structure AlphaIntegerLiveness =
    IntegerLiveness(
      structure BasicBlock	= AlphaBasicBlock
      structure IntegerDataFlow = AlphaIntegerDataFlow
      structure MLTreeExtra	= AlphaMLTreeExtra
    )

in

  (* -- structures --------------------------------------------------------- *)

  structure AlphaEmitRtlMLRISC =
    EmitRtlMLRISC(
      structure BasicBlock	    = AlphaBasicBlock
      structure CallConventionBasis = AlphaCallConventionBasis
      structure Cells		    = Alpha32Cells
      structure ExternalConvention  = AlphaExternalConvention
      structure FloatAllocation	    = AlphaFloatAllocation
      structure FloatConvention	    = AlphaFloatConvention
      structure IntegerAllocation   = AlphaIntegerAllocation
      structure IntegerConvention   = AlphaIntegerConvention
      structure IntegerLiveness	    = AlphaIntegerLiveness
      structure MLRISCConstant	    = AlphaMLRISCConstant
      structure MLRISCPseudo	    = AlphaMLRISCPseudo
      structure MLRISCRegion	    = AlphaMLRISCRegion
      structure MLTreeComp	    = AlphaMLTreeComp
      structure MLTreeExtra	    = AlphaMLTreeExtra
      structure RegisterSpillMap    = AlphaRegisterSpillMap
      structure RegisterTraceMap    = AlphaRtlRegisterTraceMap
      structure SpillReload	    = AlphaSpillReload
      structure StackFrame	    = AlphaStandardFrame
      structure TraceTable	    = LittleEndianTraceTable
    )

end

