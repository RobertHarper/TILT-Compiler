(*$import CallConventionBasis Sparc32Cells SparcIntegerConvention SparcMLRISCRegion SparcMLTreeExtra SparcStandardFrame *)


(* =========================================================================
 * SparcCallConvBasis.sml
 * ========================================================================= *)

structure SparcCallConventionBasis =
  CallConventionBasis(
    structure Cells		= SparcCells
    structure IntegerConvention = SparcIntegerConvention
    structure MLRISCRegion	= SparcMLRISCRegion
    structure MLTreeExtra	= SparcMLTreeExtra
    structure StackFrame	= SparcStandardFrame
  )

