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

