(* =========================================================================
 * AlphaCallConvBasis.sml
 * ========================================================================= *)

structure AlphaCallConventionBasis =
  CallConventionBasis(
    structure Cells		= Alpha32Cells
    structure IntegerConvention = AlphaIntegerConvention
    structure MLRISCRegion	= AlphaMLRISCRegion
    structure MLTreeExtra	= AlphaMLTreeExtra
    structure StackFrame	= AlphaStandardFrame
  )

