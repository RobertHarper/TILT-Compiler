(*$import CallConventionBasis Sparc32Cells SparcIntegerConvention SparcMLRISCRegion SparcMLTreeExtra SparcStandardFrame *)


(* =========================================================================
 * SparcCallConventionBasis.sml
 * ========================================================================= *)

structure SparcCallConventionBasis =
  CallConventionBasis(
    structure Cells		= Sparc32Cells
    structure IntegerConvention = SparcIntegerConvention
    structure MLRISCRegion	= SparcMLRISCRegion
    structure MLTreeExtra	= SparcMLTreeExtra
    structure StackFrame	= SparcStandardFrame
  )

