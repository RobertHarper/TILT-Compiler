(* =========================================================================
 * SparcRegisterSpillMap.sml
 * ========================================================================= *)

structure SparcRegisterSpillMap =
  RegisterSpillMap(structure MLRISCConstant = SparcMLRISCConstant)

