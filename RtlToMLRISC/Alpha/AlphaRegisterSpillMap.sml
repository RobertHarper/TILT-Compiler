
(* =========================================================================
 * AlphaRegisterSpillMap.sml
 * ========================================================================= *)

structure AlphaRegisterSpillMap =
  RegisterSpillMap(structure MLRISCConstant = AlphaMLRISCConstant)

