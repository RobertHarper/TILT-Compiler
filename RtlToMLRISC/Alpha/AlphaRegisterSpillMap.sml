(*$import RegisterSpillMap AlphaMLRISCConstant *)


(* =========================================================================
 * AlphaRegisterSpillMap.sml
 * ========================================================================= *)

structure AlphaRegisterSpillMap =
  RegisterSpillMap(structure MLRISCConstant = AlphaMLRISCConstant)

