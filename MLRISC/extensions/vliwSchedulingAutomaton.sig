signature VLIW_SCHEDULING_AUTOMATON =
sig
   structure I : VLIW_INSTRUCTIONS
   structure FU : FUNITS
      sharing FU = I.FU

   type state
   type instrClass 

   exception Hazard

   val startState : state
   val go : state * instrClass -> state
   val instrToClass : I.instruction -> instrClass
   val alternatives : instrClass -> FU.fu list
   val mayConflict  : instrClass * instrClass -> bool
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:15:44  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:14  pscheng
# *** empty log message ***
#
 *)
