(*$import Prelude *)
(* $Id$ *)

signature NWIZ =
sig

  type exp_
      
  datatype CCconditionCode =
      CCExp  of exp_
    | CCAnd  of CCconditionCode * CCconditionCode
    | CCOr   of CCconditionCode * CCconditionCode
    | CCNot  of CCconditionCode 
end

structure EverythingWiz : NWIZ =
struct
    
  datatype Sswitch =
      SIfthenelse of {arg : CCconditionCode,             
                      thenArm : exp_,
                      elseArm : exp_}
  and Eexp =
      EVar
    | ESwitch of Sswitch
      
  and CCconditionCode =
      CCExp  of exp_
    | CCAnd  of CCconditionCode * CCconditionCode
    | CCOr   of CCconditionCode * CCconditionCode
    | CCNot  of CCconditionCode
      
  withtype exp_ = Eexp
end
