(*$import *)

signature RTL =
sig
  datatype label = ML_EXTERN_LABEL
  datatype sregi = THREADPTR
  datatype regi = REGI
  datatype rep_path = Projvar_p
  datatype rep = COMPUTE
  datatype regf = REGF
  datatype reg = I
  datatype ea = REA
  datatype cmp = EQ
end

functor Bug (structure Rtl : RTL) =
struct
  fun cmpf2s (Rtl.EQ) = "eq"
  fun cmpi2s c signflag = (cmpf2s c)
end
