(*$import Nil *)

signature MEASURE = 
  sig 
    val con_size   : Nil.con  -> int
    val kind_size  : Nil.kind -> int
    val exp_size   : Nil.exp  -> int
    val measureMod : Nil.module -> Nil.module
  end