signature LILTOTALEXP = 
  sig
    val chatlev : int ref
    val debuglev : int ref
    val modtrans : Lil.module -> (string * Tal.tal_int * Tal.tal_imp * Tal.tal_int)
    val inttrans : Lil.interface -> (string * Tal.tal_int)
  end