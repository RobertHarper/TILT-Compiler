signature NILTOLIL = 
  sig
    val chatlev : int ref
    val debuglev : int ref
    val niltolil : string -> Nil.module -> Lil.module
    val niltolil_int : string -> Nil.interface -> Lil.interface
  end