signature NILTOLIL = 
  sig
    val chatlev : int ref
    val debuglev : int ref
    val niltolil : Nil.module -> Lil.module
  end