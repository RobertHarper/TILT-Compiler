signature LILCLOSURE = 
  sig 
    val chatlev : int ref
    val debuglev : int ref
    val close_mod : Lil.module -> Lil.module
  end
  