(*$import Nil *)
signature OPTIMIZE = 
  sig
      val debug : bool ref
      val optimize : Nil.module -> Nil.module
  end