(*$import Prelude Nil *)

signature SPECIALIZE =
  sig
      val debug : bool ref
      val optimize : Nil.module -> Nil.module
  end