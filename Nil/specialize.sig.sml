(*$import NIL *)
signature SPECIALIZE =
  sig
      val debug : bool ref
      val optimize : Nil.module -> Nil.module
  end