(*$import NIL *)
signature OPTIMIZE = 
  sig
      structure Nil : NIL
      val debug : bool ref
      val optimize : Nil.module -> Nil.module
  end