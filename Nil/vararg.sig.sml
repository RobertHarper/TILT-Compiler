signature VARARG = 
  sig

      structure Nil : NIL

      val vararg : int -> Nil.function
      val onearg : int -> Nil.function

  end
