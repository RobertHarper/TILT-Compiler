signature LIGHT = 
  sig

    (*Light, position, direction, color result
     *)
    val illuminate : Eval.light * Matrix.v3 * Matrix.v3 -> Eval.color
  end