signature LIGHT = 
  sig

      type v3 = Matrix.v3
      type light = Eval.light

      (* position, light: unit vector from position to light *)
      val toLight  : v3 * light -> v3 

      (*Light, position, direction: returns intensity *)
      val illuminate : light * v3 * v3 -> real

  end
