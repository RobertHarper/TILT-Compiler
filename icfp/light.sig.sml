signature LIGHT = 
  sig

      type v3 = Matrix.v3
      type light = Base.light

      (* position, light: unit vector from position to light and distance to light *)
      val toLight  : v3 * light -> v3 * real

      (*Light, position, direction: returns intensity *)
      val illuminate : light * v3 * v3 -> Base.color

  end
