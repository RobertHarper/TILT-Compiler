signature RENDER = 
  sig

    val render : (* closure, face, u, v -> surface property *)
	         (Base.closure * int * real * real -> Base.color * real * real * real) ->
	         {amb    : Base.color,
		  lights : Base.light list,
		  scene  : Base.obj,
		  depth  : int,
		  hfov   : real,
		  hres   : int,
		  vres   : int} -> Ppm.ppm
  end
