signature RENDER = 
  sig

    val render : (* closure, face, u, v -> surface property *)
	         (Eval.closure * real * real * real -> Eval.color * real * real * real) *
	         {amb    : Eval.color,
		  lights : Eval.light list,
		  scene  : Eval.obj,
		  depth  : int,
		  hfov   : real,
		  hres   : int,
		  vres   : int} -> Ppm.ppm
  end
