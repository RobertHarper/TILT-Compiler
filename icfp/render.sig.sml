signature RENDER = 
  sig
    val render : {amb    : Eval.color,
		  lights : Eval.light vector,
		  scene  : Eval.obj,
		  depth  : int,
		  hfov   : real,
		  width  : int,
		  height : int} -> Ppm.ppm
  end