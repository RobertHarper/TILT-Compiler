signature RENDER = 
  sig
    val render : {amb    : Eval.color,
		  lights : Eval.light vector,
		  scene  : Eval.object,
		  depth  : int,
		  hfov   : real,
		  width  : int,
		  height : int} -> ??.bmp
  end