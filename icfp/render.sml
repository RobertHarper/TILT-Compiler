structure Render = 
  struct

    open Eval
    open Intersect
    exception Error of String
	
    val i2r = Real.fromInt
    fun for(start,stop,f) = 
	let fun loop cur = if (cur >= stop) 
			       then ()
			   else (f cur; loop (cur+1))
	in  loop start
	end

    fun scene2primObjs (scene : Eval.obj) : Eval.obj list = (* Reduce union(s) to list of primitive objects *)
	(case scene of
	     Sphere _ => [scene]
	   | Plane _ => [scene]
	   | Cube _ => [scene]
	   | Cone _ => [scene]
	   | Cylinder _ => [scene]
	   | Union (obj1, obj2) => (scene2primobjs obj1) @ (scene2primobjs obj2)
	   | _ => raise (Error "Difference and Intersection not implemented"))

    fun cast (viewerPos, direction, scene, depth) : color =  
	let val primObjs = scene2primObjs scene
	in  raose (Error "Cast not done")
	end

    fun render {amb    : color,
		lights : light vector,
		scene  : object,
		depth  : int,
		hfov   : real,
		hres   : int,
		vres   : int} : Ppm.bmp = 
	let val hresR = i2r hres
	    val vresR = i2r vres
	    val width = 2.0 * tan (0.5 * hfov)
	    val pixelSize = width / hresR
	    val upperLeftX = ~ width / 2.0
	    val height = pixelSize * vresR  (* Since pixels are squares *)
	    val upperLeftY = ~ height / 2.0

	    val image = Array.array2 (hres, vres)
	    val viewPos = (0.0, 0.0, ~1.0)   (* Viewer position *)

	    val _ = for(0, height, fn row => 
			for (0, width, fn col => 
			     let val dir = (upperLeftX + (i2r col + 0.5) * pixelSize,
					    upperLeftY - (i2r row + 0.5) * pixelSize, 1.0)
                             in  cast (viewPos, dir, scene, depth)
                             end))

	in  (hres, vres, image)
	end
    end
