structure Render = 
  struct

    open Eval
    open Intersect
    open Vect
    exception Error of string
	
    val i2r = Real.fromInt
    val black = (0.0, 0.0, 0.0)
    local val conv = Math.pi / 180.0
    in    fun deg2rad d = conv * d
    end

    fun for(start,stop,f) = 
	let fun loop cur = if (cur >= stop) 
			       then ()
			   else (f cur; loop (cur+1))
	in  loop start
	end
    fun add2o f obj (x : real, y : real) = 
	let val (x2,y2) = f obj
	in  (x+x2, y+y2)
	end

    type scene = Eval.obj list   (* Our notion of a scene is a union of objects represented by a list *)

    fun obj2scene (obj : Eval.obj) : scene = (* Reduce union(s) to list of primitive objects *)
	(case obj of
	     Sphere _ => [obj]
	   | Plane _ => [obj]
	   | Cube _ => [obj]
	   | Cone _ => [obj]
	   | Cylinder _ => [obj]
	   | Union (obj1, obj2) => (obj2scene obj1) @ (obj2scene obj2)
	   | _ => raise (Error "Difference and Intersection not implemented"))

    fun primIntersect src dir obj = 
	let val (t,result) = (case obj of
				  Sphere (m4, t) => (t,sphere(m4,src,dir))
				| Plane (m4, t) => (t,plane(m4,src,dir))
				| Cube (m4, t) => (t,cylinder(m4,src,dir))
				| Cone (m4, t) => (t,cube(m4,src,dir))
				| Cylinder (m4, t) => (t,cone(m4,src,dir))
				| _ => raise (Error "primIntersect for non-primitive object"))
	in  case result of
	      ZERO => []
	    | ONE r => [(t,r)]
	    | TWO (r1,r2) => [(t,r1),(t,r2)]
	end

    fun primHit src dir obj : bool = 
	(case obj of
	     Sphere (m4, _) => hits_sphere(m4,src,dir)
	   | Plane (m4, _) => hits_plane(m4,src,dir)
	   | Cube (m4, _) => hits_cylinder(m4,src,dir)
	   | Cone (m4, _) => hits_cube(m4,src,dir)
	   | Cylinder (m4, _) => hits_cone(m4,src,dir)
	   | _ => raise (Error "primIntersect for non-primitive object"))

    fun shadowed (hit, dir, []) = false
      | shadowed (hit, dir, obj::rest) = (primHit hit dir obj) orelse (shadowed (hit, dir, rest))

    (* Diffuse intensity and Specular intensity *)
    fun castShadow (hit, scene, incident, N, n) light : v3 * v3 = 
	let val Lj = Light.toLight (hit, light)  
	in  if (shadowed(hit, Lj, scene))
		then (black, black)
	    else let val Ij = Light.illuminate(light, hit, Lj)
		     val Hj = halfway(Lj, incident)
		 in  (scale(dp(N,Lj), Ij), scale(Math.pow(dp(N,Hj),n), Ij))
		 end
	end

    fun cast (apply, Ia, viewerPos, dir, scene, lights, 0) : color =  black
      | cast (apply, Ia, viewerPos, dir, scene, lights, depth) : color =  
	let val intersects = foldl (fn (obj,acc) => (primIntersect viewerPos dir obj) @ acc) [] scene
	    fun greater ((_,{dist=d1,...}:ans),
			 (_,{dist=d2,...}:ans)) = d1 > d2
	    val intersects = ListMergeSort.sort greater intersects
	in  if (null intersects)
		then black
	    else let val (t, {u,v,face,N,S,theta,hit,dist}) = hd intersects
		     (* Surface properties *)
		     val (C, kd, ks, n) = apply(t,(face,u,v))
		     (* Recursive reflection *)
		     val Is = cast(apply, Ia, hit, S, scene, lights, depth - 1)
		     (* Direct contribution of light sources *)
		     val (diffuses, speculars) = ListPair.unzip (map (castShadow (hit,scene,negate dir,N,n)) lights)
		     val diffuse = foldl add black diffuses
		     val specular = foldl add black speculars 
		     (* Combine terms *)
		 in  add(mult(scale(kd, Ia), C),
			 add(mult(scale(kd, diffuse),C),
			     add(mult(scale(ks,specular),C),
				 mult(scale(ks, Is), C))))
		 end
	end


    fun render apply
	       {amb    : color,
		lights : light list,
		scene  : obj,
		depth  : int,
		hfov   : real,
		hres   : int,
		vres   : int} : Ppm.ppm = 

	let val hresR = i2r hres
	    val vresR = i2r vres
	    val width = 2.0 * Math.tan (deg2rad(0.5 * hfov))
	    val pixelSize = width / hresR
	    val upperLeftX = ~ width / 2.0
	    val height = pixelSize * vresR  (* Since pixels are squares *)
	    val upperLeftY = ~ height / 2.0

	    val image = Ppm.ppm (hres, vres)
	    val viewPos = (0.0, 0.0, ~1.0)   (* Viewer position *)

	    val scene = obj2scene scene
	    val _ = for(0, vres, fn row => 
			for (0, hres, fn col => 
			     let val dir = (upperLeftX + (i2r col + 0.5) * pixelSize,
					    upperLeftY - (i2r row + 0.5) * pixelSize, 1.0)
				 val dir = normalize dir
				 val color = cast (apply, amb, viewPos, dir, scene, lights, depth)
                             in  Ppm.pxl(col,row,Ppm.colortorgb color, image)
                             end))

	in  image
	end

  end

