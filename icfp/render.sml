structure Render = 
  struct

    open Eval
    open Intersect
    exception Error of String
	
    val i2r = Real.fromInt
    val black = (0.0, 0.0, 0.0)
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
	(case scene of
	     Sphere _ => [scene]
	   | Plane _ => [scene]
	   | Cube _ => [scene]
	   | Cone _ => [scene]
	   | Cylinder _ => [scene]
	   | Union (obj1, obj2) => (obj2scene obj1) @(obj2scene obj2)
	   | _ => raise (Error "Difference and Intersection not implemented"))

    fun primIntersect src dir obj : result = 
	let val (t,result) = (case obj
				  Sphere (m4, t) => (t,sphere(m4,src,dir))
				| Plane (m4, t) => (t,plane(m4,src,dir))
				| Cube (m4, t) => (t,cylinder(m4,src,dir))
				| Cone (m4, t) => (t,cube(m4,src,dir))
				| Cylinder (m4, t) => (t,cone(m4,src,dir))
				| _ => raise (Error "primIntersect for non-primitive object"))
	in  case result of
	      ZERO => []
	    | ONE of r => [(t,r)]
	    | TWO of (r1,r2) => [(t,r1),(t,r2)]
	end

    fun primHit src dir obj : bool = 
	(case obj
	     Sphere (m4, _) => hit_sphere(m4,src,dir))
	   | Plane (m4, _) => hit_plane(m4,src,dir))
	   | Cube (m4, _) => hit_cylinder(m4,src,dir))
	   | Cone (m4, _) => hit_cube(m4,src,dir))
	   | Cylinder (m4, _) => hit_cone(m4,src,dir))
	   | _ => raise (Error "primIntersect for non-primitive object"))

    fun shadowed (hit, dir, []) = false
      | shadowed (hit, dir, obj::rest) = (primHit hit dir obj) orelse (shadowed (hit, dir, rest))

    (* Diffuse intensity and Specular intensity *)
    fun castShadow (hit, scene, incident) light = 
	let val Lj = toLight (hit, light)  
	in  if (shadowed(hit, Lj, scene))
		then 0.0
	    else let val Ij = Light.illuminate(light, hit, Lj)
		     val  Hj = halfway(Lj, incident)
		 in  (Ij * (dp(N,Lj)), Ij * (exp(dp(N,Hj))))
		 end
	end

    fun cast (Ia, viewerPos, direction, scene, 0) : color =  black
      | cast (Ia, viewerPos, direction, scene, depth) : color =  
	let val dir = normalize direction
	    val intersects = List.flatten (map (primIntersect viewerPos dir) scene)
	    fun greater (_,{dist=d1,...}:ans, 
			 _,{dist=d2,...}:ans) = Real.compare(d1,d2)
	    val intersects = sort greater intersects
	in  if (null intersects)
		then black
	    else let val (t, {u,v,N,S,theta,hit,dist}) = hd intersects
		     (* Surface properties *)
		     val (C, kd, ks, n) = t(u,v)
		     (* Recursive reflection *)
		     val recusiveColor = cast(Ia, hit, S, scene, depth - 1)
		     val Is = magnitude recursiveColor
		     (* Direct contribution of light sources *)
		     val (diffuseIntensity,specularIntensity) = foldl (add2o (castShadow (hit,scene,reverse direction)) (0.0,0.0) lights
		     (* Combine overall intensity *)
		     val intensity = kd * Ia + kd * diffuseIntensity + ks * specularIntensity + ks * Is
		 in  scale(intensity, C)
		 end
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

	    val scene = obj2scene scene
	    val _ = for(0, height, fn row => 
			for (0, width, fn col => 
			     let val dir = (upperLeftX + (i2r col + 0.5) * pixelSize,
					    upperLeftY - (i2r row + 0.5) * pixelSize, 1.0)
                             in  cast (amb, viewPos, dir, scene, depth)
                             end))

	in  (hres, vres, image)
	end
    end
