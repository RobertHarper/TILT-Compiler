structure Render : RENDER = 
  struct

    open Base
    open Intersect
    open Vect
    exception Error of string
	
    val i2r = Real.fromInt
    val black = (0.0, 0.0, 0.0)

    fun add2o f obj (x : real, y : real) = 
	let val (x2,y2) = f obj
	in  (x+x2, y+y2)
	end
    fun say s = (print s; TextIO.flushOut TextIO.stdOut)

    type scene = obj list   (* Our notion of a scene is a union of objects represented by a list *)

    fun obj2scene (obj : obj) : scene = (* Reduce union(s) to list of primitive objects *)
	(case obj of
	     Sphere _ => [obj]
	   | Plane _ => [obj]
	   | Cube _ => [obj]
	   | Cone _ => [obj]
	   | Cylinder _ => [obj]
	   | Union (obj1, obj2) => (obj2scene obj1) @ (obj2scene obj2)
	   | _ => raise (Error "Difference and Intersection not implemented"))

    fun primIntersect src dir obj = 
	let val (t,(hit,l2,l3)) = (case obj of
				  Sphere (m4, t) => (t,sphere(m4,src,dir))
				| Plane (m4, t) => (t,plane(m4,src,dir))
				| Cube (m4, t) => (t,cylinder(m4,src,dir))
				| Cone (m4, t) => (t,cube(m4,src,dir))
				| Cylinder (m4, t) => (t,cone(m4,src,dir))
				| _ => raise (Error "primIntersect for non-primitive object"))
	in  if hit
		then map (fn info => (t, info)) (l3 ())
	    else []
	end

    fun primHit src dir obj : bool = 
	#1 (case obj of
		Sphere (m4, _) => sphere(m4,src,dir)
	      | Plane (m4, _) => plane(m4,src,dir)
	      | Cube (m4, _) => cylinder(m4,src,dir)
	      | Cone (m4, _) => cube(m4,src,dir)
	      | Cylinder (m4, _) => cone(m4,src,dir)
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
	let 
(*	    val _ = say "." *)
	    val intersects = foldl (fn (obj,acc) => (primIntersect viewerPos dir obj) @ acc) [] scene
	    fun greater ((_,{dist=d1,...}:l3info),
			 (_,{dist=d2,...}:l3info)) = d1 > d2
	    val intersects = ListMergeSort.sort greater intersects
	in  if (null intersects)
		then black
	    else let val (t, {u,v,face,N,hit,dist}) = hd intersects
		     (* Surface properties *)
		     val (C : Base.color, kd, ks, n) = apply(t,face,u,v)
		     (* Recursive reflection *)
		     val incident = negate dir
		     val S = reverseHalfway (incident, N)
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

	    val image = Ppm.ppm (vres, hres)
	    val viewPos = (0.0, 0.0, ~1.0)   (* Viewer position *)

	    val scene = obj2scene scene
	    val _ = for(0, vres, fn row => 
			(say "\nRendering row "; say (Int.toString row); say ":  ";
			 for (0, hres, fn col => 
			      let val dir = (upperLeftX + (i2r col + 0.5) * pixelSize,
					     upperLeftY - (i2r row + 0.5) * pixelSize, 1.0)
				  val dir = normalize dir
				  val _ = if (col mod 10 = 0) then say "!" else ()
				  val color = cast (apply, amb, viewPos, dir, scene, lights, depth)
			      in  Ppm.pxl(col,row,Ppm.colortorgb color, image)
			      end)))

	in  image
	end

  end

