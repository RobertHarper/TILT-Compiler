structure Render : RENDER = 
  struct

    open Base
    open Intersect
    open Vect
    exception Error of string

    val chat = ref 0	
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
	let val (t,name,(hit,l2,l3)) = (case obj of
				  Sphere (name, m4,t) => (t,name,sphere(m4,src,dir))
				| Plane (name, m4,t) => (t,name,plane(m4,src,dir))
				| Cube (name, m4,t) => (t,name,cube(m4,src,dir))
				| Cone (name, m4,t) => (t,name,cone(m4,src,dir))
				| Cylinder (name, m4,t) => (t,name,cylinder(m4,src,dir))
				| _ => raise (Error "primIntersect for non-primitive object"))
	in  if hit
		then map (fn info => (t, name, info)) (l3 ())
	    else []
	end

    fun primHit src dir obj : bool = 
	#1 (case obj of
		Sphere (_, m4, _) => sphere(m4,src,dir)
	      | Plane (name, m4, _) => plane(m4,src,dir)
	      | Cube (name, m4, _) => cube(m4,src,dir)
	      | Cone (name, m4, _) => cone(m4,src,dir)
	      | Cylinder (name, m4, _) => cylinder(m4,src,dir)
	      | _ => raise (Error "primIntersect for non-primitive object"))

    fun getIntersects viewerPos dir scene = 
	let val intersects = foldl (fn (obj,acc) => (primIntersect viewerPos dir obj) @ acc) [] scene
	    fun greater ((_,_,{dist=d1,...}:l3info),
			 (_,_,{dist=d2,...}:l3info)) = d1 > d2
	    val intersects = ListMergeSort.sort greater intersects
	in intersects
	end

    fun shadowed (hit, dir, []) = false
      | shadowed (hit, dir, obj::rest) = (primHit hit dir obj) orelse (shadowed (hit, dir, rest))

    (* Diffuse intensity and Specular intensity *)
    fun castShadow (hit, scene, incident, N, n) light : v3 * v3 = 
	let val hit = add(hit, scale(1e~10, incident))
	    val (Lj, Ldist) = Light.toLight (hit, light)  
	    val intersects = getIntersects hit Lj scene
	    val shadowed = (case intersects of
				[] => false
			      | (_,_,{hit,dist,u,v,face,N})::_ => dist < Ldist)
	in  if shadowed
		then (black, black)
	    else let 
		     val Ij = Light.illuminate(light, hit, Lj)
		     val Hj = halfway(Lj, incident)
		     val diffuse = scale(dp(N,Lj), Ij)
		     val spec = scale(Math.pow(dp(N,Hj),n), Ij)
(*
		     val _ = (print "castShadow can find light    N = "; printV3 N; print "  Lj = "; printV3 Lj; print "\n")
		     val _ = (print "           Ij = "; printV3 Ij; print "  diffuse = "; printV3 diffuse; print "\n")
*)
		 in  (diffuse, spec)
		 end
	end

    fun cast (apply, Ia, viewerPos, dir, scene, lights, 0) : color =  black
      | cast (apply, Ia, viewerPos, dir, scene, lights, depth) : color =  
	let 
	    val _ = if (!chat >= 1)
			then say "." else ()
	    val _ = if (!chat >= 2)
			then (print "Casting in direction "; printV3 dir; print "\n") 
		    else ()
	    val intersects = getIntersects viewerPos dir scene
	    val _ = if (!chat >= 2)
			then for (0, (length intersects),
				  fn i => let val intersect = List.nth(intersects, i)
					      val (_,name,{hit, dist, u,v,face,N}) = intersect
					  in   (print "Intersect #"; print (Int.toString i);
						print " with face "; print (Int.toString face);
						print " at distance "; printR dist;
						print " and intersection "; printV3 hit; print "\n")
					  end)
		    else ()
	in  if (null intersects)
		then black
	    else let val (t, name, {u,v,face,N,hit,dist}) = hd intersects
		     (* Surface properties *)
		     val (C : Base.color, kd, ks, n) = apply(t,face,u,v)
		     (* Direct contribution of light sources *)
		     val (diffuses, speculars) = ListPair.unzip (map (castShadow (hit,scene,negate dir,N,n)) lights)
		     val diffuse = foldl add black diffuses
		     val finalDiffuse = mult(scale(kd, diffuse),C)
		     val specular = foldl add black speculars 
		     (* Recursive reflection *)
		     val incident = negate dir
		     val S = reverseHalfway (incident, N)
		     val Is = cast(apply, Ia, hit, S, scene, lights, depth - 1)
		     (* Combine terms *)
		     val finalIntensity = add(mult(scale(kd, Ia), C),
					      add(finalDiffuse, 
						  add(mult(scale(ks,specular),C),
						      mult(scale(ks, Is), C))))
(*
		     val _ = (print "finalDiffuse is "; printV3 finalDiffuse; print "\n")
		     val _ = (print "finalIntensity is "; printV3 finalIntensity; print "\n")
*)
		 in  finalIntensity
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
	    val upperLeftY = height / 2.0

	    val image = Ppm.ppm (vres, hres)
	    val viewPos = (0.0, 0.0, ~1.0)   (* Viewer position *)

	    val scene = obj2scene scene
	    val _ = for(0, vres, fn row => 
			(say "\nRendering row "; say (Int.toString row); say ":  ";
			 for (0, hres, fn col => 
			      let val toScreen = (upperLeftX + (i2r col + 0.5) * pixelSize,
						  upperLeftY - (i2r row + 0.5) * pixelSize, 1.0)
				  val dir = normalize toScreen
				  val _ = if (col mod 10 = 0) then say "!" else ()
				  val _ = if (!chat >= 1)
					      then (print "\nDrawing pixel: "; printV3 toScreen; print "\n") 
					  else ()
				  val color = cast (apply, amb, viewPos, dir, scene, lights, depth)
			      in  Ppm.pxl(col,row,Ppm.colortorgb color, image)
			      end)))

	in  image
	end

  end

