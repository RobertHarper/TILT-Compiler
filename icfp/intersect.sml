structure Intersect : INTERSECT = 
  struct
    type m4 = Matrix.m4
    type v4 = Matrix.v4
    type v3 = Matrix.v3

    open Vect

    type l1 = bool

    type l2 = {hit : v3,dist : real} list

    type l3info = {u:real,v:real,face:int, 
		   N : v3,         (*Normal vector*)
		   hit : v3,       (*Point of intersection in world coordinates *)
		   dist : real     (*Distance to viewer *)
		   } 
    type l3 = l3info list

    type result = l1 * (unit -> l2) * (unit -> l3)

    val invert    = Matrix.invert
    val apply     = Matrix.applyV3

    val dot       = Vect.dp 
    val distance  = Vect.distance
    val normalize = Vect.normalize

    fun no_l2 () : l2 = []
    fun no_l3 () : l3 = []
    val noIntersect = (false, no_l2, no_l3)
    fun memoize f = 
	let val result = ref NONE
	in  fn () => (case (!result) of
			  SOME r => r
			| NONE => let val r = f()
				      val _ = (result := (SOME r))
				  in r
				  end)
	end


    (* Where does the plane y=0 transformed by m4 intersect the vector originating from src0 in the direction of dir0 *)
    fun plane (m4, src0, dir0) = (* Plane is Ax + By + Cz = D *)
	let val newOrigin = v4tov3 (Matrix.apply(m4, (0.0, 0.0, 0.0, 1.0)))
	    val newNormalPoint = v4tov3 (Matrix.apply(m4, (0.0, 1.0, 0.0, 1.0)))
	    val N = makeDir(newOrigin, newNormalPoint)
	    val D = dp(N, newOrigin)
	    val numerator = D - dp(N, src0)
	    val denominator = dp(N, dir0)
(*
	    val _ = (print "\n--newOrigin = "; printV3 newOrigin;
		     print "\n  newNormalPoint= "; printV3 newNormalPoint;
		     print "\n  N = "; printV3 N;
		     print "\n  D = "; printR D;
		     print "  num = "; printR numerator;
		     print "  denom = "; printR denominator; print "\n")
*)
	in  if (Real.==(numerator, 0.0) orelse  (* point in plane *)
		Real.==(denominator, 0.0))      (* direction parallel to plane *)
		then noIntersect
	    else let val t = numerator / denominator 
(*
		     val _ = (print "  t = "; printR t; print "\n")
*)
		     fun l2() : l2 = let val dist = Real.abs t (* since dir0 is unit vector *)
					 val hit = add(src0, scale(t, dir0))
				     in  [{dist = dist, hit = hit}]
				     end
		     val l2 = memoize l2
		     fun l3() : l3 = let val [{hit, dist}] = l2()
				    val (u,_,v) = v4tov3(Matrix.apply(Matrix.invert m4, v3tov4 hit))
				    val tPos = t > 0.0
				    val dpNeg = dp(dir0, N) < 0.0
				    val N = if ((tPos andalso dpNeg) orelse
						(not tPos andalso not dpNeg))
						then N
					    else negate N
				in  [{u = u, v = v, face = 0, N = N,
				      hit = hit, dist = dist}]
				end
		     val l3 = memoize l3
		 in  if (t < 0.0)
			 then noIntersect
		     else (true, l2, l3)
		 end
	end

    val eps = 1.0e~10
    fun iszero r = (Real.abs r) < eps

    fun sphere (M: m4,orig:v3,dir:v3) : result = 
      let
	local
	  fun l2info p = 
	    let 
	      val hit = apply (M,p)
	      val dist = distance (orig,hit)
	    in {hit = hit,dist = dist}
	    end
	  
	  fun l3info ({hit,dist},(x,y,z)) = 
	    let
	      val v = y+1.0/2.0
	      val u = if iszero v orelse iszero (v-1.0) then 0.0  (* What to do here?*)
		      else (Math.atan2(x,z)) / (2.0 * Math.pi)     (*180/pi * (atan (x/z))/360 *)
	      val face = 0
	      val N = normalize hit
	    in {u = u,v = v,face = face,
		hit = hit,
		dist = dist,
		N = N}
	    end
	  
	  fun level2 (l1,ps) () = 
	    (case !l1
	       of [] => (l1 := (map l2info ps);
			 !l1)
		| is => is)
	       
	  fun level3 (level2,ps) () = ListPair.map l3info (level2 (),ps)
	in
	  fun NO () = (false,fn () => [],fn () => [])
	    
	  fun YES ps = 
	    let
	      val get2 = level2 (ref [],ps)
	      val get3 = level3 (get2,ps)
	    in (true,get2,get3)
	    end
	end

	val M' = invert M
	val orig_oc as (x0,y0,z0) = apply (M',orig)
	val dir_oc  as (xd,yd,zd) = apply (M',dir)

	fun intersect t = (x0 + t*xd, 
			   y0 + t*yd,
			   z0 + t*zd)

	val a = dot (dir_oc,dir_oc)                (*dx*dx + dy*dy + dz*dz*)              (*If unit length, this could be 1*)
	val b = 2.0 * (dot (orig_oc,dir_oc))       (*(x0*dx + y0*dy + z0*dz)*)
	val c = dot (orig_oc,orig_oc) - 1.0        (*x0*x0 + y0*y0 + z0*z0 - 1.0*)
	val disc = b*b - 4.0*a* c
      in
	if iszero disc then 
	  let val t = ~b / (2.0*a)
	  in if t < 0.0 then NO ()
	     else YES [intersect t]
	  end
	else if disc < 0.0 then NO ()
	else 
	  let
	    val d = Math.sqrt disc
	    val t0 = (~b + d) / (2.0*a)
	    val t1 = (~b - d) / (2.0*a)
	  in
	    if t0 < 0.0 then NO ()     (* t0 is always larger then t1*)
	    else if t1 < 0.0 then YES [intersect t0]
	    else YES [intersect t1,intersect t0]
	  end
      end

    fun dummy _ : result = raise Div

    val cylinder : m4 * v3 * v3 -> result = dummy
    val cube     : m4 * v3 * v3 -> result = dummy
    val cone     : m4 * v3 * v3 -> result = dummy

  end
