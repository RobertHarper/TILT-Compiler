type v3 = real * real * real

datatype result = ZERO | ONE of v3 | TWO of v3 * v3

fun dot (v1 as (x1,y1,z1) : v3, v2 as (x2,y2,z2)) = x1+x2 + y1*y2 + z1*z2

fun distance ((x1,y1,z1),(x2,y2,z2) : v3) = magnitude(x1-x2,y1-y2,z1-z2)

fun diff ((a,b,c),(a',b',c')) : v3 = (a-a',b-b',c-c')

   fun sphere (orig:v3,dir:v3) : result =
      let
	val p0 as (x0,y0,z0) = orig
	val p1 as (x1,y1,z1) = dir
	val pd as (dx,dy,dz) = diff (p1,p0)
	val a = dot (pd,pd)             (*dx*dx + dy*dy + dz*dz*)              (*If unit length, this could be 1*)
	val _ = print ("a is:"^(Real.toString a)^"\n")
	val b = 2.0 * (dot (p0,pd))       (*(x0*dx + y0*dy + z0*dz)*)
	val _ = print ("b is:"^(Real.toString b)^"\n")
	val c = dot (p0,p0) - 1       (*x0*x0 + y0*y0 + z0*z0 - 1.0*)
	val _ = print ("c is:"^(Real.toString c)^"\n")
	val disc = b*b - 4.0*a* c
	val _ = print ("disc is:"^(Real.toString disc)^"\n")

	fun intersect t = (x0 + t*dx,
			   y0 + t*dy,
			   z0 + t*dz)

	fun l2info p =
	  let
	    val hit = apply (M,p)
	    val dist = distance (orig,hit)
	  in {hit = hit,dist = dist}
	  end

	fun l3info ({hit,dist}) =
	  let
	    val u = 0.0
	    val v = 0.0
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

	fun level3 level2 () = map l3info (level2 ())

	fun NO () = (false,fn () => [],fn () => [])

	fun YES ps =
	  let
	    val get2 = level2 (ref [],ps)
	    val get3 = level3 get2
	  in (true,get2,get3)
	  end
      in
	if disc < 0.0 then
	  NO ()
	else if Real.==(disc, 0.0) then  (*Damn round off error...*)
	  let
	    val t = ~b / (2.0*a)
	  in if t < 0 then NO ()
	     else YES [intersect t]
	  end
	else
	  let
	    val d = Math.sqrt disc
	    val t0 = (~b + d) / (2.0*a)
	    val t1 = (~b - d) / (2.0*a)
	  in
	    if t0 < 0.0 then
	      if t1 < 0.0 then NO
	      else YES [intersect t1]
	    else if t1 < 0.0 then
	      if t0 < 0.0 then NO
	      else YES [intersect t0]
	    else YES [intersect t0,intersect t1]
	  end
      end
(*
fun sphere (p0 as (x0,y0,z0):v3,p1 as (x1,y1,z1):v3) : result =
      let
	val pd as (dx,dy,dz) = diff (p1,p0)
	val a = dx*dx + dy*dy + dz*dz (*If unit length, this could be 1*)
	val _ = print ("a is:"^(Real.toString a)^"\n")
	val b = 2.0 * (x0*dx + y0*dy + z0*dz)
	val _ = print ("b is:"^(Real.toString b)^"\n")
	val c = x0*x0 + y0*y0 + z0*z0 - 1.0
	val _ = print ("c is:"^(Real.toString c)^"\n")
	val disc = b*b - 4.0*a* c
	val _ = print ("disc is:"^(Real.toString disc)^"\n")
	fun hit t = (x0 + t*dx,
		     y0 + t*dy,
		     z0 + t*dz)
      in
	if disc < 0.0 then
	  ZERO
	else if Real.==(disc, 0.0) then  (*Damn round off error...*)
	  let
	    val t = ~b / (2.0*a)
	  in if t < 0 then ZERO
	     else ONE (hit t)
	  end
	else
	  let
	    val d = Math.sqrt disc
	    val t0 = (~b + d) / (2.0*a)
	    val t1 = (~b - d) / (2.0*a)
	  in
	    if t0 < 0.0 then
	      if t1 < 0.0 then ZERO
	      else ONE (hit t1)
	    else if t1 < 0.0 then
	      if t0 < 0.0 then ZERO
	      else ONE (hit t0)
	    else TWO (hit t0,hit t1)
	  end
      end

fun sphere2 (p0 as (x0,y0,z0):v3,p1 as (x1,y1,z1):v3) : result =
  let
    val l_oc = Math.sqrt(x0*x0 + y0*y0 + z0*z0)
    val t_ca = ~x0*(x1-x0) - y0*(y1-y0) - z0*(z1-z0)
  in
    if t_ca < 0.0 then ZERO
    else
      let
	val d = Math.sqrt
*)
