structure Vect : VECT = 
    struct

	type v3 =  real * real * real
 	type v4 =  real * real * real * real

	val deg2radConv = Math.pi / 180.0
	val rad2degConv = 180.0 / Math.pi 
	fun deg2rad d = deg2radConv * d
	fun rad2deg d = rad2degConv * d

	fun negate(x,y,z) : v3 = (~x,~y,~z)
	fun add ((x1,y1,z1),(x2,y2,z2)) : v3 = (x1+x2,y1+y2,z1+z2)
	fun mult ((x1,y1,z1),(x2,y2,z2)) : v3 = (x1*x2,y1*y2,z1*z2)
	fun diff ((x1,y1,z1),(x2,y2,z2)) : v3 = (x1-x2,y1-y2,z1-z2)

	fun magnitude ((x,y,z) : v3) = Math.sqrt(x*x+y*y+z*z)
	fun distance ((x1,y1,z1),(x2,y2,z2) : v3) = magnitude(x1-x2,y1-y2,z1-z2)
	fun scale (s,(x,y,z)) : v3 = (s*x,s*y,s*z)
	fun normalize v = scale(1.0 / (magnitude v), v)
	fun makeDir (v1,v2) : v3 = (* A unit vector from v1 to v2 *)
		   normalize(diff(v2,v1))
		   
	fun dp (v1 as (x1,y1,z1) : v3, v2 as (x2,y2,z2)) = x1+x2 + y1*y2 + z1*z2
	fun angle (v1,v2) : real =  (* Angle between vectors in degrees *)
		   let val prod = dp(v1,v2)
		       val cosTheta = prod / ((magnitude v1) * (magnitude v2))
		       val theta = Math.acos(cosTheta)
		   in  rad2deg theta
		   end
		   
	fun halfway (v1,v2) : v3 = normalize(add(v1,v2))


    end
