signature VECT = 
    sig

	type v3 =  real * real * real
 	type v4 =  real * real * real * real

	val v3tov4 : v3 -> v4
	val v4tov3 : v4 -> v3

	val negate : v3 -> v3
	val add : v3 * v3 -> v3
	val diff : v3 * v3 -> v3
	val mult : v3 * v3 -> v3

	val scale : real * v3 -> v3
	val magnitude : v3 -> real
	val distance : v3 * v3 -> real
	val makeDir : v3 * v3 -> v3  (* A unit vector from A to B *)
	val normalize : v3 -> v3

	val angle : v3 * v3 -> real  (* Angle between vectors in degrees *)
	val halfway : v3 * v3 -> v3  (* given two unit vector a and b, compute the middle unit vector c *)
	val reverseHalfway : v3 * v3 -> v3  (* given two unit vectors a and c, compute unit vector b so c is in the middle *)
	val dp : v3 * v3 -> real

	val rad2deg : real -> real
	val deg2rad: real -> real

    end
