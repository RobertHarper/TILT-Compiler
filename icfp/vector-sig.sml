signature VECT = 
    sig

	type v3 =  real * real * real
 	type v4 =  real * real * real * real

	val halfway : v3 * v3 -> v3
	val scale : real * v3 -> v3
	val dp : v3 * v3 -> real
	val magnitude : v3 -> real
	val distance : v3 * v3 -> real
	val makeDir : v3 * v3 -> v3  (* A unit vector from A to B *)
	val angle : v3 * v3 -> real  (* Angle between vectors in degrees *)
	val negate : v3 -> v3
	val normalize : v3 -> v3

	val add : v3 * v3 -> v3
	val mult : v3 * v3 -> v3

    end
