structure Vect : VECT = 
    struct

	type v3 =  real * real * real
 	type v4 =  real * real * real * real

	val halfway : v3 * v3 -> v3 = fn _ => raise Div
	val scale : real * v3 -> v3 = fn _ => raise Div
	val dp : v3 * v3 -> real = fn _ => raise Div
	val magnitude : v3 -> real = fn _ => raise Div
	val distance : v3 * v3 -> real = fn _ => raise Div
	val makeDir : v3 * v3 -> v3  (* A unit vector from A to B *) = fn _ => raise Div
	val angle : v3 * v3 -> real  (* Angle between vectors in degrees *) = fn _ => raise Div
	val negate : v3 -> v3 = fn _ => raise Div
	val normalize : v3 -> v3 = fn _ => raise Div

	val add : v3 * v3 -> v3 = fn _ => raise Div
	val mult : v3 * v3 -> v3 = fn _ => raise Div

    end
