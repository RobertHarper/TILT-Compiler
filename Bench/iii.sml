
(* This is a compiling benchmark, not a running benchmark. 

   This should *not* take forever to compile (see Shao, "Implementing
	Typed Intermediate Languages"), but it seems to under both NJ 110.0.6 and
	TILT!

*)

fun I x = x

fun f x = I I I I I I I I I I I I I I I I I I I x

val _ = f 0