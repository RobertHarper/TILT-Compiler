(*
	Tilt should reject the shadow_list declaration.
	See p.12 in the Definition.
*)
val empty : int list = nil
datatype shadow_list = :: of int | nil of unit
fun f [a,b,c] = 0 | f _ = 1
val one = f empty
