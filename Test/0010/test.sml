(*
	The pattern compiler was comparing ch with 0w0
	using the 32-bit comparison primitive.  The Nil
	typechecker caught the error.
*)
fun f (ch : char) : unit = (case ch of 0w0 => () | _ => ())
