(*$import Firstlude TiltPrim Prelude *)

val f	: int * int option -> unit
	= fn (_, x) => (case x
			  of NONE => ()
			   | _ => raise Subscript)

val g	: ('a -> unit) -> 'a -> unit
	= fn x => fn y => x y

val _	: unit
	= g f (0, NONE)	(* raises Subscript *)
