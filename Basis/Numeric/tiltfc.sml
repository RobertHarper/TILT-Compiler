(*
	The corresponding runtime functions should probably throw
	SysErr for unsupported modes.

	The basis library assumes all rounding modes are supported but
	only uses the default precision and DOUBLE.
*)
structure TiltFc =
struct

	datatype rounding_mode =
		TO_NEAREST
	|	TO_NEGINF
	|	TO_POSINF
	|	TO_ZERO

	datatype precision =
		SINGLE
	|	DOUBLE
	|	EXTENDED

	fun ccall2 (f : ('a, 'b, 'c cresult) -->, a:'a, b:'b) : 'c =
		(case (Ccall(f,a,b)) of
			Normal r => r
		|	Error e => raise e)

	(* Must agree with the runtime. *)
	val getfc : unit -> rounding_mode * precision =
		fn () =>
		let	val (r,p) = Ccall(getfc,())
			val p =
				(case p of
					0 => SINGLE
				|	1 => DOUBLE
				|	2 => EXTENDED)
			val r =
				(case r of
					0 => TO_NEAREST
				|	1 => TO_ZERO
				|	2 => TO_POSINF
				|	3 => TO_NEGINF)
		in	(r,p)
		end

	(* Must agree with the runtime. *)
	val setfc : rounding_mode * precision -> unit =
		fn (r,p) =>
		let	val p =
				(case p of
					SINGLE => 0
				|	DOUBLE => 1
				|	EXTENDED => 2)
			val r =
				(case r of
					TO_NEAREST => 0
				|	TO_ZERO => 1
				|	TO_POSINF => 2
				|	TO_NEGINF => 3)
		in	ccall2(setfc,r,p)
		end

end
