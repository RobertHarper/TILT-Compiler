(*
	The compiler should reject this code because Array.Vector does
	not exist.  In the past, it bombed.
*)

functor MonoArrayFn (type elem) :>
sig
	type array
	structure Vector :
	sig
		type vector
	end
	val extract  : array * int * int option -> Vector.vector
end =
struct
	type array = elem Array.array
	structure Vector = Array.Vector
	val extract = Array.extract
end
