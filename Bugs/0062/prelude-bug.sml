(*$import *)
infixr 5 ::
infix  4 =
datatype 'a list = nil | :: of 'a * 'a list

(* vector_eq needed so we can use vector types: may change if elaborator changes *)
structure TiltVectorEq =
struct
    fun vector_eq (equaler : 'a * 'a -> bool) (x : 'a vector, y : 'a vector) : bool = 
	let val lx = TiltPrim.vector_length x
	    val ly = TiltPrim.vector_length y
	    fun vector_eq_loop n =
		TiltPrim.ugte (n, lx) orelse (equaler(TiltPrim.unsafe_vsub(x,n),TiltPrim.unsafe_vsub(y,n))
				andalso (vector_eq_loop (TiltPrim.uplus (n, 0w1))))
	in  (lx = ly) andalso vector_eq_loop 0w0
	end
end

type string = char vector
