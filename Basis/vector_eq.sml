(* Vector equality; see ../Elaborator/equal.sml.  *)

structure TiltVectorEq =
struct
    fun vector_eq (equaler : 'a * 'a -> bool)
		  (x : 'a vector, y : 'a vector) : bool =
	let val lx = TiltPrim.vector_length x
	    val ly = TiltPrim.vector_length y
	    fun vector_eq_loop n =
		TiltPrim.ugte (n, lx) orelse
		let val ax = TiltPrim.unsafe_vsub(x,n)
		    val ay = TiltPrim.unsafe_vsub(y,n)
		in  equaler(ax,ay) andalso vector_eq_loop(TiltPrim.uplus(n, 0w1))
		end
	in  TiltPrim.ueq (lx, ly) andalso vector_eq_loop 0w0
	end
end
