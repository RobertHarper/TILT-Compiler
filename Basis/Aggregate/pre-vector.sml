(*$import Firstlude TiltPrim Prelude *)

structure PreVector :
    sig
	val maxLen : int
	val checkLen : int -> unit
	val arrayFromList' : int * 'a list -> 'a array (* known length *)
	val arrayFromList : 'a list -> 'a array
	val vectorFromList' : int * 'a list -> 'a vector (* known length *)
	val vectorFromList : 'a list -> 'a vector
    end =
struct

    fun list_length l =
	let
	    fun len ([], n) = n
	      | len ([_], n) = n+1
	      | len (_::_::r, n) = len(r, n+2)
	in  len (l, 0)
	end
    fun list_hd' (h :: _) = h		(* list_hd' nil does not behave like List.hd nil *)
	    
    val maxLen = 1024 * 1024
    fun checkLen n = if maxLen < n then raise Size else ()
    
    fun arrayFromList'(n,l) = 
	let val _ = checkLen n
	in
	    if (n = 0)
		then TiltPrim.empty_array
	    else let val e = list_hd' l
		     val ar = TiltPrim.unsafe_array(TiltPrim.int32touint32 n, e)
		     fun loop [] _ = ()
		       | loop (a::b) n = (TiltPrim.unsafe_update(ar,n,a);
					  loop b (TiltPrim.uplus(n,0w1)))
		     val _ = loop l 0w0
		 in  ar
		 end
	end

    fun arrayFromList l = arrayFromList' (list_length l, l)

    fun vectorFromList' arg = TiltPrim.unsafe_array2vector(arrayFromList' arg)

    fun vectorFromList arg = TiltPrim.unsafe_array2vector(arrayFromList arg)
end
