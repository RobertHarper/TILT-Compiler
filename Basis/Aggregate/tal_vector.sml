(* vector.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Vector :> VECTOR where type 'a vector = 'a vector =
  struct

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32

    val empty_vector = TiltPrim.empty_vector
    val unsafe_array = TiltPrim.unsafe_array
    val unsafe_update = TiltPrim.unsafe_update
    val unsafe_vsub = TiltPrim.unsafe_vsub
    val vector_length = TiltPrim.vector_length

    val unsafe_array2vector = TiltPrim.unsafe_array2vector
    val unsafe_vector2array = TiltPrim.unsafe_vector2array

    val ugt = TiltPrim.ugt
    val ugte = TiltPrim.ugte
    val ult = TiltPrim.ult

    val uminus = TiltPrim.uminus
    val uplus = TiltPrim.uplus

    val maxLen = PreVector.maxLen
    val checkLen = PreVector.checkLen
    val fromList' = PreVector.vectorFromList'
    val fromList = PreVector.vectorFromList

    type 'a vector = 'a vector

    val vector0 : 'a vector = empty_vector

    fun tabulate(n,f) = unsafe_array2vector(Array.tabulate(n,f))
    fun length (a : 'a vector) : int = uint32toint32(vector_length a)

    fun sub (a : 'a vector, index :int) : 'a =
	let val index = int32touint32 index
	in unsafe_vsub(a,index)
	end


  (* a utility function *)
    fun rev ([], l) = l
      | rev (x::r, l) = rev (r, x::l)

    fun extract (v : 'a vector, base : int, optLen : int option) =
	Array.extract (unsafe_vector2array v, base, optLen)

    fun concat [v] = v
      | concat vl = let
	(* get the total length and flatten the list *)
	  fun len ([], n, l) = (checkLen n; (n, rev(l, [])))
	    | len (v::r, n, l) = let
		val n' = length v
		fun explode (i, l) = if (i < n')
		      then explode(i+1, unsafe_vsub(v, int32touint32 i)::l)
		      else l
		in
		  len (r, n + n', explode(0, l))
		end
	  in
	    case len (vl, 0, [])
	     of (0, _) => vector0
	      | (n, l) => fromList'(n, l)
	    (* end case *)
	  end

    fun map f vec = let
          val len = vector_length vec
          fun mapf (i, l) = if ult(i,len)
                then mapf (uplus(i,0w1), f (unsafe_vsub(vec, i)) :: l)
                else fromList'(uint32toint32 len, rev(l, []))
          in
            if ugt(len,0w0)
              then mapf (0w0, [])
              else vector0
          end

    fun chkSlice (vec, i, NONE) = let val len = length vec
	  in
	    if (len < i)
	      then raise Subscript
	      else (vec, int32touint32 i, int32touint32 len)
	  end
      | chkSlice (vec, i, SOME n) = let val len = length vec
	  in
	    if ((0 <= i) andalso (0 <= n) andalso (i+n <= len))
	      then (vec, int32touint32 i, int32touint32(i+n))
	      else raise Subscript
	  end
     fun mapi f slice = let
          val (vec, start, stop) = chkSlice slice
          val len = uminus(stop,start)
          fun mapf (i, l) = if ult(i,stop)
                then mapf (uplus(i,0w1), f (uint32toint32 i, unsafe_vsub(vec, i)) :: l)
                else fromList'(uint32toint32 len, rev(l, []))
          in
            if ugt(len,0w0)
              then mapf (start, [])
              else vector0
          end

    fun app f vec = Array.app f (unsafe_vector2array vec)
    fun foldl f init vec = Array.foldl f init (unsafe_vector2array vec)
    fun foldr f init vec = Array.foldr f init (unsafe_vector2array vec)
    fun appi f (vec,start,len) = Array.appi f (unsafe_vector2array vec,start,len)
    fun foldli f acc (vec,start,len) = Array.foldli f acc (unsafe_vector2array vec,start,len)
    fun foldri f acc (vec,start,len) = Array.foldri f acc (unsafe_vector2array vec,start,len)




  end  (* Vector *)


